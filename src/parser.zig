const std = @import("std");

const Ast = @import("Ast.zig");
const Diagnostic = @import("Diagnostic.zig");
const akos = @import("akos.zig");
const awiz = @import("awiz.zig");
const BlockId = @import("block_id.zig").BlockId;
const parseBlockId = @import("block_id.zig").parseBlockId;
const lexer = @import("lexer.zig");

const State = struct {
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForTextFile,
    source: []const u8,
    lex: *const lexer.Lex,
    token_index: u32,
    result: Ast,
};

const ParseError = error{ OutOfMemory, AddedToDiagnostic };

pub fn parseProject(
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForTextFile,
    source: []const u8,
    lex: *const lexer.Lex,
) ParseError!Ast {
    var state: State = .{
        .gpa = gpa,
        .diag = diag,
        .source = source,
        .lex = lex,
        .token_index = 0,
        .result = .{
            .root = undefined, // filled in at the end
            .nodes = .empty,
            .extra = .empty,
        },
    };
    errdefer state.result.deinit(gpa);

    state.result.root = try parseProjectChildren(&state);
    return state.result;
}

fn parseProjectChildren(state: *State) !Ast.NodeIndex {
    const Keyword = enum {
        index,
        disk,
        @"var",
    };

    var index_children_opt: ?std.BoundedArray(Ast.NodeIndex, 16) = null;
    var disks: [2]Ast.NodeIndex = @splat(Ast.null_node);
    var variables: std.BoundedArray(u32, 768) = .{};

    while (true) {
        skipWhitespace(state);
        const token = consumeToken(state);
        switch (token.kind) {
            .identifier => switch (try parseIdentifier(state, token, Keyword)) {
                .index => {
                    if (index_children_opt != null)
                        return reportError(state, token.span, "duplicate index", .{});
                    index_children_opt = try parseIndex(state);
                },
                .disk => {
                    try parseDisk(state, token.span, &disks);
                },
                .@"var" => {
                    const node = try parseVar(state, token.span);
                    try appendNode(state, token.span, &variables, node);
                },
            },
            .eof => break,
            else => return reportUnexpected(state, token),
        }
    }

    const index_children = index_children_opt orelse
        return reportError(state, .{ .start = .origin, .end = .origin }, "missing index", .{});
    const index_extra = try storeExtra(state, index_children.slice());
    const disks_extra = try storeExtra(state, &disks);
    const variables_extra = try storeExtra(state, variables.slice());

    return storeNode(state, .{ .project = .{
        .index = index_extra,
        .disks = disks_extra,
        .variables = variables_extra,
    } });
}

fn parseIndex(state: *State) !std.BoundedArray(Ast.NodeIndex, 16) {
    const Keyword = enum {
        @"raw-block",
        @"index-block",
    };

    try expect(state, .brace_l);

    var children: std.BoundedArray(Ast.NodeIndex, 16) = .{};

    while (true) {
        skipWhitespace(state);
        const token = consumeToken(state);
        switch (token.kind) {
            .identifier => switch (try parseIdentifier(state, token, Keyword)) {
                .@"raw-block" => {
                    const node_index = try parseRawBlock(state, token.span);
                    try appendNode(state, token.span, &children, node_index);
                },
                .@"index-block" => {
                    const block_id_str = try expectString(state);
                    try expect(state, .newline);
                    const IndexBlock = @FieldType(Ast.Node, "index_block");
                    const block_id = std.meta.stringToEnum(IndexBlock, block_id_str) orelse
                        return reportError(state, token.span, "unsupported block id", .{});
                    const index_block = try storeNode(state, .{ .index_block = block_id });
                    try appendNode(state, token.span, &children, index_block);
                },
            },
            .brace_r => break,
            else => return reportUnexpected(state, token),
        }
    }

    return children;
}

fn parseDisk(state: *State, span: lexer.Span, disks: *[2]Ast.NodeIndex) !void {
    const Keyword = enum {
        @"raw-block",
        room,
    };

    const disk_number = try expectInteger(state);
    try expect(state, .brace_l);

    if (!(1 <= disk_number and disk_number <= disks.len))
        return reportError(state, span, "disk number out of range", .{});
    const disk_index: u8 = @intCast(disk_number - 1);
    if (disks[disk_index] != Ast.null_node)
        return reportError(state, span, "duplicate disk number", .{});

    var children: std.BoundedArray(Ast.NodeIndex, 32) = .{};

    while (true) {
        skipWhitespace(state);
        const token = consumeToken(state);
        switch (token.kind) {
            .identifier => switch (try parseIdentifier(state, token, Keyword)) {
                .@"raw-block" => {
                    const node_index = try parseRawBlock(state, token.span);
                    try appendNode(state, token.span, &children, node_index);
                },
                .room => {
                    const node_index = try parseDiskRoom(state, token.span);
                    try appendNode(state, token.span, &children, node_index);
                },
            },
            .brace_r => break,
            else => return reportUnexpected(state, token),
        }
    }

    disks[disk_index] = try storeNode(state, .{ .disk = .{
        .children = try storeExtra(state, children.slice()),
    } });
}

fn parseDiskRoom(state: *State, span: lexer.Span) !Ast.NodeIndex {
    const room_number_i32 = try expectInteger(state);
    const room_name = try expectString(state);
    const path = try expectString(state);
    try expect(state, .newline);

    if (!(1 <= room_number_i32 and room_number_i32 <= 255))
        return reportError(state, span, "room number out of range", .{});
    const room_number: u8 = @intCast(room_number_i32);
    if (!(1 <= room_name.len and room_name.len <= Ast.max_room_name_len))
        return reportError(state, span, "invalid room name", .{});

    return storeNode(state, .{ .disk_room = .{
        .room_number = room_number,
        .name = room_name,
        .path = path,
    } });
}

fn parseVar(state: *State, span: lexer.Span) !Ast.NodeIndex {
    const name = try expectIdentifier(state);
    try expect(state, .swat);
    const number_i32 = try expectInteger(state);
    try expect(state, .newline);

    const number = std.math.cast(u16, number_i32) orelse
        return reportError(state, span, "out of range", .{});

    return storeNode(state, .{ .variable = .{
        .name = name,
        .number = number,
    } });
}

pub fn parseRoom(
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForTextFile,
    source: []const u8,
    lex: *const lexer.Lex,
) ParseError!Ast {
    var state: State = .{
        .gpa = gpa,
        .diag = diag,
        .source = source,
        .lex = lex,
        .token_index = 0,
        .result = .{
            .root = undefined, // filled in at the end
            .nodes = .empty,
            .extra = .empty,
        },
    };
    errdefer state.result.deinit(gpa);

    state.result.root = try parseRoomChildren(&state);
    return state.result;
}

fn parseRoomChildren(state: *State) !Ast.NodeIndex {
    const Keyword = enum {
        @"raw-block",
        @"raw-glob",
        rmim,
        rmda,
        scrp,
        encd,
        excd,
        lscr,
        awiz,
        mult,
        akos,
        @"var",
        script,
        @"local-script",
        enter,
        exit,
    };

    var children: std.BoundedArray(Ast.NodeIndex, 5120) = .{};
    var variables: std.BoundedArray(Ast.NodeIndex, 160) = .{};

    while (true) {
        skipWhitespace(state);
        const token = consumeToken(state);
        switch (token.kind) {
            .identifier => switch (try parseIdentifier(state, token, Keyword)) {
                .@"raw-block" => {
                    const node_index = try parseRawBlock(state, token.span);
                    try appendNode(state, token.span, &children, node_index);
                },
                .@"raw-glob" => {
                    const node_index = try parseRawGlob(state, token.span);
                    try appendNode(state, token.span, &children, node_index);
                },
                .rmim => {
                    const compression_i32 = try expectInteger(state);
                    const path = try expectString(state);
                    try expect(state, .newline);

                    const compression = std.math.cast(u8, compression_i32) orelse
                        return reportError(state, token.span, "out of range", .{});

                    const node_index = try storeNode(state, .{ .rmim = .{
                        .compression = compression,
                        .path = path,
                    } });
                    try appendNode(state, token.span, &children, node_index);
                },
                .rmda => {
                    const node_index = try parseRmda(state);
                    try appendNode(state, token.span, &children, node_index);
                },
                .scrp => {
                    const glob_number_i32 = try expectInteger(state);
                    const path = try expectString(state);
                    try expect(state, .newline);

                    const glob_number = std.math.cast(u16, glob_number_i32) orelse
                        return reportError(state, token.span, "invalid glob number", .{});

                    const node_index = try storeNode(state, .{ .scrp = .{
                        .glob_number = glob_number,
                        .path = path,
                    } });
                    try appendNode(state, token.span, &children, node_index);
                },
                .encd => {
                    const path = try expectString(state);
                    try expect(state, .newline);

                    const node_index = try storeNode(state, .{ .encd = .{ .path = path } });
                    try appendNode(state, token.span, &children, node_index);
                },
                .excd => {
                    const path = try expectString(state);
                    try expect(state, .newline);

                    const node_index = try storeNode(state, .{ .excd = .{ .path = path } });
                    try appendNode(state, token.span, &children, node_index);
                },
                .lscr => {
                    const script_number_i32 = try expectInteger(state);
                    const path = try expectString(state);
                    try expect(state, .newline);

                    const script_number = std.math.cast(u16, script_number_i32) orelse
                        return reportError(state, token.span, "invalid script number", .{});

                    const node_index = try storeNode(state, .{ .lscr = .{
                        .script_number = script_number,
                        .path = path,
                    } });
                    try appendNode(state, token.span, &children, node_index);
                },
                .awiz => {
                    const node_index = try parseAwiz(state, token.span);
                    try appendNode(state, token.span, &children, node_index);
                },
                .mult => {
                    const node_index = try parseMult(state, token.span);
                    try appendNode(state, token.span, &children, node_index);
                },
                .akos => {
                    const node_index = try parseAkos(state, token.span);
                    try appendNode(state, token.span, &children, node_index);
                },
                .@"var" => {
                    const node_index = try parseVar(state, token.span);
                    try appendNode(state, token.span, &variables, node_index);
                },
                .script => {
                    const glob_number_i32 = try expectInteger(state);
                    const glob_number = std.math.cast(u16, glob_number_i32) orelse
                        return reportError(state, token.span, "out of range", .{});
                    try expect(state, .brace_l);
                    const node_index = try parseScript(state, glob_number);
                    try appendNode(state, token.span, &children, node_index);
                },
                .@"local-script" => {
                    const script_number_i32 = try expectInteger(state);
                    const script_number = std.math.cast(u16, script_number_i32) orelse
                        return reportError(state, token.span, "out of range", .{});
                    try expect(state, .brace_l);
                    const node_index = try parseLocalScript(state, script_number);
                    try appendNode(state, token.span, &children, node_index);
                },
                .enter => {
                    try expect(state, .brace_l);
                    const statements = try parseScriptBody(state);
                    const node_index = try storeNode(state, .{ .enter = .{
                        .statements = statements,
                    } });
                    try appendNode(state, token.span, &children, node_index);
                },
                .exit => {
                    try expect(state, .brace_l);
                    const statements = try parseScriptBody(state);
                    const node_index = try storeNode(state, .{ .exit = .{
                        .statements = statements,
                    } });
                    try appendNode(state, token.span, &children, node_index);
                },
            },
            .eof => break,
            else => return reportUnexpected(state, token),
        }
    }

    return storeNode(state, .{ .room_file = .{
        .children = try storeExtra(state, children.slice()),
        .variables = try storeExtra(state, variables.slice()),
    } });
}

fn parseRmda(state: *State) !Ast.NodeIndex {
    const Keyword = enum {
        @"raw-block",
    };

    try expect(state, .brace_l);

    var children: std.BoundedArray(Ast.NodeIndex, 640) = .{};

    while (true) {
        skipWhitespace(state);
        const token = consumeToken(state);
        switch (token.kind) {
            .identifier => switch (try parseIdentifier(state, token, Keyword)) {
                .@"raw-block" => {
                    const node_index = try parseRawBlock(state, token.span);
                    try appendNode(state, token.span, &children, node_index);
                },
            },
            .brace_r => break,
            else => return reportUnexpected(state, token),
        }
    }

    return storeNode(state, .{ .rmda = .{
        .children = try storeExtra(state, children.slice()),
    } });
}

fn parseAwiz(state: *State, span: lexer.Span) !Ast.NodeIndex {
    const glob_number_i32 = try expectInteger(state);
    try expect(state, .brace_l);

    const glob_number = std.math.cast(u16, glob_number_i32) orelse
        return reportError(state, span, "invalid glob number", .{});

    const children = try parseAwizChildren(state);

    return storeNode(state, .{ .awiz = .{
        .glob_number = glob_number,
        .children = children,
    } });
}

fn parseAwizChildren(state: *State) !Ast.ExtraSlice {
    const Keyword = enum {
        rgbs,
        @"two-ints",
        wizh,
        bmp,
    };

    var children: std.BoundedArray(Ast.NodeIndex, awiz.Awiz.max_blocks) = .{};

    while (true) {
        skipWhitespace(state);
        const token = consumeToken(state);
        switch (token.kind) {
            .identifier => switch (try parseIdentifier(state, token, Keyword)) {
                .rgbs => {
                    try expect(state, .newline);

                    const node_index = try storeNode(state, .awiz_rgbs);
                    try appendNode(state, token.span, &children, node_index);
                },
                .@"two-ints" => {
                    const block_id_str = try expectString(state);
                    const ints = .{ try expectInteger(state), try expectInteger(state) };
                    try expect(state, .newline);

                    const block_id = parseBlockId(block_id_str) orelse
                        return reportError(state, token.span, "invalid block id", .{});

                    const node_index = try storeNode(state, .{ .awiz_two_ints = .{
                        .block_id = block_id,
                        .ints = ints,
                    } });
                    try appendNode(state, token.span, &children, node_index);
                },
                .wizh => {
                    try expect(state, .newline);

                    const node_index = try storeNode(state, .awiz_wizh);
                    try appendNode(state, token.span, &children, node_index);
                },
                .bmp => {
                    const compression_int = try expectInteger(state);
                    const path = try expectString(state);
                    try expect(state, .newline);

                    const compression = std.meta.intToEnum(awiz.Compression, compression_int) catch
                        return reportError(state, token.span, "invalid compression", .{});

                    const node_index = try storeNode(state, .{ .awiz_bmp = .{
                        .compression = compression,
                        .path = path,
                    } });
                    try appendNode(state, token.span, &children, node_index);
                },
            },
            .brace_r => break,
            else => return reportUnexpected(state, token),
        }
    }

    return storeExtra(state, children.slice());
}

fn parseMult(state: *State, span: lexer.Span) !Ast.NodeIndex {
    const Keyword = enum {
        @"raw-block",
        awiz,
        indices,
    };

    const glob_number_i32 = try expectInteger(state);
    try expect(state, .brace_l);

    const glob_number = std.math.cast(u16, glob_number_i32) orelse
        return reportError(state, span, "invalid glob number", .{});

    var raw_block = Ast.null_node;
    var children: std.BoundedArray(Ast.NodeIndex, Ast.max_mult_children) = .{};
    var indices_opt: ?Ast.ExtraSlice = null;

    while (true) {
        skipWhitespace(state);
        const token = consumeToken(state);
        switch (token.kind) {
            .identifier => switch (try parseIdentifier(state, token, Keyword)) {
                .@"raw-block" => {
                    if (raw_block != Ast.null_node)
                        return reportError(state, token.span, "too many children", .{});
                    raw_block = try parseRawBlockNested(state, token.span);
                },
                .awiz => {
                    try expect(state, .brace_l);
                    const awiz_children = try parseAwizChildren(state);

                    const node_index = try storeNode(state, .{ .mult_awiz = .{
                        .children = awiz_children,
                    } });
                    try appendNode(state, token.span, &children, node_index);
                },
                .indices => {
                    if (indices_opt != null)
                        return reportError(state, token.span, "too many children", .{});
                    try expect(state, .bracket_l);
                    indices_opt = try parseIntegerList(state);
                },
            },
            .brace_r => break,
            else => return reportUnexpected(state, token),
        }
    }

    const indices = indices_opt orelse return reportError(state, span, "missing indices", .{});
    for (state.result.getExtra(indices)) |index|
        if (index >= children.len)
            return reportError(state, span, "out of range", .{});

    return storeNode(state, .{ .mult = .{
        .glob_number = glob_number,
        .raw_block = raw_block,
        .children = try storeExtra(state, children.slice()),
        .indices = indices,
    } });
}

fn parseAkos(state: *State, span: lexer.Span) !Ast.NodeIndex {
    const Keyword = enum {
        @"raw-block",
        akpl,
        akcd,
    };

    const glob_number_i32 = try expectInteger(state);
    try expect(state, .brace_l);

    const glob_number = std.math.cast(u16, glob_number_i32) orelse
        return reportError(state, span, "invalid glob number", .{});

    var children: std.BoundedArray(Ast.NodeIndex, 1536) = .{};

    while (true) {
        skipWhitespace(state);
        const token = consumeToken(state);
        switch (token.kind) {
            .identifier => switch (try parseIdentifier(state, token, Keyword)) {
                .@"raw-block" => {
                    const node_index = try parseRawBlock(state, token.span);
                    try appendNode(state, token.span, &children, node_index);
                },
                .akpl => {
                    const path = try expectString(state);
                    try expect(state, .newline);

                    const node_index = try storeNode(state, .{ .akpl = .{
                        .path = path,
                    } });
                    try appendNode(state, token.span, &children, node_index);
                },
                .akcd => {
                    const codec_token = consumeToken(state);
                    const path = try expectString(state);
                    try expect(state, .newline);

                    // XXX: a bit janky, this
                    const CompressionKeyword = enum(u8) {
                        byle = @intFromEnum(akos.CompressionCodec.byle_rle),
                        trle = @intFromEnum(akos.CompressionCodec.trle),
                    };
                    const compression_keyword = try parseIdentifier(state, codec_token, CompressionKeyword);
                    const compression: akos.CompressionCodec = @enumFromInt(@intFromEnum(compression_keyword));

                    const node_index = try storeNode(state, .{ .akcd = .{
                        .compression = compression,
                        .path = path,
                    } });
                    try appendNode(state, token.span, &children, node_index);
                },
            },
            .brace_r => break,
            else => return reportUnexpected(state, token),
        }
    }

    return storeNode(state, .{ .akos = .{
        .glob_number = glob_number,
        .children = try storeExtra(state, children.slice()),
    } });
}

fn parseIntegerList(state: *State) !Ast.ExtraSlice {
    var result: std.BoundedArray(u32, 256) = .{};
    while (true) {
        var token = consumeToken(state);
        switch (token.kind) {
            .integer => {
                const source = state.source[token.span.start.offset..token.span.end.offset];
                const int = std.fmt.parseInt(u32, source, 10) catch
                    return reportError(state, token.span, "invalid integer", .{});
                try appendNode(state, token.span, &result, int);
            },
            .bracket_r => break,
            else => return reportUnexpected(state, token),
        }

        token = consumeToken(state);
        switch (token.kind) {
            .comma => {},
            .bracket_r => break,
            else => return reportUnexpected(state, token),
        }
    }
    return storeExtra(state, result.slice());
}

fn parseRawBlock(state: *State, span: lexer.Span) !Ast.NodeIndex {
    const block_id_str = try expectString(state);
    const path = try expectString(state);
    try expect(state, .newline);

    const block_id = parseBlockId(block_id_str) orelse
        return reportError(state, span, "invalid block id", .{});

    return storeNode(state, .{ .raw_block = .{
        .block_id = block_id,
        .path = path,
    } });
}

fn parseRawBlockNested(state: *State, span: lexer.Span) !Ast.NodeIndex {
    const Keyword = enum {
        @"raw-block",
    };

    const block_id_str = try expectString(state);
    try expect(state, .brace_l);

    const block_id = parseBlockId(block_id_str) orelse
        return reportError(state, span, "invalid block id", .{});

    var children: std.BoundedArray(Ast.NodeIndex, 4) = .{};

    while (true) {
        skipWhitespace(state);
        const token = consumeToken(state);
        switch (token.kind) {
            .identifier => switch (try parseIdentifier(state, token, Keyword)) {
                .@"raw-block" => {
                    const node_index = try parseRawBlock(state, token.span);
                    try appendNode(state, token.span, &children, node_index);
                },
            },
            .brace_r => break,
            else => return reportUnexpected(state, token),
        }
    }

    return storeNode(state, .{ .raw_block_nested = .{
        .block_id = block_id,
        .children = try storeExtra(state, children.slice()),
    } });
}

fn parseRawGlob(state: *State, span: lexer.Span) !Ast.NodeIndex {
    const block_id_str = try expectString(state);
    const glob_number_i32 = try expectInteger(state);

    const block_id = parseBlockId(block_id_str) orelse
        return reportError(state, span, "invalid block id", .{});
    const glob_number = std.math.cast(u16, glob_number_i32) orelse
        return reportError(state, span, "invalid glob number", .{});

    const contents = consumeToken(state);
    return switch (contents.kind) {
        .string => parseRawGlobFile(state, block_id, glob_number, contents),
        .brace_l => parseRawGlobBlock(state, block_id, glob_number),
        else => reportUnexpected(state, contents),
    };
}

fn parseRawGlobFile(
    state: *State,
    block_id: BlockId,
    glob_number: u16,
    path_token: *const lexer.Token,
) !Ast.NodeIndex {
    try expect(state, .newline);

    const path_source = state.source[path_token.span.start.offset..path_token.span.end.offset];
    const path = path_source[1 .. path_source.len - 1];

    return storeNode(state, .{ .raw_glob_file = .{
        .block_id = block_id,
        .glob_number = glob_number,
        .path = path,
    } });
}

fn parseRawGlobBlock(state: *State, block_id: BlockId, glob_number: u16) !Ast.NodeIndex {
    const Keyword = enum {
        @"raw-block",
    };

    var children: std.BoundedArray(Ast.NodeIndex, 640) = .{};

    while (true) {
        skipWhitespace(state);
        const token = consumeToken(state);
        switch (token.kind) {
            .identifier => switch (try parseIdentifier(state, token, Keyword)) {
                .@"raw-block" => {
                    const node_index = try parseRawBlock(state, token.span);
                    try appendNode(state, token.span, &children, node_index);
                },
            },
            .brace_r => break,
            else => return reportUnexpected(state, token),
        }
    }

    return storeNode(state, .{ .raw_glob_block = .{
        .block_id = block_id,
        .glob_number = glob_number,
        .children = try storeExtra(state, children.slice()),
    } });
}

fn parseScript(state: *State, glob_number: u16) !Ast.NodeIndex {
    const statements = try parseScriptBody(state);
    return try storeNode(state, .{ .script = .{
        .glob_number = glob_number,
        .statements = statements,
    } });
}

fn parseLocalScript(state: *State, script_number: u16) !Ast.NodeIndex {
    const statements = try parseScriptBody(state);
    return try storeNode(state, .{ .local_script = .{
        .script_number = script_number,
        .statements = statements,
    } });
}

fn parseScriptBody(state: *State) !Ast.ExtraSlice {
    var statements: std.BoundedArray(Ast.NodeIndex, 256) = .{};
    while (true) {
        skipWhitespace(state);
        const token = consumeToken(state);
        if (token.kind == .brace_r) break;
        const node = try parseStatement(state, token);
        try appendNode(state, token.span, &statements, node);
    }
    return try storeExtra(state, statements.slice());
}

fn parseStatement(state: *State, token: *const lexer.Token) !Ast.NodeIndex {
    // Check for labels
    if (token.kind == .identifier) {
        const next = peekToken(state);
        if (next.kind == .colon) {
            _ = consumeToken(state);
            const identifier = state.source[token.span.start.offset..token.span.end.offset];
            return storeNode(state, .{ .label = identifier });
        }
    }

    const ei = try parseExpr(state, token, .all);
    const expr = &state.result.nodes.items[ei];
    if (expr.* == .identifier) {
        // A lonely identifier is a call with 0 args
        return storeNode(state, .{ .call = .{ .callee = ei, .args = .empty } });
    }
    return ei;
}

pub const Precedence = enum {
    all,
    space,
    field,
};

fn parseExpr(state: *State, first: *const lexer.Token, prec: Precedence) ParseError!Ast.NodeIndex {
    var cur = try parseAtom(state, first);
    while (true) {
        const token = peekToken(state);
        switch (token.kind) {
            .period => {
                if (@intFromEnum(prec) >= @intFromEnum(Precedence.field)) break;
                _ = consumeToken(state);
                const field = try expectIdentifier(state);
                cur = try storeNode(state, .{ .field = .{ .lhs = cur, .field = field } });
            },
            .integer, .identifier => {
                if (@intFromEnum(prec) >= @intFromEnum(Precedence.space)) break;
                const args = try parseArgs(state);
                cur = try storeNode(state, .{ .call = .{ .callee = cur, .args = args } });
            },
            else => break,
        }
    }
    return cur;
}

fn parseAtom(state: *State, token: *const lexer.Token) !Ast.NodeIndex {
    switch (token.kind) {
        .integer => {
            const source = state.source[token.span.start.offset..token.span.end.offset];
            const integer = std.fmt.parseInt(i32, source, 10) catch
                return reportError(state, token.span, "invalid integer", .{});
            return try storeNode(state, .{ .integer = integer });
        },
        .identifier => {
            const identifier = state.source[token.span.start.offset..token.span.end.offset];
            return try storeNode(state, .{ .identifier = identifier });
        },
        else => return reportUnexpected(state, token),
    }
}

fn parseArgs(state: *State) !Ast.ExtraSlice {
    var result: std.BoundedArray(Ast.NodeIndex, 8) = .{};
    while (true) {
        const token = peekToken(state);
        if (token.kind == .newline) break;
        _ = consumeToken(state);
        const node = try parseExpr(state, token, .space);
        try appendNode(state, token.span, &result, node);
    }
    return try storeExtra(state, result.slice());
}

fn storeNode(state: *State, node: Ast.Node) !Ast.NodeIndex {
    const result: Ast.NodeIndex = @intCast(state.result.nodes.items.len);
    try state.result.nodes.append(state.gpa, node);
    return result;
}

fn appendNode(state: *State, span: lexer.Span, nodes: anytype, node_index: Ast.NodeIndex) !void {
    nodes.append(node_index) catch
        return reportError(state, span, "too many children", .{});
}

fn storeExtra(state: *State, items: []const u32) !Ast.ExtraSlice {
    const start: u32 = @intCast(state.result.extra.items.len);
    const len: u32 = @intCast(items.len);
    try state.result.extra.appendSlice(state.gpa, items);
    return .{ .start = start, .len = len };
}

fn peekToken(state: *const State) *const lexer.Token {
    return &state.lex.tokens.items[state.token_index];
}

fn consumeToken(state: *State) *const lexer.Token {
    const result = &state.lex.tokens.items[state.token_index];
    state.token_index += 1;
    return result;
}

fn skipWhitespace(state: *State) void {
    while (true) {
        const token = peekToken(state);
        if (token.kind == .newline)
            _ = consumeToken(state)
        else
            break;
    }
}

fn expectString(state: *State) ![]const u8 {
    const token = consumeToken(state);
    if (token.kind != .string)
        return reportExpected(state, token, .string);
    const source = state.source[token.span.start.offset..token.span.end.offset];
    return source[1 .. source.len - 1];
}

fn expectInteger(state: *State) !i32 {
    const token = consumeToken(state);
    if (token.kind != .integer)
        return reportExpected(state, token, .integer);
    const source = state.source[token.span.start.offset..token.span.end.offset];
    return std.fmt.parseInt(i32, source, 10) catch
        reportError(state, token.span, "invalid integer", .{});
}

fn expectIdentifier(state: *State) ![]const u8 {
    const token = consumeToken(state);
    if (token.kind != .identifier)
        return reportExpected(state, token, .integer);
    return state.source[token.span.start.offset..token.span.end.offset];
}

fn parseIdentifier(state: *State, token: *const lexer.Token, T: type) !T {
    if (token.kind != .identifier)
        return reportExpected(state, token, .identifier);
    const identifier = state.source[token.span.start.offset..token.span.end.offset];
    inline for (comptime std.meta.fieldNames(T)) |f|
        if (std.mem.eql(u8, identifier, f))
            return @field(T, f);
    return reportUnexpected(state, token);
}

fn expect(state: *State, kind: lexer.Token.Kind) !void {
    const token = consumeToken(state);
    if (token.kind != kind)
        return reportExpected(state, token, kind);
}

fn reportExpected(
    state: *State,
    found: *const lexer.Token,
    expected: lexer.Token.Kind,
) error{AddedToDiagnostic} {
    return reportError(
        state,
        found.span,
        "expected {s}, found {s}",
        .{ expected.describe(), found.kind.describe() },
    );
}

fn reportUnexpected(state: *State, found: *const lexer.Token) error{AddedToDiagnostic} {
    return reportError(state, found.span, "unexpected {s}", .{found.kind.describe()});
}

fn reportError(
    state: *State,
    span: lexer.Span,
    comptime message: []const u8,
    args: anytype,
) error{AddedToDiagnostic} {
    state.diag.err(span.start.line, span.start.column, message, args);
    return error.AddedToDiagnostic;
}

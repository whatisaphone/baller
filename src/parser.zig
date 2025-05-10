const std = @import("std");

const Ast = @import("Ast.zig");
const Diagnostic = @import("Diagnostic.zig");
const akos = @import("akos.zig");
const awiz = @import("awiz.zig");
const BlockId = @import("block_id.zig").BlockId;
const parseBlockId = @import("block_id.zig").parseBlockId;
const lexer = @import("lexer.zig");

const Cx = struct {
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForTextFile,
    source: []const u8,
    lex: *const lexer.Lex,
    token_index: u32,
    result: Ast,
};

const ParseError = error{ OutOfMemory, AddedToDiagnostic };

const dummy_root_token = 0;

pub fn parseProject(
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForTextFile,
    source: []const u8,
    lex: *const lexer.Lex,
) ParseError!Ast {
    var cx: Cx = .{
        .gpa = gpa,
        .diag = diag,
        .source = source,
        .lex = lex,
        .token_index = 0,
        .result = .{
            .root = undefined, // filled in at the end
            .nodes = .empty,
            .node_tokens = .empty,
            .extra = .empty,
        },
    };
    errdefer cx.result.deinit(gpa);

    cx.result.root = try parseProjectChildren(&cx);
    return cx.result;
}

fn parseProjectChildren(cx: *Cx) !Ast.NodeIndex {
    const Keyword = enum {
        index,
        disk,
        @"var",
    };

    var index_children_opt: ?std.BoundedArray(Ast.NodeIndex, 16) = null;
    var disks: [2]Ast.NodeIndex = @splat(Ast.null_node);
    var variables: std.BoundedArray(u32, 768) = .{};

    while (true) {
        skipWhitespace(cx);
        const token = consumeToken(cx);
        switch (token.kind) {
            .identifier => switch (try parseIdentifier(cx, token, Keyword)) {
                .index => {
                    if (index_children_opt != null)
                        return reportError(cx, token, "duplicate index", .{});
                    index_children_opt = try parseIndex(cx);
                },
                .disk => {
                    try parseDisk(cx, token, &disks);
                },
                .@"var" => {
                    const node = try parseVar(cx, token);
                    try appendNode(cx, &variables, node);
                },
            },
            .eof => break,
            else => return reportUnexpected(cx, token),
        }
    }

    const token = &cx.lex.tokens.items[dummy_root_token];
    const index_children = index_children_opt orelse
        return reportError(cx, token, "missing index", .{});
    const index_extra = try storeExtra(cx, index_children.slice());
    const disks_extra = try storeExtra(cx, &disks);
    const variables_extra = try storeExtra(cx, variables.slice());

    return storeNode(cx, token, .{ .project = .{
        .index = index_extra,
        .disks = disks_extra,
        .variables = variables_extra,
    } });
}

fn parseIndex(cx: *Cx) !std.BoundedArray(Ast.NodeIndex, 16) {
    const Keyword = enum {
        @"raw-block",
        @"index-block",
    };

    try expect(cx, .brace_l);

    var children: std.BoundedArray(Ast.NodeIndex, 16) = .{};

    while (true) {
        skipWhitespace(cx);
        const token = consumeToken(cx);
        switch (token.kind) {
            .identifier => switch (try parseIdentifier(cx, token, Keyword)) {
                .@"raw-block" => {
                    const node_index = try parseRawBlock(cx, token);
                    try appendNode(cx, &children, node_index);
                },
                .@"index-block" => {
                    const block_id_str = try expectString(cx);
                    try expect(cx, .newline);
                    const IndexBlock = @FieldType(Ast.Node, "index_block");
                    const block_id = std.meta.stringToEnum(IndexBlock, block_id_str) orelse
                        return reportError(cx, token, "unsupported block id", .{});
                    const index_block = try storeNode(cx, token, .{ .index_block = block_id });
                    try appendNode(cx, &children, index_block);
                },
            },
            .brace_r => break,
            else => return reportUnexpected(cx, token),
        }
    }

    return children;
}

fn parseDisk(cx: *Cx, token: *const lexer.Token, disks: *[2]Ast.NodeIndex) !void {
    const Keyword = enum {
        @"raw-block",
        room,
    };

    const disk_number = try expectInteger(cx);
    try expect(cx, .brace_l);

    if (!(1 <= disk_number and disk_number <= disks.len))
        return reportError(cx, token, "disk number out of range", .{});
    const disk_index: u8 = @intCast(disk_number - 1);
    if (disks[disk_index] != Ast.null_node)
        return reportError(cx, token, "duplicate disk number", .{});

    var children: std.BoundedArray(Ast.NodeIndex, 32) = .{};

    while (true) {
        skipWhitespace(cx);
        const token2 = consumeToken(cx);
        switch (token2.kind) {
            .identifier => switch (try parseIdentifier(cx, token2, Keyword)) {
                .@"raw-block" => {
                    const node_index = try parseRawBlock(cx, token2);
                    try appendNode(cx, &children, node_index);
                },
                .room => {
                    const node_index = try parseDiskRoom(cx, token2);
                    try appendNode(cx, &children, node_index);
                },
            },
            .brace_r => break,
            else => return reportUnexpected(cx, token2),
        }
    }

    disks[disk_index] = try storeNode(cx, token, .{ .disk = .{
        .children = try storeExtra(cx, children.slice()),
    } });
}

fn parseDiskRoom(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const room_number_i32 = try expectInteger(cx);
    const room_name = try expectString(cx);
    const path = try expectString(cx);
    try expect(cx, .newline);

    if (!(1 <= room_number_i32 and room_number_i32 <= 255))
        return reportError(cx, token, "room number out of range", .{});
    const room_number: u8 = @intCast(room_number_i32);
    if (!(1 <= room_name.len and room_name.len <= Ast.max_room_name_len))
        return reportError(cx, token, "invalid room name", .{});

    return storeNode(cx, token, .{ .disk_room = .{
        .room_number = room_number,
        .name = room_name,
        .path = path,
    } });
}

fn parseVar(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const name = try expectIdentifier(cx);
    try expect(cx, .swat);
    const number_i32 = try expectInteger(cx);
    try expect(cx, .newline);

    const number = std.math.cast(u16, number_i32) orelse
        return reportError(cx, token, "out of range", .{});

    return storeNode(cx, token, .{ .variable = .{
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
    var cx: Cx = .{
        .gpa = gpa,
        .diag = diag,
        .source = source,
        .lex = lex,
        .token_index = 0,
        .result = .{
            .root = undefined, // filled in at the end
            .nodes = .empty,
            .node_tokens = .empty,
            .extra = .empty,
        },
    };
    errdefer cx.result.deinit(gpa);

    cx.result.root = try parseRoomChildren(&cx);
    return cx.result;
}

fn parseRoomChildren(cx: *Cx) !Ast.NodeIndex {
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
        skipWhitespace(cx);
        const token = consumeToken(cx);
        switch (token.kind) {
            .identifier => switch (try parseIdentifier(cx, token, Keyword)) {
                .@"raw-block" => {
                    const node_index = try parseRawBlock(cx, token);
                    try appendNode(cx, &children, node_index);
                },
                .@"raw-glob" => {
                    const node_index = try parseRawGlob(cx, token);
                    try appendNode(cx, &children, node_index);
                },
                .rmim => {
                    const compression_i32 = try expectInteger(cx);
                    const path = try expectString(cx);
                    try expect(cx, .newline);

                    const compression = std.math.cast(u8, compression_i32) orelse
                        return reportError(cx, token, "out of range", .{});

                    const node_index = try storeNode(cx, token, .{ .rmim = .{
                        .compression = compression,
                        .path = path,
                    } });
                    try appendNode(cx, &children, node_index);
                },
                .rmda => {
                    const node_index = try parseRmda(cx, token);
                    try appendNode(cx, &children, node_index);
                },
                .scrp => {
                    const glob_number_i32 = try expectInteger(cx);
                    const path = try expectString(cx);
                    try expect(cx, .newline);

                    const glob_number = std.math.cast(u16, glob_number_i32) orelse
                        return reportError(cx, token, "invalid glob number", .{});

                    const node_index = try storeNode(cx, token, .{ .scrp = .{
                        .glob_number = glob_number,
                        .path = path,
                    } });
                    try appendNode(cx, &children, node_index);
                },
                .encd => {
                    const path = try expectString(cx);
                    try expect(cx, .newline);

                    const node_index = try storeNode(cx, token, .{ .encd = .{ .path = path } });
                    try appendNode(cx, &children, node_index);
                },
                .excd => {
                    const path = try expectString(cx);
                    try expect(cx, .newline);

                    const node_index = try storeNode(cx, token, .{ .excd = .{ .path = path } });
                    try appendNode(cx, &children, node_index);
                },
                .lscr => {
                    const script_number_i32 = try expectInteger(cx);
                    const path = try expectString(cx);
                    try expect(cx, .newline);

                    const script_number = std.math.cast(u16, script_number_i32) orelse
                        return reportError(cx, token, "invalid script number", .{});

                    const node_index = try storeNode(cx, token, .{ .lscr = .{
                        .script_number = script_number,
                        .path = path,
                    } });
                    try appendNode(cx, &children, node_index);
                },
                .awiz => {
                    const node_index = try parseAwiz(cx, token);
                    try appendNode(cx, &children, node_index);
                },
                .mult => {
                    const node_index = try parseMult(cx, token);
                    try appendNode(cx, &children, node_index);
                },
                .akos => {
                    const node_index = try parseAkos(cx, token);
                    try appendNode(cx, &children, node_index);
                },
                .@"var" => {
                    const node_index = try parseVar(cx, token);
                    try appendNode(cx, &variables, node_index);
                },
                .script => {
                    const glob_number_i32 = try expectInteger(cx);
                    const glob_number = std.math.cast(u16, glob_number_i32) orelse
                        return reportError(cx, token, "out of range", .{});
                    try expect(cx, .brace_l);
                    const node_index = try parseScript(cx, token, glob_number);
                    try appendNode(cx, &children, node_index);
                },
                .@"local-script" => {
                    const script_number_i32 = try expectInteger(cx);
                    const script_number = std.math.cast(u16, script_number_i32) orelse
                        return reportError(cx, token, "out of range", .{});
                    try expect(cx, .brace_l);
                    const node_index = try parseLocalScript(cx, token, script_number);
                    try appendNode(cx, &children, node_index);
                },
                .enter => {
                    try expect(cx, .brace_l);
                    const statements = try parseScriptBlock(cx);
                    const node_index = try storeNode(cx, token, .{ .enter = .{
                        .statements = statements,
                    } });
                    try appendNode(cx, &children, node_index);
                },
                .exit => {
                    try expect(cx, .brace_l);
                    const statements = try parseScriptBlock(cx);
                    const node_index = try storeNode(cx, token, .{ .exit = .{
                        .statements = statements,
                    } });
                    try appendNode(cx, &children, node_index);
                },
            },
            .eof => break,
            else => return reportUnexpected(cx, token),
        }
    }

    const token = &cx.lex.tokens.items[dummy_root_token];
    return storeNode(cx, token, .{ .room_file = .{
        .children = try storeExtra(cx, children.slice()),
        .variables = try storeExtra(cx, variables.slice()),
    } });
}

fn parseRmda(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const Keyword = enum {
        @"raw-block",
    };

    try expect(cx, .brace_l);

    var children: std.BoundedArray(Ast.NodeIndex, 640) = .{};

    while (true) {
        skipWhitespace(cx);
        const token2 = consumeToken(cx);
        switch (token2.kind) {
            .identifier => switch (try parseIdentifier(cx, token2, Keyword)) {
                .@"raw-block" => {
                    const node_index = try parseRawBlock(cx, token2);
                    try appendNode(cx, &children, node_index);
                },
            },
            .brace_r => break,
            else => return reportUnexpected(cx, token2),
        }
    }

    return storeNode(cx, token, .{ .rmda = .{
        .children = try storeExtra(cx, children.slice()),
    } });
}

fn parseAwiz(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const glob_number_i32 = try expectInteger(cx);
    try expect(cx, .brace_l);

    const glob_number = std.math.cast(u16, glob_number_i32) orelse
        return reportError(cx, token, "invalid glob number", .{});

    const children = try parseAwizChildren(cx);

    return storeNode(cx, token, .{ .awiz = .{
        .glob_number = glob_number,
        .children = children,
    } });
}

fn parseAwizChildren(cx: *Cx) !Ast.ExtraSlice {
    const Keyword = enum {
        rgbs,
        @"two-ints",
        wizh,
        bmp,
    };

    var children: std.BoundedArray(Ast.NodeIndex, awiz.Awiz.max_blocks) = .{};

    while (true) {
        skipWhitespace(cx);
        const token = consumeToken(cx);
        switch (token.kind) {
            .identifier => switch (try parseIdentifier(cx, token, Keyword)) {
                .rgbs => {
                    try expect(cx, .newline);

                    const node_index = try storeNode(cx, token, .awiz_rgbs);
                    try appendNode(cx, &children, node_index);
                },
                .@"two-ints" => {
                    const block_id_str = try expectString(cx);
                    const ints = .{ try expectInteger(cx), try expectInteger(cx) };
                    try expect(cx, .newline);

                    const block_id = parseBlockId(block_id_str) orelse
                        return reportError(cx, token, "invalid block id", .{});

                    const node_index = try storeNode(cx, token, .{ .awiz_two_ints = .{
                        .block_id = block_id,
                        .ints = ints,
                    } });
                    try appendNode(cx, &children, node_index);
                },
                .wizh => {
                    try expect(cx, .newline);

                    const node_index = try storeNode(cx, token, .awiz_wizh);
                    try appendNode(cx, &children, node_index);
                },
                .bmp => {
                    const compression_int = try expectInteger(cx);
                    const path = try expectString(cx);
                    try expect(cx, .newline);

                    const compression = std.meta.intToEnum(awiz.Compression, compression_int) catch
                        return reportError(cx, token, "invalid compression", .{});

                    const node_index = try storeNode(cx, token, .{ .awiz_bmp = .{
                        .compression = compression,
                        .path = path,
                    } });
                    try appendNode(cx, &children, node_index);
                },
            },
            .brace_r => break,
            else => return reportUnexpected(cx, token),
        }
    }

    return storeExtra(cx, children.slice());
}

fn parseMult(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const Keyword = enum {
        @"raw-block",
        awiz,
        indices,
    };

    const glob_number_i32 = try expectInteger(cx);
    try expect(cx, .brace_l);

    const glob_number = std.math.cast(u16, glob_number_i32) orelse
        return reportError(cx, token, "invalid glob number", .{});

    var raw_block = Ast.null_node;
    var children: std.BoundedArray(Ast.NodeIndex, Ast.max_mult_children) = .{};
    var indices_opt: ?Ast.ExtraSlice = null;

    while (true) {
        skipWhitespace(cx);
        const token2 = consumeToken(cx);
        switch (token2.kind) {
            .identifier => switch (try parseIdentifier(cx, token2, Keyword)) {
                .@"raw-block" => {
                    if (raw_block != Ast.null_node)
                        return reportError(cx, token2, "too many children", .{});
                    raw_block = try parseRawBlockNested(cx, token2);
                },
                .awiz => {
                    try expect(cx, .brace_l);
                    const awiz_children = try parseAwizChildren(cx);

                    const node_index = try storeNode(cx, token2, .{ .mult_awiz = .{
                        .children = awiz_children,
                    } });
                    try appendNode(cx, &children, node_index);
                },
                .indices => {
                    if (indices_opt != null)
                        return reportError(cx, token2, "too many children", .{});
                    try expect(cx, .bracket_l);
                    indices_opt = try parseIntegerList(cx);
                },
            },
            .brace_r => break,
            else => return reportUnexpected(cx, token2),
        }
    }

    const indices = indices_opt orelse return reportError(cx, token, "missing indices", .{});
    for (cx.result.getExtra(indices)) |index|
        if (index >= children.len)
            return reportError(cx, token, "out of range", .{});

    return storeNode(cx, token, .{ .mult = .{
        .glob_number = glob_number,
        .raw_block = raw_block,
        .children = try storeExtra(cx, children.slice()),
        .indices = indices,
    } });
}

fn parseAkos(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const Keyword = enum {
        @"raw-block",
        akpl,
        akcd,
    };

    const glob_number_i32 = try expectInteger(cx);
    try expect(cx, .brace_l);

    const glob_number = std.math.cast(u16, glob_number_i32) orelse
        return reportError(cx, token, "invalid glob number", .{});

    var children: std.BoundedArray(Ast.NodeIndex, 1536) = .{};

    while (true) {
        skipWhitespace(cx);
        const token2 = consumeToken(cx);
        switch (token2.kind) {
            .identifier => switch (try parseIdentifier(cx, token2, Keyword)) {
                .@"raw-block" => {
                    const node_index = try parseRawBlock(cx, token2);
                    try appendNode(cx, &children, node_index);
                },
                .akpl => {
                    const path = try expectString(cx);
                    try expect(cx, .newline);

                    const node_index = try storeNode(cx, token2, .{ .akpl = .{
                        .path = path,
                    } });
                    try appendNode(cx, &children, node_index);
                },
                .akcd => {
                    const codec_token = consumeToken(cx);
                    const path = try expectString(cx);
                    try expect(cx, .newline);

                    // XXX: a bit janky, this
                    const CompressionKeyword = enum(u8) {
                        byle = @intFromEnum(akos.CompressionCodec.byle_rle),
                        trle = @intFromEnum(akos.CompressionCodec.trle),
                    };
                    const compression_keyword = try parseIdentifier(cx, codec_token, CompressionKeyword);
                    const compression: akos.CompressionCodec = @enumFromInt(@intFromEnum(compression_keyword));

                    const node_index = try storeNode(cx, token2, .{ .akcd = .{
                        .compression = compression,
                        .path = path,
                    } });
                    try appendNode(cx, &children, node_index);
                },
            },
            .brace_r => break,
            else => return reportUnexpected(cx, token2),
        }
    }

    return storeNode(cx, token, .{ .akos = .{
        .glob_number = glob_number,
        .children = try storeExtra(cx, children.slice()),
    } });
}

fn parseIntegerList(cx: *Cx) !Ast.ExtraSlice {
    var result: std.BoundedArray(u32, 256) = .{};
    while (true) {
        var token = consumeToken(cx);
        switch (token.kind) {
            .integer => {
                const source = cx.source[token.span.start.offset..token.span.end.offset];
                const int = std.fmt.parseInt(u32, source, 10) catch
                    return reportError(cx, token, "invalid integer", .{});
                try appendNode(cx, &result, int);
            },
            .bracket_r => break,
            else => return reportUnexpected(cx, token),
        }

        token = consumeToken(cx);
        switch (token.kind) {
            .comma => {},
            .bracket_r => break,
            else => return reportUnexpected(cx, token),
        }
    }
    return storeExtra(cx, result.slice());
}

fn parseRawBlock(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const block_id_str = try expectString(cx);
    const path = try expectString(cx);
    try expect(cx, .newline);

    const block_id = parseBlockId(block_id_str) orelse
        return reportError(cx, token, "invalid block id", .{});

    return storeNode(cx, token, .{ .raw_block = .{
        .block_id = block_id,
        .path = path,
    } });
}

fn parseRawBlockNested(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const Keyword = enum {
        @"raw-block",
    };

    const block_id_str = try expectString(cx);
    try expect(cx, .brace_l);

    const block_id = parseBlockId(block_id_str) orelse
        return reportError(cx, token, "invalid block id", .{});

    var children: std.BoundedArray(Ast.NodeIndex, 4) = .{};

    while (true) {
        skipWhitespace(cx);
        const token2 = consumeToken(cx);
        switch (token2.kind) {
            .identifier => switch (try parseIdentifier(cx, token2, Keyword)) {
                .@"raw-block" => {
                    const node_index = try parseRawBlock(cx, token2);
                    try appendNode(cx, &children, node_index);
                },
            },
            .brace_r => break,
            else => return reportUnexpected(cx, token2),
        }
    }

    return storeNode(cx, token, .{ .raw_block_nested = .{
        .block_id = block_id,
        .children = try storeExtra(cx, children.slice()),
    } });
}

fn parseRawGlob(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const block_id_str = try expectString(cx);
    const glob_number_i32 = try expectInteger(cx);

    const block_id = parseBlockId(block_id_str) orelse
        return reportError(cx, token, "invalid block id", .{});
    const glob_number = std.math.cast(u16, glob_number_i32) orelse
        return reportError(cx, token, "invalid glob number", .{});

    const contents = consumeToken(cx);
    return switch (contents.kind) {
        .string => parseRawGlobFile(cx, token, block_id, glob_number, contents),
        .brace_l => parseRawGlobBlock(cx, token, block_id, glob_number),
        else => reportUnexpected(cx, contents),
    };
}

fn parseRawGlobFile(
    cx: *Cx,
    token: *const lexer.Token,
    block_id: BlockId,
    glob_number: u16,
    path_token: *const lexer.Token,
) !Ast.NodeIndex {
    try expect(cx, .newline);

    const path_source = cx.source[path_token.span.start.offset..path_token.span.end.offset];
    const path = path_source[1 .. path_source.len - 1];

    return storeNode(cx, token, .{ .raw_glob_file = .{
        .block_id = block_id,
        .glob_number = glob_number,
        .path = path,
    } });
}

fn parseRawGlobBlock(cx: *Cx, token: *const lexer.Token, block_id: BlockId, glob_number: u16) !Ast.NodeIndex {
    const Keyword = enum {
        @"raw-block",
    };

    var children: std.BoundedArray(Ast.NodeIndex, 640) = .{};

    while (true) {
        skipWhitespace(cx);
        const token2 = consumeToken(cx);
        switch (token2.kind) {
            .identifier => switch (try parseIdentifier(cx, token2, Keyword)) {
                .@"raw-block" => {
                    const node_index = try parseRawBlock(cx, token2);
                    try appendNode(cx, &children, node_index);
                },
            },
            .brace_r => break,
            else => return reportUnexpected(cx, token2),
        }
    }

    return storeNode(cx, token, .{ .raw_glob_block = .{
        .block_id = block_id,
        .glob_number = glob_number,
        .children = try storeExtra(cx, children.slice()),
    } });
}

fn parseScript(cx: *Cx, token: *const lexer.Token, glob_number: u16) !Ast.NodeIndex {
    const statements = try parseScriptBlock(cx);
    return try storeNode(cx, token, .{ .script = .{
        .glob_number = glob_number,
        .statements = statements,
    } });
}

fn parseLocalScript(cx: *Cx, token: *const lexer.Token, script_number: u16) !Ast.NodeIndex {
    const statements = try parseScriptBlock(cx);
    return try storeNode(cx, token, .{ .local_script = .{
        .script_number = script_number,
        .statements = statements,
    } });
}

fn parseScriptBlock(cx: *Cx) ParseError!Ast.ExtraSlice {
    var statements: std.BoundedArray(Ast.NodeIndex, 256) = .{};
    while (true) {
        skipWhitespace(cx);
        const token = consumeToken(cx);
        if (token.kind == .brace_r) break;
        const node = try parseStatement(cx, token);
        try appendNode(cx, &statements, node);
    }
    return try storeExtra(cx, statements.slice());
}

fn parseStatement(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const Keyword = enum {
        @"if",
    };

    if (token.kind == .identifier) {
        // Check for labels
        if (peekToken(cx).kind == .colon) {
            _ = consumeToken(cx);
            const identifier = cx.source[token.span.start.offset..token.span.end.offset];
            return storeNode(cx, token, .{ .label = identifier });
        }
        // Check for keywords
        if (parseIdentifierOpt(cx, token, Keyword)) |kw| switch (kw) {
            .@"if" => {
                try expect(cx, .paren_l);
                const next = consumeToken(cx);
                const condition = try parseExpr(cx, next, .all);
                try expect(cx, .paren_r);
                try expect(cx, .brace_l);
                const true_stmts = try parseScriptBlock(cx);
                return storeNode(cx, token, .{ .@"if" = .{
                    .condition = condition,
                    .true = true_stmts,
                } });
            },
        };
    }

    const ei = try parseExpr(cx, token, .all);
    try expect(cx, .newline);

    const expr = &cx.result.nodes.items[ei];
    if (expr.* == .identifier) {
        // A lonely identifier is a call with 0 args
        return storeNode(cx, token, .{ .call = .{ .callee = ei, .args = .empty } });
    }
    return ei;
}

pub const Precedence = enum {
    all,
    space,
    field,
};

fn parseExpr(cx: *Cx, token: *const lexer.Token, prec: Precedence) ParseError!Ast.NodeIndex {
    var cur = try parseAtom(cx, token);
    while (true) {
        const token2 = peekToken(cx);
        switch (token2.kind) {
            .period => {
                if (@intFromEnum(prec) >= @intFromEnum(Precedence.field)) break;
                _ = consumeToken(cx);
                const field = try expectIdentifier(cx);
                cur = try storeNode(cx, token2, .{ .field = .{ .lhs = cur, .field = field } });
            },
            // This is everything that parseAtom recognizes
            .integer, .identifier, .paren_l, .bracket_l => {
                if (@intFromEnum(prec) >= @intFromEnum(Precedence.space)) break;
                const args = try parseArgs(cx);
                cur = try storeNode(cx, token, .{ .call = .{ .callee = cur, .args = args } });
            },
            else => break,
        }
    }
    return cur;
}

fn parseAtom(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    switch (token.kind) {
        .integer => {
            const source = cx.source[token.span.start.offset..token.span.end.offset];
            const integer = std.fmt.parseInt(i32, source, 10) catch
                return reportError(cx, token, "invalid integer", .{});
            return try storeNode(cx, token, .{ .integer = integer });
        },
        .identifier => {
            const identifier = cx.source[token.span.start.offset..token.span.end.offset];
            return try storeNode(cx, token, .{ .identifier = identifier });
        },
        .paren_l => {
            const next = consumeToken(cx);
            const result = try parseExpr(cx, next, .all);
            try expect(cx, .paren_r);
            return result;
        },
        .bracket_l => {
            const items = try parseArgs(cx);
            try expect(cx, .bracket_r);
            return try storeNode(cx, token, .{ .list = .{ .items = items } });
        },
        else => return reportUnexpected(cx, token),
    }
}

fn parseArgs(cx: *Cx) !Ast.ExtraSlice {
    var result: std.BoundedArray(Ast.NodeIndex, 16) = .{};
    while (true) {
        const token = peekToken(cx);
        if (token.kind == .newline or
            token.kind == .paren_r or
            token.kind == .bracket_r)
            break;
        _ = consumeToken(cx);
        const node = try parseExpr(cx, token, .space);
        try appendNode(cx, &result, node);
    }
    return try storeExtra(cx, result.slice());
}

fn storeNode(cx: *Cx, token: *const lexer.Token, node: Ast.Node) !Ast.NodeIndex {
    const result: Ast.NodeIndex = @intCast(cx.result.nodes.items.len);
    try cx.result.nodes.append(cx.gpa, node);
    const token_index = recoverTokenIndex(cx, token);
    try cx.result.node_tokens.append(cx.gpa, token_index);
    return result;
}

fn recoverTokenIndex(cx: *Cx, token: *const lexer.Token) lexer.TokenIndex {
    return @intCast(token - cx.lex.tokens.items.ptr);
}

fn appendNode(cx: *Cx, nodes: anytype, node_index: Ast.NodeIndex) !void {
    nodes.append(node_index) catch {
        const token_index = cx.result.node_tokens.items[node_index];
        const token = &cx.lex.tokens.items[token_index];
        return reportError(cx, token, "too many children", .{});
    };
}

fn storeExtra(cx: *Cx, items: []const u32) !Ast.ExtraSlice {
    const start: u32 = @intCast(cx.result.extra.items.len);
    const len: u32 = @intCast(items.len);
    try cx.result.extra.appendSlice(cx.gpa, items);
    return .{ .start = start, .len = len };
}

fn peekToken(cx: *const Cx) *const lexer.Token {
    return &cx.lex.tokens.items[cx.token_index];
}

fn consumeToken(cx: *Cx) *const lexer.Token {
    const result = &cx.lex.tokens.items[cx.token_index];
    cx.token_index += 1;
    return result;
}

fn skipWhitespace(cx: *Cx) void {
    while (true) {
        const token = peekToken(cx);
        if (token.kind == .newline)
            _ = consumeToken(cx)
        else
            break;
    }
}

fn expectString(cx: *Cx) ![]const u8 {
    const token = consumeToken(cx);
    if (token.kind != .string)
        return reportExpected(cx, token, .string);
    const source = cx.source[token.span.start.offset..token.span.end.offset];
    return source[1 .. source.len - 1];
}

fn expectInteger(cx: *Cx) !i32 {
    const token = consumeToken(cx);
    if (token.kind != .integer)
        return reportExpected(cx, token, .integer);
    const source = cx.source[token.span.start.offset..token.span.end.offset];
    return std.fmt.parseInt(i32, source, 10) catch
        reportError(cx, token, "invalid integer", .{});
}

fn expectIdentifier(cx: *Cx) ![]const u8 {
    const token = consumeToken(cx);
    if (token.kind != .identifier)
        return reportExpected(cx, token, .integer);
    return cx.source[token.span.start.offset..token.span.end.offset];
}

fn parseIdentifierOpt(cx: *Cx, token: *const lexer.Token, T: type) ?T {
    if (token.kind != .identifier) return null;
    const identifier = cx.source[token.span.start.offset..token.span.end.offset];
    inline for (comptime std.meta.fieldNames(T)) |f|
        if (std.mem.eql(u8, identifier, f))
            return @field(T, f);
    return null;
}

fn parseIdentifier(cx: *Cx, token: *const lexer.Token, T: type) !T {
    return parseIdentifierOpt(cx, token, T) orelse
        return reportUnexpected(cx, token);
}

fn expect(cx: *Cx, kind: lexer.Token.Kind) !void {
    const token = consumeToken(cx);
    if (token.kind != kind)
        return reportExpected(cx, token, kind);
}

fn reportExpected(
    cx: *Cx,
    found: *const lexer.Token,
    expected: lexer.Token.Kind,
) error{AddedToDiagnostic} {
    return reportError(
        cx,
        found,
        "expected {s}, found {s}",
        .{ expected.describe(), found.kind.describe() },
    );
}

fn reportUnexpected(cx: *Cx, found: *const lexer.Token) error{AddedToDiagnostic} {
    return reportError(cx, found, "unexpected {s}", .{found.kind.describe()});
}

fn reportError(
    cx: *Cx,
    token: *const lexer.Token,
    comptime message: []const u8,
    args: anytype,
) error{AddedToDiagnostic} {
    const s = &token.span.start;
    cx.diag.err(s.line, s.column, message, args);
    return error.AddedToDiagnostic;
}

const std = @import("std");

const Ast = @import("Ast.zig");
const Diagnostic = @import("Diagnostic.zig");
const UsageTracker = @import("UsageTracker.zig");
const akos = @import("akos.zig");
const awiz = @import("awiz.zig");
const BlockId = @import("block_id.zig").BlockId;
const BoundedArray = @import("bounded_array.zig").BoundedArray;
const games = @import("games.zig");
const lexer = @import("lexer.zig");

const Cx = struct {
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForTextFile,
    source: []const u8,
    lex: *const lexer.Lex,
    token_index: @typeInfo(lexer.TokenIndex).@"enum".tag_type,
    bracket_nesting: u32,
    result: Ast,
};

const ParseError = error{ OutOfMemory, AddedToDiagnostic };

const dummy_root_token: lexer.TokenIndex = .fromIndex(0);

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
        .bracket_nesting = 0,
        .result = .{
            .root = undefined, // filled in at the end
            .nodes = .empty,
            .node_tokens = .empty,
            .extra = .empty,
            .strings = .empty,
        },
    };
    errdefer cx.result.deinit(gpa);
    try cx.result.nodes.ensureTotalCapacity(gpa, source.len / 16);
    try cx.result.node_tokens.ensureTotalCapacity(gpa, source.len / 16);
    try cx.result.extra.ensureTotalCapacity(gpa, source.len / 32);
    try cx.result.strings.buf.ensureTotalCapacity(gpa, source.len / 2);

    cx.result.root = try parseProjectChildren(&cx);
    return cx.result;
}

fn parseProjectChildren(cx: *Cx) !Ast.NodeIndex {
    const Keyword = enum {
        index,
        disk,
        music,
        @"var",
        @"const",
    };

    var children: BoundedArray(Ast.NodeIndex, 8192) = .{};
    var parsed_index = false;

    {
        const token = consumeDown(cx);
        _ = try parseIdentifier(cx, token, enum { target });
        const target = try parseIdentifier(cx, consumeRight(cx), games.Target);
        try expect(cx, .newline);

        const node = try storeNode(cx, token, .{ .target = target });
        try appendNode(cx, &children, node);
    }

    while (true) {
        const token = consumeDown(cx);
        switch (token.kind) {
            .identifier => switch (try parseIdentifier(cx, token, Keyword)) {
                .index => {
                    if (parsed_index)
                        return reportError(cx, token, "duplicate index", .{});
                    const node = try parseIndex(cx, token);
                    try appendNode(cx, &children, node);
                    parsed_index = true;
                },
                .disk => {
                    const node = try parseDisk(cx, token);
                    try appendNode(cx, &children, node);
                },
                .music => {
                    const node = try parseMusic(cx, token);
                    try appendNode(cx, &children, node);
                },
                .@"var" => {
                    const node = try parseVar(cx, token);
                    try appendNode(cx, &children, node);
                },
                .@"const" => {
                    const node = try parseConst(cx, token);
                    try appendNode(cx, &children, node);
                },
            },
            .eof => break,
            else => return reportUnexpected(cx, token),
        }
    }

    const token = cx.lex.tokens.at(dummy_root_token);

    if (!parsed_index)
        return reportError(cx, token, "missing index", .{});

    return storeNode(cx, token, .{ .project = .{
        .children = try storeExtra(cx, children.slice()),
    } });
}

fn parseIndex(cx: *Cx, index_token: *const lexer.Token) !Ast.NodeIndex {
    const Keyword = enum {
        @"raw-block",
        maxs,
        @"index-block",
    };

    try expect(cx, .brace_l);

    var children: BoundedArray(Ast.NodeIndex, 16) = .{};

    while (true) {
        const token = consumeDown(cx);
        switch (token.kind) {
            .identifier => switch (try parseIdentifier(cx, token, Keyword)) {
                .@"raw-block" => {
                    const node_index = try parseRawBlock(cx, token);
                    try appendNode(cx, &children, node_index);
                },
                .maxs => {
                    const path = try expectString(cx);
                    try expect(cx, .newline);
                    const node = try storeNode(cx, token, .{ .maxs = .{ .path = path } });
                    try appendNode(cx, &children, node);
                },
                .@"index-block" => {
                    const block_id_ident = try expectIdentifier(cx);
                    try expect(cx, .newline);
                    const IndexBlock = @FieldType(Ast.Node, "index_block");
                    const block_id_str = cx.result.strings.get(block_id_ident);
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

    return storeNode(cx, index_token, .{ .index = .{
        .children = try storeExtra(cx, children.slice()),
    } });
}

fn parseDisk(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const Keyword = enum {
        @"raw-block",
        room,
    };

    const disk_number = try expectInteger(cx, u8);
    try expect(cx, .brace_l);

    var children: BoundedArray(Ast.NodeIndex, 32) = .{};

    while (true) {
        const token2 = consumeDown(cx);
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

    return storeNode(cx, token, .{ .disk = .{
        .number = disk_number,
        .children = try storeExtra(cx, children.slice()),
    } });
}

fn parseDiskRoom(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const room_number = try expectInteger(cx, u8);
    const room_name = try expectString(cx);
    const path = try expectString(cx);
    try expect(cx, .newline);

    if (room_number == 0)
        return reportError(cx, token, "out of range", .{});
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
    const number = try expectInteger(cx, u16);
    try expect(cx, .newline);

    return storeNode(cx, token, .{ .variable = .{
        .name = name,
        .number = number,
    } });
}

fn parseConst(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const name = try expectIdentifier(cx);
    try expect(cx, .eq);
    const value = try expectInteger(cx, i32);
    try expect(cx, .newline);

    return storeNode(cx, token, .{ .constant = .{
        .name = name,
        .value = value,
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
        .bracket_nesting = 0,
        .result = .{
            .root = undefined, // filled in at the end
            .nodes = .empty,
            .node_tokens = .empty,
            .extra = .empty,
            .strings = .empty,
        },
    };
    errdefer cx.result.deinit(gpa);
    try cx.result.nodes.list.ensureTotalCapacity(gpa, source.len / 16);
    try cx.result.node_tokens.list.ensureTotalCapacity(gpa, source.len / 16);
    try cx.result.extra.ensureTotalCapacity(gpa, source.len / 32);
    try cx.result.strings.buf.ensureTotalCapacity(gpa, source.len / 2);

    cx.result.root = try parseRoomChildren(&cx);
    return cx.result;
}

fn parseRoomChildren(cx: *Cx) !Ast.NodeIndex {
    const Keyword = enum {
        @"raw-block",
        @"raw-glob",
        rmim,
        rmda,
        scr,
        encd,
        excd,
        lsc,
        obim,
        sound,
        awiz,
        mult,
        akos,
        talkie,
        @"var",
        script,
        @"local-script",
        enter,
        exit,
        object,
    };

    var children: BoundedArray(Ast.NodeIndex, 5120) = .{};
    var variables: BoundedArray(Ast.NodeIndex, 160) = .{};

    while (true) {
        const token = consumeDown(cx);
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
                    const node_index = try parseRmim(cx, token);
                    try appendNode(cx, &children, node_index);
                },
                .rmda => {
                    const node_index = try parseRmda(cx, token);
                    try appendNode(cx, &children, node_index);
                },
                .scr => {
                    const name = try expectIdentifier(cx);
                    try expect(cx, .swat);
                    const glob_number = try expectInteger(cx, u16);
                    const path = try expectString(cx);
                    try expect(cx, .newline);

                    const node_index = try storeNode(cx, token, .{ .scr = .{
                        .name = name,
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
                .lsc => {
                    const name = try expectIdentifier(cx);
                    try expect(cx, .swat);
                    const script_number = try expectInteger(cx, u16);
                    const path = try expectString(cx);
                    try expect(cx, .newline);

                    const node_index = try storeNode(cx, token, .{ .lsc = .{
                        .name = name,
                        .script_number = script_number,
                        .path = path,
                    } });
                    try appendNode(cx, &children, node_index);
                },
                .obim => {
                    const node_index = try parseObim(cx, token);
                    try appendNode(cx, &children, node_index);
                },
                .sound => {
                    const node_index = try parseSound(cx, token);
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
                .talkie => {
                    const node_index = try parseTalkie(cx, token);
                    try appendNode(cx, &children, node_index);
                },
                .@"var" => {
                    const node_index = try parseVar(cx, token);
                    try appendNode(cx, &variables, node_index);
                },
                .script => {
                    const node_index = try parseScript(cx, token);
                    try appendNode(cx, &children, node_index);
                },
                .@"local-script" => {
                    const node_index = try parseLocalScript(cx, token);
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
                .object => {
                    const node_index = try parseObject(cx, token);
                    try appendNode(cx, &children, node_index);
                },
            },
            .eof => break,
            else => return reportUnexpected(cx, token),
        }
    }

    const token = cx.lex.tokens.at(dummy_root_token);
    return storeNode(cx, token, .{ .room_file = .{
        .children = try storeExtra(cx, children.slice()),
        .variables = try storeExtra(cx, variables.slice()),
    } });
}

fn parseRmim(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    try expect(cx, .brace_l);

    const rmih = rmih: {
        const token2 = consumeDown(cx);
        _ = try parseIdentifier(cx, token2, enum { @"raw-block" });
        break :rmih try parseRawBlock(cx, token2);
    };

    const im = im: {
        const token2 = consumeDown(cx);
        _ = try parseIdentifier(cx, token2, enum { im });
        break :im try parseRmimIm(cx, token2);
    };

    try expectDown(cx, .brace_r);

    return storeNode(cx, token, .{ .rmim = .{
        .rmih = rmih,
        .im = im,
    } });
}

// TODO: maybe merge this with obim at some point?
fn parseRmimIm(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const Keyword = enum {
        @"raw-block",
        bmap,
    };

    try expect(cx, .brace_l);

    var children: BoundedArray(Ast.NodeIndex, 4) = .{};

    while (true) {
        const token2 = consumeDown(cx);
        switch (token2.kind) {
            .identifier => switch (try parseIdentifier(cx, token2, Keyword)) {
                .@"raw-block" => {
                    const node_index = try parseRawBlock(cx, token2);
                    try appendNode(cx, &children, node_index);
                },
                .bmap => {
                    const compression = try expectInteger(cx, u8);
                    const path = try expectString(cx);
                    try expect(cx, .newline);

                    const node_index = try storeNode(cx, token, .{ .bmap = .{
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

    return storeNode(cx, token, .{ .rmim_im = .{
        .children = try storeExtra(cx, children.slice()),
    } });
}

fn parseRmda(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const Keyword = enum {
        @"raw-block",
    };

    try expect(cx, .brace_l);

    var children: BoundedArray(Ast.NodeIndex, 640) = .{};

    while (true) {
        const token2 = consumeDown(cx);
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

fn parseObim(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const Keyword = enum {
        @"raw-block",
        im,
    };

    try expect(cx, .brace_l);

    var children: BoundedArray(Ast.NodeIndex, 12) = .{};

    while (true) {
        const token2 = consumeDown(cx);
        switch (token2.kind) {
            .identifier => switch (try parseIdentifier(cx, token2, Keyword)) {
                .@"raw-block" => {
                    const node_index = try parseRawBlock(cx, token2);
                    try appendNode(cx, &children, node_index);
                },
                .im => {
                    const node_index = try parseIm(cx, token2);
                    try appendNode(cx, &children, node_index);
                },
            },
            .brace_r => break,
            else => return reportUnexpected(cx, token2),
        }
    }

    return storeNode(cx, token, .{ .obim = .{
        .children = try storeExtra(cx, children.slice()),
    } });
}

fn parseIm(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const Keyword = enum {
        @"raw-block",
        smap,
    };

    try expect(cx, .brace_l);

    var children: BoundedArray(Ast.NodeIndex, 2) = .{};

    while (true) {
        const token2 = consumeDown(cx);
        switch (token2.kind) {
            .identifier => switch (try parseIdentifier(cx, token2, Keyword)) {
                .@"raw-block" => {
                    const node_index = try parseRawBlock(cx, token2);
                    try appendNode(cx, &children, node_index);
                },
                .smap => {
                    const path = try expectString(cx);
                    try expect(cx, .bracket_l);
                    const strip_compression = try parseIntegerList(cx);
                    try expect(cx, .newline);

                    const node_index = try storeNode(cx, token, .{ .smap = .{
                        .path = path,
                        .strip_compression = strip_compression,
                    } });
                    try appendNode(cx, &children, node_index);
                },
            },
            .brace_r => break,
            else => return reportUnexpected(cx, token2),
        }
    }

    return storeNode(cx, token, .{ .obim_im = .{
        .children = try storeExtra(cx, children.slice()),
    } });
}

fn parseSound(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const block_id = try expectBlockId(cx);
    const name = try expectIdentifier(cx);
    try expect(cx, .swat);
    const glob_number = try expectInteger(cx, u16);
    try expect(cx, .brace_l);

    const children = try parseSoundChildren(cx);

    return storeNode(cx, token, .{ .sound = .{
        .block_id = block_id,
        .name = name,
        .glob_number = glob_number,
        .children = children,
    } });
}

fn parseSoundChildren(cx: *Cx) !Ast.ExtraSlice {
    const Keyword = enum {
        @"raw-block",
        sdat,
    };

    var children: BoundedArray(Ast.NodeIndex, 3) = .{};

    while (true) {
        const token = consumeDown(cx);
        switch (token.kind) {
            .identifier => switch (try parseIdentifier(cx, token, Keyword)) {
                .@"raw-block" => {
                    const node_index = try parseRawBlock(cx, token);
                    try appendNode(cx, &children, node_index);
                },
                .sdat => {
                    const path = try expectString(cx);
                    try expect(cx, .newline);
                    const node_index = try storeNode(cx, token, .{ .sdat = .{ .path = path } });
                    try appendNode(cx, &children, node_index);
                },
            },
            .brace_r => break,
            else => return reportUnexpected(cx, token),
        }
    }

    return storeExtra(cx, children.slice());
}

fn parseAwiz(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const name = try expectIdentifier(cx);
    try expect(cx, .swat);
    const glob_number = try expectInteger(cx, u16);
    try expect(cx, .brace_l);

    const children = try parseAwizChildren(cx);

    return storeNode(cx, token, .{ .awiz = .{
        .name = name,
        .glob_number = glob_number,
        .children = children,
    } });
}

fn parseAwizChildren(cx: *Cx) !Ast.ExtraSlice {
    const Keyword = enum {
        @"raw-block",
        rgbs,
        wizh,
        wizd,
    };

    var children: BoundedArray(Ast.NodeIndex, 8) = .{};

    while (true) {
        const token = consumeDown(cx);
        switch (token.kind) {
            .identifier => switch (try parseIdentifier(cx, token, Keyword)) {
                .@"raw-block" => {
                    const node_index = try parseRawBlock(cx, token);
                    try appendNode(cx, &children, node_index);
                },
                .rgbs => {
                    try expect(cx, .newline);

                    const node_index = try storeNode(cx, token, .awiz_rgbs);
                    try appendNode(cx, &children, node_index);
                },
                .wizh => {
                    try expect(cx, .newline);

                    const node_index = try storeNode(cx, token, .awiz_wizh);
                    try appendNode(cx, &children, node_index);
                },
                .wizd => {
                    const compression_int = try expectInteger(cx, u8);
                    const path = try expectString(cx);
                    try expect(cx, .newline);

                    const compression = std.meta.intToEnum(awiz.Compression, compression_int) catch
                        return reportError(cx, token, "invalid compression", .{});

                    const node_index = try storeNode(cx, token, .{ .awiz_wizd = .{
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

    const name = try expectIdentifier(cx);
    try expect(cx, .swat);
    const glob_number = try expectInteger(cx, u16);
    try expect(cx, .brace_l);

    var raw_block: Ast.NodeIndex.Optional = .null;
    var children: BoundedArray(Ast.NodeIndex, Ast.max_mult_children) = .{};
    var indices_opt: ?Ast.ExtraSlice = null;

    while (true) {
        const token2 = consumeDown(cx);
        switch (token2.kind) {
            .identifier => switch (try parseIdentifier(cx, token2, Keyword)) {
                .@"raw-block" => {
                    if (raw_block != .null)
                        return reportError(cx, token2, "too many children", .{});
                    raw_block = (try parseRawBlockNested(cx, token2)).wrap();
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
    for (cx.result.getExtraU32(indices)) |index|
        if (index >= children.len)
            return reportError(cx, token, "out of range", .{});

    return storeNode(cx, token, .{ .mult = .{
        .name = name,
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

    const name = try expectIdentifier(cx);
    try expect(cx, .swat);
    const glob_number = try expectInteger(cx, u16);
    try expect(cx, .brace_l);

    var children: BoundedArray(Ast.NodeIndex, 1536) = .{};

    while (true) {
        const token2 = consumeDown(cx);
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
                    const codec_token = consumeRight(cx);
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
        .name = name,
        .glob_number = glob_number,
        .children = try storeExtra(cx, children.slice()),
    } });
}

fn parseTalkie(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const name = try expectIdentifier(cx);
    try expect(cx, .swat);
    const glob_number = try expectInteger(cx, u16);
    try expect(cx, .eq);
    const text = try expectString(cx);
    try expect(cx, .newline);

    return storeNode(cx, token, .{ .talkie = .{
        .name = name,
        .glob_number = glob_number,
        .text = text,
    } });
}

fn parseIntegerList(cx: *Cx) !Ast.ExtraSlice {
    var result: BoundedArray(u32, 256) = .{};
    while (true) {
        const token = consumeDown(cx);
        switch (token.kind) {
            .integer => {
                const source = cx.source[token.span.start.offset..token.span.end.offset];
                const int = std.fmt.parseInt(u32, source, 10) catch
                    return reportError(cx, token, "invalid integer", .{});
                try appendU32(cx, token, &result, int);
            },
            .bracket_r => break,
            else => return reportUnexpected(cx, token),
        }
    }
    return storeExtraU32(cx, result.slice());
}

fn parseMusic(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    try expect(cx, .brace_l);

    const children = try parseMusicChildren(cx);

    return storeNode(cx, token, .{ .music = .{
        .children = children,
    } });
}

fn parseMusicChildren(cx: *Cx) !Ast.ExtraSlice {
    const Keyword = enum {
        sound,
        riff,
    };

    var children: BoundedArray(Ast.NodeIndex, 192) = .{};

    while (true) {
        const token = consumeDown(cx);
        switch (token.kind) {
            .identifier => switch (try parseIdentifier(cx, token, Keyword)) {
                .sound => {
                    const node_index = try parseSound(cx, token);
                    try appendNode(cx, &children, node_index);
                },
                .riff => {
                    const name = try expectIdentifier(cx);
                    try expect(cx, .swat);
                    const glob_number = try expectInteger(cx, u16);
                    const path = try expectString(cx);
                    try expect(cx, .newline);

                    const node_index = try storeNode(cx, token, .{ .riff = .{
                        .name = name,
                        .glob_number = glob_number,
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

fn parseRawBlock(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const block_id = try expectBlockId(cx);

    const contents_token = consumeRight(cx);
    const contents: Ast.RawContents = switch (contents_token.kind) {
        .string => .{ .path = try parseString(cx, contents_token) },
        .hex_string => .{ .data = try parseHexString(cx, contents_token) },
        else => return reportUnexpected(cx, contents_token),
    };

    try expect(cx, .newline);

    return storeNode(cx, token, .{ .raw_block = .{
        .block_id = block_id,
        .contents = contents,
    } });
}

fn parseRawBlockNested(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const Keyword = enum {
        @"raw-block",
    };

    const block_id = try expectBlockId(cx);
    try expect(cx, .brace_l);

    var children: BoundedArray(Ast.NodeIndex, 4) = .{};

    while (true) {
        const token2 = consumeDown(cx);
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
    const block_id = try expectBlockId(cx);

    const name = if (peekRight(cx).kind == .identifier) name: {
        const name = try expectIdentifier(cx);
        try expect(cx, .swat);
        break :name name;
    } else null;

    const glob_number = try expectInteger(cx, u16);

    const contents = consumeRight(cx);
    return switch (contents.kind) {
        .string => parseRawGlobFile(cx, token, block_id, name, glob_number, contents),
        .brace_l => parseRawGlobBlock(cx, token, block_id, name, glob_number),
        else => reportUnexpected(cx, contents),
    };
}

fn parseRawGlobFile(
    cx: *Cx,
    token: *const lexer.Token,
    block_id: BlockId,
    name: ?Ast.StringSlice,
    glob_number: u16,
    path_token: *const lexer.Token,
) !Ast.NodeIndex {
    try expect(cx, .newline);

    const path = try parseString(cx, path_token);

    return storeNode(cx, token, .{ .raw_glob_file = .{
        .block_id = block_id,
        .name = name,
        .glob_number = glob_number,
        .path = path,
    } });
}

fn parseRawGlobBlock(
    cx: *Cx,
    token: *const lexer.Token,
    block_id: BlockId,
    name: ?Ast.StringSlice,
    glob_number: u16,
) !Ast.NodeIndex {
    const Keyword = enum {
        @"raw-block",
    };

    var children: BoundedArray(Ast.NodeIndex, 640) = .{};

    while (true) {
        const token2 = consumeDown(cx);
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
        .name = name,
        .glob_number = glob_number,
        .children = try storeExtra(cx, children.slice()),
    } });
}

fn parseScript(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const name = try expectIdentifier(cx);
    try expect(cx, .swat);
    const glob_number = try expectInteger(cx, u16);
    const params = try parseScriptParams(cx);
    const statements = try parseScriptBlock(cx);
    return try storeNode(cx, token, .{ .script = .{
        .name = name,
        .glob_number = glob_number,
        .params = params,
        .statements = statements,
    } });
}

fn parseLocalScript(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const name = try expectIdentifier(cx);
    try expect(cx, .swat);
    const script_number = try expectInteger(cx, u16);
    const params = try parseScriptParams(cx);
    const statements = try parseScriptBlock(cx);
    return try storeNode(cx, token, .{ .local_script = .{
        .name = name,
        .script_number = script_number,
        .params = params,
        .statements = statements,
    } });
}

fn parseScriptParams(cx: *Cx) ParseError!Ast.ExtraSlice {
    var result: BoundedArray(Ast.NodeIndex.Optional, UsageTracker.max_local_vars) = .{};
    while (true) {
        const token = consumeRight(cx);
        const ni: Ast.NodeIndex.Optional = switch (token.kind) {
            .brace_l => break,
            .underscore => .null,
            .identifier => (try parseIdentifierNode(cx, token)).wrap(),
            else => return reportUnexpected(cx, token),
        };
        result.append(ni) catch
            return reportError(cx, token, "too many children", .{});
    }
    return storeExtraOpt(cx, result.constSlice());
}

fn parseObject(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const Keyword = enum {
        @"raw-block",
        verb,
    };

    const name = try expectIdentifier(cx);
    try expect(cx, .swat);
    const number = try expectInteger(cx, u16);
    const obna = try expectString(cx);
    try expect(cx, .brace_l);

    var children: BoundedArray(Ast.NodeIndex, 3) = .{};

    while (true) {
        const token2 = consumeDown(cx);
        switch (token2.kind) {
            .identifier => switch (try parseIdentifier(cx, token2, Keyword)) {
                .@"raw-block" => {
                    const node_index = try parseRawBlock(cx, token2);
                    try appendNode(cx, &children, node_index);
                },
                .verb => {
                    const node_index = try parseVerb(cx, token2);
                    try appendNode(cx, &children, node_index);
                },
            },
            .brace_r => break,
            else => return reportUnexpected(cx, token2),
        }
    }

    return storeNode(cx, token, .{ .object = .{
        .name = name,
        .number = number,
        .obna = obna,
        .children = try storeExtra(cx, children.slice()),
    } });
}

fn parseVerb(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const number = try expectInteger(cx, u8);

    const contents = consumeRight(cx);
    const body: Ast.VerbBody = body: switch (contents.kind) {
        .string => {
            try expect(cx, .newline);

            const path = try parseString(cx, contents);

            break :body .{ .assembly = path };
        },
        .brace_l => {
            const statements = try parseScriptBlock(cx);
            break :body .{ .script = statements };
        },
        else => return reportUnexpected(cx, contents),
    };

    return storeNode(cx, token, .{ .verb = .{ .number = number, .body = body } });
}

fn parseScriptBlock(cx: *Cx) ParseError!Ast.ExtraSlice {
    var statements: BoundedArray(Ast.NodeIndex, 4096) = .{};
    while (true) {
        const token = consumeDown(cx);
        if (token.kind == .brace_r) break;
        const node = try parseStatement(cx, token);
        try appendNode(cx, &statements, node);
    }
    return try storeExtra(cx, statements.slice());
}

fn parseStatement(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const Keyword = enum {
        @"var",
        @"if",
        @"while",
        @"for",
        do,
        case,
    };

    if (token.kind == .identifier) {
        // Check for labels
        if (peekRight(cx).kind == .colon) {
            _ = consumeRight(cx);
            const identifier = try getIdentifier(cx, token);
            return storeNode(cx, token, .{ .label = identifier });
        }
        // Check for keywords
        if (parseIdentifierOpt(cx, token, Keyword)) |kw| switch (kw) {
            .@"var" => {
                var children: BoundedArray(Ast.NodeIndex, UsageTracker.max_local_vars) = .{};
                while (true) {
                    const token2 = consumeRight(cx);
                    const name = switch (token2.kind) {
                        .newline => break,
                        .underscore => null,
                        .identifier => try getIdentifier(cx, token2),
                        else => return reportUnexpected(cx, token2),
                    };
                    const node_index = try storeNode(cx, token2, .{ .local_var = .{
                        .name = name,
                    } });
                    try appendNode(cx, &children, node_index);
                }
                return storeNode(cx, token, .{ .local_vars = .{
                    .children = try storeExtra(cx, children.slice()),
                } });
            },
            .@"if" => {
                const next = consumeRight(cx);
                const condition = try parseExpr(cx, next, .space);
                try expect(cx, .brace_l);
                const true_stmts = try parseScriptBlock(cx);

                var false_stmts: Ast.ExtraSlice = .empty;
                if (parseIdentifierOpt(cx, peekRight(cx), enum { @"else" })) |_| {
                    _ = consumeRight(cx);
                    if (parseIdentifierOpt(cx, peekRight(cx), enum { @"if" })) |_| {
                        const else_if = try parseStatement(cx, consumeRight(cx));
                        false_stmts = try storeExtra(cx, (&else_if)[0..1]);
                    } else {
                        try expect(cx, .brace_l);
                        false_stmts = try parseScriptBlock(cx);
                    }
                }

                return storeNode(cx, token, .{ .@"if" = .{
                    .condition = condition,
                    .true = true_stmts,
                    .false = false_stmts,
                } });
            },
            .@"while" => {
                const next = consumeRight(cx);
                const condition = try parseExpr(cx, next, .space);
                try expect(cx, .brace_l);
                const body = try parseScriptBlock(cx);
                return storeNode(cx, token, .{ .@"while" = .{
                    .condition = condition,
                    .body = body,
                } });
            },
            .@"for" => {
                const target = try parseIdentifierNode(cx, consumeRight(cx));
                try expect(cx, .eq);
                const token2 = consumeRight(cx);
                if (token2.kind == .brace_l) {
                    const backing = try parseIdentifierNode(cx, consumeRight(cx));
                    try expect(cx, .brace_r);
                    if (peekRight(cx).kind != .bracket_l)
                        return reportUnexpected(cx, peekRight(cx));
                    const list = try parseExpr(cx, consumeRight(cx), .space);
                    try expect(cx, .brace_l);
                    const body = try parseScriptBlock(cx);
                    return storeNode(cx, token, .{ .for_in = .{
                        .target = target,
                        .list = list,
                        .backing = backing,
                        .body = body,
                    } });
                } else if (isAtomToken(token2)) {
                    const start = try parseExpr(cx, token2, .space);
                    _ = try parseIdentifier(cx, consumeRight(cx), enum { to });
                    const end = try parseExpr(cx, consumeRight(cx), .space);
                    const dir_tok = consumeRight(cx);
                    const dir: Ast.ForDirection = switch (dir_tok.kind) {
                        .plus => .up,
                        .minus => .down,
                        else => return reportUnexpected(cx, dir_tok),
                    };
                    try expect(cx, .brace_l);
                    const body = try parseScriptBlock(cx);
                    return storeNode(cx, token, .{ .@"for" = .{
                        .accumulator = target,
                        .start = start,
                        .end = end,
                        .direction = dir,
                        .body = body,
                    } });
                } else {
                    return reportUnexpected(cx, token2);
                }
            },
            .do => {
                try expect(cx, .brace_l);
                const body = try parseScriptBlock(cx);
                const token2 = consumeRight(cx);
                const condition: Ast.NodeIndex.Optional = condition: switch (token2.kind) {
                    .newline => .null,
                    .identifier => {
                        _ = try parseIdentifier(cx, token2, enum { until });
                        const next = consumeRight(cx);
                        const condition = try parseExpr(cx, next, .space);
                        try expect(cx, .newline);
                        break :condition condition.wrap();
                    },
                    else => return reportUnexpected(cx, token2),
                };
                return storeNode(cx, token, .{ .do = .{
                    .body = body,
                    .condition = condition,
                } });
            },
            .case => {
                const case_value = try parseExpr(cx, consumeRight(cx), .space);
                try expect(cx, .brace_l);
                var branches: BoundedArray(Ast.NodeIndex, Ast.max_case_branches) = .{};
                while (true) {
                    const token2 = consumeDown(cx);
                    if (token2.kind == .brace_r) break;
                    const condition: Ast.CaseCondition = if (token2.kind == .bracket_l) blk: {
                        const list = try parseList(cx);
                        try expectDown(cx, .bracket_r);
                        break :blk .{ .in = list };
                    } else if (parseIdentifierOpt(cx, token2, enum { @"else" })) |_|
                        .default
                    else
                        .{ .eq = try parseExpr(cx, token2, .space) };
                    try expect(cx, .brace_l);
                    const stmts = try parseScriptBlock(cx);
                    const node_index = try storeNode(cx, token2, .{ .case_branch = .{
                        .condition = condition,
                        .body = stmts,
                    } });
                    try appendNode(cx, &branches, node_index);

                    // Don't allow more branches after an `else`
                    if (condition == .default) {
                        try expectDown(cx, .brace_r);
                        break;
                    }
                }
                return storeNode(cx, token, .{ .case = .{
                    .value = case_value,
                    .branches = try storeExtra(cx, branches.slice()),
                } });
            },
        };
    }

    const ei = try parseExpr(cx, token, .all);

    if (peekRight(cx).kind == .eq) {
        const eq_token = consumeRight(cx);
        const rhs = try parseExpr(cx, consumeRight(cx), .all);
        try expect(cx, .newline);
        return try storeNode(cx, eq_token, .{ .set = .{ .lhs = ei, .rhs = rhs } });
    }

    if (getBinOp(peekRight(cx))) |op| {
        const op_token = consumeRight(cx);
        try expect(cx, .eq);
        const rhs = try parseExpr(cx, consumeRight(cx), .all);
        try expect(cx, .newline);
        return try storeNode(cx, op_token, .{ .binop_assign = .{
            .op = op,
            .lhs = ei,
            .rhs = rhs,
        } });
    }

    try expect(cx, .newline);
    return try makeExprTopLevel(cx, ei);
}

fn parseTopLevelExpr(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    const result = try parseExpr(cx, token, .all);
    return makeExprTopLevel(cx, result);
}

fn makeExprTopLevel(cx: *Cx, ei: Ast.NodeIndex) !Ast.NodeIndex {
    if (cx.result.nodes.at(ei).* == .identifier) {
        // A lonely identifier is a call with 0 args
        const token = cx.result.node_tokens.get(ei);
        return storeNodeWithTokenIndex(cx, token, .{ .call = .{ .callee = ei, .args = .empty } });
    }
    return ei;
}

pub const Precedence = enum {
    all,
    logical,
    equality,
    inequality,
    add,
    mul,
    bit,
    space,

    pub fn oneLower(self: Precedence) Precedence {
        return @enumFromInt(@intFromEnum(self) - 1);
    }
};

fn parseExpr(cx: *Cx, token: *const lexer.Token, prec: Precedence) ParseError!Ast.NodeIndex {
    var cur = try parseUnit(cx, token);
    while (true) {
        if (getBinOp(peekWrapped(cx))) |op| {
            // If it's a binop assign like `+=`, fall back to where it's
            // handled in `parseStatement`.
            if (peekWrappedSecond(cx).kind == .eq) break;

            cur = try parseBinOp(cx, cur, prec, op) orelse break;
        } else if (isAtomToken(peekRight(cx))) {
            if (@intFromEnum(prec) >= @intFromEnum(Precedence.space)) break;
            const args = try parseList(cx);
            cur = try storeNode(cx, token, .{ .call = .{ .callee = cur, .args = args } });
        } else break;
    }
    return cur;
}

fn parseUnit(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    var cur = try parseAtom(cx, token);
    if (peekRight(cx).kind == .bracket_l and !anyWhitespaceBetweenNextToken(cx)) {
        const token2 = consumeRight(cx);
        const index = try parseExpr(cx, consumeDown(cx), .all);
        try expectDown(cx, .bracket_r);
        cur = try storeNode(cx, token2, .{ .array_get = .{ .lhs = cur, .index = index } });
    }
    if (peekRight(cx).kind == .bracket_l and !anyWhitespaceBetweenNextToken(cx)) {
        _ = consumeRight(cx);
        const index2 = try parseExpr(cx, consumeDown(cx), .all);
        try expectDown(cx, .bracket_r);
        const cur_node = cx.result.nodes.at(cur);
        const lhs = cur_node.array_get.lhs;
        const index1 = cur_node.array_get.index;
        cur_node.* = .{ .array_get2 = .{
            .lhs = lhs,
            .index1 = index1,
            .index2 = index2,
        } };
    }
    return cur;
}

fn isAtomToken(token: *const lexer.Token) bool {
    return switch (token.kind) {
        .integer, .char, .string, .identifier, .paren_l, .bracket_l => true,
        else => false,
    };
}

fn parseAtom(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    switch (token.kind) {
        .integer => {
            const source = cx.source[token.span.start.offset..token.span.end.offset];
            const integer = std.fmt.parseInt(i32, source, 10) catch
                return reportError(cx, token, "invalid integer", .{});
            return try storeNode(cx, token, .{ .integer = integer });
        },
        .char => {
            std.debug.assert(token.span.end.offset == token.span.start.offset + 3);
            const char = cx.source[token.span.start.offset + 1];
            return try storeNode(cx, token, .{ .integer = char });
        },
        .string => {
            const string = try parseString(cx, token);
            return try storeNode(cx, token, .{ .string = string });
        },
        .identifier => {
            const identifier = try getIdentifier(cx, token);
            return try storeNode(cx, token, .{ .identifier = identifier });
        },
        .paren_l => {
            const next = consumeDown(cx);
            const result = try parseTopLevelExpr(cx, next);
            try expectDown(cx, .paren_r);
            return result;
        },
        .bracket_l => {
            const items = try parseList(cx);
            try expectDown(cx, .bracket_r);
            return storeNode(cx, token, .{ .list = .{ .items = items } });
        },
        else => return reportUnexpected(cx, token),
    }
}

fn getBinOp(token: *const lexer.Token) ?Ast.BinOp {
    return switch (token.kind) {
        .eq_eq => .eq,
        .bang_eq => .ne,
        .lt => .lt,
        .lt_eq => .le,
        .gt => .gt,
        .gt_eq => .ge,
        .plus => .add,
        .minus => .sub,
        .star => .mul,
        .slash => .div,
        .percent => .mod,
        .lt_lt => .shl,
        .gt_gt => .shr,
        .amp_amp => .land,
        .pipe_pipe => .lor,
        else => null,
    };
}

fn parseBinOp(cx: *Cx, lhs: Ast.NodeIndex, prec: Precedence, op: Ast.BinOp) !?Ast.NodeIndex {
    if (@intFromEnum(prec) >= @intFromEnum(op.precedence())) return null;
    const token = consumeWrapped(cx);
    std.debug.assert(getBinOp(token) == op);
    const rhs = try parseExpr(cx, consumeWrapped(cx), op.precedence());
    return try storeNode(cx, token, .{ .binop = .{
        .op = op,
        .lhs = lhs,
        .rhs = rhs,
    } });
}

fn parseList(cx: *Cx) !Ast.ExtraSlice {
    var result: BoundedArray(Ast.NodeIndex, 64) = .{};
    while (true) {
        const token = peekWrapped(cx);
        if (!isAtomToken(token)) break;
        _ = consumeWrapped(cx);
        const node = try parseExpr(cx, token, .space);
        try appendNode(cx, &result, node);
    }
    return try storeExtra(cx, result.slice());
}

fn parseIdentifierNode(cx: *Cx, token: *const lexer.Token) !Ast.NodeIndex {
    if (token.kind != .identifier)
        return reportExpected(cx, token, .identifier);
    const identifier = try getIdentifier(cx, token);
    return storeNode(cx, token, .{ .identifier = identifier });
}

fn storeNode(cx: *Cx, token: *const lexer.Token, node: Ast.Node) !Ast.NodeIndex {
    const token_index = recoverTokenIndex(cx, token);
    return storeNodeWithTokenIndex(cx, token_index, node);
}

fn storeNodeWithTokenIndex(cx: *Cx, token_index: lexer.TokenIndex, node: Ast.Node) !Ast.NodeIndex {
    const result = try cx.result.nodes.append(cx.gpa, node);
    _ = try cx.result.node_tokens.append(cx.gpa, token_index);
    return result;
}

fn recoverTokenIndex(cx: *Cx, token: *const lexer.Token) lexer.TokenIndex {
    return .fromIndex(@intCast(token - cx.lex.tokens.at(.fromIndex(0))));
}

fn appendNode(cx: *Cx, nodes: anytype, node_index: Ast.NodeIndex) !void {
    nodes.append(node_index) catch {
        const token_index = cx.result.node_tokens.get(node_index);
        const token = cx.lex.tokens.at(token_index);
        return reportError(cx, token, "too many children", .{});
    };
}

fn appendU32(cx: *Cx, token: *const lexer.Token, nodes: anytype, int: u32) !void {
    nodes.append(int) catch {
        return reportError(cx, token, "too many children", .{});
    };
}

fn storeExtra(cx: *Cx, items: []const Ast.NodeIndex) !Ast.ExtraSlice {
    return storeExtraU32(cx, @ptrCast(items));
}

fn storeExtraOpt(cx: *Cx, items: []const Ast.NodeIndex.Optional) !Ast.ExtraSlice {
    return storeExtraU32(cx, @ptrCast(items));
}

fn storeExtraU32(cx: *Cx, items: []const u32) !Ast.ExtraSlice {
    const start: u32 = @intCast(cx.result.extra.items.len);
    const len: u32 = @intCast(items.len);
    try cx.result.extra.appendSlice(cx.gpa, items);
    return .{ .start = start, .len = len };
}

fn peekRight(cx: *const Cx) *const lexer.Token {
    return cx.lex.tokens.at(.fromIndex(cx.token_index));
}

fn peekDown(cx: *const Cx) *const lexer.Token {
    const index = findNonNewline(cx);
    return cx.lex.tokens.at(.fromIndex(index));
}

fn wrappedPeekIndex(cx: *const Cx) u32 {
    return if (cx.bracket_nesting == 0)
        cx.token_index
    else
        findNonNewline(cx);
}

fn peekWrapped(cx: *const Cx) *const lexer.Token {
    const index = wrappedPeekIndex(cx);
    return cx.lex.tokens.at(.fromIndex(index));
}

fn peekWrappedSecond(cx: *const Cx) *const lexer.Token {
    const index = wrappedPeekIndex(cx) + 1;
    return cx.lex.tokens.at(.fromIndex(index));
}

fn consumeRight(cx: *Cx) *const lexer.Token {
    const result = cx.lex.tokens.at(.fromIndex(cx.token_index));
    cx.token_index += 1;
    switch (result.kind) {
        .paren_l, .bracket_l => cx.bracket_nesting += 1,
        .paren_r, .bracket_r => cx.bracket_nesting -= 1,
        else => {},
    }
    return result;
}

fn consumeDown(cx: *Cx) *const lexer.Token {
    skipNewlines(cx);
    return consumeRight(cx);
}

fn consumeWrapped(cx: *Cx) *const lexer.Token {
    if (cx.bracket_nesting != 0)
        skipNewlines(cx);
    return consumeRight(cx);
}

fn findNonNewline(cx: *const Cx) u32 {
    var i = cx.token_index;
    while (true) : (i += 1) {
        const token = cx.lex.tokens.at(.fromIndex(i));
        if (token.kind != .newline)
            return i;
    }
}

fn skipNewlines(cx: *Cx) void {
    cx.token_index = findNonNewline(cx);
}

fn anyWhitespaceBetweenNextToken(cx: *const Cx) bool {
    const prev = cx.lex.tokens.at(.fromIndex(cx.token_index - 1));
    const next = cx.lex.tokens.at(.fromIndex(cx.token_index));
    std.debug.assert(prev.span.end.offset <= next.span.start.offset);
    return prev.span.end.offset != next.span.start.offset;
}

fn expectString(cx: *Cx) !Ast.StringSlice {
    const token = consumeRight(cx);
    if (token.kind != .string)
        return reportExpected(cx, token, .string);
    return parseString(cx, token);
}

fn parseString(cx: *Cx, token: *const lexer.Token) !Ast.StringSlice {
    std.debug.assert(token.kind == .string);
    const source = cx.source[token.span.start.offset..token.span.end.offset];
    const str = source[1 .. source.len - 1];
    const result_start: u32 = @intCast(cx.result.strings.buf.items.len);
    // yeah uhhhh sorry if you're reading this, but it works
    var i: usize = 0;
    while (true) {
        const next_escape = std.mem.indexOfScalar(u8, str[i..], '\\') orelse {
            try cx.result.strings.buf.appendSlice(cx.gpa, str[i..]);
            break;
        };
        try cx.result.strings.buf.appendSlice(cx.gpa, str[i..][0..next_escape]);
        i += next_escape + 1;
        if (i >= str.len)
            return reportError(cx, token, "invalid string", .{});
        const e = str[i];
        i += 1;
        switch (e) {
            '\\' => try cx.result.strings.buf.append(cx.gpa, '\\'),
            'x' => {
                if (i + 2 > str.len)
                    return reportError(cx, token, "invalid string", .{});
                const hex = str[i..][0..2];
                i += 2;
                const num = std.fmt.parseInt(u8, hex, 16) catch
                    return reportError(cx, token, "invalid string", .{});
                try cx.result.strings.buf.append(cx.gpa, num);
            },
            else => return reportError(cx, token, "invalid string", .{}),
        }
    }
    const result_len: u32 = @intCast(cx.result.strings.buf.items.len - result_start);
    return .{ .start = result_start, .len = result_len };
}

fn parseHexString(cx: *Cx, token: *const lexer.Token) !Ast.StringSlice {
    std.debug.assert(token.kind == .hex_string);
    const source = cx.source[token.span.start.offset..token.span.end.offset];
    const str = source[1 .. source.len - 1];
    const len = std.math.divExact(usize, str.len, 2) catch
        return reportError(cx, token, "invalid hex string", .{});

    const result_start: u32 = @intCast(cx.result.strings.buf.items.len);
    const dest = try cx.result.strings.buf.addManyAsSlice(cx.gpa, len);
    _ = std.fmt.hexToBytes(dest, str) catch
        return reportError(cx, token, "invalid hex string", .{});
    const result_len: u32 = @intCast(cx.result.strings.buf.items.len - result_start);
    return .{ .start = result_start, .len = result_len };
}

fn expectInteger(cx: *Cx, T: type) !T {
    const token = consumeRight(cx);
    if (token.kind != .integer)
        return reportExpected(cx, token, .integer);
    const source = cx.source[token.span.start.offset..token.span.end.offset];
    const int = std.fmt.parseInt(i32, source, 10) catch
        return reportError(cx, token, "invalid integer", .{});
    return std.math.cast(T, int) orelse
        return reportError(cx, token, "out of range", .{});
}

fn expectIdentifier(cx: *Cx) !Ast.StringSlice {
    const token = consumeRight(cx);
    if (token.kind != .identifier)
        return reportExpected(cx, token, .integer);
    return getIdentifier(cx, token);
}

fn parseIdentifierOpt(cx: *Cx, token: *const lexer.Token, T: type) ?T {
    if (token.kind != .identifier) return null;
    const identifier = cx.source[token.span.start.offset..token.span.end.offset];
    return std.meta.stringToEnum(T, identifier);
}

fn parseIdentifier(cx: *Cx, token: *const lexer.Token, T: type) !T {
    return parseIdentifierOpt(cx, token, T) orelse
        return reportUnexpected(cx, token);
}

fn getIdentifier(cx: *Cx, token: *const lexer.Token) !Ast.StringSlice {
    std.debug.assert(token.kind == .identifier);
    const str = cx.source[token.span.start.offset..token.span.end.offset];
    return cx.result.strings.add(cx.gpa, str);
}

fn expectBlockId(cx: *Cx) !BlockId {
    const token = consumeRight(cx);
    const ident = try getIdentifier(cx, token);
    return BlockId.parse(cx.result.strings.get(ident)) orelse
        reportError(cx, token, "invalid block id", .{});
}

fn expect(cx: *Cx, kind: lexer.Token.Kind) !void {
    const token = consumeRight(cx);
    if (token.kind != kind)
        return reportExpected(cx, token, kind);
}

fn expectDown(cx: *Cx, kind: lexer.Token.Kind) !void {
    std.debug.assert(kind != .newline);
    skipNewlines(cx);
    return expect(cx, kind);
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
    const loc = token.span.start;
    cx.diag.err(loc, message, args);
    return error.AddedToDiagnostic;
}

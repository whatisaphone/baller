const std = @import("std");

const Ast = @import("Ast.zig");
const Diagnostic = @import("Diagnostic.zig");
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
    var index_children_opt: ?std.BoundedArray(Ast.NodeIndex, 16) = null;
    var disks: [2]Ast.NodeIndex = @splat(Ast.null_node);

    while (true) {
        skipWhitespace(state);
        const token = consumeToken(state);
        switch (token.kind) {
            .identifier => {
                const identifier = state.source[token.span.start.offset..token.span.end.offset];
                if (std.mem.eql(u8, identifier, "index")) {
                    if (index_children_opt != null)
                        return reportError(state, token.span, "duplicate index", .{});
                    index_children_opt = try parseIndex(state);
                } else if (std.mem.eql(u8, identifier, "disk")) {
                    try parseDisk(state, token.span, &disks);
                } else {
                    return reportUnexpected(state, token);
                }
            },
            .eof => break,
            else => return reportUnexpected(state, token),
        }
    }

    const index_children = index_children_opt orelse
        return reportError(state, .{ .start = .origin, .end = .origin }, "missing index", .{});
    const index_extra = try appendExtra(state, index_children.slice());
    const disks_extra = try appendExtra(state, &disks);

    return appendNode(state, .{ .project = .{
        .index = index_extra,
        .disks = disks_extra,
    } });
}

fn parseIndex(state: *State) !std.BoundedArray(Ast.NodeIndex, 16) {
    try expect(state, .brace_l);

    var children: std.BoundedArray(Ast.NodeIndex, 16) = .{};

    while (true) {
        skipWhitespace(state);
        const token = consumeToken(state);
        switch (token.kind) {
            .identifier => {
                const identifier = state.source[token.span.start.offset..token.span.end.offset];
                if (std.mem.eql(u8, identifier, "raw-block")) {
                    const node_index = try parseRawBlock(state, token.span);
                    children.append(node_index) catch
                        return reportError(state, token.span, "too many children", .{});
                } else if (std.mem.eql(u8, identifier, "index-block")) {
                    const block_id_str = try expectString(state);
                    try expect(state, .newline);
                    const IndexBlock = @FieldType(Ast.Node, "index_block");
                    const block_id = std.meta.stringToEnum(IndexBlock, block_id_str) orelse
                        return reportError(state, token.span, "unsupported block id", .{});
                    const index_block = try appendNode(state, .{ .index_block = block_id });
                    children.append(index_block) catch
                        return reportError(state, token.span, "too many children", .{});
                } else {
                    return reportUnexpected(state, token);
                }
            },
            .brace_r => break,
            else => return reportUnexpected(state, token),
        }
    }

    return children;
}

fn parseDisk(state: *State, span: lexer.Span, disks: *[2]Ast.NodeIndex) !void {
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
            .identifier => {
                const identifier = state.source[token.span.start.offset..token.span.end.offset];
                if (std.mem.eql(u8, identifier, "raw-block")) {
                    const node_index = try parseRawBlock(state, token.span);
                    children.append(node_index) catch
                        return reportError(state, token.span, "too many children", .{});
                } else if (std.mem.eql(u8, identifier, "room")) {
                    const node_index = try parseDiskRoom(state, token.span);
                    children.append(node_index) catch
                        return reportError(state, token.span, "too many children", .{});
                } else {
                    return reportUnexpected(state, token);
                }
            },
            .brace_r => break,
            else => return reportUnexpected(state, token),
        }
    }

    disks[disk_index] = try appendNode(state, .{ .disk = .{
        .children = try appendExtra(state, children.slice()),
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

    return appendNode(state, .{ .disk_room = .{
        .room_number = room_number,
        .name = room_name,
        .path = path,
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
    var children: std.BoundedArray(Ast.NodeIndex, 2560) = .{};

    while (true) {
        skipWhitespace(state);
        const token = consumeToken(state);
        switch (token.kind) {
            .identifier => {
                const identifier = state.source[token.span.start.offset..token.span.end.offset];
                if (std.mem.eql(u8, identifier, "raw-block")) {
                    const node_index = try parseRawBlock(state, token.span);
                    children.append(node_index) catch
                        return reportError(state, token.span, "too many children", .{});
                } else if (std.mem.eql(u8, identifier, "raw-glob")) {
                    const node_index = try parseRawGlob(state, token.span);
                    children.append(node_index) catch
                        return reportError(state, token.span, "too many children", .{});
                } else if (std.mem.eql(u8, identifier, "awiz")) {
                    const node_index = try parseAwiz(state, token.span);
                    children.append(node_index) catch
                        return reportError(state, token.span, "too many children", .{});
                } else if (std.mem.eql(u8, identifier, "mult")) {
                    const node_index = try parseMult(state, token.span);
                    children.append(node_index) catch
                        return reportError(state, token.span, "too many children", .{});
                } else {
                    return reportUnexpected(state, token);
                }
            },
            .eof => break,
            else => return reportUnexpected(state, token),
        }
    }

    return appendNode(state, .{ .room_file = .{
        .children = try appendExtra(state, children.slice()),
    } });
}

fn parseAwiz(state: *State, span: lexer.Span) !Ast.NodeIndex {
    const glob_number_i32 = try expectInteger(state);
    try expect(state, .brace_l);

    const glob_number = std.math.cast(u16, glob_number_i32) orelse
        return reportError(state, span, "invalid glob number", .{});

    const children = try parseAwizChildren(state);

    return appendNode(state, .{ .awiz = .{
        .glob_number = glob_number,
        .children = children,
    } });
}

fn parseAwizChildren(state: *State) !Ast.ExtraSlice {
    var children: std.BoundedArray(Ast.NodeIndex, awiz.Awiz.max_blocks) = .{};

    while (true) {
        skipWhitespace(state);
        const token = consumeToken(state);
        switch (token.kind) {
            .identifier => {
                const identifier = state.source[token.span.start.offset..token.span.end.offset];
                if (std.mem.eql(u8, identifier, "rgbs")) {
                    try expect(state, .newline);

                    const node_index = try appendNode(state, .awiz_rgbs);
                    children.append(node_index) catch
                        return reportError(state, token.span, "too many children", .{});
                } else if (std.mem.eql(u8, identifier, "two-ints")) {
                    const block_id_str = try expectString(state);
                    const ints = .{ try expectInteger(state), try expectInteger(state) };
                    try expect(state, .newline);

                    const block_id = parseBlockId(block_id_str) orelse
                        return reportError(state, token.span, "invalid block id", .{});

                    const node_index = try appendNode(state, .{ .awiz_two_ints = .{
                        .block_id = block_id,
                        .ints = ints,
                    } });
                    children.append(node_index) catch
                        return reportError(state, token.span, "too many children", .{});
                } else if (std.mem.eql(u8, identifier, "wizh")) {
                    try expect(state, .newline);

                    const node_index = try appendNode(state, .awiz_wizh);
                    children.append(node_index) catch
                        return reportError(state, token.span, "too many children", .{});
                } else if (std.mem.eql(u8, identifier, "bmp")) {
                    const compression_int = try expectInteger(state);
                    const path = try expectString(state);
                    try expect(state, .newline);

                    const compression = std.meta.intToEnum(awiz.Compression, compression_int) catch
                        return reportError(state, token.span, "invalid compression", .{});

                    const node_index = try appendNode(state, .{ .awiz_bmp = .{
                        .compression = compression,
                        .path = path,
                    } });
                    children.append(node_index) catch
                        return reportError(state, token.span, "too many children", .{});
                } else {
                    return reportUnexpected(state, token);
                }
            },
            .brace_r => break,
            else => return reportUnexpected(state, token),
        }
    }

    return appendExtra(state, children.slice());
}

fn parseMult(state: *State, span: lexer.Span) !Ast.NodeIndex {
    const glob_number_i32 = try expectInteger(state);
    try expect(state, .brace_l);

    const glob_number = std.math.cast(u16, glob_number_i32) orelse
        return reportError(state, span, "invalid glob number", .{});

    var raw_defa_path: ?[]const u8 = null;
    var children: std.BoundedArray(Ast.NodeIndex, Ast.max_mult_children) = .{};
    var indices_opt: ?Ast.ExtraSlice = null;

    while (true) {
        skipWhitespace(state);
        const token = consumeToken(state);
        switch (token.kind) {
            .identifier => {
                const identifier = state.source[token.span.start.offset..token.span.end.offset];
                if (std.mem.eql(u8, identifier, "defa-raw")) {
                    const path = try expectString(state);
                    try expect(state, .newline);

                    if (raw_defa_path != null)
                        return reportError(state, token.span, "too many children", .{});
                    raw_defa_path = path;
                } else if (std.mem.eql(u8, identifier, "awiz")) {
                    try expect(state, .brace_l);
                    const awiz_children = try parseAwizChildren(state);

                    const node_index = try appendNode(state, .{ .mult_awiz = .{
                        .children = awiz_children,
                    } });
                    children.append(node_index) catch
                        return reportError(state, token.span, "too many children", .{});
                } else if (std.mem.eql(u8, identifier, "indices")) {
                    if (indices_opt != null)
                        return reportError(state, token.span, "too many children", .{});
                    try expect(state, .bracket_l);
                    indices_opt = try parseIntegerList(state);
                } else {
                    return reportUnexpected(state, token);
                }
            },
            .brace_r => break,
            else => return reportUnexpected(state, token),
        }
    }

    const indices = indices_opt orelse return reportError(state, span, "missing indices", .{});
    for (state.result.getExtra(indices)) |index|
        if (index >= children.len)
            return reportError(state, span, "out of range", .{});

    return appendNode(state, .{ .mult = .{
        .glob_number = glob_number,
        .raw_defa_path = raw_defa_path,
        .children = try appendExtra(state, children.slice()),
        .indices = indices,
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
                result.append(int) catch
                    return reportError(state, token.span, "too many children", .{});
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
    return appendExtra(state, result.slice());
}

fn parseRawBlock(state: *State, span: lexer.Span) !Ast.NodeIndex {
    const block_id_str = try expectString(state);
    const path = try expectString(state);
    try expect(state, .newline);

    const block_id = parseBlockId(block_id_str) orelse
        return reportError(state, span, "invalid block id", .{});

    return appendNode(state, .{ .raw_block = .{
        .block_id = block_id,
        .path = path,
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

    return appendNode(state, .{ .raw_glob_file = .{
        .block_id = block_id,
        .glob_number = glob_number,
        .path = path,
    } });
}

fn parseRawGlobBlock(state: *State, block_id: BlockId, glob_number: u16) !Ast.NodeIndex {
    var children: std.BoundedArray(Ast.NodeIndex, 512) = .{};

    while (true) {
        skipWhitespace(state);
        const token = consumeToken(state);
        switch (token.kind) {
            .identifier => {
                const identifier = state.source[token.span.start.offset..token.span.end.offset];
                if (std.mem.eql(u8, identifier, "raw-block")) {
                    const node_index = try parseRawBlock(state, token.span);
                    children.append(node_index) catch
                        return reportError(state, token.span, "too many children", .{});
                } else {
                    return reportUnexpected(state, token);
                }
            },
            .brace_r => break,
            else => return reportUnexpected(state, token),
        }
    }

    return appendNode(state, .{ .raw_glob_block = .{
        .block_id = block_id,
        .glob_number = glob_number,
        .children = try appendExtra(state, children.slice()),
    } });
}

fn appendNode(state: *State, node: Ast.Node) !Ast.NodeIndex {
    const result: u32 = @intCast(state.result.nodes.items.len);
    try state.result.nodes.append(state.gpa, node);
    return result;
}

fn appendExtra(state: *State, items: []const u32) !Ast.ExtraSlice {
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

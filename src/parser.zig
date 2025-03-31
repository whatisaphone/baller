const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const awiz = @import("awiz.zig");
const BlockId = @import("block_id.zig").BlockId;
const parseBlockId = @import("block_id.zig").parseBlockId;
const lexer = @import("lexer.zig");

pub const Ast = struct {
    root: NodeIndex,
    nodes: std.ArrayListUnmanaged(Node),
    extra: std.ArrayListUnmanaged(u32),

    pub fn deinit(self: *Ast, gpa: std.mem.Allocator) void {
        self.extra.deinit(gpa);
        self.nodes.deinit(gpa);
    }

    pub fn getExtra(self: *const Ast, slice: ExtraSlice) []const u32 {
        return self.extra.items[slice.start..][0..slice.len];
    }
};

pub const NodeIndex = u32;
pub const null_node: NodeIndex = std.math.maxInt(NodeIndex);

pub const Node = union(enum) {
    project: struct {
        index: ExtraSlice,
        disks: ExtraSlice,
    },
    index_block: enum { DIRI, DIRR, DIRS, DIRN, DIRC, DIRF, DIRM, DIRT, DLFL, DISK, RNAM },
    disk: struct {
        children: ExtraSlice,
    },
    disk_room: struct {
        room_number: u8,
        name: []const u8,
        path: []const u8,
    },
    room_file: struct {
        children: ExtraSlice,
    },
    raw_block: struct {
        block_id: BlockId,
        path: []const u8,
    },
    raw_glob_file: struct {
        block_id: BlockId,
        glob_number: u16,
        path: []const u8,
    },
    raw_glob_block: struct {
        block_id: BlockId,
        glob_number: u16,
        children: ExtraSlice,
    },
    awiz: struct {
        glob_number: u16,
        children: ExtraSlice,
    },
    awiz_rgbs,
    awiz_two_ints: struct {
        block_id: BlockId,
        ints: [2]i32,
    },
    awiz_wizh,
    awiz_bmp: struct {
        compression: awiz.Compression,
        path: []const u8,
    },
};

const ExtraSlice = struct {
    start: u32,
    len: u32,
};

const State = struct {
    gpa: std.mem.Allocator,
    diagnostic: *const Diagnostic,
    source: []const u8,
    lex: *const lexer.Lex,
    token_index: u32,
    result: Ast,
};

const ParseError = error{ OutOfMemory, Reported };

pub fn parseProject(
    gpa: std.mem.Allocator,
    diagnostic: *const Diagnostic,
    source: []const u8,
    lex: *const lexer.Lex,
) ParseError!Ast {
    var state: State = .{
        .gpa = gpa,
        .diagnostic = diagnostic,
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

fn parseProjectChildren(state: *State) !NodeIndex {
    var index_children_opt: ?std.BoundedArray(NodeIndex, 16) = null;
    var disks: [2]NodeIndex = @splat(null_node);

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

fn parseIndex(state: *State) !std.BoundedArray(NodeIndex, 16) {
    try expect(state, .brace_l);

    var children: std.BoundedArray(NodeIndex, 16) = .{};

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
                    const IndexBlock = @FieldType(Node, "index_block");
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

fn parseDisk(state: *State, span: lexer.Span, disks: *[2]NodeIndex) !void {
    const disk_number = try expectInteger(state);
    try expect(state, .brace_l);

    if (!(1 <= disk_number and disk_number <= disks.len))
        return reportError(state, span, "disk number out of range", .{});
    const disk_index: u8 = @intCast(disk_number - 1);
    if (disks[disk_index] != null_node)
        return reportError(state, span, "duplicate disk number", .{});

    var children: std.BoundedArray(NodeIndex, 32) = .{};

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

fn parseDiskRoom(state: *State, span: lexer.Span) !NodeIndex {
    const room_number_i32 = try expectInteger(state);
    const room_name = try expectString(state);
    const path = try expectString(state);
    try expect(state, .newline);

    if (!(1 <= room_number_i32 and room_number_i32 <= 255))
        return reportError(state, span, "room number out of range", .{});
    const room_number: u8 = @intCast(room_number_i32);

    return appendNode(state, .{ .disk_room = .{
        .room_number = room_number,
        .name = room_name,
        .path = path,
    } });
}

pub fn parseRoom(
    gpa: std.mem.Allocator,
    diagnostic: *const Diagnostic,
    source: []const u8,
    lex: *const lexer.Lex,
) ParseError!Ast {
    var state: State = .{
        .gpa = gpa,
        .diagnostic = diagnostic,
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

fn parseRoomChildren(state: *State) !NodeIndex {
    var children: std.BoundedArray(NodeIndex, 2048) = .{};

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

fn parseAwiz(state: *State, span: lexer.Span) !NodeIndex {
    const glob_number_i32 = try expectInteger(state);
    try expect(state, .brace_l);

    const glob_number = std.math.cast(u16, glob_number_i32) orelse
        return reportError(state, span, "invalid glob number", .{});

    var children: std.BoundedArray(NodeIndex, awiz.Awiz.max_blocks) = .{};

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
                        return reportError(state, span, "invalid block id", .{});

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

    return appendNode(state, .{ .awiz = .{
        .glob_number = glob_number,
        .children = try appendExtra(state, children.slice()),
    } });
}

fn parseRawBlock(state: *State, span: lexer.Span) !NodeIndex {
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

fn parseRawGlob(state: *State, span: lexer.Span) !NodeIndex {
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
) !NodeIndex {
    try expect(state, .newline);

    const path_source = state.source[path_token.span.start.offset..path_token.span.end.offset];
    const path = path_source[1 .. path_source.len - 1];

    return appendNode(state, .{ .raw_glob_file = .{
        .block_id = block_id,
        .glob_number = glob_number,
        .path = path,
    } });
}

fn parseRawGlobBlock(state: *State, block_id: BlockId, glob_number: u16) !NodeIndex {
    var children: std.BoundedArray(NodeIndex, 512) = .{};

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

fn appendNode(state: *State, node: Node) !NodeIndex {
    const result: u32 = @intCast(state.result.nodes.items.len);
    try state.result.nodes.append(state.gpa, node);
    return result;
}

fn appendExtra(state: *State, items: []const u32) !ExtraSlice {
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
) error{Reported} {
    return reportError(
        state,
        found.span,
        "expected {s}, found {s}",
        .{ expected.describe(), found.kind.describe() },
    );
}

fn reportUnexpected(state: *State, found: *const lexer.Token) error{Reported} {
    return reportError(state, found.span, "unexpected {s}", .{found.kind.describe()});
}

fn reportError(
    state: *State,
    span: lexer.Span,
    comptime message: []const u8,
    args: anytype,
) error{Reported} {
    const out = std.io.getStdErr();
    out.writer().print(
        "{s}:{}:{}: ",
        .{ state.diagnostic.path, span.start.line, span.start.column },
    ) catch {};
    out.writer().print(message, args) catch {};
    out.writer().writeByte('\n') catch {};
    return error.Reported;
}

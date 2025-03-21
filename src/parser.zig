const std = @import("std");

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
        index_path: []const u8,
        disks: ExtraSlice,
    },
    disk: struct {
        children: ExtraSlice,
    },
    room: struct {
        children: ExtraSlice,
    },
    raw_block: struct {
        block_id: BlockId,
        path: []const u8,
    },
};

const ExtraSlice = struct {
    start: u32,
    len: u32,
};

const State = struct {
    gpa: std.mem.Allocator,
    source: []const u8,
    lex: *const lexer.Lex,
    token_index: u32,
    result: Ast,
};

const ParseError = error{ OutOfMemory, Reported };

pub fn run(gpa: std.mem.Allocator, source: []const u8, lex: *const lexer.Lex) ParseError!Ast {
    var state: State = .{
        .gpa = gpa,
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

    state.result.root = try parseProject(&state);
    return state.result;
}

fn parseProject(state: *State) !NodeIndex {
    var index_path_opt: ?[]const u8 = null;
    var disks: [2]NodeIndex = @splat(null_node);

    while (true) {
        skipWhitespace(state);
        const token = consumeToken(state);
        switch (token.kind) {
            .identifier => {
                const identifier = state.source[token.span.start.offset..token.span.end.offset];
                if (std.mem.eql(u8, identifier, "index")) {
                    if (index_path_opt != null)
                        return reportError(token.span, "duplicate index", .{});
                    index_path_opt = try expectString(state);
                    try expect(state, .newline);
                } else if (std.mem.eql(u8, identifier, "disk")) {
                    try parseDisk(state, token.span, &disks);
                } else {
                    return reportUnexpected(token);
                }
            },
            .eof => break,
            else => return reportUnexpected(token),
        }
    }

    const index_path = index_path_opt orelse
        return reportError(.{ .start = .origin, .end = .origin }, "missing index", .{});
    const disks_extra = try appendExtra(state, &disks);

    return appendNode(state, .{ .project = .{
        .index_path = index_path,
        .disks = disks_extra,
    } });
}

fn parseDisk(state: *State, span: lexer.Span, disks: *[2]NodeIndex) !void {
    const disk_number = try expectInteger(state);
    try expect(state, .brace_l);

    if (disk_number == 0)
        return reportError(span, "disk number out of range", .{});
    const disk_index = disk_number - 1;
    if (disk_index >= disks.len)
        return reportError(span, "disk number out of range", .{});
    if (disks[disk_index] != null_node)
        return reportError(span, "duplicate disk number", .{});

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
                        return reportError(token.span, "too many children", .{});
                } else if (std.mem.eql(u8, identifier, "room")) {
                    const node_index = try parseRoom(state);
                    children.append(node_index) catch
                        return reportError(token.span, "too many children", .{});
                } else {
                    return reportUnexpected(token);
                }
            },
            .brace_r => break,
            else => return reportUnexpected(token),
        }
    }

    disks[disk_index] = try appendNode(state, .{ .disk = .{
        .children = try appendExtra(state, children.slice()),
    } });
}

fn parseRoom(state: *State) !NodeIndex {
    try expect(state, .brace_l);

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
                        return reportError(token.span, "too many children", .{});
                } else {
                    return reportUnexpected(token);
                }
            },
            .brace_r => break,
            else => return reportUnexpected(token),
        }
    }

    return appendNode(state, .{ .room = .{
        .children = try appendExtra(state, children.slice()),
    } });
}

fn parseRawBlock(state: *State, span: lexer.Span) !NodeIndex {
    const block_id_str = try expectString(state);
    const path = try expectString(state);
    try expect(state, .newline);

    const block_id = parseBlockId(block_id_str) orelse
        return reportError(span, "invalid block id", .{});

    return appendNode(state, .{ .raw_block = .{
        .block_id = block_id,
        .path = path,
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
        return reportExpected(token, .string);
    const source = state.source[token.span.start.offset..token.span.end.offset];
    return source[1 .. source.len - 1];
}

fn expectInteger(state: *State) !u32 {
    const token = consumeToken(state);
    if (token.kind != .integer)
        return reportExpected(token, .integer);
    const source = state.source[token.span.start.offset..token.span.end.offset];
    return std.fmt.parseInt(u32, source, 10) catch
        reportError(token.span, "invalid integer", .{});
}

fn expect(state: *State, kind: lexer.Token.Kind) !void {
    const token = consumeToken(state);
    if (token.kind != kind)
        return reportExpected(token, kind);
}

fn reportExpected(found: *const lexer.Token, expected: lexer.Token.Kind) error{Reported} {
    return reportError(
        found.span,
        "expected {s}, found {s}",
        .{ expected.describe(), found.kind.describe() },
    );
}

fn reportUnexpected(found: *const lexer.Token) error{Reported} {
    return reportError(found.span, "unexpected {s}", .{found.kind.describe()});
}

fn reportError(span: lexer.Span, comptime message: []const u8, args: anytype) error{Reported} {
    const out = std.io.getStdErr();
    out.writer().print("{}:{}: ", .{ span.start.line, span.start.column }) catch {};
    out.writer().print(message, args) catch {};
    out.writer().writeByte('\n') catch {};
    return error.Reported;
}

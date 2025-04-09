const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");

pub const Lex = struct {
    tokens: std.ArrayListUnmanaged(Token),

    pub fn deinit(self: *Lex, gpa: std.mem.Allocator) void {
        self.tokens.deinit(gpa);
    }
};

pub const Loc = struct {
    offset: u32,
    line: u32,
    column: u32,

    pub const origin: Loc = .{
        .offset = 0,
        .line = 1,
        .column = 1,
    };
};

pub const Span = struct {
    start: Loc,
    end: Loc,
};

pub const Token = struct {
    span: Span,
    kind: Kind,

    pub const Kind = enum {
        eof,
        newline,
        comma,
        bracket_l,
        bracket_r,
        brace_l,
        brace_r,
        integer,
        string,
        identifier,

        pub fn describe(self: Kind) []const u8 {
            return switch (self) {
                .eof => "<eof>",
                .newline => "<newline>",
                .comma => "','",
                .bracket_l => "'['",
                .bracket_r => "']'",
                .brace_l => "'{'",
                .brace_r => "'}'",
                .integer => "<integer>",
                .string => "<string>",
                .identifier => "<identifier>",
            };
        }
    };
};

const State = struct {
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForTextFile,
    source: []const u8,
    loc: Loc,
    result: Lex,
};

pub const LexError = error{ OutOfMemory, AddedToDiagnostic };

pub fn run(
    gpa: std.mem.Allocator,
    diag: *const Diagnostic.ForTextFile,
    source: []const u8,
) LexError!Lex {
    var state: State = .{
        .gpa = gpa,
        .diag = diag,
        .source = source,
        .loc = .origin,
        .result = .{
            .tokens = .empty,
        },
    };
    errdefer state.result.deinit(gpa);

    while (true) {
        skipWhitespace(&state);
        const loc = state.loc;
        const ch = consumeChar(&state) orelse break;
        if (ch == '\n') {
            try appendToken(&state, loc, .newline);
        } else if (ch == ',') {
            try appendToken(&state, loc, .comma);
        } else if (ch == '[') {
            try appendToken(&state, loc, .bracket_l);
        } else if (ch == ']') {
            try appendToken(&state, loc, .bracket_r);
        } else if (ch == '{') {
            try appendToken(&state, loc, .brace_l);
        } else if (ch == '}') {
            try appendToken(&state, loc, .brace_r);
        } else if (ch == '-' and is_num: {
            const ch2 = peekChar(&state) orelse break :is_num false;
            break :is_num '0' <= ch2 and ch2 <= '9';
        }) {
            try lexInteger(&state, loc);
        } else if (ch >= '0' and ch <= '9') {
            try lexInteger(&state, loc);
        } else if (ch == '"') {
            try lexStringLiteral(&state, loc);
        } else if (isIdentStart(ch)) {
            try lexIdent(&state, loc);
        } else {
            return reportError(&state, loc, "unexpected character '{c}'", .{ch});
        }
    }

    try state.result.tokens.append(gpa, .{
        .span = .{ .start = state.loc, .end = state.loc },
        .kind = .eof,
    });
    return state.result;
}

fn skipWhitespace(state: *State) void {
    while (peekChar(state)) |ch| {
        if (ch == ' ')
            _ = consumeChar(state)
        else
            break;
    }
}

fn consumeChar(state: *State) ?u8 {
    if (state.loc.offset == state.source.len)
        return null;
    const ch = state.source[state.loc.offset];
    state.loc.offset += 1;
    if (ch == '\n') {
        state.loc.line += 1;
        state.loc.column = 1;
    } else {
        state.loc.column += 1;
    }
    return ch;
}

fn peekChar(state: *const State) ?u8 {
    if (state.loc.offset == state.source.len)
        return null;
    return state.source[state.loc.offset];
}

fn appendToken(state: *State, start: Loc, kind: Token.Kind) !void {
    try state.result.tokens.append(state.gpa, .{
        .span = .{ .start = start, .end = state.loc },
        .kind = kind,
    });
}

fn lexIdent(state: *State, start: Loc) !void {
    while (peekChar(state)) |ch| {
        if (isIdentContinue(ch))
            _ = consumeChar(state)
        else
            break;
    }
    try appendToken(state, start, .identifier);
}

fn lexStringLiteral(state: *State, start: Loc) !void {
    while (true) {
        const ch = consumeChar(state) orelse
            return reportError(state, start, "string not terminated", .{});
        if (ch == '"')
            break;
    }
    try appendToken(state, start, .string);
}

fn lexInteger(state: *State, start: Loc) !void {
    while (peekChar(state)) |ch| {
        if ('0' <= ch and ch <= '9')
            _ = consumeChar(state)
        else
            break;
    }
    try appendToken(state, start, .integer);
}

fn isIdentStart(ch: u8) bool {
    return 'A' <= ch and ch <= 'Z' or 'a' <= ch and ch <= 'z';
}

fn isIdentContinue(ch: u8) bool {
    return isIdentStart(ch) or ch == '-' or ch == '_';
}

fn reportError(
    state: *const State,
    loc: Loc,
    comptime fmt: []const u8,
    args: anytype,
) error{AddedToDiagnostic} {
    state.diag.err(loc.line, loc.column, fmt, args);
    return error.AddedToDiagnostic;
}

const std = @import("std");

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
        brace_l,
        brace_r,
        integer,
        string,
        identifier,

        pub fn describe(self: Kind) []const u8 {
            return switch (self) {
                .eof => "<eof>",
                .newline => "<newline>",
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
    source: []const u8,
    loc: Loc,
    result: Lex,
};

pub const LexError = error{ OutOfMemory, Reported };

pub fn run(gpa: std.mem.Allocator, source: []const u8) LexError!Lex {
    var state: State = .{
        .gpa = gpa,
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
        } else if (ch == '{') {
            try appendToken(&state, loc, .brace_l);
        } else if (ch == '}') {
            try appendToken(&state, loc, .brace_r);
        } else if (ch >= '0' and ch <= '9') {
            try lexInteger(&state, loc);
        } else if (ch == '"') {
            try lexStringLiteral(&state, loc);
        } else if (isIdentStart(ch)) {
            try lexIdent(&state, loc);
        } else {
            return reportError(loc, "unexpected character '{c}'", .{ch});
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
            return reportError(start, "string not terminated", .{});
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

fn reportError(loc: Loc, comptime message: []const u8, args: anytype) error{Reported} {
    const out = std.io.getStdErr();
    out.writer().print("{}:{}: ", .{ loc.line, loc.column }) catch {};
    out.writer().print(message, args) catch {};
    out.writer().writeByte('\n') catch {};
    return error.Reported;
}

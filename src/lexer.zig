const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const keyed = @import("keyed.zig");

pub const TokenIndex = keyed.Key(enum(u32) {});

pub const Lex = struct {
    tokens: keyed.List(TokenIndex, Token),

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
        colon,
        bang_eq,
        lt,
        lt_lt,
        lt_eq,
        eq,
        eq_eq,
        gt,
        gt_gt,
        gt_eq,
        plus,
        minus,
        star,
        slash,
        percent,
        amp_amp,
        pipe_pipe,
        underscore,
        paren_l,
        paren_r,
        bracket_l,
        bracket_r,
        brace_l,
        brace_r,
        swat,
        integer,
        char,
        string,
        hex_string,
        identifier,

        pub fn describe(self: Kind) []const u8 {
            return switch (self) {
                .eof => "<eof>",
                .newline => "<newline>",
                .comma => "','",
                .colon => "':'",
                .bang_eq => "'!='",
                .lt => "'<'",
                .lt_lt => "'<<'",
                .lt_eq => "'<='",
                .eq => "'='",
                .eq_eq => "'=='",
                .gt => "'>'",
                .gt_gt => "'>>'",
                .gt_eq => "'>='",
                .plus => "'+'",
                .minus => "'-'",
                .star => "'*'",
                .slash => "'/'",
                .percent => "'%'",
                .amp_amp => "'&&'",
                .pipe_pipe => "'||'",
                .underscore => "'_'",
                .paren_l => "'('",
                .paren_r => "')'",
                .bracket_l => "'['",
                .bracket_r => "']'",
                .brace_l => "'{'",
                .brace_r => "'}'",
                .swat => "'@'",
                .integer => "<integer>",
                .char => "<character>",
                .string => "<string>",
                .hex_string => "<hex-string>",
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
        } else if (ch == '\r' and peekChar(&state) == '\n') {
            _ = consumeChar(&state);
            try appendToken(&state, loc, .newline);
        } else if (ch == ',') {
            try appendToken(&state, loc, .comma);
        } else if (ch == ':') {
            try appendToken(&state, loc, .colon);
        } else if (ch == '!' and peekChar(&state) == '=') {
            _ = consumeChar(&state);
            try appendToken(&state, loc, .bang_eq);
        } else if (ch == '<') {
            if (peekChar(&state) == '<') {
                _ = consumeChar(&state);
                try appendToken(&state, loc, .lt_lt);
            } else if (peekChar(&state) == '=') {
                _ = consumeChar(&state);
                try appendToken(&state, loc, .lt_eq);
            } else {
                try appendToken(&state, loc, .lt);
            }
        } else if (ch == '=') {
            if (peekChar(&state) == '=') {
                _ = consumeChar(&state);
                try appendToken(&state, loc, .eq_eq);
            } else {
                try appendToken(&state, loc, .eq);
            }
        } else if (ch == '>') {
            if (peekChar(&state) == '>') {
                _ = consumeChar(&state);
                try appendToken(&state, loc, .gt_gt);
            } else if (peekChar(&state) == '=') {
                _ = consumeChar(&state);
                try appendToken(&state, loc, .gt_eq);
            } else {
                try appendToken(&state, loc, .gt);
            }
        } else if (ch == '+') {
            try appendToken(&state, loc, .plus);
        } else if (ch == '-') {
            if (is_num: {
                const ch2 = peekChar(&state) orelse break :is_num false;
                break :is_num '0' <= ch2 and ch2 <= '9';
            }) {
                try lexInteger(&state, loc);
            } else {
                try appendToken(&state, loc, .minus);
            }
        } else if (ch == '*') {
            try appendToken(&state, loc, .star);
        } else if (ch == '/') {
            try appendToken(&state, loc, .slash);
        } else if (ch == '%') {
            try appendToken(&state, loc, .percent);
        } else if (ch == '&' and peekChar(&state) == '&') {
            _ = consumeChar(&state);
            try appendToken(&state, loc, .amp_amp);
        } else if (ch == '|' and peekChar(&state) == '|') {
            _ = consumeChar(&state);
            try appendToken(&state, loc, .pipe_pipe);
        } else if (ch == '_') {
            try appendToken(&state, loc, .underscore);
        } else if (ch == '(') {
            try appendToken(&state, loc, .paren_l);
        } else if (ch == ')') {
            try appendToken(&state, loc, .paren_r);
        } else if (ch == '[') {
            try appendToken(&state, loc, .bracket_l);
        } else if (ch == ']') {
            try appendToken(&state, loc, .bracket_r);
        } else if (ch == '{') {
            try appendToken(&state, loc, .brace_l);
        } else if (ch == '}') {
            try appendToken(&state, loc, .brace_r);
        } else if (ch == '@') {
            try appendToken(&state, loc, .swat);
        } else if (ch >= '0' and ch <= '9') {
            try lexInteger(&state, loc);
        } else if (ch == '\'') {
            try lexCharLiteral(&state, loc);
        } else if (ch == '"') {
            try lexStringLiteral(&state, loc);
        } else if (ch == '`') {
            try lexHexStringLiteral(&state, loc);
        } else if (isIdentStart(ch)) {
            try lexIdent(&state, loc);
        } else if (ch == ';') {
            try skipComment(&state);
        } else {
            return reportError(&state, loc, "unexpected character '{c}'", .{ch});
        }
    }

    _ = try state.result.tokens.append(gpa, .{
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
    _ = try state.result.tokens.append(state.gpa, .{
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

fn lexCharLiteral(state: *State, start: Loc) !void {
    (err: {
        const ch = consumeChar(state) orelse break :err error.E;
        if (!isValidRawCharInString(ch) or ch == '\'' or ch == '\\') break :err error.E;
        if (consumeChar(state) != '\'') break :err error.E;
    }) catch return reportError(state, start, "bad char literal", .{});
    try appendToken(state, start, .char);
}

fn lexStringLiteral(state: *State, start: Loc) !void {
    while (true) {
        const loc = state.loc;
        const ch = consumeChar(state) orelse
            return reportError(state, start, "string not terminated", .{});
        if (!isValidRawCharInString(ch))
            return reportError(state, loc, "invalid character in string", .{});
        if (ch == '"')
            break;
    }
    try appendToken(state, start, .string);
}

fn lexHexStringLiteral(state: *State, start: Loc) !void {
    while (true) {
        const ch = consumeChar(state) orelse
            return reportError(state, start, "string not terminated", .{});
        if (!isValidRawCharInString(ch))
            return reportError(state, start, "invalid character in string", .{});
        if (ch == '`')
            break;
    }
    try appendToken(state, start, .hex_string);
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

pub fn isIdentStart(ch: u8) bool {
    return 'A' <= ch and ch <= 'Z' or 'a' <= ch and ch <= 'z';
}

pub fn isIdentContinue(ch: u8) bool {
    return isIdentStart(ch) or '0' <= ch and ch <= '9' or ch == '-' or ch == '_' or ch == '.';
}

fn isValidRawCharInString(ch: u8) bool {
    return ch >= 32 and ch < 127;
}

fn skipComment(state: *State) !void {
    while (peekChar(state)) |ch| {
        if (ch == '\n') break;
        _ = consumeChar(state);
    }
}

fn reportError(
    state: *const State,
    loc: Loc,
    comptime fmt: []const u8,
    args: anytype,
) error{AddedToDiagnostic} {
    state.diag.err(loc, fmt, args);
    return error.AddedToDiagnostic;
}

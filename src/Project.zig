const std = @import("std");

const lexer = @import("lexer.zig");
const parser = @import("parser.zig");

const Project = @This();

/// 0 is project.scu; 1+ are the room scus indexed by room number
files: std.ArrayListUnmanaged(?SourceFile),

pub const empty: Project = .{
    .files = .empty,
};

pub fn deinit(self: *Project, gpa: std.mem.Allocator) void {
    var i = self.files.items.len;
    while (i > 0) {
        i -= 1;
        if (self.files.items[i]) |*sf| sf.deinit(gpa);
    }
    self.files.deinit(gpa);
}

pub const SourceFile = struct {
    source: []const u8,
    lex: lexer.Lex,
    ast: parser.Ast,

    fn deinit(self: *SourceFile, gpa: std.mem.Allocator) void {
        self.ast.deinit(gpa);
        self.lex.deinit(gpa);
        gpa.free(self.source);
    }
};

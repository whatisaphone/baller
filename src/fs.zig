const std = @import("std");

pub fn makeDirIfNotExistZ(dir: std.fs.Dir, sub_path: [*:0]const u8) !void {
    dir.makeDirZ(sub_path) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };
}

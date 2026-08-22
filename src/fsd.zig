const std = @import("std");

const Diagnostic = @import("Diagnostic.zig");
const fs = @import("fs.zig");

pub fn openDir(
    diagnostic: *Diagnostic,
    dir: std.fs.Dir,
    sub_path: []const u8,
) !std.fs.Dir {
    return dir.openDir(sub_path, .{}) catch |err| {
        diagnostic.err("failed to open dir: {s} ({s})", .{ sub_path, @errorName(err) });
        return error.AddedToDiagnostic;
    };
}

pub fn openDirZ(
    diagnostic: *Diagnostic,
    dir: std.fs.Dir,
    sub_path: [*:0]const u8,
) !std.fs.Dir {
    return dir.openDirZ(sub_path, .{}) catch |err| {
        diagnostic.err("failed to open dir: {s} ({s})", .{ sub_path, @errorName(err) });
        return error.AddedToDiagnostic;
    };
}

pub fn makeDirIfNotExist(
    diagnostic: *Diagnostic,
    dir: std.fs.Dir,
    sub_path: []const u8,
) !void {
    fs.makeDirIfNotExist(dir, sub_path) catch |err| {
        diagnostic.err("failed to create dir: {s} ({s})", .{ sub_path, @errorName(err) });
        return error.AddedToDiagnostic;
    };
}

pub fn makeDirIfNotExistZ(
    diagnostic: *Diagnostic,
    dir: std.fs.Dir,
    sub_path: [*:0]const u8,
) !void {
    fs.makeDirIfNotExistZ(dir, sub_path) catch |err| {
        diagnostic.err("failed to create dir: {s} ({s})", .{ sub_path, @errorName(err) });
        return error.AddedToDiagnostic;
    };
}

pub fn openFile(
    diagnostic: *Diagnostic,
    location: ?Diagnostic.Location,
    dir: std.fs.Dir,
    sub_path: []const u8,
) !std.fs.File {
    return dir.openFile(sub_path, .{}) catch |err| {
        diagnostic.errAt(location, "failed to open file: {s} ({s})", .{
            sub_path,
            @errorName(err),
        });
        return error.AddedToDiagnostic;
    };
}

pub fn openFileZ(
    diagnostic: *Diagnostic,
    dir: std.fs.Dir,
    sub_path: [*:0]const u8,
) !std.fs.File {
    return dir.openFileZ(sub_path, .{}) catch |err| {
        diagnostic.err("failed to open file: {s} ({s})", .{ sub_path, @errorName(err) });
        return error.AddedToDiagnostic;
    };
}

pub fn createFileZ(
    diagnostic: *Diagnostic,
    dir: std.fs.Dir,
    sub_path: [*:0]const u8,
) !std.fs.File {
    return dir.createFileZ(sub_path, .{}) catch |err| {
        diagnostic.err("failed to create file: {s} ({s})", .{ sub_path, @errorName(err) });
        return error.AddedToDiagnostic;
    };
}

pub fn readFile(
    gpa: std.mem.Allocator,
    diagnostic: *Diagnostic,
    location: ?Diagnostic.Location,
    dir: std.fs.Dir,
    sub_path: []const u8,
) ![]u8 {
    return fs.readFile(gpa, dir, sub_path) catch |err| {
        diagnostic.errAt(
            location,
            "failed to read file: {s} ({s})",
            .{ sub_path, @errorName(err) },
        );
        return error.AddedToDiagnostic;
    };
}

pub fn readFileZ(
    gpa: std.mem.Allocator,
    diagnostic: *Diagnostic,
    dir: std.fs.Dir,
    sub_path: [*:0]const u8,
) ![]u8 {
    return fs.readFileZ(gpa, dir, sub_path) catch |err| {
        diagnostic.err("failed to read file: {s} ({s})", .{ sub_path, @errorName(err) });
        return error.AddedToDiagnostic;
    };
}

pub fn writeFileZ(
    diagnostic: *Diagnostic,
    dir: std.fs.Dir,
    sub_path: [*:0]const u8,
    bytes: []const u8,
) !void {
    fs.writeFileZ(dir, sub_path, bytes) catch |err| {
        diagnostic.err("failed to write file: {s} ({s})", .{ sub_path, @errorName(err) });
        return error.AddedToDiagnostic;
    };
}

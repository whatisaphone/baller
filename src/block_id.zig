const std = @import("std");

pub const BlockId = enum(u32) {
    pub const Raw = u32;

    pub fn init(n: Raw) ?BlockId {
        if (!isValid(n)) return null;
        return @enumFromInt(n);
    }

    pub fn parse(s: []const u8) ?BlockId {
        if (s.len != 4) return null;
        return .init(std.mem.bytesToValue(Raw, s));
    }

    fn isValid(n: Raw) bool {
        const b = std.mem.asBytes(&n);
        if (!('A' <= b[0] and b[0] <= 'Z')) return false;
        for (b[1..]) |c|
            if (!('A' <= c and c <= 'Z' or '0' <= c and c <= '9')) return false;
        return true;
    }

    pub fn raw(self: BlockId) Raw {
        return @intFromEnum(self);
    }

    pub fn str(self: *const BlockId) []const u8 {
        return std.mem.asBytes(self);
    }

    /// use with std.fmt
    pub fn format(self: BlockId, w: *std.io.Writer) !void {
        std.debug.assert(isValid(self.raw()));
        try w.writeAll(self.str());
    }

    pub fn fmtInvalid(value: Raw) FmtInvalid {
        return .{ .value = value };
    }

    const FmtInvalid = struct {
        value: Raw,

        pub fn format(self: FmtInvalid, w: *std.io.Writer) !void {
            try w.print("\"{f}\"", .{std.ascii.hexEscape(std.mem.asBytes(&self.value), .lower)});
        }
    };

    pub fn fmtSafe(value: Raw) FmtSafe {
        return .{ .value = value };
    }

    const FmtSafe = struct {
        value: Raw,

        pub fn format(self: FmtSafe, w: *std.io.Writer) !void {
            if (init(self.value)) |block_id|
                try block_id.format(w)
            else
                try fmtInvalid(self.value).format(w);
        }
    };

    AARY = make("AARY"),
    AKCD = make("AKCD"),
    AKCI = make("AKCI"),
    AKHD = make("AKHD"),
    AKOF = make("AKOF"),
    AKOS = make("AKOS"),
    AKPL = make("AKPL"),
    APAL = make("APAL"),
    AWIZ = make("AWIZ"),
    BMAP = make("BMAP"),
    CDHD = make("CDHD"),
    CHAR = make("CHAR"),
    CNVS = make("CNVS"),
    DBGL = make("DBGL"),
    DEFA = make("DEFA"),
    DIGI = make("DIGI"),
    DIRC = make("DIRC"),
    DIRF = make("DIRF"),
    DIRI = make("DIRI"),
    DIRM = make("DIRM"),
    DIRN = make("DIRN"),
    DIRR = make("DIRR"),
    DIRS = make("DIRS"),
    DIRT = make("DIRT"),
    DISK = make("DISK"),
    DLFL = make("DLFL"),
    DOBJ = make("DOBJ"),
    ENCD = make("ENCD"),
    EXCD = make("EXCD"),
    HBGL = make("HBGL"),
    HSHD = make("HSHD"),
    IM00 = make("IM00"),
    IMHD = make("IMHD"),
    INIB = make("INIB"),
    LECF = make("LECF"),
    LFLF = make("LFLF"),
    LSC2 = make("LSC2"),
    LSCR = make("LSCR"),
    MAXS = make("MAXS"),
    MULT = make("MULT"),
    NLSC = make("NLSC"),
    NOTE = make("NOTE"),
    OBCD = make("OBCD"),
    OBIM = make("OBIM"),
    OBNA = make("OBNA"),
    OFFS = make("OFFS"),
    PALS = make("PALS"),
    POLD = make("POLD"),
    RELO = make("RELO"),
    RGBS = make("RGBS"),
    RMDA = make("RMDA"),
    RMIH = make("RMIH"),
    RMIM = make("RMIM"),
    RNAM = make("RNAM"),
    SAVE = make("SAVE"),
    SCRP = make("SCRP"),
    SDAT = make("SDAT"),
    SGEN = make("SGEN"),
    SGHD = make("SGHD"),
    SMAP = make("SMAP"),
    SONG = make("SONG"),
    SOUN = make("SOUN"),
    SPOT = make("SPOT"),
    SVER = make("SVER"),
    TALK = make("TALK"),
    TEXT = make("TEXT"),
    TLKB = make("TLKB"),
    TLKE = make("TLKE"),
    TRNS = make("TRNS"),
    VARS = make("VARS"),
    VERB = make("VERB"),
    WIZD = make("WIZD"),
    WIZH = make("WIZH"),
    WRAP = make("WRAP"),
    WSOU = make("WSOU"),
    ZP01 = make("ZP01"),
    _,

    fn make(s: []const u8) Raw {
        return std.mem.bytesToValue(Raw, s);
    }
};

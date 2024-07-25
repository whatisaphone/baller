const blockId = @import("block_id.zig").blockId;

const WAVE_FORMAT_PCM = 1;

const Header = struct {
    channels: u16,
    samples_per_sec: u32,
    bits_per_sample: u16,
};

pub fn writeHeader(data_len: u32, out: anytype) !void {
    try out.writeAll("RIFF");
    try out.writeInt(u32, 44 + data_len, .little);

    try out.writeAll("WAVE");

    try out.writeAll("fmt ");
    try out.writeInt(u32, 16, .little);

    try out.writeInt(u16, WAVE_FORMAT_PCM, .little);
    try out.writeInt(u16, 1, .little);
    try out.writeInt(u32, 11025, .little);
    try out.writeInt(u32, 11025, .little);
    try out.writeInt(u16, 1, .little);
    // PCM
    try out.writeInt(u16, 8, .little);

    try out.writeAll("data");
    try out.writeInt(u32, data_len, .little);
}

pub fn readHeader(in: anytype) !Header {
    if (!try in.isBytes("RIFF")) return error.BadData;
    _ = try in.readInt(u32, .little);

    if (!try in.isBytes("WAVE")) return error.BadData;

    if (!try in.isBytes("fmt ")) return error.BadData;
    if (try in.readInt(u32, .little) != 16) return error.BadData;

    if (try in.readInt(u16, .little) != WAVE_FORMAT_PCM) return error.WavFormat;
    const channels = try in.readInt(u16, .little);
    const samples_per_sec = try in.readInt(u32, .little);
    _ = try in.readInt(u32, .little);
    _ = try in.readInt(u16, .little);
    // PCM
    const bits_per_sample = try in.readInt(u16, .little);

    return .{
        .channels = channels,
        .samples_per_sec = samples_per_sec,
        .bits_per_sample = bits_per_sample,
    };
}

pub fn findData(in: anytype) !u32 {
    while (true) {
        const chunk_id = try in.readInt(u32, .little);
        const chunk_len = try in.readInt(u32, .little);
        // We're looking for the data chunk
        if (chunk_id == comptime blockId("data"))
            return chunk_len;
        // If that wasn't it, skip over it and try the next one
        try in.skipBytes(chunk_len, .{});
    }
}

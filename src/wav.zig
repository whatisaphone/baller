pub fn writeHeader(data_len: u32, out: anytype) !void {
    try out.writeAll("RIFF");
    try out.writeInt(u32, 44 + data_len, .little);

    try out.writeAll("WAVE");

    try out.writeAll("fmt ");
    try out.writeInt(u32, 16, .little);

    // WAVE_FORMAT_PCM
    try out.writeInt(u16, 1, .little);
    try out.writeInt(u16, 1, .little);
    try out.writeInt(u32, 11025, .little);
    try out.writeInt(u32, 11025, .little);
    try out.writeInt(u16, 1, .little);
    // PCM
    try out.writeInt(u16, 8, .little);

    try out.writeAll("data");
    try out.writeInt(u32, data_len, .little);
}

pub fn readHeader(in: anytype) !void {
    if (!try in.isBytes("RIFF")) return error.BadData;
    _ = try in.readInt(u32, .little);

    if (!try in.isBytes("WAVE")) return error.BadData;

    if (!try in.isBytes("fmt ")) return error.BadData;
    if (try in.readInt(u32, .little) != 16) return error.BadData;

    // WAVE_FORMAT_PCM
    if (try in.readInt(u16, .little) != 1) return error.WavFormat;
    if (try in.readInt(u16, .little) != 1) return error.WavFormat;
    if (try in.readInt(u32, .little) != 11025) return error.WavFormat;
    if (try in.readInt(u32, .little) != 11025) return error.WavFormat;
    if (try in.readInt(u16, .little) != 1) return error.WavFormat;
    // PCM
    if (try in.readInt(u16, .little) != 8) return error.WavFormat;

    if (!try in.isBytes("data")) return error.BadData;
    _ = try in.readInt(u32, .little);
}

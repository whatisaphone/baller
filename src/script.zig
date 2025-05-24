const lang = @import("lang.zig");

pub const Symbol = union(enum) {
    variable: lang.Variable,
    constant: u16,
};

pub const max_params = 8;

pub const Param = union(enum) {
    int,
    string,
    list,
    variadic,
};

pub const Compound = enum {
    @"sprite-select",
    @"array-sort-row",
    @"lock-and-load-script",
    @"palette-set-slot-color",
    @"break-until",
};

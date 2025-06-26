const lang = @import("lang.zig");

pub const Symbol = union(enum) {
    variable: lang.Variable,
    constant: i32,
};

pub const max_params = 13;

pub const Param = union(enum) {
    int,
    string,
    list,
    variadic,

    room,
    script,
    sound,
    costume,
    charset,
    image,
    talkie,
};

pub const Compound = enum {
    @"sprite-select",
    @"array-sort-rows",
    @"array-sort-cols",
    @"lock-and-load-script",
    @"lock-and-load-costume",
    @"lock-and-load-image",
    @"palette-set-slot-rgb",
    @"palette-set-slot-color",
    @"delete-one-polygon",
    @"break-until",
};

pub const max_params = 8;

pub const Param = union(enum) {
    int,
    string,
    list,
};

pub const Compound = enum {
    @"sprite-select",
    @"lock-and-load-script",
    @"palette-set-slot-color",
    @"break-until",
};

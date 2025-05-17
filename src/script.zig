pub const max_params = 8;

pub const Param = union(enum) {
    int,
    string,
    list,
};

pub const Compound = enum {
    @"sprite-select",
    @"palette-set-slot-color",
};

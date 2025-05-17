pub const max_params = 8;

pub const Param = union(enum) {
    int,
    string,
    list,
};

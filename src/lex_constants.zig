pub const reserved = struct {
    pub const _function = "function";
    pub const _return = "return";
    pub const _if = "if";
    pub const _else = "else";
    pub const _while = "while";
    pub const _for = "for";
    pub const _break = "break";
    pub const _continue = "continue";
    pub const _struct = "struct";
};

pub const math = struct {
    pub const plus = "+";
    pub const minus = "-";
    pub const mul = "*";
    pub const div = "/";
    pub const mod = "%";
};

pub const cmp = struct {
    pub const lt = "<";
    pub const le = "<=";
    pub const lq = "==";
    pub const ne = "!=";
    pub const ge = ">=";
    pub const gt = ">";
};

pub const logical = struct {
    pub const and_op = "&&";
    pub const or_op = "||";
    pub const not_op = "!";
};

pub const assign = struct {
    pub const assign = "=";
};

pub const delimeters = struct {
    pub const lparen = "(";
    pub const rparen = ")";
    pub const lbrace = "{";
    pub const rbrace = "}";
    pub const lbrack = "[";
    pub const rbrack = "]";
};

pub const separators = struct {
    pub const comma = ",";
    pub const semi = ";";
    pub const dot = ".";
    pub const colon = ":";
};

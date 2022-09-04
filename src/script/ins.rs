use crate::script::misc::AnsiStr;
use std::{fmt, isize};

pub enum Ins<'a> {
    Push(Operand),
    PushString(&'a [u8]),
    GetArrayItem(Variable),
    StackDup,
    Not,
    Equal,
    NotEqual,
    Greater,
    Less,
    LessOrEqual,
    Add,
    Sub,
    LogicalOr,
    PopDiscard,
    DimArray(ItemSize, Variable),
    Set(Variable),
    SetArrayItem(Variable),
    Inc(Variable),
    JumpIf(i16),
    JumpUnless(i16),
    StartScript(u8),
    CursorCharset,
    FreeScript,
    StopScript,
    Jump(i16),
    Random,
    LoadScript,
    LockScript,
    LoadCharset,
    AssignString(Variable),
    Sprintf(Variable),
    SomethingWithString([u8; 2], &'a [u8]),
    FreeArray(Variable),
    Now,
    SetWindowTitle,
    Undecoded1([u8; 1]),
    Undecoded2([u8; 2]),
    Generic2Simple([u8; 2]),
}

#[derive(Copy, Clone)]
pub enum Operand {
    Byte(u8),
    I16(i16),
    I32(i32),
    Var(Variable),
}

#[derive(Copy, Clone)]
pub struct Variable(pub u16);

#[derive(Copy, Clone)]
pub enum ItemSize {
    Byte,
    I16,
}

impl Ins<'_> {
    pub fn jump_target(&self, offset: usize) -> Option<usize> {
        match *self {
            Self::JumpIf(rel) | Self::JumpUnless(rel) | Self::Jump(rel) => {
                Some(
                    (isize::try_from(offset).unwrap() + 3 + isize::try_from(rel).unwrap())
                        .try_into()
                        .unwrap(),
                )
            }
            _ => None,
        }
    }
}

impl fmt::Display for Ins<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Push(op) => write!(f, "push {op}"),
            Self::PushString(s) => write!(f, "push-string {:?}", AnsiStr(s)),
            Self::GetArrayItem(var) => write!(f, "get-array-item {var}"),
            Self::StackDup => write!(f, "stack-dup"),
            Self::Not => write!(f, "not"),
            Self::Equal => write!(f, "equal"),
            Self::NotEqual => write!(f, "not-equal"),
            Self::Greater => write!(f, "greater"),
            Self::Less => write!(f, "less"),
            Self::LessOrEqual => write!(f, "less-or-equal"),
            Self::Add => write!(f, "add"),
            Self::Sub => write!(f, "sub"),
            Self::LogicalOr => write!(f, "logical-or"),
            Self::PopDiscard => write!(f, "pop-discard"),
            Self::DimArray(size, var) => write!(f, "dim-array {var}[{size}]"),
            Self::Set(var) => write!(f, "set {var}"),
            Self::SetArrayItem(var) => write!(f, "set-array-item {var}"),
            Self::Inc(var) => write!(f, "inc {var}"),
            Self::JumpIf(rel) => write!(f, "jump-if {}", RelHex(rel)),
            Self::JumpUnless(rel) => write!(f, "jump-unless {}", RelHex(rel)),
            Self::StartScript(n) => write!(f, "start-script {}", n),
            Self::CursorCharset => write!(f, "cursor-charset"),
            Self::FreeScript => write!(f, "free-script"),
            Self::StopScript => write!(f, "stop-script"),
            Self::Jump(rel) => write!(f, "jump {}", RelHex(rel)),
            Self::Random => write!(f, "random"),
            Self::LoadScript => write!(f, "load-script"),
            Self::LockScript => write!(f, "lock-script"),
            Self::LoadCharset => write!(f, "load-charset"),
            Self::AssignString(var) => write!(f, "assign-string {var}"),
            Self::Sprintf(var) => write!(f, "sprintf {var}"),
            Self::SomethingWithString([b1, b2], s) => {
                write!(f, ".db 0x{b1:02x},0x{b2:02x},{:?}", AnsiStr(s))
            }
            Self::FreeArray(var) => write!(f, "free-array {var}"),
            Self::Now => write!(f, "now"),
            Self::SetWindowTitle => write!(f, "set-window-title"),
            Self::Undecoded1([b1]) => write!(f, ".db 0x{b1:02x}"),
            Self::Undecoded2([b1, b2]) | Self::Generic2Simple([b1, b2]) => {
                write!(f, ".db 0x{b1:02x},0x{b2:02x}")
            }
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Byte(n) => n.fmt(f),
            Self::I16(n) => n.fmt(f),
            Self::I32(n) => n.fmt(f),
            Self::Var(var) => var.fmt(f),
        }
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "var{}", self.0)
    }
}

impl fmt::Display for ItemSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Byte => write!(f, "byte"),
            Self::I16 => write!(f, "i16"),
        }
    }
}

struct RelHex(i16);

impl fmt::Display for RelHex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 >= 0 {
            write!(f, "+0x{:04x}", self.0)
        } else {
            write!(f, "-0x{:04x}", -self.0)
        }
    }
}
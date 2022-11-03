pub struct Token {
    pub offset: u32,
    pub payload: TokenPayload,
}

impl Token {
    pub fn new(offset: u32, payload: TokenPayload) -> Self {
        Self { offset, payload }
    }

    pub fn kind(&self) -> TokenKind {
        self.payload.kind()
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum TokenKind {
    BraceL,
    BraceR,
    Eq,
    Integer,
    Ident,
    String,
    Newline,
    Eof,
}

pub enum TokenPayload {
    BraceL,
    BraceR,
    Eq,
    Integer { value: i32 },
    Ident { len: u32 },
    String { len: u32 },
    Newline,
    Eof,
}

impl TokenKind {
    pub fn describe(self) -> &'static str {
        match self {
            TokenKind::BraceL => "'{'",
            TokenKind::BraceR => "'}'",
            TokenKind::Eq => "'='",
            TokenKind::Integer => "integer",
            TokenKind::Ident => "identifier",
            TokenKind::String => "string",
            TokenKind::Newline => "newline",
            TokenKind::Eof => "eof",
        }
    }
}

impl TokenPayload {
    pub fn kind(&self) -> TokenKind {
        match self {
            TokenPayload::BraceL => TokenKind::BraceL,
            TokenPayload::BraceR => TokenKind::BraceR,
            TokenPayload::Eq => TokenKind::Eq,
            TokenPayload::Integer { .. } => TokenKind::Integer,
            TokenPayload::Ident { .. } => TokenKind::Ident,
            TokenPayload::String { .. } => TokenKind::String,
            TokenPayload::Newline => TokenKind::Newline,
            TokenPayload::Eof => TokenKind::Eof,
        }
    }
}

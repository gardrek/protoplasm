// token.rs

use crate::ast::Object;
use crate::scanner::SourceLocation;

use std::fmt;

use serde::Deserialize;

#[derive(Deserialize, Debug, Clone, PartialEq, Eq, Hash)]
#[serde(from = "String")]
pub struct Sym(pub String);

impl Sym {
    pub fn new(s: &str) -> Sym {
        Sym(s.to_string())
    }
}

impl From<String> for Sym {
    fn from(other: String) -> Self {
        Sym(other)
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub location: SourceLocation,
    pub kind: TokenKind,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    // Single-character symbols.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,

    // Operators, of course
    Op(Operator),

    // Literals
    Obj(Object),

    Identifier(Sym),

    UnknownTickIdentifier(Sym),
    InvalidNumber,
    InvalidString,

    // End of Line
    Eol,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Comma,
    Dot,
    Semicolon,

    Minus,
    Plus,
    Slash,
    Star,
    Percent,

    Pipe,
    PipePipe,
    And,
    AndAnd,
    Caret,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,

    Greater,
    GreaterGreater,
    GreaterEqual,
    Less,
    LessLess,
    LessEqual,

    MinusEqual,
    PlusEqual,
    SlashEqual,
    StarEqual,
    PercentEqual,

    PipeEqual,
    AndEqual,

    LeftArrow,
    RightArrow,

    Hash,
    Colon,
}

impl Token {
    pub fn in_kinds(&self, kinds: &[TokenKind]) -> bool {
        for k in kinds {
            if self.kind.same_kind(k) {
                return true;
            }
        }
        false
    }

    pub fn is_object(&self) -> bool {
        matches!(&self.kind, TokenKind::Obj(_))
        //~ match &self.kind {
        //~ TokenKind::Obj(_) => true,
        //~ _ => false,
        //~ }
    }

    pub fn to_operator(&self) -> Operator {
        match &self.kind {
            TokenKind::Op(op) => op.clone(),
            _ => panic!("called to_operator on non-operator token"),
        }
    }
}

impl TokenKind {
    pub fn same_kind(&self, other: &Self) -> bool {
        use TokenKind::*;
        match (self, other) {
            (LeftParen, LeftParen)
            | (RightParen, RightParen)
            | (LeftBrace, LeftBrace)
            | (RightBrace, RightBrace)
            | (LeftBracket, LeftBracket)
            | (RightBracket, RightBracket)
            | (InvalidNumber, InvalidNumber)
            | (InvalidString, InvalidString)
            | (Identifier(_), Identifier(_))
            | (UnknownTickIdentifier(_), UnknownTickIdentifier(_))
            | (Eol, Eol) => true,

            (Obj(obj_a), Obj(obj_b)) => obj_a.same_kind(obj_b),

            (Op(op_a), Op(op_b)) => op_a == op_b,

            /*  The reason we don't replace this with just `_ => false` is so that if a new
            token type is added, you have to come here and properly add it's true condition.
            we also allow unreachable patterns here because technically, whatever token you
            list last will technically be an unreachable pattern, for the same reason. */
            #[allow(unreachable_patterns)]
            (LeftParen, LeftParen)
            | (RightParen, _)
            | (LeftBrace, _)
            | (RightBrace, _)
            | (LeftBracket, _)
            | (RightBracket, _)
            | (InvalidNumber, _)
            | (InvalidString, _)
            | (Identifier(_), _)
            | (UnknownTickIdentifier(_), _)
            | (Obj(_), _)
            | (Op(_), _)
            | (Eol, _)
            | (_, LeftParen)
            | (_, RightParen)
            | (_, LeftBrace)
            | (_, RightBrace)
            | (_, LeftBracket)
            | (_, RightBracket)
            | (_, InvalidNumber)
            | (_, InvalidString)
            | (_, Identifier(_))
            | (_, UnknownTickIdentifier(_))
            | (_, Obj(_))
            | (_, Op(_))
            | (_, Eol) => false,
        }
    }
}

pub fn operator_as_string(s: &Operator) -> &'static str {
    use Operator as Op;
    match s {
        Op::Comma => ",",
        Op::Dot => ".",
        Op::Semicolon => ";",
        Op::Minus => "-",
        Op::Plus => "+",
        Op::Slash => "/",
        Op::Star => "*",
        Op::Percent => "%",
        Op::Pipe => "|",
        Op::PipePipe => "||",
        Op::And => "&",
        Op::AndAnd => "&&",
        Op::Caret => "^",
        Op::Bang => "!",
        Op::BangEqual => "!=",
        Op::Equal => "=",
        Op::EqualEqual => "==",
        Op::Greater => ">",
        Op::GreaterGreater => ">>",
        Op::GreaterEqual => ">=",
        Op::Less => "<",
        Op::LessLess => "<<",
        Op::LessEqual => "<=",
        Op::MinusEqual => "-=",
        Op::PlusEqual => "+=",
        Op::SlashEqual => "/=",
        Op::StarEqual => "*=",
        Op::PercentEqual => "%=",
        Op::PipeEqual => "|=",
        Op::AndEqual => "&=",
        Op::LeftArrow => "<-",
        Op::RightArrow => "->",
        Op::Hash => "#",
        Op::Colon => ":",
    }
}

impl Operator {
    pub fn binary_from_combined(&self) -> Option<Operator> {
        use Operator as Op;
        Some(match self {
            Op::MinusEqual => Op::Minus,
            Op::PlusEqual => Op::Plus,
            Op::SlashEqual => Op::Slash,
            Op::StarEqual => Op::Star,
            Op::PercentEqual => Op::Percent,
            Op::PipeEqual => Op::Pipe,
            Op::AndEqual => Op::And,
            _ => return None,
        })
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TokenKind as Tk;

        match self {
            Tk::LeftParen => write!(f, "'('"),
            Tk::RightParen => write!(f, "')'"),
            Tk::LeftBrace => write!(f, "'{{'"),
            Tk::RightBrace => write!(f, "'}}'"),
            Tk::LeftBracket => write!(f, "'['"),
            Tk::RightBracket => write!(f, "']'"),
            Tk::Op(op) => write!(f, "{}", op),
            Tk::Obj(obj) => write!(f, "(Object {:?})", obj),
            Tk::Identifier(sym) => write!(f, "{}", sym),
            Tk::UnknownTickIdentifier(sym) => write!(f, "Tick#{}", sym),
            Tk::InvalidNumber => write!(f, "InvalidNumber"),
            Tk::InvalidString => write!(f, "InvalidString"),

            Tk::Eol => write!(f, "Eol"),
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", operator_as_string(self))
    }
}

impl fmt::Display for Sym {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ID#{}", self.0)
    }
}

// ast.rs

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::scanner::SourceLocation;
use crate::token::{Operator, Sym, Token, TokenKind};

use serde::Deserialize;

#[derive(Clone)]
pub enum Object {
    Boolean(bool),
    String(String),
    Integer(Integer),
    Float(f32),
    Array(Rc<RefCell<Vec<Object>>>),
}

#[derive(Clone)]
pub struct Integer(pub IntegerType, pub u32);

#[derive(Deserialize, Clone, PartialEq, Eq)]
#[serde(from = "IntegerTypeSerialize")]
pub struct IntegerType {
    pub signed: bool,
    pub bit_width: Option<u8>,
}

#[derive(Deserialize)]
struct IntegerTypeSerialize(
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        with = "::serde_with::rust::unwrap_or_skip"
    )]
    Option<Signed>,
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        with = "::serde_with::rust::unwrap_or_skip"
    )]
    Option<u8>,
);

#[derive(Deserialize, Clone)]
struct Signed;

impl From<IntegerTypeSerialize> for IntegerType {
    fn from(other: IntegerTypeSerialize) -> Self {
        IntegerType {
            signed: other.0.is_some(),
            bit_width: other.1,
        }
    }
}

impl Integer {
    pub fn new(signed: bool, bit_width: Option<u8>, value: u32) -> Integer {
        Integer(IntegerType { signed, bit_width }, value)
    }

    pub fn min_width(&self) -> u8 {
        32 - self.1.leading_zeros() as u8
    }

    pub fn get_width_or_min_width(&self) -> u8 {
        match self.0.bit_width {
            Some(v) => v,
            None => 32 - self.1.leading_zeros() as u8,
        }
    }

    pub fn coerce(self, target: &IntegerType) -> Option<Integer> {
        if self.0 == *target {
            Some(self)
        } else {
            if target.signed != self.0.signed {
                return None;
            }

            match self.0.bit_width {
                Some(_w) => match target.bit_width {
                    Some(_tw) => todo!(),
                    //
                    None => todo!(),
                },
                None => match target.bit_width {
                    Some(tw) => {
                        if tw < self.min_width() {
                            None
                        } else {
                            Some(Integer(
                                IntegerType {
                                    signed: self.0.signed,
                                    bit_width: target.bit_width,
                                },
                                self.1,
                            ))
                        }
                    }
                    None => todo!(),
                },
            }
        }
    }
}

impl Object {
    pub fn from_token(token: &Token) -> Option<Object> {
        Some(match &token.kind {
            TokenKind::Obj(obj) => obj.clone(),
            _ => return None,
        })
    }

    pub fn integer(signed: bool, bit_width: Option<u8>, value: u32) -> Object {
        Object::Integer(Integer::new(signed, bit_width, value))
    }

    pub fn to_asm_type(&self) -> AsmType {
        match self {
            Object::Boolean(_) => AsmType::Boolean,
            Object::String(_) => AsmType::String,
            Object::Integer(int) => AsmType::Integer(int.0.clone()),
            Object::Float(_) => AsmType::Float,
            Object::Array(_) => AsmType::Array,
        }
    }

    pub fn coerce(self, target: &AsmType) -> Option<Object> {
        if self.to_asm_type() == *target {
            Some(self)
        } else {
            match self {
                Object::Integer(int) => match target {
                    AsmType::Integer(t) => int.coerce(t).map(Object::Integer),
                    _ => None,
                },
                _ => None,
            }
        }
    }

    pub fn same_kind(&self, other: &Self) -> bool {
        self.to_asm_type() == other.to_asm_type()
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Boolean(b) => *b,
            _ => false,
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        use Object::*;
        match (self, other) {
            (Boolean(a), Boolean(b)) => a == b,
            (Integer { .. }, Integer { .. }) => todo!(),
            (Float(a), Float(b)) => a == b,
            (String(a), String(b)) => a == b,
            (Array(rc_a), Array(rc_b)) => Rc::ptr_eq(rc_a, rc_b),

            _ => false,
        }
    }
}

impl fmt::Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        //~ write!(f, "obj#")?;
        use Object::*;
        match self {
            Boolean(b) => write!(f, "{}", b),
            String(s) => write!(f, "{:?}", s),
            Integer(i) => write!(f, "(integer {:?})", i),
            Float(value) => write!(f, "(Float {})", value),
            Array(vec) => {
                write!(f, "[")?;
                for v in vec.borrow().iter() {
                    write!(f, "{:?}", v)?;
                }
                write!(f, "]")
            }
        }
    }
}

impl fmt::Debug for IntegerType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let sign = if self.signed { "i" } else { "u" };
        write!(f, "{}", sign)?;
        match self.bit_width {
            Some(width) => {
                write!(f, "{}", width)
            }
            None => write!(f, "dec"),
        }
    }
}

impl fmt::Debug for Integer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}:{:?}", self.0, self.1)
    }
}

#[derive(Deserialize, Clone, PartialEq, Eq, Debug)]
#[serde(from = "AsmTypeSerialize")]
pub enum AsmType {
    Boolean,
    String,
    Integer(IntegerType), // {width: Option<u32>,}
    Float,
    //~ TypeDef(Sym), // {identifier: Sym,}
    Array,
}

#[derive(Deserialize, Clone, Debug)]
pub enum AsmTypeSerialize {
    Boolean,
    String,
    UintNone,
    Uint(u8),
    SintNone,
    Sint(u8),
    Float,
    Array,
}

impl From<AsmTypeSerialize> for AsmType {
    fn from(other: AsmTypeSerialize) -> Self {
        match other {
            AsmTypeSerialize::Boolean => AsmType::Boolean,
            AsmTypeSerialize::String => AsmType::String,
            AsmTypeSerialize::UintNone => AsmType::Integer(IntegerType {
                signed: false,
                bit_width: None,
            }),
            AsmTypeSerialize::Uint(u) => AsmType::Integer(IntegerType {
                signed: false,
                bit_width: Some(u),
            }),
            AsmTypeSerialize::SintNone => AsmType::Integer(IntegerType {
                signed: true,
                bit_width: None,
            }),
            AsmTypeSerialize::Sint(u) => AsmType::Integer(IntegerType {
                signed: true,
                bit_width: Some(u),
            }),
            AsmTypeSerialize::Float => AsmType::Float,
            AsmTypeSerialize::Array => AsmType::Array,
        }
    }
}

#[derive(Clone)]
pub enum Stmt {
    Label(Sym),
    Instruction(Sym, Vec<Expr>),
    Directive(Sym, Vec<Token>),
}

impl fmt::Debug for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Stmt::*;
        match self {
            Label(sym) => write!(f, "(:Label: {})", sym),
            Instruction(sym, exprs) => {
                write!(f, "({}", sym)?;
                for expr in exprs {
                    write!(f, " {}", expr)?;
                }
                write!(f, ")")
            }
            Directive(sym, tokens) => {
                write!(f, "(:Directive: {} [", sym)?;
                for (i, tk) in tokens.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", tk)?;
                }
                write!(f, "])")
            }
        }
    }
}

#[derive(Deserialize, Clone)]
#[serde(try_from = "String")]
pub struct Expr {
    pub location: SourceLocation,
    pub kind: ExprKind,
}

impl TryFrom<String> for Expr {
    type Error = crate::parser::ParseError;

    fn try_from(other: String) -> Result<Self, Self::Error> {
        let scanner = crate::scanner::Scanner::new(&other);
        let mut parser = crate::parser::Parser::new(scanner);
        parser.parse_as_expression()
    }
}

#[derive(Clone)]
pub enum ExprKind {
    Literal(Object),
    Unary(Operator, Box<Expr>),
    Binary(Box<Expr>, Operator, Box<Expr>),
    VariableAccess(Sym, Option<usize>),
    Assign(Sym, Box<Expr>, Option<usize>),
    LogicalOr(Box<Expr>, Box<Expr>),
    LogicalAnd(Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    PropertyAccess(Box<Expr>, Sym),
    PropertyAssign(Box<Expr>, Sym, Box<Expr>),
    ArrayConstructor(Vec<Expr>),
    ArrayConstructorMulti(Box<Expr>, Box<Expr>),
    Index(Box<Expr>, Box<Expr>),
    IndexAssign {
        obj: Box<Expr>,
        index: Box<Expr>,
        val: Box<Expr>,
    },
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Expr {} at {}", self.kind, self.location,)
    }
}

impl fmt::Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ExprKind::*;
        match self {
            Literal(obj) => write!(f, "{:?}", obj),
            Unary(op, expr) => write!(f, "({} {})", op, expr),
            Binary(expr_a, op, expr_b) => write!(f, "({} {} {})", op, expr_a, expr_b),
            VariableAccess(sym, _depth) => write!(f, "(var {})", sym),
            Assign(sym, expr, _depth) => write!(f, "(assign {} {})", sym, expr),
            LogicalOr(a, b) => write!(f, "(or {} {})", a, b),
            LogicalAnd(a, b) => write!(f, "(and {} {})", a, b),
            Call(callee, args) => {
                write!(f, "(call {}", callee)?;
                for e in args {
                    write!(f, " {}", e)?;
                }
                write!(f, ")")
            }
            PropertyAccess(obj, name) => write!(f, "(property-access {} {:?})", obj, name),
            PropertyAssign(obj, name, value) => {
                write!(f, "(property-assign {} {} {})", obj, name, value)
            }
            ArrayConstructor(exprs) => {
                write!(f, "(array")?;
                for e in exprs {
                    write!(f, " {}", e)?;
                }
                write!(f, ")")
            }
            ArrayConstructorMulti(obj, multi) => write!(f, "(array-multi {} {})", obj, multi),
            Index(obj, index) => write!(f, "(index {} {})", obj, index),
            IndexAssign { obj, index, val } => write!(f, "(index {} {} {})", obj, index, val),
        }
    }
}

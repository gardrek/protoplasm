// recipe.rs

//~ use std::collections::HashMap;

use serde::Deserialize;

use crate::ast::AsmType;
use crate::ast::Expr;
use crate::token::Sym;

#[derive(Deserialize, Debug, Clone)]
pub struct Recipe {
    pub word_width: u8,

    pub address_width: u8,

    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        with = "::serde_with::rust::unwrap_or_skip"
    )]
    pub default_instruction_width: Option<u8>,

    /*
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        with = "::serde_with::rust::unwrap_or_skip"
    )]
    type_defs: Option<HashMap<Sym, TypeDef>>,
    */
    pub instructions: Vec<Instruction>,
}

impl Recipe {
    pub fn load(path: &std::path::Path) -> Result<Recipe, ron::error::SpannedError> {
        let s = std::fs::read_to_string(path).unwrap();
        ron::from_str(&s)
    }

    pub fn get_instruction(&self, mnemonic: &Sym) -> Option<&Instruction> {
        self.instructions
            .iter()
            .find(|&instr| &instr.mnemonic == mnemonic)
    }
}

#[derive(Deserialize, Debug, Clone)]
pub struct Instruction {
    pub mnemonic: Sym,

    pub parameters: Vec<Param>,

    pub assembly: Expr,

    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        with = "::serde_with::rust::unwrap_or_skip"
    )]
    pub width: Option<u8>,
}

#[derive(Deserialize, Debug, Clone)]
#[serde(from = "ParamSerialize")]
pub struct Param {
    pub identifier: Sym,

    pub asm_type: AsmType,

    #[serde(default, skip_serializing_if = "Clone::clone")]
    pub default: Option<Expr>,
}

impl From<ParamSerialize> for Param {
    fn from(other: ParamSerialize) -> Self {
        Param {
            identifier: other.0,
            asm_type: other.1,
            default: other.2.map(|v| v.0),
        }
    }
}

#[derive(Deserialize, Debug, Clone)]
pub struct ParamSerialize(
    Sym,
    AsmType,
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        with = "::serde_with::rust::unwrap_or_skip"
    )]
    Option<Optional>,
);

// Optional code string evaluates to the default value, in the scope of the argument evaluation
// i.e. you have access to other arguments that have already been evaluated
// evaluation order is from left to right
#[derive(Deserialize, Debug, Clone)]
pub struct Optional(Expr);

/*

#[derive(Deserialize, Debug, Clone)]
#[serde(from = "TypeDefSerialize")]
pub struct TypeDef {
    identifier: Sym,
    kind: TypeDefKind,
}

impl From<TypeDefSerialize> for TypeDef {
    fn from(other: TypeDefSerialize) -> Self {
        TypeDef {
            identifier: other.0,
            kind: other.1,
        }
    }
}

#[derive(Deserialize, Debug, Clone)]
pub struct TypeDefSerialize(Sym, TypeDefKind);

#[derive(Deserialize, Debug, Clone)]
pub enum TypeDefKind {
    Enum(Vec<(Sym, EnumVariant)>),
    Struct(Vec<(Sym, AsmType)>),
}

#[derive(Deserialize, Debug, Clone)]
pub struct EnumVariant(
    Sym,
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        with = "::serde_with::rust::unwrap_or_skip"
    )]
    Option<AsmType>,
);

impl PartialEq for TypeDef {
    fn eq(&self, other: &Self) -> bool {
        self.identifier == other.identifier
    }
}

impl Eq for TypeDef {}

*/

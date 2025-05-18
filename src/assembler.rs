// assembler.rs

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{Expr, ExprKind, Stmt};
use crate::ast::{Integer, IntegerType, Object};
use crate::recipe::Recipe;
use crate::resolver::Resolver;
use crate::scanner::SourceLocation;
use crate::token::Operator;
use crate::token::Sym;

#[derive(Debug, Clone)]
pub struct RuntimeError {
    message: &'static str,
    kind: RuntimeErrorKind,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub enum RuntimeErrorKind {
    Ice,
    Unimplemented,
    TypeError,
    UndefinedVariable,
    IndexError,
}

pub enum ControlFlow {
    RuntimeError(RuntimeError),
    //~ Return(Object),
    //~ Break(Option<Object>),
}

impl From<RuntimeError> for ControlFlow {
    fn from(other: RuntimeError) -> Self {
        ControlFlow::RuntimeError(other)
    }
}

impl std::error::Error for RuntimeError {}

//~ impl std::error::Error for Exception {}

impl RuntimeError {
    pub fn ice(message: &'static str, location: SourceLocation) -> RuntimeError {
        RuntimeError {
            message,
            location,
            kind: RuntimeErrorKind::Ice,
        }
    }

    pub fn unimplemented(message: &'static str, location: SourceLocation) -> RuntimeError {
        RuntimeError {
            message,
            location,
            kind: RuntimeErrorKind::Unimplemented,
        }
    }

    pub fn type_error(message: &'static str, location: SourceLocation) -> RuntimeError {
        RuntimeError {
            message,
            location,
            kind: RuntimeErrorKind::TypeError,
        }
    }

    pub fn undefined_variable(location: SourceLocation) -> RuntimeError {
        RuntimeError {
            message: "Undefined variable",
            location,
            kind: RuntimeErrorKind::UndefinedVariable,
        }
    }

    pub fn index_error(location: SourceLocation) -> RuntimeError {
        RuntimeError {
            message: "Undefined variable",
            location,
            kind: RuntimeErrorKind::IndexError,
        }
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "Runtime Error {:?} at {}: {}",
            self.kind, self.location, self.message
        )
    }
}

#[derive(Debug, Default)]
pub struct Assembler {
    address: usize,
    pub resolver: Resolver,
    environment: Option<Rc<RefCell<Environment>>>,
    //~ native_classes: HashMap<Sym, NativeClass>,
}

impl Assembler {
    pub fn from_environment(env: Environment) -> Assembler {
        Assembler {
            address: 0,
            environment: Some(Rc::new(RefCell::new(env))),
            //~ native_classes: HashMap::new(),
            resolver: Resolver::default(),
        }
    }

    fn get_at_depth(&self, sym: &Sym, depth: usize) -> Option<Object> {
        self.environment.clone()?.borrow().get_at_depth(sym, depth)
    }

    pub fn set_at_depth(&mut self, sym: &Sym, depth: usize, object: Object) -> Option<Object> {
        self.environment
            .clone()?
            .borrow_mut()
            .set_at_depth(sym, depth, object)
    }

    fn access_native_method(
        &self,
        _class_name: &str,
        _object: Object,
        _name: &Sym,
        _location: SourceLocation,
    ) -> Result<Object, ControlFlow> {
        todo!()
    }

    /*
    fn call(
        &mut self,
        _callee: &Object,
        _arguments: Vec<Object>,
        _location: SourceLocation,
    ) -> Result<Object, ControlFlow> {
        todo!()
    }
    */
}

#[derive(Debug, Default)]
pub struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    bindings: HashMap<Sym, Object>,
}

impl Environment {
    /*
    pub fn new_inner(env: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment {
            enclosing: Some(env),
            bindings: HashMap::default(),
        }))
    }
    */

    fn get_at_depth(&self, sym: &Sym, depth: usize) -> Option<Object> {
        if depth == 0 {
            self.bindings.get(sym).cloned()
        } else if let Some(enc) = &self.enclosing {
            enc.borrow().get_at_depth(sym, depth - 1)
        } else {
            None
        }
    }

    fn set_at_depth(&mut self, sym: &Sym, depth: usize, obj: Object) -> Option<Object> {
        if depth == 0 {
            self.bindings.insert(sym.clone(), obj)
        } else if let Some(enc) = &self.enclosing {
            enc.borrow_mut().set_at_depth(sym, depth - 1, obj)
        } else {
            None
        }
    }

    /*
    pub fn flat_copy(&self) -> Rc<RefCell<Environment>> {
        let bindings = self.bindings_flattened();

        Rc::new(RefCell::new(Environment {
            enclosing: None,
            bindings,
        }))
    }

    pub fn bindings_flattened(&self) -> HashMap<Sym, Object> {
        let bindings = HashMap::new();
        self.flatten_recurse(bindings)
    }

    fn flatten_recurse(&self, bindings: HashMap<Sym, Object>) -> HashMap<Sym, Object> {
        //~ bindings.extend(self.bindings.clone());

        let mut next_bindings = self.bindings.clone();

        next_bindings.extend(bindings);

        match &self.enclosing {
            Some(e) => e.borrow().flatten_recurse(next_bindings),
            None => next_bindings,
        }
    }
    */
}

impl Assembler {
    pub fn assemble_item(
        &mut self,
        recipe: &Recipe,
        item: &Stmt,
    ) -> Result<Option<(usize, AssembledInstruction)>, RuntimeError> {
        use Stmt::*;
        match item {
            Label(sym) => {
                let address = self.address as u32;
                let bit_width = Some(recipe.address_width);
                let obj = Object::integer(false, bit_width, address);
                self.set_at_depth(sym, 0, obj);
                Ok(None)
            }
            Instruction(mnemonic, arguments) => {
                let assembled = self.assemble_instruction(recipe, mnemonic, arguments)?;
                let num_of_words = (assembled.bit_width - 1) / recipe.word_width + 1;
                let address = self.address;
                self.address += num_of_words as usize;
                Ok(Some((address, assembled)))
            }
            Directive(_sym, _tokens) => Err(RuntimeError::unimplemented(
                "directives not yet implemented",
                SourceLocation::bull(),
            )),
        }
    }

    pub fn assemble_instruction(
        &mut self,
        recipe: &Recipe,
        mnemonic: &Sym,
        arguments: &[Expr],
    ) -> Result<AssembledInstruction, RuntimeError> {
        let instruction = recipe
            .get_instruction(mnemonic)
            .ok_or(RuntimeError::unimplemented(
                "unknown mnemonic",
                SourceLocation::bull(),
            ))?;

        self.resolver.begin_scope();

        for (param_index, param) in instruction.parameters.iter().enumerate() {
            let value = match arguments.get(param_index) {
                Some(expr) => {
                    // evaluate argument expression
                    self.eval(expr)?
                }
                None => {
                    let expr = param.default.as_ref().ok_or(RuntimeError::unimplemented(
                        "missing argument",
                        SourceLocation::bull(),
                    ))?;

                    // evaluate default expression
                    self.eval(expr)?
                }
            };

            let value = if value.to_asm_type() != param.asm_type {
                /*
                eprintln!(
                    "coerce param {param_index} from {:?} to {:?}",
                    value.to_asm_type(),
                    param.asm_type
                );
                //~ */
                match value.coerce(&param.asm_type) {
                    Some(v) => v,
                    None => {
                        return Err(RuntimeError::unimplemented(
                            "types not matching (need to implement integer coersion?)",
                            SourceLocation::bull(),
                        ));
                    }
                }
            } else {
                value
            };

            // place value into environment variable so that it can be accessed by later evals
            self.set_at_depth(&param.identifier, 0, value);

            self.resolver.insert_declaration(&param.identifier, true);
        }

        let mut assembly_expr = instruction.assembly.clone();

        self.resolver.resolve_expression(&mut assembly_expr);

        self.resolver.end_scope();

        // evaluate assembly expression
        let bit_width = instruction
            .width
            .or(recipe.default_instruction_width)
            .unwrap();

        let obj = self.eval(&assembly_expr)?;

        let data = match obj {
            Object::Integer(Integer(_, value)) => value,
            _ => {
                return Err(RuntimeError::type_error(
                    "bad recipe: assembly expression must return an integer",
                    SourceLocation::bull(),
                ));
            }
        };

        Ok(AssembledInstruction { bit_width, data })
    }
}

pub struct AssembledInstruction {
    pub bit_width: u8,
    pub data: u32,
}

impl Assembler {
    pub fn eval(&mut self, expr: &Expr) -> Result<Object, RuntimeError> {
        match self.evaluate(expr) {
            Ok(obj) => Ok(obj),
            Err(e) => match e {
                ControlFlow::RuntimeError(r) => Err(r),
                /*
                ControlFlow::Return(_) => Err(RuntimeError::ice(
                    "ICE: leaked return",
                    expr.location.clone(),
                )),
                ControlFlow::Break(_) => Err(RuntimeError::ice(
                    "ICE: leaked break",
                    expr.location.clone(),
                )),
                */
            },
        }
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<Object, ControlFlow> {
        use ExprKind::*;
        Ok(match &expr.kind {
            Literal(literal) => literal.clone(),
            Unary(operator, operand_expr) => {
                let operand = self.evaluate(operand_expr)?;
                match operator {
                    Operator::Minus | Operator::Plus => match operand {
                        Object::Integer(Integer(IntegerType { signed, bit_width }, value)) => {
                            if !signed {
                                return Err(RuntimeError::type_error(
                                    "attempt to perform signed arithmetic on non-signed number",
                                    operand_expr.location.clone(),
                                )
                                .into());
                            }
                            let value = match operator {
                                Operator::Minus => todo!(),
                                Operator::Plus => value,
                                _ => unreachable!(),
                            };
                            Object::integer(signed, bit_width, value)
                        }
                        Object::Float(v) => Object::Float(match operator {
                            Operator::Minus => -v,
                            Operator::Plus => v,
                            _ => unreachable!(),
                        }),
                        _ => {
                            return Err(RuntimeError::type_error(
                                "attempt to perform arithmetic on non-number",
                                expr.location.clone(),
                            )
                            .into());
                        }
                    },
                    Operator::Bang => {
                        //~ let inner = operand.is_truthy();
                        //~ Object::Boolean(!inner)
                        todo!()
                        // probably more useful if this also can do bitwise invert? or maybe save that for a tilde op? hm
                    }
                    Operator::Hash => operand,
                    _ => {
                        return Err(RuntimeError::ice(
                            "op is not a unary operator. bad syntax tree",
                            expr.location.clone(),
                        )
                        .into());
                    }
                }
            }
            Binary(left_operand_expr, operator, right_operand_expr) => {
                let left = self.evaluate(left_operand_expr)?;
                let right = self.evaluate(right_operand_expr)?;
                use Object::*;
                match operator {
                    Operator::Minus
                    | Operator::Plus
                    | Operator::Slash
                    | Operator::Star
                    | Operator::Percent => match left {
                        Integer(l) => match right {
                            Integer(r) => Object::Integer(
                                self.evaluate_integer_arithmetic_binary_op(operator, &l, &r),
                            ),
                            Float(_) => {
                                return Err(RuntimeError::type_error(
                                    "cannot coerce float to integer",
                                    right_operand_expr.location.clone(),
                                )
                                .into());
                            }
                            _ => {
                                return Err(RuntimeError::type_error(
                                    "attempt to perform arithmetic on non-number",
                                    right_operand_expr.location.clone(),
                                )
                                .into());
                            }
                        },
                        Float(_) => match right {
                            Float(_) => {
                                todo!()
                            }
                            Integer(_) => {
                                return Err(RuntimeError::type_error(
                                    "cannot coerce integer to float",
                                    right_operand_expr.location.clone(),
                                )
                                .into());
                            }
                            _ => {
                                return Err(RuntimeError::type_error(
                                    "attempt to perform arithmetic on non-number",
                                    right_operand_expr.location.clone(),
                                )
                                .into());
                            }
                        },
                        _ => {
                            return Err(RuntimeError::type_error(
                                "attempt to perform arithmetic on non-number",
                                expr.location.clone(),
                            )
                            .into());
                        }
                    },
                    Operator::Greater
                    | Operator::Less
                    | Operator::GreaterEqual
                    | Operator::LessEqual => match left {
                        Integer(_) | Float(_) => match right {
                            Integer(_) | Float(_) => {
                                self.evaluate_arithmetic_compare_op(operator, &left, &right)
                            }
                            _ => {
                                return Err(RuntimeError::type_error(
                                    "attempt to arithmetically compare non-number",
                                    right_operand_expr.location.clone(),
                                )
                                .into());
                            }
                        },
                        _ => {
                            return Err(RuntimeError::type_error(
                                "attempt to arithmetically compare non-number",
                                expr.location.clone(),
                            )
                            .into());
                        }
                    },
                    Operator::BangEqual => Object::Boolean(left != right),
                    Operator::EqualEqual => Object::Boolean(left == right),
                    Operator::Pipe | Operator::And | Operator::Caret => match left {
                        Integer(l) => match right {
                            Integer(r) => {
                                Object::Integer(self.evaluate_bitwise_op(operator, &l, &r))
                            }
                            _ => {
                                return Err(RuntimeError::type_error(
                                    "attempt to perform bitwise operation on non-integer",
                                    right_operand_expr.location.clone(),
                                )
                                .into());
                            }
                        },
                        _ => {
                            return Err(RuntimeError::type_error(
                                "attempt to perform bitwise operation on non-integer",
                                expr.location.clone(),
                            )
                            .into());
                        }
                    },
                    Operator::GreaterGreater | Operator::LessLess => match left {
                        Integer(l) => match right {
                            Integer(r) => {
                                Object::Integer(self.evaluate_bitwise_shift(operator, &l, &r))
                            }
                            _ => {
                                return Err(RuntimeError::type_error(
                                    "attempt to perform bitwise operation on non-integer",
                                    right_operand_expr.location.clone(),
                                )
                                .into());
                            }
                        },
                        _ => {
                            return Err(RuntimeError::type_error(
                                "attempt to perform bitwise operation on non-integer",
                                expr.location.clone(),
                            )
                            .into());
                        }
                    },
                    _ => {
                        return Err(RuntimeError::ice(
                            "op is not a binary operator. bad syntax tree",
                            expr.location.clone(),
                        )
                        .into());
                    }
                }
            }
            VariableAccess(sym, depth) => {
                let depth = depth.expect("ICE: Depth not resolved");
                self.get_at_depth(sym, depth)
                    .ok_or(RuntimeError::undefined_variable(expr.location.clone()))?
            }
            Assign(sym, assign_expr, depth) => {
                let value = self.evaluate(assign_expr)?;

                self.set_at_depth(sym, depth.expect("ICE: Depth not resolved"), value.clone())
                    .ok_or(RuntimeError::undefined_variable(expr.location.clone()))?;
                /*
                if self.environment.borrow().is_defined(sym) {
                    self.environment.borrow_mut().assign(sym.clone(), value.clone());

                    value
                } else if self.globals.borrow().is_defined(sym) {
                    self.globals.borrow_mut().assign(sym.clone(), value.clone());

                    value
                } else {
                    return Err(RuntimeError::undefined_variable(expr.location.clone()).into());
                }
                */

                value
            }
            LogicalOr(left, right) => {
                let value = self.evaluate(left)?;

                if value.is_truthy() {
                    return Ok(value);
                }

                self.evaluate(right)?
            }
            LogicalAnd(left, right) => {
                let value = self.evaluate(left)?;

                if !value.is_truthy() {
                    return Ok(value);
                }

                self.evaluate(right)?
            }
            Call(_callee, _args) => {
                todo!()
                /*
                let location = callee.location.clone();

                let callee = self.evaluate(callee)?;

                let mut evaluated_args = vec![];

                for a in args {
                    evaluated_args.push(self.evaluate(a)?);
                }

                let arity = match callee.arity() {
                    Some(a) => a,
                    None => {
                        return Err(
                            RuntimeError::ice("callee not a callable type", location).into()
                        );
                    }
                };

                if arity != evaluated_args.len() {
                    return Err(RuntimeError::type_error(
                        "Incorrect number of arguments",
                        location,
                    )
                    .into());
                }

                self.call(&callee, evaluated_args, location)?
                */
            }
            PropertyAccess(obj_expr, name) => {
                let location = obj_expr.location.clone();

                let obj = self.evaluate(obj_expr)?;

                use Object::*;
                match obj {
                    Integer(_) => {
                        //~ self.access_native_method("Number", obj.clone(), name, location)?
                        todo!()
                    }
                    Float(_) => {
                        //~ self.access_native_method("Number", obj.clone(), name, location)?
                        todo!()
                    }
                    String(_) => {
                        self.access_native_method("String", obj.clone(), name, location)?
                    }
                    Array(_) => self.access_native_method("Array", obj.clone(), name, location)?,
                    _ => {
                        return Err(RuntimeError::type_error(
                            "Cannot access property, not an instance or class",
                            location,
                        )
                        .into());
                    }
                }
            }
            PropertyAssign(obj, _name, _value) => {
                let location = obj.location.clone();

                let _obj = self.evaluate(obj)?;

                //~ match obj
                {
                    return Err(RuntimeError::unimplemented(
                        "property assign not yet implemented",
                        location,
                    )
                    .into());
                }
            }
            ArrayConstructor(exprs) => {
                let mut v = vec![];

                for e in exprs {
                    v.push(self.evaluate(e)?);
                }

                Object::Array(Rc::new(RefCell::new(v)))
            }
            ArrayConstructorMulti(value_expr, multi) => {
                let location = multi.location.clone();

                let copied_value = self.evaluate(value_expr)?;

                let multi_value = match self.evaluate(multi)? {
                    Object::Integer(Integer(IntegerType { signed, .. }, value)) => {
                        if signed {
                            return Err(RuntimeError::type_error(
                                "Array constructer length must be an unsigned integer",
                                location,
                            )
                            .into());
                        } else {
                            value as usize
                        }
                    }
                    _o => {
                        return Err(RuntimeError::type_error(
                            "Array constructer length must be an unsigned integer",
                            location,
                        )
                        .into());
                    }
                };

                let v = vec![copied_value; multi_value];

                Object::Array(Rc::new(RefCell::new(v)))
            }
            Index(obj_expr, index_expr) => {
                let location = obj_expr.location.clone();

                let obj = self.evaluate(obj_expr)?;

                let index = match self.evaluate(index_expr)? {
                    Object::Integer(Integer(IntegerType { signed, .. }, value)) => {
                        if signed {
                            return Err(RuntimeError::type_error(
                                "Index must be an unsigned integer",
                                location,
                            )
                            .into());
                        } else {
                            value as usize
                        }
                    }
                    _o => {
                        return Err(RuntimeError::type_error(
                            "Index must be an unsigned integer",
                            location,
                        )
                        .into());
                    }
                };

                match obj {
                    Object::Array(v) => {
                        if index < v.borrow().len() {
                            v.borrow()[index].clone()
                        } else {
                            return Err(RuntimeError::index_error(location).into());
                        }
                    }
                    Object::String(s) => {
                        let s = s.to_string();
                        let bstr = s.as_bytes();
                        if index < bstr.len() {
                            Object::integer(false, Some(8), bstr[index] as u32)
                        } else {
                            return Err(RuntimeError::index_error(location).into());
                        }
                    }
                    _o => {
                        return Err(RuntimeError::type_error(
                            "Attempt to index non-Array",
                            location,
                        )
                        .into());
                    }
                }
            }
            IndexAssign {
                obj: obj_expr,
                index: index_expr,
                val: val_expr,
            } => {
                let location = obj_expr.location.clone();

                let obj = self.evaluate(obj_expr)?;

                let index = match self.evaluate(index_expr)? {
                    Object::Integer(Integer(IntegerType { signed, .. }, value)) => {
                        if signed {
                            return Err(RuntimeError::type_error(
                                "Index must be an unsigned integer",
                                location,
                            )
                            .into());
                        } else {
                            value as usize
                        }
                    }
                    _o => {
                        return Err(RuntimeError::type_error(
                            "Index must be an unsigned integer",
                            location,
                        )
                        .into());
                    }
                };

                match obj {
                    Object::Array(v) => {
                        let val = self.evaluate(val_expr)?;
                        if index < v.borrow().len() {
                            v.borrow_mut()[index] = val.clone();
                            val
                        } else {
                            return Err(RuntimeError::index_error(location).into());
                        }
                    }
                    _o => {
                        return Err(RuntimeError::type_error(
                            "Attempt to index non-Array",
                            location,
                        )
                        .into());
                    }
                }
            }
        })
    }

    fn evaluate_integer_arithmetic_binary_op(
        &mut self,
        op: &Operator,
        left: &Integer,
        right: &Integer,
    ) -> Integer {
        match op {
            Operator::Minus => {
                if left.0.signed != right.0.signed {
                    todo!()
                }

                let signed = left.0.signed;

                let width = min_width(left, right);

                let value = mask(left.1.wrapping_sub(right.1), width);

                Integer::new(signed, width, value)
            }
            Operator::Plus => {
                if left.0.signed != right.0.signed {
                    todo!()
                }

                let signed = left.0.signed;

                let width = min_width(left, right);

                let value = mask(left.1.wrapping_add(right.1), width);

                Integer::new(signed, width, value)
            }
            Operator::Slash => todo!(),
            Operator::Star => todo!(),
            Operator::Percent => todo!(),
            _ => unreachable!(),
        }
    }

    fn evaluate_arithmetic_compare_op(
        &mut self,
        _op: &Operator,
        _left: &Object,
        _right: &Object,
    ) -> Object {
        todo!()
    }

    fn evaluate_bitwise_op(&mut self, op: &Operator, left: &Integer, right: &Integer) -> Integer {
        match op {
            Operator::And => {
                if left.0.signed != right.0.signed {
                    todo!()
                }

                let signed = left.0.signed;

                Integer::new(signed, min_width(left, right), left.1 & right.1)
            }
            Operator::Caret => {
                if left.0.signed != right.0.signed {
                    todo!()
                }

                let signed = left.0.signed;

                Integer::new(signed, min_width(left, right), left.1 ^ right.1)
            }
            Operator::Pipe => {
                if left.0.signed != right.0.signed {
                    todo!()
                }

                let signed = left.0.signed;

                Integer::new(signed, min_width(left, right), left.1 | right.1)
            }
            _ => unreachable!(),
        }
    }

    fn evaluate_bitwise_shift(
        &mut self,
        op: &Operator,
        left: &Integer,
        right: &Integer,
    ) -> Integer {
        match op {
            Operator::GreaterGreater => {
                if left.0.signed != right.0.signed {
                    todo!()
                }

                let signed = left.0.signed;

                let width = {
                    let u = left.get_width_or_min_width();
                    if u > right.1 as u8 {
                        u - right.1 as u8
                    } else {
                        1
                    }
                };

                Integer::new(signed, Some(width), left.1 >> right.1)
            }
            Operator::LessLess => {
                if left.0.signed != right.0.signed {
                    todo!()
                }

                let signed = left.0.signed;

                let width = (left.get_width_or_min_width() + right.1 as u8).min(32);

                Integer::new(signed, Some(width), left.1 << right.1)
            }
            _ => unreachable!(),
        }
    }
}

fn mask(value: u32, width: Option<u8>) -> u32 {
    let mask = match width {
        Some(32) | None => 0xffff_ffff,
        Some(u) => (1 << u) - 1,
    };

    value & mask
}

fn min_width(left: &Integer, right: &Integer) -> Option<u8> {
    match left.0.bit_width {
        Some(left_width) => match right.0.bit_width {
            Some(right_width) => {
                let width = left_width.max(right_width);
                Some(width)
            }
            None => {
                let width = left_width.max(right.min_width());
                Some(width)
            }
        },
        None => match right.0.bit_width {
            Some(right_width) => {
                let width = right_width.max(left.min_width());
                Some(width)
            }
            None => None,
        },
    }
}

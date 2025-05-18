use std::collections::HashMap;

use crate::ast::{Expr, ExprKind, Stmt};

use crate::token::Sym;

fn sym_to_str(sym: &Sym) -> String {
    sym.0.clone()
}

#[derive(Default)]
pub struct Resolver {
    scopes: Vec<HashMap<Sym, VariableState>>,
    pub errors: Vec<String>,
    pub warnings: Vec<String>,
    //~ loop_depth: usize,
}

impl std::fmt::Debug for Resolver {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Resolver")
    }
}

#[derive(Clone)]
enum VariableState {
    Declared,
    Initialized,
    AssignedBeforeUse,
    Used,
    DeclaredConstant,
    InitializedConstant,
    ReassignedConstant,
    UsedConstant,
}

impl Resolver {
    pub fn resolve_all(&mut self, list: &mut [Stmt]) {
        self.begin_scope();
        self.resolve_list(list);
        self.end_scope();
    }

    pub fn resolve_list(&mut self, list: &mut [Stmt]) {
        for s in list {
            self.resolve_statement(s);
        }
    }

    fn resolve_statement(&mut self, stmt: &mut Stmt) {
        use Stmt::*;
        match stmt {
            Label(label) => {
                self.declare(label, true);
                self.init_variable(label, Some(0)); // why is this hardcoded to depth zero?
            }
            Instruction(_mnemonic, arguments) => {
                for e in arguments.iter_mut() {
                    self.resolve_expression(e);
                }
            }
            Directive(_sym, _tokens) => todo!(),
            /*
            Block(block) => {
                self.begin_scope();
                for s in block {
                    self.resolve_statement(s);
                }
                self.end_scope();
            }
            Break(expr) => {
                if self.loop_depth == 0 {
                    self.errors.push("Break outside loop".to_string());
                }
                if let Some(e) = expr {
                    self.resolve_expression(e);
                }
            }
            Expr(expr) => self.resolve_expression(expr),
            FunctionDeclaration {
                global,
                constant,
                name,
                func,
            } => {
                let old_loop_depth = self.loop_depth;

                self.loop_depth = 0;

                if *global {
                    self.declare_global(name, *constant);
                    self.init_variable(name, None);
                } else {
                    self.declare(name, *constant);
                    self.init_variable(name, Some(0));
                }
                self.resolve_function(func, FunctionKind::Function);

                self.loop_depth = old_loop_depth;
            }
            If(cond, body, else_body) => {
                self.resolve_expression(cond);
                self.resolve_statement(body);
                if let Some(s) = else_body {
                    self.resolve_statement(s);
                }
            }
            Print(expr) => self.resolve_expression(expr),
            Return(expr) => self.resolve_expression(expr),
            VariableDeclaration {
                global,
                constant,
                name,
                initializer,
            } => {
                if let Some(e) = initializer {
                    self.resolve_expression(e);
                }
                if *global {
                    self.declare_global(name, *constant);
                    if initializer.is_some() {
                        self.init_variable(name, None);
                    }
                } else {
                    self.declare(name, *constant);
                    if initializer.is_some() {
                        self.init_variable(name, Some(0));
                    }
                }
            }
            While(cond, body) => {
                self.loop_depth += 1;
                self.resolve_expression(cond);
                self.resolve_statement(body);
                self.loop_depth -= 1;
            }
            */
        }
    }

    pub fn resolve_expression(&mut self, expr: &mut Expr) {
        use ExprKind::*;
        match &mut expr.kind {
            Literal(_obj) => {
                // TODO: should protoplasm have function literals?
            }
            Unary(_op, a) => self.resolve_expression(a),
            Binary(a, _op, b) => {
                self.resolve_expression(a);
                self.resolve_expression(b);
            }
            VariableAccess(name, depth) => {
                *depth = self.resolve(name);
                self.use_variable(name, *depth);
            }
            Assign(name, expr, depth) => {
                self.resolve_expression(expr);
                *depth = self.resolve(name);
                self.assign_variable(name, *depth);
            }
            LogicalOr(a, b) => {
                self.resolve_expression(a);
                self.resolve_expression(b);
            }
            LogicalAnd(a, b) => {
                self.resolve_expression(a);
                self.resolve_expression(b);
            }
            Call(callee, args) => {
                self.resolve_expression(callee);
                for e in args {
                    self.resolve_expression(e);
                }
            }
            PropertyAccess(obj, _sym) => self.resolve_expression(obj),
            PropertyAssign(obj, _sym, val) => {
                self.resolve_expression(val);
                self.resolve_expression(obj);
            }
            ArrayConstructor(list) => {
                for e in list {
                    self.resolve_expression(e);
                }
            }
            ArrayConstructorMulti(val, _repeat) => self.resolve_expression(val),
            Index(obj, index) => {
                self.resolve_expression(index);
                self.resolve_expression(obj);
            }
            IndexAssign { obj, index, val } => {
                self.resolve_expression(val);
                self.resolve_expression(index);
                self.resolve_expression(obj);
            }
        }
    }

    /*
    fn resolve_function(&mut self, func: &mut LoxFunction, kind: FunctionKind) {
        let enclosing_kind = self.function_kind.take();

        self.function_kind = Some(kind.clone());

        self.begin_scope(); // closure created at function declaration

        if let FunctionKind::Method = kind {
            let this = {
                let mut interner = INTERNER.write().unwrap();

                interner.get_or_intern("this")
            };

            self.declare(&this, true);
            self.use_variable(&this, Some(0)); // supress "unused variable this" everywhere
        }

        self.begin_scope(); // environment created at function call

        for name in func.parameters.iter() {
            self.declare(name, true);
        }

        for s in func.body.iter_mut() {
            self.resolve_statement(s);
        }

        self.end_scope();

        self.end_scope();

        self.function_kind = enclosing_kind;
    }
    */

    pub fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn end_scope(&mut self) {
        if let Some(scope) = self.scopes.pop() {
            self.evaluate_scope(&scope);
        }
    }

    fn evaluate_scope(&mut self, scope: &HashMap<Sym, VariableState>) {
        for (name, usage) in scope.iter() {
            use VariableState::*;
            match usage {
                Declared | Initialized | DeclaredConstant | InitializedConstant => {
                    let name = sym_to_str(name);
                    if let Some(b'_') = name.as_bytes().first() {
                        // do nothing
                    } else {
                        self.warnings.push(format!("Unused variable `{}`.", name))
                    }
                }
                AssignedBeforeUse => self.warnings.push(format!(
                    "Variable `{}` re-assigned before use.",
                    sym_to_str(name)
                )),
                ReassignedConstant => self
                    .warnings
                    .push(format!("Constant `{}` re-assigned.", sym_to_str(name))),
                Used | UsedConstant => (),
            }
        }
    }

    pub fn insert_declaration(&mut self, name: &Sym, constant: bool) {
        self.declare(name, constant);
        self.init_variable(name, Some(0));
    }

    fn declare(&mut self, name: &Sym, constant: bool) {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(name) {
                self.errors
                    .push("Already a variable with this name in this scope.".to_string());
            }
            let declared = if constant {
                VariableState::DeclaredConstant
            } else {
                VariableState::Declared
            };
            scope.insert(name.clone(), declared);
        }
    }

    fn init_in_scope(scope: &mut HashMap<Sym, VariableState>, name: &Sym) -> Option<()> {
        if let Some(state) = scope.get_mut(name) {
            use VariableState::*;
            match state {
                Declared => *state = Initialized,
                Initialized => *state = AssignedBeforeUse,
                Used | AssignedBeforeUse => (),
                DeclaredConstant => *state = InitializedConstant,
                InitializedConstant | UsedConstant => *state = ReassignedConstant,
                ReassignedConstant => (),
            }
            Some(())
        } else {
            None
        }
    }

    fn assign_in_scope(scope: &mut HashMap<Sym, VariableState>, name: &Sym) -> Option<()> {
        if let Some(state) = scope.get_mut(name) {
            use VariableState::*;
            match state {
                Declared => *state = Used, // ??
                Initialized => *state = AssignedBeforeUse,
                Used | AssignedBeforeUse => (),
                DeclaredConstant => *state = InitializedConstant,
                InitializedConstant | UsedConstant => *state = ReassignedConstant,
                ReassignedConstant => (),
            }

            Some(())
        } else {
            None
        }
    }

    fn use_in_scope(scope: &mut HashMap<Sym, VariableState>, name: &Sym) -> Option<()> {
        if let Some(state) = scope.get_mut(name) {
            use VariableState::*;
            match state {
                DeclaredConstant | Declared | Initialized | Used => *state = Used,
                InitializedConstant | UsedConstant => *state = UsedConstant,
                ReassignedConstant | AssignedBeforeUse => (),
            }

            Some(())
        } else {
            None
        }
    }

    fn init_variable(&mut self, name: &Sym, depth: Option<usize>) {
        if let Some(mut depth) = depth {
            let scope = 'scope: {
                for scope in self.scopes.iter_mut().rev() {
                    if depth == 0 {
                        break 'scope Some(scope);
                    }
                    depth -= 1;
                }

                None
            };

            if let Some(scope) = scope {
                if Self::init_in_scope(scope, name).is_none() {
                    self.errors.push(format!(
                        "ICE No variable named {} in this scope.",
                        sym_to_str(name)
                    ));
                }
            }
        } else {
            self.errors
                .push(format!("ICE No variable named {}.", sym_to_str(name)));
        }
    }

    fn assign_variable(&mut self, name: &Sym, depth: Option<usize>) {
        if let Some(mut depth) = depth {
            let scope = 'scope: {
                for scope in self.scopes.iter_mut().rev() {
                    if depth == 0 {
                        break 'scope Some(scope);
                    }
                    depth -= 1;
                }

                None
            };

            if let Some(scope) = scope {
                if Self::assign_in_scope(scope, name).is_none() {
                    self.errors.push(format!(
                        "ICE No variable named {} in this scope.",
                        sym_to_str(name)
                    ));
                }
            }
        } else {
            todo!()
            //~ let _ = Self::assign_in_scope(&mut self.global_scope, name);
        }
    }

    fn use_variable(&mut self, name: &Sym, depth: Option<usize>) {
        if let Some(mut depth) = depth {
            let scope = 'scope: {
                for scope in self.scopes.iter_mut().rev() {
                    if depth == 0 {
                        break 'scope Some(scope);
                    }
                    depth -= 1;
                }

                None
            };

            if let Some(scope) = scope {
                if Self::use_in_scope(scope, name).is_none() {
                    self.errors.push(format!(
                        "ICE No variable named {} in this scope.",
                        sym_to_str(name)
                    ));
                }
            }
        } else {
            eprintln!("no depth for binding {}", name);
            todo!()
            //~ let _ = Self::use_in_scope(&mut self.global_scope, name);
        }
    }

    fn resolve(&self, name: &Sym) -> Option<usize> {
        for (depth, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(name) {
                return Some(depth);
            }
        }

        None
    }
}

// parser.rs

use crate::ast::{Expr, ExprKind, Object, Stmt};
use crate::scanner::{Scanner, SourceLocation};
use crate::token::{Operator, Sym, Token, TokenKind};

const MAX_ARGUMENTS: usize = 255;

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    ExpectedIdentifier,
    //~ ExpectedLeftBrace,
    ExpectedToken(Vec<TokenKind>, Option<Token>),
    Ice(&'static str),
    MaxArgumentsExceeded,
    UnexpectedToken(Token),
    InvalidAssignmentTarget,
    ScannerError(Box<crate::scanner::ScannerError>),
    InvalidConstructor,
}

impl core::fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            ParseErrorKind::ExpectedIdentifier => write!(f, "Expected Identifier"),
            //~ ParseErrorKind::ExpectedLeftBrace => write!(f, "Expected Starting Brace"),
            ParseErrorKind::ExpectedToken(expected, found) => {
                if expected.len() == 1 {
                    write!(f, "Expected `{}`, ", expected[0])?;
                } else {
                    write!(f, "Expected one of ")?;
                    for e in expected {
                        write!(f, "`{}`, ", e)?;
                    }
                }

                match found {
                    None => write!(f, "found end of stream"),
                    Some(token) => write!(f, "found `{}`", token),
                }
            }
            ParseErrorKind::Ice(s) => write!(f, "Internal Compiler Error: {}", s),
            ParseErrorKind::MaxArgumentsExceeded => write!(
                f,
                "Max of {} function call arguments exceeded",
                MAX_ARGUMENTS
            ),
            ParseErrorKind::UnexpectedToken(t) => write!(f, "Unexpected Token `{}`", t),
            ParseErrorKind::InvalidAssignmentTarget => write!(f, "Invalid Assignment Target"),
            ParseErrorKind::ScannerError(e) => write!(f, "Scanner Error {}", e),
            ParseErrorKind::InvalidConstructor => write!(f, "Invalid Constructor"),
        }
    }
}

impl std::error::Error for ParseError {}

#[derive(Debug, Clone)]
pub struct ParseError {
    kind: ParseErrorKind,
    pub location: Option<SourceLocation>,
}

impl core::fmt::Display for ParseError {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

pub struct Parser {
    scanner: Scanner,
    previous: Option<Token>,
    current: Option<Token>,
    errors: Vec<ParseError>,
    at_end: bool,
    //~ scope_depth: usize,
}

impl Parser {
    pub fn new(scanner: Scanner) -> Self {
        let mut p = Self {
            scanner,
            previous: None,
            current: None,
            errors: vec![],
            at_end: false,
            //~ scope_depth: 0,
        };

        p.init();

        p
    }

    pub fn parse_all(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = vec![];
        while !self.is_at_end() {
            if let Some(x) = self.parse_next() {
                let stmt = x?;
                statements.push(stmt);
            }
        }
        Ok(statements)
    }

    pub fn errors(&self) -> &Vec<ParseError> {
        &self.errors
    }

    /*
    pub fn clear_errors(&mut self) {
        self.errors.clear();
    }
    */

    fn init(&mut self) {
        self.current = self.next_token();
        self.at_end = self.current.is_none();
    }

    fn is_at_end(&self) -> bool {
        self.at_end
    }

    fn parse_next(&mut self) -> Option<Result<Stmt, ParseError>> {
        if self.is_at_end() {
            return None;
        }

        match self.initial() {
            Ok(maybe_stmt) => maybe_stmt.map(Ok),
            Err(err) => {
                self.report(err);
                self.synchronize();
                None
            }
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.current.as_ref()
    }

    fn next_token(&mut self) -> Option<Token> {
        match self.scanner.next() {
            Some(r) => match r {
                Ok(t) => Some(t),
                Err(e) => {
                    let location = self.current.clone().map(|t| t.location.clone());
                    self.report(ParseError {
                        location,
                        kind: ParseErrorKind::ScannerError(Box::new(e)),
                    });
                    None
                }
            },
            None => None,
        }
    }

    fn advance(&mut self) -> Option<&Token> {
        self.previous = self.current.clone();
        self.current = self.next_token();
        if self.current.is_none() {
            self.at_end = true;
        }
        self.previous.as_ref()
    }

    fn check(&self, kinds: &[TokenKind]) -> bool {
        match self.peek() {
            Some(t) => t.in_kinds(kinds),
            None => false,
        }
    }

    fn check_advance(&mut self, kinds: &[TokenKind]) -> Option<&Token> {
        if self.check(kinds) {
            self.advance()
        } else {
            None
        }
    }

    fn advance_if_match(&mut self, kind: TokenKind) -> bool {
        match self.peek() {
            Some(t) => {
                if t.kind.same_kind(&kind) {
                    self.advance();
                    true
                } else {
                    false
                }
            }
            None => false,
        }
    }

    fn peek_previous(&self) -> Option<&Token> {
        self.previous.as_ref()
    }

    fn consume(
        &mut self,
        kinds: &[TokenKind],
        err: ParseErrorKind,
    ) -> Result<Option<&Token>, ParseError> {
        if self.check(kinds) {
            Ok(self.advance())
        } else {
            Err(self.error(err))
        }
    }

    fn consume_expected(&mut self, kinds: &[TokenKind]) -> Result<Option<&Token>, ParseError> {
        self.consume(
            kinds,
            ParseErrorKind::ExpectedToken(kinds.to_owned(), self.peek().cloned()),
        )
    }

    fn consume_identifier(&mut self) -> Result<Sym, ParseError> {
        if let Some(t) = self.peek() {
            if let TokenKind::Identifier(sym) = &t.kind {
                let sym = sym.clone();
                self.advance();
                return Ok(sym);
            }
        }

        Err(self.error(ParseErrorKind::ExpectedIdentifier))
    }

    fn error(&self, kind: ParseErrorKind) -> ParseError {
        let location = match self.peek() {
            Some(t) => Some(t.location.clone()),
            None => self.peek_previous().map(|t| t.location.clone()),
        };

        ParseError { kind, location }
    }

    fn report(&mut self, error: ParseError) {
        self.errors.push(error)
    }

    fn synchronize(&mut self) {
        self.advance();

        while let Some(token) = self.peek() {
            if let Some(t) = self.peek_previous() {
                if let TokenKind::Op(Operator::Semicolon) = t.kind {
                    break;
                }
            }

            if let TokenKind::Eol = &token.kind {
                break;
            }

            self.advance();
        }
    }

    /// This is a helper function used by the separate rules
    /// for each binary operator function.
    fn binary_op(
        &mut self,
        get_argument: fn(&mut Parser) -> Result<Expr, ParseError>,
        kinds: &[TokenKind],
    ) -> Result<Expr, ParseError> {
        let mut expr = get_argument(self)?;
        let mut location = expr.location.clone();

        while let Some(op_token) = self.check_advance(kinds) {
            location = location.combine(&op_token.location);
            let op = op_token.to_operator();
            let right = get_argument(self)?;
            location = location.combine(&right.location);
            expr = Expr {
                location: location.clone(),
                kind: ExprKind::Binary(Box::new(expr), op, Box::new(right)),
            };
        }

        Ok(expr)
    }
}

/**
### Recursive Descent
Most of the following functions each represent one rule of the language's grammar.
The rest are helper functions that directly assist with that.
**/
impl Parser {
    pub fn parse_as_expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn initial(&mut self) -> Result<Option<Stmt>, ParseError> {
        self.item()
    }

    // a directive, label, or instruction
    fn item(&mut self) -> Result<Option<Stmt>, ParseError> {
        while !self.is_at_end() {
            if self.check(&[TokenKind::Eol]) {
                self.advance();
                continue;
            }

            break;
        }

        if self.is_at_end() {
            return Ok(None);
        }

        if self
            .check_advance(&[TokenKind::Op(Operator::Dot)])
            .is_some()
        {
            Ok(Some(self.directive()?))
        } else {
            Ok(Some(self.label_or_instruction()?))
        }
    }

    fn directive(&mut self) -> Result<Stmt, ParseError> {
        let directive = self.consume_identifier()?;

        let mut tokens = vec![];

        while !self.is_at_end() {
            if self.check_advance(&[TokenKind::Eol]).is_some() {
                break;
            }

            let t = self.peek().unwrap();

            tokens.push(t.clone());

            self.advance();
        }

        Ok(Stmt::Directive(directive, tokens))
    }

    fn label_or_instruction(&mut self) -> Result<Stmt, ParseError> {
        while !self.is_at_end() {
            if self.check_advance(&[TokenKind::Eol]).is_some() {
                continue;
            }

            self.consume_identifier()?;

            if self.check(&[TokenKind::Op(Operator::Colon)]) {
                return self.label();
            } else {
                return self.instruction();
            }
        }

        unreachable!()
    }

    // an identifier followed by a colon
    fn label(&mut self) -> Result<Stmt, ParseError> {
        let expect_err = self.error(ParseErrorKind::Ice("label expects identifier"));

        let id = self.peek_previous().ok_or(expect_err.clone())?;

        let label = if let TokenKind::Identifier(sym) = &id.kind {
            sym.clone()
        } else {
            return Err(expect_err);
        };

        self.consume_expected(&[TokenKind::Op(Operator::Colon)])?;

        Ok(Stmt::Label(label))
    }

    // an identifier, followed by a comma-separated list of expressions
    fn instruction(&mut self) -> Result<Stmt, ParseError> {
        let expect_err = self.error(ParseErrorKind::Ice("instruction expects identifier"));

        let id = self.peek_previous().ok_or(expect_err.clone())?;

        let mnemonic = if let TokenKind::Identifier(sym) = &id.kind {
            sym.clone()
        } else {
            return Err(expect_err);
        };

        let args = self.argument_list()?;

        Ok(Stmt::Instruction(mnemonic, args))
    }

    // a comma-separated list of arguments
    fn argument_list(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut arguments = vec![];

        if !self.check(&[TokenKind::Eol]) {
            loop {
                arguments.push(self.expression()?);

                if !self.advance_if_match(TokenKind::Op(Operator::Comma)) {
                    break;
                }
            }
        }

        if arguments.len() > MAX_ARGUMENTS {
            self.report(self.error(ParseErrorKind::MaxArgumentsExceeded));
        }

        Ok(arguments)
    }

    /*
    fn item_block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = vec![];

        self.scope_depth += 1;

        while !self.check(&[TokenKind::RightBrace]) && !self.is_at_end() {
            match self.item()? {
                Some(item) => stmts.push(item),
                None => break,
            }
        }

        self.scope_depth -= 1;

        self.consume_expected(&[TokenKind::RightBrace])?;

        Ok(stmts)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParseError> {
        let mut arguments = vec![];

        if !self.check(&[TokenKind::RightParen]) {
            loop {
                arguments.push(self.expression()?);

                if !self.advance_if_match(TokenKind::Op(Operator::Comma)) {
                    break;
                }
            }
        }

        if arguments.len() > MAX_ARGUMENTS {
            self.report(self.error(ParseErrorKind::MaxArgumentsExceeded));
        }

        let t = self.consume_expected(&[TokenKind::RightParen])?.unwrap();

        Ok(Expr {
            location: t.location.clone(),
            kind: ExprKind::Call(Box::new(callee), arguments),
        })
    }
    */

    /*

    fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        self.consume_expected(&[TokenKind::Op(Operator::Semicolon)])?;
        Ok(Stmt::ExprStmt(expr))
    }
    */

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.logical_or()?;

        if let Some(op_token) = self.check_advance(&[
            TokenKind::Op(Operator::Equal),
            TokenKind::Op(Operator::MinusEqual),
            TokenKind::Op(Operator::PlusEqual),
            TokenKind::Op(Operator::SlashEqual),
            TokenKind::Op(Operator::StarEqual),
            TokenKind::Op(Operator::PercentEqual),
            TokenKind::Op(Operator::PipeEqual),
            TokenKind::Op(Operator::AndEqual),
        ]) {
            let token = op_token.clone();

            let value = Box::new(self.assignment()?);

            let r_expr = match &token.kind {
                TokenKind::Op(op) => match op {
                    Operator::Equal => value,
                    a => Box::new(Expr {
                        location: expr.location.clone(),
                        kind: ExprKind::Binary(
                            Box::new(expr.clone()),
                            a.binary_from_combined()
                                .ok_or(self.error(ParseErrorKind::Ice(
                                    "not a combined assignment operator; internal scanner error",
                                )))?,
                            value,
                        ),
                    }),
                },
                _ => unreachable!(),
            };

            return Ok(match expr.kind {
                ExprKind::VariableAccess(name, _depth) => Expr {
                    location: expr.location,
                    kind: ExprKind::Assign(name, r_expr, None),
                },
                ExprKind::PropertyAccess(obj, name) => Expr {
                    location: expr.location,
                    kind: ExprKind::PropertyAssign(obj, name, r_expr),
                },
                ExprKind::Index(obj, index) => Expr {
                    location: expr.location,
                    kind: ExprKind::IndexAssign {
                        obj,
                        index,
                        val: r_expr,
                    },
                },

                // TODO: Can we report and synchronize here?
                _ => return Err(self.error(ParseErrorKind::InvalidAssignmentTarget)),
            });
        }

        Ok(expr)
    }

    fn logical_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.logical_and()?;

        while let Some(operator) = self.check_advance(&[TokenKind::Op(Operator::PipePipe)]) {
            let location = operator.location.clone();

            let right = self.logical_and()?;

            expr = Expr {
                location,
                kind: ExprKind::LogicalOr(Box::new(expr), Box::new(right)),
            };
        }

        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;

        while let Some(operator) = self.check_advance(&[TokenKind::Op(Operator::AndAnd)]) {
            let location = operator.location.clone();

            let right = self.equality()?;

            expr = Expr {
                location,
                kind: ExprKind::LogicalAnd(Box::new(expr), Box::new(right)),
            };
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        self.binary_op(
            Parser::comparison,
            &[
                TokenKind::Op(Operator::EqualEqual),
                TokenKind::Op(Operator::BangEqual),
            ],
        )
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        self.binary_op(
            Parser::bit_or,
            &[
                TokenKind::Op(Operator::Greater),
                TokenKind::Op(Operator::GreaterEqual),
                TokenKind::Op(Operator::Less),
                TokenKind::Op(Operator::LessEqual),
            ],
        )
    }

    fn bit_or(&mut self) -> Result<Expr, ParseError> {
        self.binary_op(Parser::bit_xor, &[TokenKind::Op(Operator::Pipe)])
    }

    fn bit_xor(&mut self) -> Result<Expr, ParseError> {
        self.binary_op(Parser::bit_and, &[TokenKind::Op(Operator::Caret)])
    }

    fn bit_and(&mut self) -> Result<Expr, ParseError> {
        self.binary_op(Parser::bit_shift, &[TokenKind::Op(Operator::And)])
    }

    fn bit_shift(&mut self) -> Result<Expr, ParseError> {
        self.binary_op(
            Parser::addition,
            &[
                TokenKind::Op(Operator::GreaterGreater),
                TokenKind::Op(Operator::LessLess),
            ],
        )
    }

    fn addition(&mut self) -> Result<Expr, ParseError> {
        self.binary_op(
            Parser::multiplication,
            &[
                TokenKind::Op(Operator::Minus),
                TokenKind::Op(Operator::Plus),
            ],
        )
    }

    fn multiplication(&mut self) -> Result<Expr, ParseError> {
        self.binary_op(
            Parser::unary,
            &[
                TokenKind::Op(Operator::Slash),
                TokenKind::Op(Operator::Star),
                TokenKind::Op(Operator::Percent),
            ],
        )
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if let Some(op_token) = self.check_advance(&[
            TokenKind::Op(Operator::Bang),
            TokenKind::Op(Operator::Minus),
            TokenKind::Op(Operator::Plus),
            TokenKind::Op(Operator::Hash),
        ]) {
            let op_location = op_token.location.clone();
            let op = op_token.to_operator();

            let right = self.unary()?;
            let location = op_location.combine(&right.location);

            return Ok(Expr {
                location,
                kind: ExprKind::Unary(op, Box::new(right)),
            });
        }

        self.call()
    }

    fn call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;

        loop {
            if self.advance_if_match(TokenKind::LeftParen) {
                expr = self.finish_call(expr)?;
            } else if self.advance_if_match(TokenKind::LeftBracket) {
                expr = self.finish_index(expr)?;
            } else if self.advance_if_match(TokenKind::Op(Operator::Dot)) {
                let name = self.consume_identifier()?;

                expr = Expr {
                    location: self.peek_previous().unwrap().location.clone(),
                    kind: ExprKind::PropertyAccess(Box::new(expr), name),
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParseError> {
        let mut arguments = vec![];

        if !self.check(&[TokenKind::RightParen]) {
            loop {
                arguments.push(self.expression()?);

                if !self.advance_if_match(TokenKind::Op(Operator::Comma)) {
                    break;
                }
            }
        }

        if arguments.len() > MAX_ARGUMENTS {
            self.report(self.error(ParseErrorKind::MaxArgumentsExceeded));
        }

        let t = self.consume_expected(&[TokenKind::RightParen])?.unwrap();

        Ok(Expr {
            location: t.location.clone(),
            kind: ExprKind::Call(Box::new(callee), arguments),
        })
    }

    fn finish_index(&mut self, callee: Expr) -> Result<Expr, ParseError> {
        let index = Box::new(self.expression()?);

        let t = self.consume_expected(&[TokenKind::RightBracket])?.unwrap();

        Ok(Expr {
            location: t.location.clone(),
            kind: ExprKind::Index(Box::new(callee), index),
        })
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        let next_token = match self.peek() {
            Some(t) => t,
            None => return Err(self.error(ParseErrorKind::Ice("Unexpected end of Token stream"))),
        };

        let location = next_token.location.clone();

        if next_token.is_object() {
            let literal = Object::from_token(next_token).unwrap();

            self.advance();

            Ok(Expr {
                location,
                kind: ExprKind::Literal(literal),
            })
        } else {
            match &next_token.kind {
                TokenKind::LeftParen => {
                    self.advance();

                    let expr = self.expression()?;

                    self.consume_expected(&[TokenKind::RightParen])?;

                    Ok(expr)
                }
                TokenKind::LeftBracket => {
                    self.advance();

                    let mut exprs = vec![];
                    while !self.check(&[TokenKind::RightBracket]) && !self.is_at_end() {
                        exprs.push(self.expression()?);

                        if !self.advance_if_match(TokenKind::Op(Operator::Comma)) {
                            break;
                        }
                    }

                    if self.advance_if_match(TokenKind::Op(Operator::Semicolon)) {
                        let multiplier = self.expression()?;

                        if exprs.len() == 1 {
                            self.consume_expected(&[TokenKind::RightBracket])?;

                            return Ok(Expr {
                                location,
                                kind: ExprKind::ArrayConstructorMulti(
                                    Box::new(exprs.pop().unwrap()),
                                    Box::new(multiplier),
                                ),
                            });
                        } else {
                            return Err(self.error(ParseErrorKind::InvalidConstructor));
                        }
                    }

                    self.consume_expected(&[TokenKind::RightBracket])?;

                    Ok(Expr {
                        location,
                        kind: ExprKind::ArrayConstructor(exprs),
                    })
                }
                TokenKind::Identifier(sym) => {
                    let sym = sym.clone();

                    self.advance();

                    Ok(Expr {
                        location,
                        kind: ExprKind::VariableAccess(sym, None),
                    })
                }
                _ => Err(self.error(ParseErrorKind::UnexpectedToken(next_token.clone()))),
            }
        }
    }
}

impl Iterator for Parser {
    type Item = Result<Stmt, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse_next()
    }
}

/* old rules
expression      → assignment ;

assignment      → ( call "." )? IDENTIFIER "=" assignment | logic_or ;

logic_or        → logic_and ( "||" logic_and )* ;
logic_and       → equality ( "&&" equality )* ;
equality        → comparison ( ( "!=" | "==" ) comparison )* ;
comparison      → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term            → factor ( ( "-" | "+" ) factor )* ;
factor          → unary ( ( "/" | "*" | "%" ) unary )* ;

unary           → ( "!" | "-" | "+" ) unary | call ;
call            → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
primary         → | NUMBER | STRING | IDENTIFIER | "(" expression ")" ;
*/

/* new rules
expression      → assignment ;

assignment      → ( call "." )? IDENTIFIER "=" assignment | logic_or ;

logic_or        → logic_and ( "||" logic_and )* ;
logic_and       → equality ( "&&" equality )* ;
equality        → comparison ( ( "!=" | "==" ) comparison )* ;
comparison      → bit_or ( ( ">" | ">=" | "<" | "<=" ) bit_or )* ;
bit_or          → bit_xor ( "|" bit_xor )* ;
bit_xor         → bit_and ( "^" bit_and )* ;
bit_and         → bit_shift ( "&" bit_shift )* ;
bit_shift       → term ( ( "<<" | ">>" ) term )* ;
term            → factor ( ( "-" | "+" ) factor )* ;
factor          → unary ( ( "/" | "*" | "%" ) unary )* ;

unary           → ( "!" | "-" | "+" ) unary | call ;
call            → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
primary         → | NUMBER | STRING | IDENTIFIER | "(" expression ")" ;
*/

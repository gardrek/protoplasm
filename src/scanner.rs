// scanner.rs

static MAX_INTEGER_LENGTH: usize = 256;

use crate::ast::Object;
use crate::token::{Operator, Sym, Token, TokenKind};
use std::ops::Range;

fn is_identifier_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_identifier_continue(c: char) -> bool {
    c.is_digit(36) || c == '_'
}

#[derive(Debug, Clone)]
pub struct SourceLocation {
    // byte-indexed location from the start of the file/string
    pub range: Range<usize>,
}

impl SourceLocation {
    pub fn new(range: Range<usize>) -> SourceLocation {
        SourceLocation { range }
    }

    pub fn bull() -> SourceLocation {
        SourceLocation::new(0..0)
    }

    pub fn combine(&self, other: &SourceLocation) -> SourceLocation {
        if self.range.start > self.range.end || other.range.start > other.range.end {
            panic!();
        }

        SourceLocation {
            range: self.range.start.min(other.range.start)..self.range.end.max(other.range.end),
        }
    }

    pub fn get_line_number(&self, s: &str) -> usize {
        'l: {
            let mut line_number = 1;

            for (index, byte) in s.as_bytes().iter().enumerate() {
                if self.range.start == index {
                    break 'l line_number;
                }

                if *byte == b'\n' {
                    line_number += 1;
                }
            }

            panic!("ICE: Source Location outside source at {}", self);
        }
    }
}

impl std::fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "SourceLocation({:?})", self.range)
    }
}

#[derive(Debug, Clone)]
pub enum ScannerError {
    //~ Ice(&'static str),
    Unimplemented(&'static str),
}

impl std::error::Error for ScannerError {}

impl std::fmt::Display for ScannerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use ScannerError::*;
        match self {
            //~ Ice(msg) => write!(f, "Internal Compiler Error: {}", msg),
            Unimplemented(msg) => write!(f, "Unimplemented: {}", msg),
        }
    }
}

pub struct Scanner {
    // the source file
    source: String,

    // location, in bytes, which we're currently looking at
    cursor: usize,

    // whether end of file was reached
    eof: bool,

    // whether scanning encountered an error
    had_error: bool,
}

impl Scanner {
    pub fn new(src: &str) -> Scanner {
        Scanner {
            source: String::from(src),
            cursor: 0,
            eof: false,
            had_error: false,
        }
    }

    pub fn source_get_slice(&self, location: &SourceLocation) -> &str {
        &self.source[location.range.clone()]
    }

    fn advance_char(&mut self) -> Option<char> {
        let index = self.cursor;
        if index < self.source.len() {
            let ch = self.peek_char().unwrap();
            self.cursor += ch.len_utf8();
            Some(ch)
        } else {
            None
        }
    }

    fn peek_char(&self) -> Option<char> {
        let cursor = self.cursor;
        self.source[cursor..].chars().next()
    }

    fn static_token(&mut self, kind: TokenKind, length: usize) -> Token {
        let location = SourceLocation::new((self.cursor - length)..self.cursor);
        Token { location, kind }
    }

    fn match_static_token(
        &mut self,
        m: char,
        a: TokenKind,
        b: TokenKind,
        b_length: usize,
    ) -> Result<Token, ScannerError> {
        if let Some(ch) = self.peek_char() {
            Ok(if ch == m {
                let a_length = ch.len_utf8();
                self.cursor += a_length;
                self.static_token(a, b_length + a_length)
            } else {
                self.static_token(b, b_length)
            })
        } else {
            Err(ScannerError::Unimplemented(
                "match_static_token unexpected end of stream",
            ))
        }
    }

    fn match_static_operator(
        &mut self,
        m: char,
        a: Operator,
        b: Operator,
        b_length: usize,
    ) -> Result<Token, ScannerError> {
        self.match_static_token(m, TokenKind::Op(a), TokenKind::Op(b), b_length)
    }

    fn line_comment(&mut self) {
        loop {
            let c = match self.advance_char() {
                None => return, // end of file
                Some(x) => x,
            };
            if c == '\n' {
                self.cursor -= 1;
                return;
            }
        }
    }

    fn block_comment(&mut self) -> Result<(), ScannerError> {
        // TODO: This code seems to work but it's kind of a mess
        // Maybe come through later and spruce it up so it's less confusing to read
        match self.advance_char() {
            Some('/') => match self.advance_char() {
                Some('*') => loop {
                    match self.advance_char() {
                        None => {
                            return Err(ScannerError::Unimplemented("Unclosed Block Comment"));
                        }
                        Some('*') => match self.peek_char() {
                            None => {
                                return Err(ScannerError::Unimplemented("Unclosed Block Comment"));
                            }
                            Some('/') => {
                                self.cursor += 1;
                                return Ok(());
                            }
                            Some(_) => continue,
                        },
                        Some('/') => match self.peek_char() {
                            None => {
                                return Err(ScannerError::Unimplemented("Unclosed Block Comment"));
                            }
                            Some('*') => {
                                self.cursor -= 1;
                                self.block_comment()?;
                            }
                            Some(_) => continue,
                        },
                        Some(_) => continue,
                    };
                },
                Some(_) => Err(ScannerError::Unimplemented("not a Block Comment?")),
                None => Err(ScannerError::Unimplemented("Unclosed Block Comment")),
            },
            Some(_) => Err(ScannerError::Unimplemented("not a Block Comment?")),
            None => Err(ScannerError::Unimplemented("Unclosed Block Comment")),
        }
    }

    fn number(&mut self) -> Token {
        let offset = self.cursor;

        if let Some('%') = self.peek_char() {
            self.advance_char();
            self.bit_int(offset, "%", 2)
        } else if let Some('$') = self.peek_char() {
            self.advance_char();
            self.bit_int(offset, "$", 16)
        } else if let Some('0') = self.peek_char() {
            self.advance_char();
            if let Some(ch) = self.peek_char() {
                match ch {
                    'x' => {
                        self.advance_char();
                        self.hex_int(offset)
                    }
                    'o' => {
                        self.advance_char();
                        self.oct_int(offset)
                    }
                    'q' => {
                        self.advance_char();
                        self.qua_int(offset)
                    }
                    'b' => {
                        self.advance_char();
                        self.bin_int(offset)
                    }
                    _ => self.decimal(offset, 1),
                }
            } else {
                self.decimal(offset, 1)
            }
        } else {
            self.decimal(offset, 0)
        }
    }

    fn hex_int(&mut self, offset: usize) -> Token {
        self.bit_int(offset, "0x", 16)
    }

    fn oct_int(&mut self, offset: usize) -> Token {
        self.bit_int(offset, "0o", 8)
    }

    fn qua_int(&mut self, offset: usize) -> Token {
        self.bit_int(offset, "0q", 4)
    }

    fn bin_int(&mut self, offset: usize) -> Token {
        self.bit_int(offset, "0b", 2)
    }

    fn bit_int(&mut self, offset: usize, prefix: &str, base: u8) -> Token {
        let mut length = prefix.len();
        let mut bit_width = 0;
        let bits_per_char = base.ilog2();

        while let Some(ch) = self.peek_char() {
            if let Some(_digit) = ch.to_digit(base.into()) {
                self.advance_char();
                length += ch.len_utf8();
                bit_width += bits_per_char;
            } else {
                match ch {
                    '_' => {
                        self.advance_char();
                        length += ch.len_utf8();
                    }
                    _ => break,
                }
            }
        }

        let location = SourceLocation::new(offset..(offset + length));

        if length > MAX_INTEGER_LENGTH {
            return Token {
                location,
                kind: TokenKind::InvalidNumber,
            };
        }

        let raw_slice = self
            .source_get_slice(&location)
            .strip_prefix(prefix)
            //~ .ok_or(Err(ScannerError::Ice("Error in integer scanning")))?;
            .unwrap();

        let mut s = String::from(raw_slice);

        s.retain(|c| c != '_');

        let slice = &s;

        let value = match u32::from_str_radix(slice, base.into()) {
            Ok(v) => v,
            Err(_) => {
                return Token {
                    location,
                    kind: TokenKind::InvalidNumber,
                };
            }
        };

        let bit_width = Some(bit_width.try_into().unwrap());

        Token {
            location,
            kind: TokenKind::Obj(Object::integer(false, bit_width, value)),
        }
    }

    fn decimal(&mut self, offset: usize, length_so_far: usize) -> Token {
        let mut is_float = false;

        let mut length = length_so_far;

        let mut decimal = false;
        while let Some(ch) = self.peek_char() {
            if let Some(_digit) = ch.to_digit(10) {
                self.advance_char();
                length += ch.len_utf8();
            } else if ch == '_' {
                self.advance_char();
                length += ch.len_utf8();
            } else if ch == '.' {
                is_float = true;
                if decimal {
                    break;
                }
                self.cursor += ch.len_utf8();
                if let Some(ch_next) = self.peek_char() {
                    if ch_next.is_ascii_digit() {
                        length += ch_next.len_utf8();
                        decimal = true;
                    } else {
                        self.cursor -= ch.len_utf8();
                        break;
                    }
                } else {
                    self.cursor -= ch.len_utf8();
                    break;
                }
            } else {
                break;
            }
        }

        let location = SourceLocation::new(offset..(offset + length));

        let raw_slice = self.source_get_slice(&location);

        let mut s = String::from(raw_slice);

        s.retain(|c| c != '_');

        let slice = &s;

        if is_float {
            let value = match slice.parse::<f32>() {
                Ok(v) => v,
                Err(_) => {
                    return Token {
                        location,
                        kind: TokenKind::InvalidNumber,
                    };
                }
            };

            Token {
                location,
                kind: TokenKind::Obj(Object::Float(value)),
            }
        } else {
            let value = match slice.parse::<u32>() {
                Ok(v) => v,
                Err(_) => {
                    return Token {
                        location,
                        kind: TokenKind::InvalidNumber,
                    };
                }
            };

            Token {
                location,
                kind: TokenKind::Obj(Object::integer(false, None, value)),
            }
        }
    }

    fn string(&mut self) -> Token {
        let offset = self.cursor - 1;
        let mut length = 0;

        loop {
            let c = self.peek_char();
            if let Some(c) = c {
                self.advance_char();
                length += c.len_utf8();
                if c == '\\' {
                    if let Some(c) = self.peek_char() {
                        self.advance_char(); // jump forward one to skip the quote check
                        length += c.len_utf8();
                    } else {
                        let location = SourceLocation::new(offset..(offset + length));
                        let token = Token {
                            location,
                            kind: TokenKind::InvalidString,
                        };
                        return token;
                    }
                } else if c == '"' {
                    length += 1;
                    break;
                }
            } else {
                let location = SourceLocation::new(offset..(offset + length));
                let token = Token {
                    location,
                    kind: TokenKind::InvalidString,
                };
                return token;
            }
        }

        let location = SourceLocation::new(offset..(offset + length));

        /*  Even though we're not multi-threaded yet,
            there's no reason to keep the lock longer than necessary
        */
        let raw = &self.source_get_slice(&location)[1..length - 1];

        // TODO: actually handle errors here, possibly introduce a new errors holder, or throw this down the line to the parser
        let s = match Scanner::parse_string(raw) {
            Ok(s) => s,
            Err(_e) => {
                self.had_error = true;
                "".to_string()
            }
        };

        Token {
            location,
            kind: TokenKind::Obj(Object::String(s)),
        }
    }

    fn parse_string(escaped_string: &str) -> Result<String, &'static str> {
        let mut final_string = String::with_capacity(escaped_string.len());
        let mut iter = escaped_string.chars();

        while let Some(ch) = iter.next() {
            final_string.push(match ch {
                '\\' => match iter.next() {
                    Some(esc) => match esc {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '\\' => '\\',
                        '0' => '\0',
                        '\'' => '\'',
                        '\"' => '\"',
                        'x' => return Err("unimplemented string escape `\\x`"),
                        'u' => return Err("unimplemented string escape `\\u`"),
                        _ => return Err("unrecognized string escape"),
                    },
                    None => return Err("unexpected end of string"),
                },
                _ => ch,
            });
        }

        Ok(final_string)
    }

    fn identifier(&mut self) -> Token {
        let offset = self.cursor;
        let mut length = 0;

        loop {
            let c = self.peek_char();
            if let Some(c) = c {
                if is_identifier_continue(c) {
                    self.advance_char();
                    length += c.len_utf8();
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        let location = SourceLocation::new(offset..(offset + length));

        let slicing = self.source_get_slice(&location);

        let sym = Sym::new(slicing);

        let kind = TokenKind::Identifier(sym);

        Token { location, kind }
    }

    fn tick_identifier(&mut self, offset: usize) -> Token {
        let t = self.identifier();
        let location = SourceLocation::new(offset..(offset + t.location.range.len()));
        let kind = match t.kind {
            TokenKind::Identifier(sym) => match &sym.0[..] {
                "true" => TokenKind::Obj(Object::Boolean(true)),
                "false" => TokenKind::Obj(Object::Boolean(false)),
                _ => TokenKind::UnknownTickIdentifier(sym),
            },
            _ => unreachable!(),
        };
        Token { location, kind }
    }

    fn error_unimplemented(&mut self, s: &'static str) -> ScannerError {
        self.had_error = true;
        ScannerError::Unimplemented(s)
    }

    fn do_eof(&mut self) -> Option<Token> {
        self.eof = true;
        None
    }

    fn scan_token(&mut self) -> Result<Option<Token>, ScannerError> {
        let total_length = self.source.len();

        // If we are past the end or already hit eof
        if self.eof || self.cursor >= total_length {
            return Ok(self.do_eof());
        }

        let token = loop {
            // If there's no bytes left return None to signal the end
            let inner = match self.advance_char() {
                Some(ch) => ch,
                None => return Ok(self.do_eof()),
            };

            use Operator as Op;
            use TokenKind as Tk;
            match inner {
                '\n' => break self.static_token(Tk::Eol, 1),
                '(' => break self.static_token(Tk::LeftParen, 1),
                ')' => break self.static_token(Tk::RightParen, 1),
                '{' => break self.static_token(Tk::LeftBrace, 1),
                '}' => break self.static_token(Tk::RightBrace, 1),
                '[' => break self.static_token(Tk::LeftBracket, 1),
                ']' => break self.static_token(Tk::RightBracket, 1),
                ',' => break self.static_token(Tk::Op(Op::Comma), 1),
                '.' => break self.static_token(Tk::Op(Op::Dot), 1),
                ';' => break self.static_token(Tk::Op(Op::Semicolon), 1),
                '-' => match self.peek_char() {
                    // no bytes left
                    None => break self.static_token(Tk::Op(Op::Minus), 1),
                    Some(c) => match c {
                        '>' => {
                            self.cursor += c.len_utf8();
                            break self.static_token(Tk::Op(Op::RightArrow), 1);
                        }
                        _ => {
                            break self.match_static_operator('=', Op::MinusEqual, Op::Minus, 1)?;
                        }
                    },
                },
                '+' => break self.match_static_operator('=', Op::PlusEqual, Op::Plus, 1)?,
                '/' => match self.peek_char() {
                    // no bytes left
                    None => break self.static_token(Tk::Op(Op::Slash), 1),
                    Some(c) => match c {
                        '/' => {
                            self.line_comment();
                            continue;
                        }
                        '*' => {
                            self.cursor -= 1;
                            self.block_comment()?;
                            continue;
                        }
                        '=' => {
                            self.cursor += c.len_utf8();
                            break self.static_token(Tk::Op(Op::SlashEqual), 1);
                        }
                        _ => break self.static_token(Tk::Op(Op::Slash), 1),
                    },
                },
                '*' => break self.match_static_operator('=', Op::StarEqual, Op::Star, 1)?,
                '%' => match self.peek_char() {
                    None => break self.static_token(Tk::Op(Op::Percent), 1),
                    Some(c) => match c {
                        ch if ch.is_ascii_digit() => {
                            self.cursor -= '%'.len_utf8();
                            break self.number();
                        }
                        _ => {
                            break self.match_static_operator(
                                '=',
                                Op::PercentEqual,
                                Op::Percent,
                                1,
                            )?;
                        }
                    },
                },
                '!' => break self.match_static_operator('=', Op::BangEqual, Op::Bang, 1)?,
                '=' => break self.match_static_operator('=', Op::EqualEqual, Op::Equal, 1)?,
                '>' => match self.peek_char() {
                    // no bytes left
                    None => break self.static_token(Tk::Op(Op::Greater), 1),
                    Some(c) => match c {
                        '>' => {
                            self.cursor += c.len_utf8();
                            break self.static_token(Tk::Op(Op::GreaterGreater), 1);
                        }
                        _ => {
                            break self.match_static_operator(
                                '=',
                                Op::GreaterEqual,
                                Op::Greater,
                                1,
                            )?;
                        }
                    },
                },
                '<' => match self.peek_char() {
                    // no bytes left
                    None => break self.static_token(Tk::Op(Op::Less), 1),
                    Some(c) => match c {
                        '<' => {
                            self.cursor += c.len_utf8();
                            break self.static_token(Tk::Op(Op::LessLess), 1);
                        }
                        '-' => {
                            self.cursor += c.len_utf8();
                            break self.static_token(Tk::Op(Op::LeftArrow), 1);
                        }
                        _ => break self.match_static_operator('=', Op::LessEqual, Op::Less, 1)?,
                    },
                },
                '|' => match self.peek_char() {
                    // no bytes left
                    None => break self.static_token(Tk::Op(Op::Pipe), 1),
                    Some(c) => match c {
                        '|' => {
                            self.cursor += c.len_utf8();
                            break self.static_token(Tk::Op(Op::PipePipe), 1);
                        }
                        _ => break self.match_static_operator('=', Op::PipeEqual, Op::Pipe, 1)?,
                    },
                },
                '&' => match self.peek_char() {
                    // no bytes left
                    None => break self.static_token(Tk::Op(Op::And), 1),
                    Some(c) => match c {
                        '&' => {
                            self.cursor += c.len_utf8();
                            break self.static_token(Tk::Op(Op::AndAnd), 1);
                        }
                        _ => break self.match_static_operator('=', Op::AndEqual, Op::And, 1)?,
                    },
                },
                '#' => break self.static_token(Tk::Op(Op::Hash), 1),
                ':' => break self.static_token(Tk::Op(Op::Colon), 1),
                '^' => break self.static_token(Tk::Op(Op::Caret), 1),
                '"' => break self.string(),
                '$' => {
                    self.cursor -= '$'.len_utf8();
                    break self.number();
                }
                '\'' => break self.tick_identifier(self.cursor - '\''.len_utf8()),
                ch if ch.is_whitespace() => {
                    continue;
                }
                ch if ch.is_ascii_digit() => {
                    self.cursor -= ch.len_utf8();
                    break self.number();
                }
                ch if is_identifier_start(ch) => {
                    self.cursor -= ch.len_utf8();
                    break self.identifier();
                }
                // FIXME: This currently treats unrecognized chars as an error.
                // We now have full support for unicode inside strings, at least.
                // Perhaps we can add more Unicode awareness.
                // E.g. using Unicode designations to allow for post-ASCII identifiers.
                // It's unclear what the benefit of this would be, exactly, especially as no-one is going to use this language
                _ => return Err(self.error_unimplemented("Unrecognized Character Reached")),
            }
        };

        Ok(Some(token))
    }
}

impl Iterator for Scanner {
    type Item = Result<Token, ScannerError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.scan_token().transpose()
    }
}

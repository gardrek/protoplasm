// protoplasm - configurable assembler designed for hobby CPU architectures

// main.rs

mod arg;
mod assembler;
mod ast;
mod parser;
mod recipe;
mod resolver;
mod scanner;
mod token;

use assembler::Assembler;
use assembler::Environment;
use parser::Parser;
use recipe::Recipe;
use resolver::Resolver;
use scanner::Scanner;

use std::error::Error;

fn main() -> std::result::Result<(), Box<dyn Error>> {
    let clargs = {
        use clap::Parser;
        arg::ArgStruct::parse()
    };

    let (items, resolver) = parse_and_resolve_file(&clargs.source_file)?;

    let recipe = clargs.recipe.map(|path| match Recipe::load(&path) {
        Ok(m) => m,
        Err(e) => {
            let ron::error::SpannedError { code, position } = e.clone();
            eprintln!("Error reading recipe: {code} at line {position}");
            std::process::exit(1)
        }
    });

    match recipe {
        Some(recipe) => {
            let mut assembler = {
                let env = Environment::default();
                Assembler::from_environment(env)
            };

            assembler.resolver = resolver;

            'l: {
                for item in items.iter() {
                    if let Some((address, assembled)) = assembler.assemble_item(&recipe, item)? {
                        println!("0x{:04x}: 0x{:04x}", address, assembled.data);
                    }

                    if !assembler.resolver.errors.is_empty() {
                        for e in assembler.resolver.errors.iter() {
                            eprintln!("{e:?}");
                        }
                        break 'l Err(ResolverError.into());
                    }
                }
                Ok(())
            }
        }
        None => {
            eprintln!("No recipe file given. Nothing to do.");
            Ok(())
        }
    }
}

fn _scan_test(scanner: &mut Scanner) -> Result<(), Box<dyn std::error::Error>> {
    loop {
        match scanner.next() {
            None => break,
            Some(tk) => {
                let tk = tk?;
                print!("[{}]", tk.kind);
                if let token::TokenKind::Eol = tk.kind {
                    println!()
                }
            }
        }
    }
    println!();
    Ok(())
}

fn _parse_test(parser: &mut Parser) -> Result<(), Box<dyn std::error::Error>> {
    loop {
        match parser.next() {
            None => break,
            Some(r) => {
                let stmt = r?;

                eprintln!("{:?}", stmt);
            }
        }
    }

    for e in parser.errors() {
        if let Some(_loc) = &e.location {

            //~ eprint!("error on line {}: ", loc.get_line_number(s));
        }
        eprint!("{}", e);
        eprintln!();
    }

    Ok(())
}

#[derive(Debug)]
struct ResolverError;

impl Error for ResolverError {}

impl core::fmt::Display for ResolverError {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug)]
struct ErrorList(Vec<Box<dyn std::error::Error>>);

impl core::fmt::Display for ErrorList {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        for e in self.0.iter() {
            write!(f, "{:?}", e)?;
        }
        Ok(())
    }
}

impl std::error::Error for ErrorList {}

fn parse_and_resolve_file(
    path: &std::path::PathBuf,
) -> Result<(Vec<ast::Stmt>, Resolver), Box<dyn std::error::Error>> {
    let source_code = {
        use std::io::Read;

        let mut source_code = String::new();
        let mut source_file = std::fs::File::open(path)?;
        source_file.read_to_string(&mut source_code)?;

        source_code
    };

    let mut statements = parse(&source_code)?;

    let resolver = resolve(&mut statements[..])?;

    Ok((statements, resolver))
}

fn parse(source: &str) -> Result<Vec<ast::Stmt>, Box<dyn std::error::Error>> {
    let mut parser = Parser::new(Scanner::new(source));

    let statements = parser.parse_all()?;

    let had_error = !parser.errors().is_empty();

    if had_error {
        for e in parser.errors().iter() {
            /*
            let line = match crate::source::SourceLocation::error_line_number(
                &result::Error::Parser(e.clone()),
                source,
            ) {
                Some(n) => n.to_string(),
                None => "unkown".to_string(),
            };

            eprintln!("error on line {}: {} at `{}`", line, e, e.get_slice(source))
            */

            if let Some(loc) = &e.location {
                eprint!("error on line {}: ", loc.get_line_number(source));
            }

            eprint!("{}", e);
        }

        let errors = parser
            .errors()
            .iter()
            .map(|e| e.clone().into())
            .collect::<Vec<_>>();

        return Err(ErrorList(errors).into());
    }

    Ok(statements)
}

fn resolve(statements: &mut [ast::Stmt]) -> Result<Resolver, Box<dyn std::error::Error>> {
    let mut resolver = resolver::Resolver::default();

    resolver.resolve_all(statements);

    let mut had_error = false;

    for err in resolver.errors.iter() {
        eprintln!("resolver error: {}", err);
        had_error = true;
    }

    for warn in resolver.warnings.iter() {
        eprintln!("resolver warning: {}", warn);
    }

    if had_error {
        let errors = resolver
            .errors
            .into_iter()
            .map(|e| e.clone().into())
            .collect::<Vec<_>>();

        return Err(ErrorList(errors).into());
    }

    Ok(resolver)
}

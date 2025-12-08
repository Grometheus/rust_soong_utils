use std::{
    error::Error,
    fmt::{Debug, Display},
    io,
};
use strum_macros::Display;

#[derive(Display,Debug)]
pub enum ParseErrorType {
    ExpectedCharacter,
    UnExpectedCharacter,
    InvalidInteger,
}

pub struct ParseCtx {
    line: u64,
    col: u64,

    file: String,
}
impl Debug for ParseCtx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ParseCtx")
            .field("line", &self.line)
            .field("col", &self.col)
            .finish()
    }
}

#[derive(Debug)]
pub struct ParseError {
    error_type: ParseErrorType,
    short_desc: String,

    context: Option<ParseCtx>,
}

impl ParseError {
    pub fn from(error_type: ParseErrorType, short_desc: String) -> ParseError {
        ParseError {
            error_type: error_type,
            short_desc: short_desc,
            context: None,
        }
    }

    pub fn with_ctx(mut self, file: String, line: u64, col: u64) -> ParseError {
        ParseError {
            error_type: self.error_type,
            short_desc: self.short_desc,
            context: Some(ParseCtx { line, col, file }),
        }
    }

    pub fn from_ctx(
        error_type: ParseErrorType,
        short_desc: String,
        file: String,
        line: u64,
        col: u64,
    ) -> ParseError {
        ParseError {
            error_type: error_type,
            short_desc: short_desc,
            context: Some(ParseCtx { line, col, file }),
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parse Error ")?;
        if let Some(ctx) = &self.context {
            write!(f, "at {},{}", ctx.line, ctx.col);
        }

        write!(f, " {}: {}", self.error_type, self.short_desc)?;

        if let Some(ctx) = &self.context {
            let line = match ctx.line { 
                0 => ctx.file.lines().next(),
                _ => ctx.file.lines().skip((ctx.line-1) as usize).next()

            };
            match line {
                None => write!(f, "{{Error context was provided, but apears invalid}}")?,
                Some(line) => {
                    let mut spacer = String::with_capacity(ctx.col as usize);
                    for c in line.chars().take(ctx.col as usize) {
                        spacer.push(match c {
                            '\t' => '\t',
                            _ => ' '
                        });
                    }
                    write!(f, "\n{}\n{}^^^\n", line, spacer);
                }            
            }
        }


        Ok(())
    }
}

impl Error for ParseError;

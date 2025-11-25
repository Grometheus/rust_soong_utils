use std::{error::Error, fmt::Debug};

#[derive(Debug)]
pub enum ParseErrorType {
    ExpectedCharactor,
    UnExpectedCharactor,
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
    short_desc: &'static str,

    context: Option<ParseCtx>,
}

impl ParseError {
    pub fn from(error_type: ParseErrorType, short_desc: &'static str) -> ParseError {
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
        short_desc: &'static str,
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

use crate::libsoong::errors::*;
use std::{
    collections::HashMap,
    error::Error,
    iter::{self, Peekable},
    slice::Iter,
    vec::IntoIter,
};

//=============================
//    Tokeniser section
//=============================

struct StrStr<'a> {
    // curr must be contained within origin
    curr: &'a str,
    origin: &'a str,
}

impl<'a> StrStr<'a> {
    fn new(val: &'a str) -> StrStr<'a> {
        StrStr {
            curr: val,
            origin: val,
        }
    }

    fn index(&self) -> usize {
        return unsafe {
            self.curr
                .as_ptr()
                .offset_from_unsigned(self.origin.as_ptr())
        };
    }

    fn consume_while(&mut self, mut predicate: impl FnMut(char) -> bool) -> &'a str {
        let i = self.curr.find(|c| !predicate(c)).unwrap_or(self.curr.len());

        let r = &self.curr[0..i];
        self.curr = &self.curr[i..];
        r
    }

    /// Consumes characters while a sliding window of size `N` matches the predicate.
    ///
    /// # Parameters
    /// - `predicate`: Called with each N-character window. Consumption continues while true.
    /// - `output_offset`: Adjusts the returned slice (e.g., -2 returns fewer chars)
    /// - `advance_by`: How many characters to advance the parser position
    ///
    /// # Returns
    /// `Some(&str)` of consumed text (adjusted by offset), or `None` if initial window fails.
    pub fn consume_while_windowed<const N: usize, F>(
        &mut self,
        mut predicate: F,

        output_offset: isize,
        advance_by: isize,
    ) -> Option<&'a str>
    where
        F: FnMut(&[char; N]) -> bool,
    {
        let mut window: [char; N] = ['\0'; N];
        let mut itr = self.curr.chars();

        for i in 0..N {
            if let Some(c) = itr.next() {
                window[i] = c;
            } else {
                return None;
            }
        }

        if !predicate(&window) {
            return None;
        }

        let mut i: usize = N;

        for c in itr {
            window.rotate_left(1);
            window[N - 1] = c;

            i += 1;
            if !predicate(&window) {
                break;
            }
        }

        let r = &self.curr[..i.saturating_add_signed(output_offset)];
        self.curr = &self.curr[i.saturating_add_signed(advance_by)..];

        Some(r)
    }

    fn skip_ws(&mut self) {
        self.consume_while(|c| c.is_whitespace());
    }

    fn increment(&mut self) -> Option<char> {
        let x = self.curr.chars().next()?;

        self.curr = &self.curr[x.len_utf8()..];

        Some(x)
    }
}

#[derive(Debug)]
enum TokenValue<'a> {
    Whitespace,
    SingleLineComment(&'a str),
    MultilineComment(&'a str),

    SubOp,

    AddOp,
    AddEqOp,

    EqOp,

    OpenCurlyBracket,
    CloseCurlyBracket,

    OpenSquareBracket,
    CloseSquareBracket,

    Colon,
    Comma,

    Number(&'a str), // Todo: What is a number?
    String(&'a str),

    Identifier(&'a str),
}

#[derive(Debug)]
struct Token<'a> {
    value: TokenValue<'a>,

    line: u64,
    col: u64,
}

fn tokenize<'a>(input: &'a str) -> Result<Vec<Token<'a>>, ParseError> {
    // Could be fine-tuned
    let mut tokens: Vec<Token> = Vec::with_capacity(input.len() / 4);

    let mut ss = StrStr::new(input);
    let mut line = 0;
    let mut line_start = 0;

    while !ss.curr.is_empty() {
        let col = (ss.index() - line_start) as u64;

        let value = match ss.curr.chars().next() {
            None => {
                break;
            }
            Some('0'..='9') => TokenValue::Number(ss.consume_while(|c| matches!(c, '0'..='9'))),
            Some('a'..='z' | 'A'..='Z') => {
                TokenValue::Identifier(ss.consume_while(|c| c.is_ascii_alphanumeric()))
            }

            Some('{') => {
                ss.increment();
                TokenValue::OpenCurlyBracket
            }
            Some('}') => {
                ss.increment();
                TokenValue::CloseCurlyBracket
            }

            Some('[') => {
                ss.increment();
                TokenValue::OpenSquareBracket
            }
            Some(']') => {
                ss.increment();
                TokenValue::CloseSquareBracket
            }

            Some(':') => {
                ss.increment();
                TokenValue::Colon
            }
            Some(',') => {
                ss.increment();
                TokenValue::Comma
            }

            Some('+') => {
                ss.increment();
                if matches!(ss.curr.chars().next(), Some('=')) {
                    ss.increment();
                    TokenValue::AddEqOp
                } else {
                    TokenValue::AddOp
                }
            }
            Some('-') => {
                ss.increment();
                TokenValue::SubOp
            }
            Some('=') => {
                ss.increment();
                TokenValue::EqOp
            }

            Some(c) if c.is_ascii_whitespace() => {
                let start_index = ss.index();
                for (i, c) in ss.curr.char_indices() {
                    if !(c.is_ascii_whitespace()) {
                        ss.curr = &ss.curr[i..];
                        break;
                    }
                    if c == '\n' {
                        line += 1;
                        line_start = start_index + i;
                    }
                }
                // Check for an eof, aka where all ws was found whitespace
                if let Some(c) = ss.curr.chars().next() {
                    if c.is_ascii_whitespace() {
                        ss.curr = &ss.curr[ss.curr.len()..];
                    }
                }

                TokenValue::Whitespace
            }

            Some('"') => {
                ss.increment();
                let r = TokenValue::String(
                    ss.consume_while_windowed(
                        |win: &[char; 2]| !(matches!(win, [_, '\"'] if win[0] != '\\')),
                        -1,
                        0,
                    )
                    .unwrap_or(""),
                );

                if (ss.curr.is_empty()) {
                    Err(ParseError::from_ctx(
                        ParseErrorType::ExpectedCharactor,
                        "Could not find end to string",
                        ss.origin.to_string(),
                        line,
                        col,
                    ))?
                }

                r
            }

            Some('/') => {
                ss.increment();

                match ss.increment() {
                    Some('/') => TokenValue::SingleLineComment(ss.consume_while(|c| c != '\n')),
                    Some('*') => {
                        let r = TokenValue::MultilineComment(
                            ss.consume_while_windowed(
                                |win: &[char; 2]| !matches!(win, ['*', '/']),
                                -2,
                                0,
                            )
                            .unwrap_or(""),
                        );
                        if (ss.curr.is_empty()) {
                            Err(ParseError::from_ctx(
                                ParseErrorType::ExpectedCharactor,
                                "Could not find end to comment",
                                ss.origin.to_string(),
                                line,
                                col,
                            ))?
                        }

                        ss.curr = &ss.curr[2..];

                        r
                    }

                    None | Some(_) => Err(ParseError::from_ctx(
                        ParseErrorType::ExpectedCharactor,
                        "Unexpeced charactor after backslash",
                        ss.origin.to_string(),
                        line,
                        col,
                    ))?,
                }
            }

            Some(c) => Err(ParseError::from_ctx(
                ParseErrorType::UnExpectedCharactor,
                "Unknown charactor",
                ss.origin.to_string(),
                line,
                col,
            ))?,
        };

        tokens.push(Token { value, line, col });
    }
    Ok(tokens)
}

#[derive(Debug)]
pub enum BlueprintValue {
    Integer(i64),
    UnknownIdentifer(String),

    // used for unknown identifiers
    Negative(Box<BlueprintValue>),
    String(String),

    Map(Vec<(String, BlueprintValue)>),
    List(Vec<BlueprintValue>),

    Add(Box<BlueprintValue>, Box<BlueprintValue>),
}

type TokenIter<'a, 'b> = &'a mut Peekable<IntoIter<Token<'b>>>;
type TokenIterVal<'a> = Peekable<IntoIter<Token<'a>>>;

fn consume_white(toks: TokenIter) {
    while let Some(Token {
        value:
            TokenValue::Whitespace | TokenValue::SingleLineComment(_) | TokenValue::MultilineComment(_),
        line: _,
        col: _,
    }) = toks.peek()
    {
        let _ = toks.next();
    }
}

macro_rules! unexpected_err {
    ($msg : expr) => {
        Err(ParseError::from(ParseErrorType::UnExpectedCharactor, $msg))?
    };
}
macro_rules! unexpected_eof_err {
    () => {
        unexpected_err!("Unexpected EOF")
    };
}

macro_rules! expected_tok_err {
    ($file_ctx:expr, $msg:expr, $line:expr, $col:expr) => {
        Err(ParseError::from_ctx(
            ParseErrorType::ExpectedCharactor,
            $msg,
            $file_ctx.to_string(),
            $line,
            $col,
        ))?
    };
}

macro_rules! expect_value {
    ($toks:expr,$file_ctx:expr, $token_value:pat, $msg:expr, $usage:expr) => {{
        let tok = $toks.next();
        match tok {
            Some(Token {
                value: $token_value,
                line: _,
                col: _,
            }) => $usage,
            Some(Token {
                value: _,
                line,
                col,
            }) => expected_tok_err!($file_ctx, $msg, line, col),
            None => unexpected_eof_err!(),
        }
    }};
}

fn parse_map(toks: TokenIter, file_ctx: &str) -> Result<Vec<(String, BlueprintValue)>, ParseError> {
    expect_value!(
        toks,
        file_ctx,
        TokenValue::OpenCurlyBracket,
        "Expected: '{'",
        ()
    );

    let mut ret = Vec::new();
    loop {
        consume_white(toks);

        // Edge case: Trailing comma
        if matches!(
            toks.peek(),
            Some(Token {
                value: TokenValue::CloseCurlyBracket,
                line,
                col
            })
        ) {
            break;
        }

        let id = expect_value!(toks, file_ctx, TokenValue::Identifier(id), "Identifier", id);
        consume_white(toks);
        expect_value!(toks, file_ctx, TokenValue::Colon, "':'", ());
        consume_white(toks);
        let val = parse_value(toks, file_ctx)?;

        ret.push((id.to_string(), val));

        consume_white(toks);

        match toks.peek() {
            Some(Token {
                value: TokenValue::Comma,
                line,
                col,
            }) => {
                toks.next();
                continue;
            }
            Some(Token {
                value: TokenValue::CloseCurlyBracket,
                line,
                col,
            }) => {
                break;
            }

            Some(Token {
                value: _,
                line,
                col,
            }) => expected_tok_err!(file_ctx, "Expected: ',' or '}'", *line, *col),
            None => unexpected_eof_err!(),
        }
    }

    consume_white(toks);
    expect_value!(
        toks,
        file_ctx,
        TokenValue::CloseCurlyBracket,
        "Expected: '}'",
        ()
    );

    Ok(ret)
}

fn parse_list(toks: TokenIter, file_ctx: &str) -> Result<Vec<BlueprintValue>, ParseError> {
    expect_value!(
        toks,
        file_ctx,
        TokenValue::OpenSquareBracket,
        "Expected: '['",
        ()
    );
    let mut ret = Vec::new();
    loop {
        consume_white(toks);
        // Edge case: Trailing comma
        if matches!(
            toks.peek(),
            Some(Token {
                value: TokenValue::CloseSquareBracket,
                line,
                col
            })
        ) {
            break;
        }

        ret.push(parse_value(toks, file_ctx)?);

        consume_white(toks);

        match toks.peek() {
            Some(Token {
                value: TokenValue::Comma,
                line,
                col,
            }) => {
                toks.next();
                continue;
            }
            Some(Token {
                value: TokenValue::CloseSquareBracket,
                line,
                col,
            }) => {
                break;
            }

            Some(Token {
                value: _,
                line,
                col,
            }) => expected_tok_err!(file_ctx, "Expected: ',' or ']'", *line, *col),
            None => unexpected_eof_err!(),
        }
    }

    expect_value!(
        toks,
        file_ctx,
        TokenValue::CloseSquareBracket,
        "Expected: ']'",
        ()
    );
    Ok(ret)
}

fn parse_value(toks: TokenIter, file_ctx: &str) -> Result<BlueprintValue, ParseError> {
    if let None = toks.peek() {
        panic!("Violation: Must not be at eof")
    }

    // Do one first attempt at parsing
    let tok = toks.peek().unwrap();
    let first = match tok.value {
        TokenValue::Whitespace
        | TokenValue::SingleLineComment(_)
        | TokenValue::MultilineComment(_) => panic!("Violation: Must start with non-ws"),

        TokenValue::CloseSquareBracket | TokenValue::CloseCurlyBracket => {
            unexpected_err!("Stray bracket")
        }

        TokenValue::Colon => unexpected_err!("Stray colon"),
        TokenValue::Comma => unexpected_err!("Stray comma"),
        TokenValue::EqOp => unexpected_err!("Stray equals sign"),
        TokenValue::AddEqOp => unexpected_err!("Stray add-equals sign"),

        TokenValue::Number(num_str) => {
            if let Ok(int) = num_str.parse() {
                // Safe to ignore
                let _ = toks.next();

                BlueprintValue::Integer(int)
            } else {
                Err(ParseError::from_ctx(
                    ParseErrorType::InvalidInteger,
                    "Not a valid integer",
                    file_ctx.to_string(),
                    tok.line,
                    tok.col,
                ))?
            }
        }

        TokenValue::Identifier(id) => {
            let _ = toks.next();
            BlueprintValue::UnknownIdentifer(id.to_string())
        }
        TokenValue::OpenCurlyBracket => BlueprintValue::Map(parse_map(toks, file_ctx)?),
        TokenValue::OpenSquareBracket => BlueprintValue::List(parse_list(toks, file_ctx)?),

        TokenValue::String(str) => {
            let _ = toks.next();

            BlueprintValue::String(str.to_string())
        }

        TokenValue::AddOp => Err(ParseError::from_ctx(
            ParseErrorType::UnExpectedCharactor,
            "A plus sign must follow a value",
            file_ctx.to_string(),
            tok.line,
            tok.col,
        ))?,
        TokenValue::SubOp => {
            let operator_line = tok.line;
            let operator_col = tok.col;

            // Safe to ignore
            let _ = toks.next();

            // Technically allowed by soong
            consume_white(toks);

            let (next_line, next_col) = match toks.peek() {
                Some(tok) => (tok.line, tok.col),
                None => expected_tok_err!(
                    file_ctx,
                    "Minus sign must be followed with something",
                    operator_line,
                    operator_col
                ),
            };

            let val = parse_value(toks, file_ctx)?;
            match val {
                BlueprintValue::Integer(i) => BlueprintValue::Integer(-i),
                BlueprintValue::UnknownIdentifer(_) => BlueprintValue::Negative(Box::from(val)),
                _ => expected_tok_err!(
                    file_ctx,
                    "Minus sign must be followed by an integer",
                    next_col,
                    next_col
                ),
            }
        }

    };

    // Afterwards see if there is a plus sign
    consume_white(toks);
    Ok(match toks.peek() {
        Some(Token {
            value: TokenValue::AddOp,
            line,
            col,
        }) => {
            // Safe due to peek check
            let _ = toks.next();
            consume_white(toks);

            BlueprintValue::Add(Box::from(first), Box::from(parse_value(toks, file_ctx)?))
        }
        _ => first,
    })
}

//=============================
//    AST section
//=============================

#[derive(Debug)]
pub enum ASTLine {
    VarSet(String, BlueprintValue),

    VarAddSet(String, BlueprintValue),

    Rule(String, Vec<(String, BlueprintValue)>),
}
fn parse_ast_line(toks: TokenIter, file_ctx: &str) -> Result<ASTLine, ParseError> {
    let id = expect_value!(
        toks,
        file_ctx,
        TokenValue::Identifier(id),
        "Expected: Identifier",
        id
    );

    consume_white(toks);

    Ok(match toks.peek() {
        None => unexpected_eof_err!(),
        Some(Token {
            value: TokenValue::EqOp,
            line,
            col,
        }) => {
            toks.next();
            consume_white(toks);

            ASTLine::VarSet(id.to_string(), parse_value(toks, file_ctx)?)
        },
        Some(Token {
            value: TokenValue::AddEqOp,
            line,
            col,
        }) => {
            toks.next();
            consume_white(toks);

            ASTLine::VarAddSet(id.to_string(), parse_value(toks, file_ctx)?)
        },
        Some(Token {
            value: TokenValue::OpenCurlyBracket,
            line,
            col,
        }) => {
            consume_white(toks);


            ASTLine::Rule(id.to_string(), match parse_value(toks, file_ctx)?{
                BlueprintValue::Map(map) => map,
                _ => unreachable!()
            })
        },
        Some(Token {
            value: _,
            line,
            col,
        }) => expected_tok_err!(
            file_ctx,
            "An = or a '{' must follow an identifier",
            *line,
            *col
        ),

        _ => todo!(),
    })

}

pub struct ASTGenerator<'a> {
    token_iter: TokenIterVal<'a>,
    file: &'a str,
}

impl<'a> ASTGenerator<'a> {
    pub fn from(file: &'a str) -> Result<ASTGenerator<'a>, ParseError> {
        Ok(ASTGenerator {
            token_iter: tokenize(file)?.into_iter().peekable(),
            file,
        })
    }
}

impl<'a> Iterator for ASTGenerator<'a> {
    type Item = Result<ASTLine, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        consume_white(&mut self.token_iter);

        let _ = self.token_iter.peek()?;

        Some(parse_ast_line(&mut self.token_iter, &self.file))
    }
}

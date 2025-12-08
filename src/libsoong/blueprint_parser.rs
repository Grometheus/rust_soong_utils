use crate::libsoong::errors::*;
use std::{
    collections::HashMap,
    error::Error,
    iter::{self, Peekable},
    panic,
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

    line: u64,
    col: u64,
}

impl<'a> StrStr<'a> {
    fn new(val: &'a str) -> StrStr<'a> {
        StrStr {
            curr: val,
            origin: val,

            line: 1,
            col: 1,
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

        for c in r.chars() {
            if c != '\n' {
                continue;
            }
            self.line += 1;
            self.col = 1;
        }
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
    /// `Some(&str)` of consumed text (adjusted by offset), or `None` if a value not matching the
    /// predicate is not found
    fn consume_while_windowed<const N: usize, F>(
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
                return Some("");
            }
        }

        let mut i: usize = 0;
        let mut did_break = false;

        for c in &mut itr {
            if !predicate(&window) {
                did_break = true;
                break;
            }

            i += 1;
            window.rotate_left(1);
            window[N - 1] = c;
        }

        if !did_break && predicate(&window) {
            return None;
        }

        let output_end_idx = i.saturating_add_signed(output_offset);
        let input_end_idx = i.saturating_add_signed(advance_by);

        for c in self.curr[..input_end_idx].chars() {
            if c != '\n' {
                continue;
            }

            self.line += 1;
            self.col = 1;
        }

        let r = &self.curr[..output_end_idx];
        self.curr = &self.curr[input_end_idx..];

        Some(r)
    }

    fn skip_ws(&mut self) {
        self.consume_while(|c| c.is_whitespace());
    }

    fn increment(&mut self) -> Option<char> {
        let x = self.curr.chars().next()?;
        if x == '\n' {
            self.line += 1;
            self.col = 1;
        }

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

    while !ss.curr.is_empty() {
        let pre_line = ss.line;
        let pre_col = ss.col;
        let value = match ss.curr.chars().next() {
            None => {
                break;
            }
            Some('0'..='9') => TokenValue::Number(ss.consume_while(|c| matches!(c, '0'..='9'))),
            Some('a'..='z' | 'A'..='Z' | '_') => {
                TokenValue::Identifier(ss.consume_while(|c| c == '_' || c.is_ascii_alphanumeric()))
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
                ss.skip_ws();

                TokenValue::Whitespace
            }
            Some('"') => {
                ss.increment();
                TokenValue::String(
                    match ss.consume_while_windowed(|win: &[char; 2]| win[0] == '\\' || win[1] != '"', 1, 2) {
                        Some(x) => x,
                        None => Err(ParseError::from_ctx(
                            ParseErrorType::ExpectedCharacter,
                            "Could not find end to string".to_string(),
                            ss.origin.to_string(),
                            pre_line,
                            pre_col,
                        ))?,
                    },
                )
            }
            Some('/') => {
                ss.increment();

                match ss.increment() {
                    Some('/') => TokenValue::SingleLineComment(ss.consume_while(|c| c != '\n')),
                    Some('*') => TokenValue::MultilineComment(
                        match ss.consume_while_windowed(
                            |win: &[char; 2]| !matches!(win, ['*', '/']),
                            0,
                            2,
                        ) {
                            Some(x) => x,
                            None => Err(ParseError::from_ctx(
                                ParseErrorType::ExpectedCharacter,
                                "Could not find end to comment".to_string(),
                                ss.origin.to_string(),
                                pre_line,
                                pre_col,
                            ))?,
                        },
                    ),

                    None | Some(_) => Err(ParseError::from_ctx(
                        ParseErrorType::ExpectedCharacter,
                        "Unexpeced character after backslash".to_string(),
                        ss.origin.to_string(),
                        pre_line,
                        pre_col,
                    ))?,
                }
            }

            Some(c) => {
                dbg!(ss.curr);
                Err(ParseError::from_ctx(
                    ParseErrorType::UnExpectedCharacter,
                    format!("Unknown character during tokenization: '{}'", c),
                    ss.origin.to_string(),
                    pre_line,
                    pre_col,
                ))?
            }
        };

        tokens.push(Token {
            value,
            line: pre_line,
            col: pre_col,
        });
    }
    Ok(tokens)
}

#[derive(Debug, Clone)]
pub enum BlueprintValue {
    Integer(i64),
    UnknownIdentifer(String),

    // used for unknown identifiers
    Negative(Box<BlueprintValue>),
    String(String),

    Map(HashMap<String, BlueprintValue>),
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
        Err(ParseError::from(ParseErrorType::UnExpectedCharacter, $msg))?
    };
}
macro_rules! unexpected_eof_err {
    () => {
        unexpected_err!("Unexpected EOF".to_string())
    };
}

macro_rules! expected_tok_err {
    ($file_ctx:expr, $msg:expr, $line:expr, $col:expr) => {
        Err(ParseError::from_ctx(
            ParseErrorType::ExpectedCharacter,
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

fn parse_map(toks: TokenIter, file_ctx: &str) -> Result<HashMap<String, BlueprintValue>, ParseError> {
    expect_value!(
        toks,
        file_ctx,
        TokenValue::OpenCurlyBracket,
        "Expected: '{'".to_string(),
        ()
    );

    let mut ret = HashMap::new();
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

        let id = expect_value!(toks, file_ctx, TokenValue::Identifier(id), "Identifier".to_string(), id);
        consume_white(toks);
        expect_value!(toks, file_ctx, TokenValue::Colon, "':'".to_string(), ());
        consume_white(toks);
        let val = parse_value(toks, file_ctx)?;

        ret.insert(id.to_string(), val);

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
            }) => expected_tok_err!(file_ctx, "Expected: ',' or '}'".to_string(), *line, *col),
            None => unexpected_eof_err!(),
        }
    }

    consume_white(toks);
    expect_value!(
        toks,
        file_ctx,
        TokenValue::CloseCurlyBracket,
        "Expected: '}'".to_string(),
        ()
    );

    Ok(ret)
}

fn parse_list(toks: TokenIter, file_ctx: &str) -> Result<Vec<BlueprintValue>, ParseError> {
    expect_value!(
        toks,
        file_ctx,
        TokenValue::OpenSquareBracket,
        "Expected: '['".to_lowercase(),
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
                value: tok,
                line,
                col,
            }) => expected_tok_err!(file_ctx, format!("Expected: ',' or ']', got {:?}", *tok), *line, *col),
            None => unexpected_eof_err!(),
        }
    }

    expect_value!(
        toks,
        file_ctx,
        TokenValue::CloseSquareBracket,
        "Expected: ']'".to_string(),
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
            unexpected_err!("Stray bracket".to_string())
        }

        TokenValue::Colon => unexpected_err!("Stray colon".to_string()),
        TokenValue::Comma => unexpected_err!("Stray comma".to_string()),
        TokenValue::EqOp => unexpected_err!("Stray equals sign".to_string()),
        TokenValue::AddEqOp => unexpected_err!("Stray add-equals sign".to_string()),

        TokenValue::Number(num_str) => {
            if let Ok(int) = num_str.parse() {
                // Safe to ignore
                let _ = toks.next();

                BlueprintValue::Integer(int)
            } else {
                Err(ParseError::from_ctx(
                    ParseErrorType::InvalidInteger,
                    "Not a valid integer".to_string(),
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
            ParseErrorType::UnExpectedCharacter,
            "A plus sign must follow a value".to_string(),
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
                    "Minus sign must be followed with something".to_string(),
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
                    "Minus sign must be followed by an integer".to_string(),
                    next_line,
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

    Rule(String, HashMap<String, BlueprintValue>),
}
fn parse_ast_line(toks: TokenIter, file_ctx: &str) -> Result<ASTLine, ParseError> {
    let id = expect_value!(
        toks,
        file_ctx,
        TokenValue::Identifier(id),
        "Expected: Identifier".to_string(),
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
        }
        Some(Token {
            value: TokenValue::AddEqOp,
            line,
            col,
        }) => {
            toks.next();
            consume_white(toks);

            ASTLine::VarAddSet(id.to_string(), parse_value(toks, file_ctx)?)
        }
        Some(Token {
            value: TokenValue::OpenCurlyBracket,
            line,
            col,
        }) => {
            consume_white(toks);

            ASTLine::Rule(
                id.to_string(),
                match parse_value(toks, file_ctx)? {
                    BlueprintValue::Map(map) => map,
                    _ => unreachable!(),
                },
            )
        }
        Some(Token {
            value: _,
            line,
            col,
        }) => expected_tok_err!(
            file_ctx,
            "An = or a '{' must follow an identifier".to_string(),
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

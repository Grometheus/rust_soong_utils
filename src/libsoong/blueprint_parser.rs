use crate::libsoong::errors::*;
use std::{error::Error, iter::Peekable};

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

enum TokenValue<'a> {
    EOF,

    Whitespace,
    SingleLineComment(&'a str),
    MultilineComment(&'a str),

    Operator(char),

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

            Some(c) if matches!(c, '+') => TokenValue::Operator(c),

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
                // Safe to ignore
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
    tokens.push(Token {
        value: TokenValue::EOF,
        line: line,
        col: (ss.index() - line_start) as u64,
    });
    Ok(tokens)
}











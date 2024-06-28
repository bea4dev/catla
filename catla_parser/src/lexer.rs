use std::ops::Range;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Function,
    Static,
    Private,
    Suspend,
    Native,
    Acyclic,
    Open,
    New,
    Drop,
    Mutex,
    ParenthesisLeft,
    ParenthesisRight,
    BraceLeft,
    BraceRight,
    BracketLeft,
    BracketRight,
    Class,
    Struct,
    Interface,
    Type,
    Implements,
    For,
    Where,
    Import,
    DoubleColon,
    Comma,
    Loop,
    While,
    Var,
    Let,
    This,
    LargeThis,
    Equal,
    Exchange,
    Or,
    And,
    EqEqual,
    NotEqual,
    GreaterThan,
    GreaterOrEq,
    LessThan,
    LessOrEq,
    Plus,
    Minus,
    Star,
    Slash,
    VerticalBar,
    Dot,
    Null,
    InterrogationMark,
    ExclamationMark,
    InterrogationElvis,
    ExclamationElvis,
    Colon,
    If,
    Else,
    Return,
    True,
    False,
    FatArrow,
    ThinArrow,
    Literal,
    Semicolon,
    LineFeed,
    Whitespace,
    UnexpectedCharacter,
    None
}

enum Tokenizer {
    Keyword(TokenKind, &'static str),
    Functional(TokenKind, fn(current_input: &str) -> usize)
}

impl Tokenizer {
    fn tokenize(&self, current_input: &str) -> (TokenKind, usize) {
        return match self {
            Tokenizer::Keyword(kind, keyword) => {
                let mut input_chars = current_input.chars();
                let mut keyword_chars = keyword.chars();
                let mut current_byte_length = 0;
                loop {
                    let keyword_char = match keyword_chars.next(){
                        Some(c) => c,
                        _ => break
                    };
                    let current_char = match input_chars.next(){
                        Some(c) => c,
                        _ => return (kind.clone(), 0) // reject
                    };
                    if current_char != keyword_char {
                        return (kind.clone(), 0) // reject
                    }

                    current_byte_length += current_char.len_utf8();
                }
                (kind.clone(), current_byte_length) // accept
            },
            Tokenizer::Functional(kind, tokenizer) => {
                (kind.clone(), tokenizer(current_input))
            }
        }
    }
}

static TOKENIZERS: &[Tokenizer] = &[
    Tokenizer::Keyword(TokenKind::Function, "function"),
    Tokenizer::Keyword(TokenKind::Static, "static"),
    Tokenizer::Keyword(TokenKind::Private, "private"),
    Tokenizer::Keyword(TokenKind::Suspend, "suspend"),
    Tokenizer::Keyword(TokenKind::Native, "native"),
    Tokenizer::Keyword(TokenKind::Acyclic, "acyclic"),
    Tokenizer::Keyword(TokenKind::Open, "open"),
    Tokenizer::Keyword(TokenKind::New, "new"),
    Tokenizer::Keyword(TokenKind::Drop, "drop"),
    Tokenizer::Keyword(TokenKind::Mutex, "mutex"),
    Tokenizer::Keyword(TokenKind::ParenthesisLeft, "("),
    Tokenizer::Keyword(TokenKind::ParenthesisRight, ")"),
    Tokenizer::Keyword(TokenKind::BraceLeft, "{"),
    Tokenizer::Keyword(TokenKind::BraceRight, "}"),
    Tokenizer::Keyword(TokenKind::BracketLeft, "["),
    Tokenizer::Keyword(TokenKind::BracketRight, "]"),
    Tokenizer::Keyword(TokenKind::Class, "class"),
    Tokenizer::Keyword(TokenKind::Struct, "struct"),
    Tokenizer::Keyword(TokenKind::Interface, "interface"),
    Tokenizer::Keyword(TokenKind::Type, "type"),
    Tokenizer::Keyword(TokenKind::Implements, "implements"),
    Tokenizer::Keyword(TokenKind::For, "for"),
    Tokenizer::Keyword(TokenKind::Where, "where"),
    Tokenizer::Keyword(TokenKind::Import, "import"),
    Tokenizer::Keyword(TokenKind::DoubleColon, "::"),
    Tokenizer::Keyword(TokenKind::Comma, ","),
    Tokenizer::Keyword(TokenKind::Loop, "loop"),
    Tokenizer::Keyword(TokenKind::Var, "var"),
    Tokenizer::Keyword(TokenKind::Let, "let"),
    Tokenizer::Keyword(TokenKind::This, "this"),
    Tokenizer::Keyword(TokenKind::LargeThis, "This"),
    Tokenizer::Keyword(TokenKind::Equal, "="),
    Tokenizer::Keyword(TokenKind::Exchange, "<=>"),
    Tokenizer::Keyword(TokenKind::Or, "or"),
    Tokenizer::Keyword(TokenKind::And, "and"),
    Tokenizer::Keyword(TokenKind::EqEqual, "=="),
    Tokenizer::Keyword(TokenKind::NotEqual, "=/"),
    Tokenizer::Keyword(TokenKind::GreaterThan, ">"),
    Tokenizer::Keyword(TokenKind::GreaterOrEq, ">="),
    Tokenizer::Keyword(TokenKind::LessThan, "<"),
    Tokenizer::Keyword(TokenKind::LessOrEq, "<="),
    Tokenizer::Keyword(TokenKind::Plus, "+"),
    Tokenizer::Keyword(TokenKind::Minus, "-"),
    Tokenizer::Keyword(TokenKind::Star, "*"),
    Tokenizer::Keyword(TokenKind::Slash, "/"),
    Tokenizer::Keyword(TokenKind::VerticalBar, "|"),
    Tokenizer::Keyword(TokenKind::Dot, "."),
    Tokenizer::Keyword(TokenKind::Null, "null"),
    Tokenizer::Keyword(TokenKind::InterrogationMark, "?"),
    Tokenizer::Keyword(TokenKind::ExclamationMark, "!"),
    Tokenizer::Keyword(TokenKind::InterrogationElvis, "?:"),
    Tokenizer::Keyword(TokenKind::ExclamationElvis, "!:"),
    Tokenizer::Keyword(TokenKind::Colon, ":"),
    Tokenizer::Keyword(TokenKind::If, "if"),
    Tokenizer::Keyword(TokenKind::Else, "else"),
    Tokenizer::Keyword(TokenKind::Return, "return"),
    Tokenizer::Keyword(TokenKind::True, "true"),
    Tokenizer::Keyword(TokenKind::False, "false"),
    Tokenizer::Keyword(TokenKind::FatArrow, "=>"),
    Tokenizer::Keyword(TokenKind::ThinArrow, "->"),
    Tokenizer::Functional(TokenKind::Literal, literal_tokenizer),
    Tokenizer::Keyword(TokenKind::Semicolon, ";"),
    Tokenizer::Keyword(TokenKind::LineFeed, "\r"),
    Tokenizer::Keyword(TokenKind::LineFeed, "\n"),
    Tokenizer::Keyword(TokenKind::LineFeed, "\r\n"),
    Tokenizer::Functional(TokenKind::Whitespace, whitespace_tokenizer)
];

fn literal_tokenizer(current_input: &str) -> usize {
    let mut input_chars = current_input.chars();
    let mut current_byte_length = 0;
    let mut is_all_char_numeric = true;
    loop {
        let current_char = match input_chars.next() {
            Some(c) => c,
            _ => break
        };

        if !(current_char == '_' || current_char.is_alphanumeric()) {
            // for float value
            if is_all_char_numeric && current_char == '.' {
                match input_chars.next() {
                    Some(next_char) => {
                        if next_char.is_numeric() {
                            current_byte_length += current_char.len_utf8();
                            current_byte_length += next_char.len_utf8();
                            continue;
                        } else {
                            break;
                        }
                    },
                    _ => break
                }
            } else {
                break;
            }
        }

        if !current_char.is_numeric() {
            is_all_char_numeric = false;
        }

        current_byte_length += current_char.len_utf8();
    }

    current_byte_length
}

fn whitespace_tokenizer(current_input: &str) -> usize {
    let mut input_chars = current_input.chars();
    let mut current_byte_length = 0;
    loop {
        let current_char = match input_chars.next() {
            Some(c) => c,
            _ => break
        };
        if !(current_char != '\n' && current_char != '\r' && current_char.is_whitespace()) {
            break;
        }
        current_byte_length += current_char.len_utf8();
    }

    current_byte_length
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token<'input> {
    pub kind: TokenKind,
    pub text: &'input str,
    pub span: Range<usize>,
}

pub struct Lexer<'input> {
    source: &'input str,
    current_byte_position: usize
}

impl<'input> Lexer<'input> {
    pub fn new(source: &'input str) -> Lexer<'input> {
        return Self {
            source,
            current_byte_position: 0
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Token<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.current_byte_position == self.source.len() {
                return None;
            }

            let current_input = &self.source[self.current_byte_position..self.source.len()];

            let mut current_max_length = 0;
            let mut current_token_kind = TokenKind::Whitespace;
            for tokenizer in TOKENIZERS.iter() {
                let result = tokenizer.tokenize(current_input);
                let token_kind = result.0;
                let byte_length = result.1;
                
                if byte_length > current_max_length {
                    current_max_length = byte_length;
                    current_token_kind = token_kind;
                }
            }

            let start_position = self.current_byte_position;

            let token = if current_max_length == 0 {
                self.current_byte_position += 1;
                let end_position = start_position + 1;
                Token {
                    kind: TokenKind::UnexpectedCharacter,
                    text: &self.source[start_position..end_position],
                    span: start_position..end_position,
                }
            } else {
                self.current_byte_position += current_max_length;

                if current_token_kind == TokenKind::Whitespace {
                    continue;
                }

                let end_position = self.current_byte_position;
                Token {
                    kind: current_token_kind,
                    text: &self.source[start_position..end_position],
                    span: start_position..end_position,
                }
            };
            
            return Some(token);
        }
    }
}
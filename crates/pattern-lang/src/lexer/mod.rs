// Hand-written lexer for the pattern language

pub mod token;

use crate::error::LexError;
use crate::name::StringInterner;
use crate::span::{SourceId, Span};
use token::{Token, TokenKind};

/// Lexer that converts source text into tokens
pub struct Lexer<'a> {
    source: &'a str,
    bytes: &'a [u8],
    pos: usize,
    source_id: SourceId,
    errors: Vec<LexError>,
    interner: &'a mut StringInterner,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str, source_id: SourceId, interner: &'a mut StringInterner) -> Self {
        Self {
            source,
            bytes: source.as_bytes(),
            pos: 0,
            source_id,
            errors: Vec::new(),
            interner,
        }
    }

    /// Tokenize the entire source, returning tokens and any errors
    pub fn tokenize(mut self) -> (Vec<Token>, Vec<LexError>) {
        let mut tokens = Vec::new();
        loop {
            self.skip_whitespace_and_comments();
            if self.pos >= self.bytes.len() {
                tokens.push(Token::new(
                    TokenKind::Eof,
                    Span::new(self.pos as u32, self.pos as u32, self.source_id),
                ));
                break;
            }
            match self.next_token() {
                Some(token) => tokens.push(token),
                None => {
                    // Skip the unrecognized character
                    let start = self.pos;
                    self.pos += 1;
                    self.errors.push(LexError::new(
                        format!(
                            "unexpected character '{}'",
                            self.source[start..start + 1].chars().next().unwrap_or('?')
                        ),
                        Span::new(start as u32, self.pos as u32, self.source_id),
                    ));
                }
            }
        }
        (tokens, self.errors)
    }

    fn peek(&self) -> Option<u8> {
        self.bytes.get(self.pos).copied()
    }

    fn peek_at(&self, offset: usize) -> Option<u8> {
        self.bytes.get(self.pos + offset).copied()
    }

    fn advance(&mut self) -> Option<u8> {
        let b = self.bytes.get(self.pos).copied();
        if b.is_some() {
            self.pos += 1;
        }
        b
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            // Skip whitespace
            while self.pos < self.bytes.len() && self.bytes[self.pos].is_ascii_whitespace() {
                self.pos += 1;
            }

            // Skip line comments
            if self.pos + 1 < self.bytes.len()
                && self.bytes[self.pos] == b'/'
                && self.bytes[self.pos + 1] == b'/'
            {
                self.pos += 2;
                while self.pos < self.bytes.len() && self.bytes[self.pos] != b'\n' {
                    self.pos += 1;
                }
                continue;
            }

            // Skip block comments
            if self.pos + 1 < self.bytes.len()
                && self.bytes[self.pos] == b'/'
                && self.bytes[self.pos + 1] == b'*'
            {
                let start = self.pos;
                self.pos += 2;
                let mut depth = 1;
                while self.pos + 1 < self.bytes.len() && depth > 0 {
                    if self.bytes[self.pos] == b'/' && self.bytes[self.pos + 1] == b'*' {
                        depth += 1;
                        self.pos += 2;
                    } else if self.bytes[self.pos] == b'*' && self.bytes[self.pos + 1] == b'/' {
                        depth -= 1;
                        self.pos += 2;
                    } else {
                        self.pos += 1;
                    }
                }
                if depth > 0 {
                    self.errors.push(LexError::new(
                        "unterminated block comment",
                        Span::new(start as u32, self.pos as u32, self.source_id),
                    ));
                }
                continue;
            }

            break;
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        let start = self.pos;
        let b = self.peek()?;

        // Number literals
        if b.is_ascii_digit() {
            return Some(self.lex_number(start));
        }

        // String literals
        if b == b'"' {
            return Some(self.lex_string(start));
        }

        // Character literals
        if b == b'\'' {
            return Some(self.lex_char(start));
        }

        // Identifiers and keywords
        if b.is_ascii_alphabetic() || b == b'_' {
            return Some(self.lex_ident(start));
        }

        // Preprocessor hash
        if b == b'#' {
            self.advance();
            return Some(Token::new(
                TokenKind::Hash,
                Span::new(start as u32, self.pos as u32, self.source_id),
            ));
        }

        // Operators and punctuation
        self.lex_operator(start)
    }

    fn lex_number(&mut self, start: usize) -> Token {
        // Check for 0x, 0o, 0b prefixes
        if self.bytes[self.pos] == b'0' && self.pos + 1 < self.bytes.len() {
            match self.bytes[self.pos + 1] {
                b'x' | b'X' => return self.lex_hex_number(start),
                b'o' | b'O' => return self.lex_octal_number(start),
                b'b' | b'B' => {
                    // Disambiguate: 0b followed by 0 or 1 is binary,
                    // otherwise it's just 0 followed by ident 'b...'
                    if self.pos + 2 < self.bytes.len()
                        && (self.bytes[self.pos + 2] == b'0' || self.bytes[self.pos + 2] == b'1')
                    {
                        return self.lex_binary_number(start);
                    }
                }
                _ => {}
            }
        }

        // Decimal number (possibly float)
        self.lex_decimal_number(start)
    }

    /// Lex a number literal with a given radix (2, 8, or 16).
    /// `start` is the position of '0', prefix (0x/0o/0b) has NOT been skipped yet.
    fn lex_radix_number(
        &mut self,
        start: usize,
        radix: u32,
        is_valid_digit: fn(u8) -> bool,
        prefix_name: &str,
    ) -> Token {
        self.pos += 2; // skip prefix (0x, 0o, 0b)
        let digit_start = self.pos;
        while self.pos < self.bytes.len() {
            if is_valid_digit(self.bytes[self.pos]) || self.bytes[self.pos] == b'_' {
                self.pos += 1;
            } else if self.bytes[self.pos] == b'\''
                && self.pos + 1 < self.bytes.len()
                && is_valid_digit(self.bytes[self.pos + 1])
            {
                // C++ style digit separator
                self.pos += 1;
            } else {
                break;
            }
        }
        let digits: String = self.source[digit_start..self.pos]
            .chars()
            .filter(|&c| c != '_' && c != '\'')
            .collect();
        let span = Span::new(start as u32, self.pos as u32, self.source_id);
        if digits.is_empty() {
            self.errors.push(LexError::new(
                format!(
                    "expected {} digits after '{}'",
                    prefix_name,
                    &self.source[start..digit_start]
                ),
                span,
            ));
            return Token::new(TokenKind::IntLiteral(0), span);
        }
        match u128::from_str_radix(&digits, radix) {
            Ok(val) => Token::new(TokenKind::IntLiteral(val), span),
            Err(_) => {
                self.errors.push(LexError::new(
                    format!("{} literal overflow", prefix_name),
                    span,
                ));
                Token::new(TokenKind::IntLiteral(0), span)
            }
        }
    }

    fn lex_hex_number(&mut self, start: usize) -> Token {
        self.lex_radix_number(start, 16, |b| b.is_ascii_hexdigit(), "hex")
    }

    fn lex_octal_number(&mut self, start: usize) -> Token {
        self.lex_radix_number(start, 8, |b| b >= b'0' && b <= b'7', "octal")
    }

    fn lex_binary_number(&mut self, start: usize) -> Token {
        self.lex_radix_number(start, 2, |b| b == b'0' || b == b'1', "binary")
    }

    fn lex_decimal_number(&mut self, start: usize) -> Token {
        // Consume integer part
        while self.pos < self.bytes.len() {
            if self.bytes[self.pos].is_ascii_digit() || self.bytes[self.pos] == b'_' {
                self.pos += 1;
            } else if self.bytes[self.pos] == b'\''
                && self.pos + 1 < self.bytes.len()
                && self.bytes[self.pos + 1].is_ascii_digit()
            {
                self.pos += 1;
            } else {
                break;
            }
        }

        // Check for decimal point (but not '..' range operator)
        let is_float = self.pos < self.bytes.len()
            && self.bytes[self.pos] == b'.'
            && self.peek_at(1) != Some(b'.');

        if is_float {
            // Float: also need a digit after '.' to be a float
            if self.peek_at(1).is_some_and(|b| b.is_ascii_digit()) {
                self.pos += 1; // skip '.'
                while self.pos < self.bytes.len()
                    && (self.bytes[self.pos].is_ascii_digit() || self.bytes[self.pos] == b'_')
                {
                    self.pos += 1;
                }
                // Exponent part
                if self.pos < self.bytes.len()
                    && (self.bytes[self.pos] == b'e' || self.bytes[self.pos] == b'E')
                {
                    self.pos += 1;
                    if self.pos < self.bytes.len()
                        && (self.bytes[self.pos] == b'+' || self.bytes[self.pos] == b'-')
                    {
                        self.pos += 1;
                    }
                    while self.pos < self.bytes.len() && self.bytes[self.pos].is_ascii_digit() {
                        self.pos += 1;
                    }
                }
                // Suffix 'F' or 'D' (optional)
                if self.pos < self.bytes.len()
                    && (self.bytes[self.pos] == b'F'
                        || self.bytes[self.pos] == b'f'
                        || self.bytes[self.pos] == b'D'
                        || self.bytes[self.pos] == b'd')
                {
                    self.pos += 1;
                }

                let text: String = self.source[start..self.pos]
                    .chars()
                    .filter(|&c| c != '_' && c != '\'')
                    .collect();
                // Strip trailing suffix
                let text = text.trim_end_matches(['F', 'f', 'D', 'd']);
                match text.parse::<f64>() {
                    Ok(val) => Token::new(
                        TokenKind::FloatLiteral(val),
                        Span::new(start as u32, self.pos as u32, self.source_id),
                    ),
                    Err(_) => {
                        self.errors.push(LexError::new(
                            "invalid float literal",
                            Span::new(start as u32, self.pos as u32, self.source_id),
                        ));
                        Token::new(
                            TokenKind::FloatLiteral(0.0),
                            Span::new(start as u32, self.pos as u32, self.source_id),
                        )
                    }
                }
            } else {
                // Just an integer followed by '.' (member access)
                let digits: String = self.source[start..self.pos]
                    .chars()
                    .filter(|&c| c != '_' && c != '\'')
                    .collect();
                let val = digits.parse::<u128>().unwrap_or_else(|_| {
                    self.errors.push(LexError::new(
                        "integer literal overflow",
                        Span::new(start as u32, self.pos as u32, self.source_id),
                    ));
                    0
                });
                Token::new(
                    TokenKind::IntLiteral(val),
                    Span::new(start as u32, self.pos as u32, self.source_id),
                )
            }
        } else {
            // Check for exponent without decimal point (e.g., 1e10)
            if self.pos < self.bytes.len()
                && (self.bytes[self.pos] == b'e' || self.bytes[self.pos] == b'E')
                && self
                    .peek_at(1)
                    .is_some_and(|b| b.is_ascii_digit() || b == b'+' || b == b'-')
            {
                self.pos += 1; // skip 'e'
                if self.pos < self.bytes.len()
                    && (self.bytes[self.pos] == b'+' || self.bytes[self.pos] == b'-')
                {
                    self.pos += 1;
                }
                while self.pos < self.bytes.len() && self.bytes[self.pos].is_ascii_digit() {
                    self.pos += 1;
                }
                let text: String = self.source[start..self.pos]
                    .chars()
                    .filter(|&c| c != '_' && c != '\'')
                    .collect();
                match text.parse::<f64>() {
                    Ok(val) => Token::new(
                        TokenKind::FloatLiteral(val),
                        Span::new(start as u32, self.pos as u32, self.source_id),
                    ),
                    Err(_) => {
                        self.errors.push(LexError::new(
                            "invalid float literal",
                            Span::new(start as u32, self.pos as u32, self.source_id),
                        ));
                        Token::new(
                            TokenKind::FloatLiteral(0.0),
                            Span::new(start as u32, self.pos as u32, self.source_id),
                        )
                    }
                }
            } else {
                // Check for 'U' suffix
                if self.pos < self.bytes.len()
                    && (self.bytes[self.pos] == b'U' || self.bytes[self.pos] == b'u')
                {
                    self.pos += 1;
                }
                let text: String = self.source[start..self.pos]
                    .chars()
                    .filter(|&c| c != '_' && c != '\'' && c != 'U' && c != 'u')
                    .collect();
                let val = text.parse::<u128>().unwrap_or_else(|_| {
                    self.errors.push(LexError::new(
                        "integer literal overflow",
                        Span::new(start as u32, self.pos as u32, self.source_id),
                    ));
                    0
                });
                Token::new(
                    TokenKind::IntLiteral(val),
                    Span::new(start as u32, self.pos as u32, self.source_id),
                )
            }
        }
    }

    /// Process an escape sequence at self.pos (the byte after '\\').
    /// Returns (resolved_char, pos_already_advanced).
    /// If pos_already_advanced is false, caller should do self.pos += 1.
    fn lex_escape_char(&mut self) -> (char, bool) {
        match self.bytes[self.pos] {
            b'n' => ('\n', false),
            b'r' => ('\r', false),
            b't' => ('\t', false),
            b'\\' => ('\\', false),
            b'"' => ('"', false),
            b'\'' => ('\'', false),
            b'0' => ('\0', false),
            b'a' => ('\x07', false),
            b'b' => ('\x08', false),
            b'f' => ('\x0C', false),
            b'v' => ('\x0B', false),
            b'x' => {
                self.pos += 1;
                let hex_start = self.pos;
                while self.pos < self.bytes.len()
                    && self.bytes[self.pos].is_ascii_hexdigit()
                    && self.pos - hex_start < 2
                {
                    self.pos += 1;
                }
                let hex_str = &self.source[hex_start..self.pos];
                if let Ok(val) = u8::from_str_radix(hex_str, 16) {
                    (val as char, true)
                } else {
                    self.errors.push(LexError::new(
                        "invalid hex escape",
                        Span::new(hex_start as u32, self.pos as u32, self.source_id),
                    ));
                    ('\0', true)
                }
            }
            other => {
                self.errors.push(LexError::new(
                    format!("unknown escape sequence '\\{}'", other as char),
                    Span::new((self.pos - 1) as u32, (self.pos + 1) as u32, self.source_id),
                ));
                (other as char, false)
            }
        }
    }

    fn lex_string(&mut self, start: usize) -> Token {
        self.pos += 1; // skip opening quote
        let mut value = String::new();
        loop {
            if self.pos >= self.bytes.len() {
                self.errors.push(LexError::new(
                    "unterminated string literal",
                    Span::new(start as u32, self.pos as u32, self.source_id),
                ));
                break;
            }
            match self.bytes[self.pos] {
                b'"' => {
                    self.pos += 1;
                    break;
                }
                b'\\' => {
                    self.pos += 1;
                    if self.pos >= self.bytes.len() {
                        self.errors.push(LexError::new(
                            "unterminated escape sequence",
                            Span::new(start as u32, self.pos as u32, self.source_id),
                        ));
                        break;
                    }
                    let (ch, pos_advanced) = self.lex_escape_char();
                    value.push(ch);
                    if !pos_advanced {
                        self.pos += 1;
                    }
                }
                _b => {
                    // Handle multi-byte UTF-8
                    let ch_start = self.pos;
                    let ch = self.source[ch_start..].chars().next().unwrap();
                    value.push(ch);
                    self.pos += ch.len_utf8();
                }
            }
        }
        Token::new(
            TokenKind::StringLiteral(value),
            Span::new(start as u32, self.pos as u32, self.source_id),
        )
    }

    fn lex_char(&mut self, start: usize) -> Token {
        self.pos += 1; // skip opening quote
        let ch = if self.pos < self.bytes.len() && self.bytes[self.pos] == b'\\' {
            self.pos += 1;
            if self.pos >= self.bytes.len() {
                self.errors.push(LexError::new(
                    "unterminated character literal",
                    Span::new(start as u32, self.pos as u32, self.source_id),
                ));
                return Token::new(
                    TokenKind::CharLiteral('\0'),
                    Span::new(start as u32, self.pos as u32, self.source_id),
                );
            }
            let (escaped, pos_advanced) = self.lex_escape_char();
            if !pos_advanced {
                self.pos += 1;
            }
            escaped
        } else if self.pos < self.bytes.len() && self.bytes[self.pos] != b'\'' {
            let ch = self.source[self.pos..].chars().next().unwrap();
            self.pos += ch.len_utf8();
            ch
        } else {
            self.errors.push(LexError::new(
                "empty character literal",
                Span::new(
                    start as u32,
                    (self.pos + 1).min(self.bytes.len()) as u32,
                    self.source_id,
                ),
            ));
            self.pos += 1; // skip closing quote if present
            return Token::new(
                TokenKind::CharLiteral('\0'),
                Span::new(start as u32, self.pos as u32, self.source_id),
            );
        };

        // Expect closing quote
        if self.pos < self.bytes.len() && self.bytes[self.pos] == b'\'' {
            self.pos += 1;
        } else {
            self.errors.push(LexError::new(
                "unterminated character literal",
                Span::new(start as u32, self.pos as u32, self.source_id),
            ));
        }

        Token::new(
            TokenKind::CharLiteral(ch),
            Span::new(start as u32, self.pos as u32, self.source_id),
        )
    }

    fn lex_ident(&mut self, start: usize) -> Token {
        while self.pos < self.bytes.len()
            && (self.bytes[self.pos].is_ascii_alphanumeric() || self.bytes[self.pos] == b'_')
        {
            self.pos += 1;
        }
        let text = &self.source[start..self.pos];
        let kind = TokenKind::keyword_from_str(text)
            .unwrap_or_else(|| TokenKind::Ident(self.interner.intern(text)));
        Token::new(
            kind,
            Span::new(start as u32, self.pos as u32, self.source_id),
        )
    }

    fn lex_operator(&mut self, start: usize) -> Option<Token> {
        let b = self.advance()?;
        let kind = match b {
            b'+' => {
                if self.peek() == Some(b'=') {
                    self.advance();
                    TokenKind::PlusEq
                } else {
                    TokenKind::Plus
                }
            }
            b'-' => {
                if self.peek() == Some(b'>') {
                    self.advance();
                    TokenKind::Arrow
                } else if self.peek() == Some(b'=') {
                    self.advance();
                    TokenKind::MinusEq
                } else {
                    TokenKind::Minus
                }
            }
            b'*' => {
                if self.peek() == Some(b'=') {
                    self.advance();
                    TokenKind::StarEq
                } else {
                    TokenKind::Star
                }
            }
            b'/' => {
                if self.peek() == Some(b'=') {
                    self.advance();
                    TokenKind::SlashEq
                } else {
                    TokenKind::Slash
                }
            }
            b'%' => {
                if self.peek() == Some(b'=') {
                    self.advance();
                    TokenKind::PercentEq
                } else {
                    TokenKind::Percent
                }
            }
            b'&' => {
                if self.peek() == Some(b'&') {
                    self.advance();
                    TokenKind::AmpAmp
                } else if self.peek() == Some(b'=') {
                    self.advance();
                    TokenKind::AmpEq
                } else {
                    TokenKind::Ampersand
                }
            }
            b'|' => {
                if self.peek() == Some(b'|') {
                    self.advance();
                    TokenKind::PipePipe
                } else if self.peek() == Some(b'=') {
                    self.advance();
                    TokenKind::PipeEq
                } else {
                    TokenKind::Pipe
                }
            }
            b'^' => {
                if self.peek() == Some(b'=') {
                    self.advance();
                    TokenKind::CaretEq
                } else {
                    TokenKind::Caret
                }
            }
            b'~' => TokenKind::Tilde,
            b'<' => {
                if self.peek() == Some(b'<') {
                    self.advance();
                    if self.peek() == Some(b'=') {
                        self.advance();
                        TokenKind::LShiftEq
                    } else {
                        TokenKind::LShift
                    }
                } else if self.peek() == Some(b'=') {
                    self.advance();
                    TokenKind::LessEq
                } else {
                    TokenKind::Less
                }
            }
            b'>' => {
                if self.peek() == Some(b'>') {
                    self.advance();
                    if self.peek() == Some(b'=') {
                        self.advance();
                        TokenKind::RShiftEq
                    } else {
                        TokenKind::RShift
                    }
                } else if self.peek() == Some(b'=') {
                    self.advance();
                    TokenKind::GreaterEq
                } else {
                    TokenKind::Greater
                }
            }
            b'=' => {
                if self.peek() == Some(b'=') {
                    self.advance();
                    TokenKind::EqEq
                } else {
                    TokenKind::Eq
                }
            }
            b'!' => {
                if self.peek() == Some(b'=') {
                    self.advance();
                    TokenKind::BangEq
                } else {
                    TokenKind::Bang
                }
            }
            b'(' => TokenKind::LParen,
            b')' => TokenKind::RParen,
            b'[' => TokenKind::LBracket,
            b']' => TokenKind::RBracket,
            b'{' => TokenKind::LBrace,
            b'}' => TokenKind::RBrace,
            b'$' => TokenKind::Dollar,
            b'@' => TokenKind::At,
            b':' => {
                if self.peek() == Some(b':') {
                    self.advance();
                    TokenKind::ColonColon
                } else {
                    TokenKind::Colon
                }
            }
            b'.' => {
                if self.peek() == Some(b'.') && self.peek_at(1) == Some(b'.') {
                    self.advance();
                    self.advance();
                    TokenKind::DotDotDot
                } else {
                    TokenKind::Dot
                }
            }
            b',' => TokenKind::Comma,
            b';' => TokenKind::Semicolon,
            b'?' => TokenKind::Question,
            _ => {
                // Undo the advance since we'll handle it as unrecognized in tokenize()
                self.pos = start;
                return None;
            }
        };

        Some(Token::new(
            kind,
            Span::new(start as u32, self.pos as u32, self.source_id),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::name::StringInterner;

    fn lex(input: &str) -> (Vec<TokenKind>, StringInterner) {
        let mut interner = StringInterner::new();
        let lexer = Lexer::new(input, SourceId(0), &mut interner);
        let (tokens, errors) = lexer.tokenize();
        assert!(errors.is_empty(), "unexpected lex errors: {:?}", errors);
        (tokens.into_iter().map(|t| t.kind).collect(), interner)
    }

    fn lex_with_errors(input: &str) -> (Vec<TokenKind>, Vec<LexError>) {
        let mut interner = StringInterner::new();
        let lexer = Lexer::new(input, SourceId(0), &mut interner);
        let (tokens, errors) = lexer.tokenize();
        (tokens.into_iter().map(|t| t.kind).collect(), errors)
    }

    // --- Integer literals ---

    #[test]
    fn test_decimal_integer() {
        let (kinds, _) = lex("42");
        assert_eq!(kinds, vec![TokenKind::IntLiteral(42), TokenKind::Eof]);
    }

    #[test]
    fn test_hex_integer() {
        let (kinds, _) = lex("0xFF");
        assert_eq!(kinds, vec![TokenKind::IntLiteral(255), TokenKind::Eof]);
    }

    #[test]
    fn test_hex_integer_upper() {
        let (kinds, _) = lex("0XAB");
        assert_eq!(kinds, vec![TokenKind::IntLiteral(0xAB), TokenKind::Eof]);
    }

    #[test]
    fn test_octal_integer() {
        let (kinds, _) = lex("0o77");
        assert_eq!(kinds, vec![TokenKind::IntLiteral(63), TokenKind::Eof]);
    }

    #[test]
    fn test_binary_integer() {
        let (kinds, _) = lex("0b1010");
        assert_eq!(kinds, vec![TokenKind::IntLiteral(10), TokenKind::Eof]);
    }

    #[test]
    fn test_integer_with_underscores() {
        let (kinds, _) = lex("1_000_000");
        assert_eq!(
            kinds,
            vec![TokenKind::IntLiteral(1_000_000), TokenKind::Eof]
        );
    }

    #[test]
    fn test_hex_with_digit_separator() {
        let (kinds, _) = lex("0x100'0000");
        assert_eq!(
            kinds,
            vec![TokenKind::IntLiteral(0x1000000), TokenKind::Eof]
        );
    }

    #[test]
    fn test_hex_with_multiple_digit_separators() {
        let (kinds, _) = lex("0x8000'0000");
        assert_eq!(
            kinds,
            vec![TokenKind::IntLiteral(0x80000000), TokenKind::Eof]
        );
    }

    #[test]
    fn test_decimal_with_digit_separator() {
        let (kinds, _) = lex("1'000'000");
        assert_eq!(
            kinds,
            vec![TokenKind::IntLiteral(1_000_000), TokenKind::Eof]
        );
    }

    #[test]
    fn test_hex_with_underscores() {
        let (kinds, _) = lex("0xFF_FF");
        assert_eq!(kinds, vec![TokenKind::IntLiteral(0xFFFF), TokenKind::Eof]);
    }

    #[test]
    fn test_zero() {
        let (kinds, _) = lex("0");
        assert_eq!(kinds, vec![TokenKind::IntLiteral(0), TokenKind::Eof]);
    }

    // --- Float literals ---

    #[test]
    fn test_float() {
        let (kinds, _) = lex("3.14");
        assert_eq!(kinds, vec![TokenKind::FloatLiteral(3.14), TokenKind::Eof]);
    }

    #[test]
    fn test_float_exponent() {
        let (kinds, _) = lex("1e10");
        assert_eq!(kinds, vec![TokenKind::FloatLiteral(1e10), TokenKind::Eof]);
    }

    #[test]
    fn test_float_full() {
        let (kinds, _) = lex("1.5e-3");
        assert_eq!(kinds, vec![TokenKind::FloatLiteral(1.5e-3), TokenKind::Eof]);
    }

    #[test]
    fn test_float_suffix_f() {
        let (kinds, _) = lex("3.14F");
        assert_eq!(kinds, vec![TokenKind::FloatLiteral(3.14), TokenKind::Eof]);
    }

    // --- String literals ---

    #[test]
    fn test_string() {
        let (kinds, _) = lex(r#""hello world""#);
        assert_eq!(
            kinds,
            vec![
                TokenKind::StringLiteral("hello world".into()),
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn test_string_escapes() {
        let (kinds, _) = lex(r#""a\nb\t\\""#);
        assert_eq!(
            kinds,
            vec![TokenKind::StringLiteral("a\nb\t\\".into()), TokenKind::Eof]
        );
    }

    #[test]
    fn test_string_hex_escape() {
        let (kinds, _) = lex(r#""\x41""#);
        assert_eq!(
            kinds,
            vec![TokenKind::StringLiteral("A".into()), TokenKind::Eof]
        );
    }

    #[test]
    fn test_unterminated_string() {
        let (kinds, errors) = lex_with_errors(r#""hello"#);
        assert_eq!(
            kinds,
            vec![TokenKind::StringLiteral("hello".into()), TokenKind::Eof]
        );
        assert!(!errors.is_empty());
    }

    // --- Character literals ---

    #[test]
    fn test_char() {
        let (kinds, _) = lex("'A'");
        assert_eq!(kinds, vec![TokenKind::CharLiteral('A'), TokenKind::Eof]);
    }

    #[test]
    fn test_char_escape() {
        let (kinds, _) = lex(r"'\n'");
        assert_eq!(kinds, vec![TokenKind::CharLiteral('\n'), TokenKind::Eof]);
    }

    #[test]
    fn test_char_hex_escape() {
        let (kinds, _) = lex(r"'\x41'");
        assert_eq!(kinds, vec![TokenKind::CharLiteral('A'), TokenKind::Eof]);
    }

    // --- Keywords ---

    #[test]
    fn test_keywords() {
        let (kinds, _) = lex("struct union enum bitfield fn if else while for return");
        assert_eq!(
            kinds,
            vec![
                TokenKind::KwStruct,
                TokenKind::KwUnion,
                TokenKind::KwEnum,
                TokenKind::KwBitfield,
                TokenKind::KwFn,
                TokenKind::KwIf,
                TokenKind::KwElse,
                TokenKind::KwWhile,
                TokenKind::KwFor,
                TokenKind::KwReturn,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_endianness_keywords() {
        let (kinds, _) = lex("le be");
        assert_eq!(
            kinds,
            vec![TokenKind::KwLe, TokenKind::KwBe, TokenKind::Eof]
        );
    }

    #[test]
    fn test_more_keywords() {
        let (kinds, _) = lex("match break continue try catch namespace using");
        assert_eq!(
            kinds,
            vec![
                TokenKind::KwMatch,
                TokenKind::KwBreak,
                TokenKind::KwContinue,
                TokenKind::KwTry,
                TokenKind::KwCatch,
                TokenKind::KwNamespace,
                TokenKind::KwUsing,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_bool_null_keywords() {
        let (kinds, _) = lex("true false null");
        assert_eq!(
            kinds,
            vec![
                TokenKind::KwTrue,
                TokenKind::KwFalse,
                TokenKind::KwNull,
                TokenKind::Eof,
            ]
        );
    }

    // --- Identifiers ---

    #[test]
    fn test_identifier() {
        let (kinds, mut i) = lex("myVar _private foo123");
        assert_eq!(
            kinds,
            vec![
                TokenKind::Ident(i.intern("myVar")),
                TokenKind::Ident(i.intern("_private")),
                TokenKind::Ident(i.intern("foo123")),
                TokenKind::Eof,
            ]
        );
    }

    // --- Operators ---

    #[test]
    fn test_arithmetic_operators() {
        let (kinds, _) = lex("+ - * / %");
        assert_eq!(
            kinds,
            vec![
                TokenKind::Plus,
                TokenKind::Minus,
                TokenKind::Star,
                TokenKind::Slash,
                TokenKind::Percent,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_comparison_operators() {
        let (kinds, _) = lex("== != < > <= >=");
        assert_eq!(
            kinds,
            vec![
                TokenKind::EqEq,
                TokenKind::BangEq,
                TokenKind::Less,
                TokenKind::Greater,
                TokenKind::LessEq,
                TokenKind::GreaterEq,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_logical_operators() {
        let (kinds, _) = lex("&& || !");
        assert_eq!(
            kinds,
            vec![
                TokenKind::AmpAmp,
                TokenKind::PipePipe,
                TokenKind::Bang,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_bitwise_operators() {
        let (kinds, _) = lex("& | ^ ~ << >>");
        assert_eq!(
            kinds,
            vec![
                TokenKind::Ampersand,
                TokenKind::Pipe,
                TokenKind::Caret,
                TokenKind::Tilde,
                TokenKind::LShift,
                TokenKind::RShift,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_assignment_operators() {
        let (kinds, _) = lex("= += -= *= /= %= &= |= ^= <<= >>=");
        assert_eq!(
            kinds,
            vec![
                TokenKind::Eq,
                TokenKind::PlusEq,
                TokenKind::MinusEq,
                TokenKind::StarEq,
                TokenKind::SlashEq,
                TokenKind::PercentEq,
                TokenKind::AmpEq,
                TokenKind::PipeEq,
                TokenKind::CaretEq,
                TokenKind::LShiftEq,
                TokenKind::RShiftEq,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_special_symbols() {
        let (kinds, _) = lex("$ @ :: ... . -> , ; : ?");
        assert_eq!(
            kinds,
            vec![
                TokenKind::Dollar,
                TokenKind::At,
                TokenKind::ColonColon,
                TokenKind::DotDotDot,
                TokenKind::Dot,
                TokenKind::Arrow,
                TokenKind::Comma,
                TokenKind::Semicolon,
                TokenKind::Colon,
                TokenKind::Question,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_brackets() {
        let (kinds, _) = lex("( ) [ ] { }");
        assert_eq!(
            kinds,
            vec![
                TokenKind::LParen,
                TokenKind::RParen,
                TokenKind::LBracket,
                TokenKind::RBracket,
                TokenKind::LBrace,
                TokenKind::RBrace,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_attribute_brackets() {
        let (kinds, _) = lex("[[ ]]");
        assert_eq!(
            kinds,
            vec![
                TokenKind::LBracket,
                TokenKind::LBracket,
                TokenKind::RBracket,
                TokenKind::RBracket,
                TokenKind::Eof,
            ]
        );
    }

    // --- Comments ---

    #[test]
    fn test_line_comment() {
        let (kinds, _) = lex("42 // this is a comment\n43");
        assert_eq!(
            kinds,
            vec![
                TokenKind::IntLiteral(42),
                TokenKind::IntLiteral(43),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_block_comment() {
        let (kinds, _) = lex("42 /* block */ 43");
        assert_eq!(
            kinds,
            vec![
                TokenKind::IntLiteral(42),
                TokenKind::IntLiteral(43),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_nested_block_comment() {
        let (kinds, _) = lex("42 /* outer /* inner */ still outer */ 43");
        assert_eq!(
            kinds,
            vec![
                TokenKind::IntLiteral(42),
                TokenKind::IntLiteral(43),
                TokenKind::Eof,
            ]
        );
    }

    // --- Preprocessor ---

    #[test]
    fn test_hash() {
        let (kinds, mut i) = lex("#include");
        assert_eq!(
            kinds,
            vec![
                TokenKind::Hash,
                TokenKind::Ident(i.intern("include")),
                TokenKind::Eof,
            ]
        );
    }

    // --- Complex expressions ---

    #[test]
    fn test_struct_definition() {
        let (kinds, mut i) = lex("struct Header { u32 magic; };");
        assert_eq!(
            kinds,
            vec![
                TokenKind::KwStruct,
                TokenKind::Ident(i.intern("Header")),
                TokenKind::LBrace,
                TokenKind::Ident(i.intern("u32")),
                TokenKind::Ident(i.intern("magic")),
                TokenKind::Semicolon,
                TokenKind::RBrace,
                TokenKind::Semicolon,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_variable_placement() {
        let (kinds, mut i) = lex("u8 x @ 0x10;");
        assert_eq!(
            kinds,
            vec![
                TokenKind::Ident(i.intern("u8")),
                TokenKind::Ident(i.intern("x")),
                TokenKind::At,
                TokenKind::IntLiteral(0x10),
                TokenKind::Semicolon,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_endian_prefixed_type() {
        let (kinds, mut i) = lex("le u32 value;");
        assert_eq!(
            kinds,
            vec![
                TokenKind::KwLe,
                TokenKind::Ident(i.intern("u32")),
                TokenKind::Ident(i.intern("value")),
                TokenKind::Semicolon,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_attribute_syntax() {
        let (kinds, mut i) = lex("[[color(\"FF0000\")]]");
        assert_eq!(
            kinds,
            vec![
                TokenKind::LBracket,
                TokenKind::LBracket,
                TokenKind::Ident(i.intern("color")),
                TokenKind::LParen,
                TokenKind::StringLiteral("FF0000".into()),
                TokenKind::RParen,
                TokenKind::RBracket,
                TokenKind::RBracket,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_dollar_current_offset() {
        let (kinds, _) = lex("$ + 4");
        assert_eq!(
            kinds,
            vec![
                TokenKind::Dollar,
                TokenKind::Plus,
                TokenKind::IntLiteral(4),
                TokenKind::Eof,
            ]
        );
    }

    // --- Edge cases ---

    #[test]
    fn test_empty_input() {
        let (kinds, _) = lex("");
        assert_eq!(kinds, vec![TokenKind::Eof]);
    }

    #[test]
    fn test_whitespace_only() {
        let (kinds, _) = lex("   \n\t  ");
        assert_eq!(kinds, vec![TokenKind::Eof]);
    }

    #[test]
    fn test_adjacent_operators() {
        let (kinds, mut i) = lex("a<<b>>c");
        assert_eq!(
            kinds,
            vec![
                TokenKind::Ident(i.intern("a")),
                TokenKind::LShift,
                TokenKind::Ident(i.intern("b")),
                TokenKind::RShift,
                TokenKind::Ident(i.intern("c")),
                TokenKind::Eof,
            ]
        );
    }
}

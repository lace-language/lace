use logos::{Logos, SpannedIter};

pub type Lexer<'s> = SpannedIter<'s, Token<'s>>;

#[derive(Logos, Clone, Copy, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token<'a> {
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident(&'a str),
    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#)]
    String(&'a str),
    #[regex(r"[0-9][0-9_]*")]
    Int(&'a str),

    // Multi-character operators
    #[token("&&")]
    AmpAmp,
    #[token("||")]
    PipePipe,
    #[token("<=")]
    AngleLeftEquals,
    #[token(">=")]
    AngleRightEquals,
    #[token("==")]
    EqualsEquals,
    #[token("!=")]
    BangEquals,
    #[token("->")]
    RightArrow,

    // Single-character operators
    #[token("!")]
    Bang,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token(".")]
    Period,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token("=")]
    Equals,

    // Keywords
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("false")]
    False,
    #[token("true")]
    True,

    // Brackets
    #[token("(")]
    RoundLeft,
    #[token(")")]
    RoundRight,
    #[token("{")]
    CurlyLeft,
    #[token("}")]
    CurlyRight,
    #[token("[")]
    SquareLeft,
    #[token("]")]
    SquareRight,
    #[token("<")]
    AngleLeft,
    #[token(">")]
    AngleRight,
}

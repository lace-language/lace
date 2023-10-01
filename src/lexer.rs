use logos::Logos;

pub type Lexer<'source> = logos::Lexer<'source, Token>;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident,
    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, |lex| lex.slice().to_owned())]
    String(String),
    #[regex(r"-?(\d+)", |lex| lex.slice().parse::<i64>().unwrap())]
    Int(i64),
    // Operators and the like
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("/")]
    Slash,
    #[token(".")]
    Period,
    #[token(",")]
    Comma,
    // Keywords
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("false")]
    False,
    #[token("true")]
    True,
    // Brackets
    #[token("(")]
    RoundOpen,
    #[token(")")]
    RoundClose,
    #[token("{")]
    CurlyOpen,
    #[token("}")]
    CurlyClose,
    #[token("[")]
    SquareOpen,
    #[token("]")]
    SquareClose,
    #[token("<")]
    AngleOpen,
    #[token(">")]
    AngleClose,
}

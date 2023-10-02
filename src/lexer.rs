use logos::Logos;

pub type Lexer<'source> = logos::Lexer<'source, Token<'source>>;

#[derive(Logos, Clone, Copy, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token<'a> {
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident,
    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, |lex| lex.slice())]
    String(&'a str),
    #[regex(r"[0-9][0-9_]*", |lex| lex.slice().replace('_', "").parse::<u64>().unwrap())]
    Int(u64),
    // Operators and the like
    #[token("&&")]
    AmpAmp,
    #[token("||")]
    PipePipe,
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

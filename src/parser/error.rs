pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
pub enum ParseError {
    EndOfInput,
    Expected,
    // TODO: Figure out how to get the error from logos
    UnrecognizedToken(()),
}

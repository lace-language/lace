macro_rules! tok {
    (->) => {
        Token::RightArrow
    };
    (&&) => {
        Token::AmpAmp
    };
    (||) => {
        Token::PipePipe
    };
    (<=) => {
        Token::AngleLeftEquals
    };
    (>=) => {
        Token::AngleRightEquals
    };
    (==) => {
        Token::EqualsEquals
    };
    (!=) => {
        Token::BangEquals
    };
    (!) => {
        Token::Bang
    };
    (+) => {
        Token::Plus
    };
    (-) => {
        Token::Minus
    };
    (*) => {
        Token::Star
    };
    (/) => {
        Token::Slash
    };
    (.) => {
        Token::Period
    };
    (,) => {
        Token::Comma
    };
    (;) => {
        Token::Semicolon
    };
    (:) => {
        Token::Colon
    };
    (=) => {
        Token::Equals
    };
    (fn) => {
        Token::Fn
    };
    (let) => {
        Token::Let
    };
    (if) => {
        Token::If
    };
    (else) => {
        Token::Else
    };
    (false) => {
        Token::False
    };
    (true) => {
        Token::True
    };
    (<) => {
        Token::AngleLeft
    };
    (>) => {
        Token::AngleRight
    };
}

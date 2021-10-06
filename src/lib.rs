use logos::{Lexer, Logos};
use semver::{Comparator, Op, Prerelease, VersionReq};

pub fn parse(text: &str) -> Result<VersionReq, Error<'_>> {
    let mut lexer = Token::lexer(text);
    let mut comparators = Vec::new();
    let mut remainder = None;

    while let Some((comparator, r)) = parse_one(&mut lexer, remainder)? {
        comparators.push(comparator);
        remainder = r;
    }

    Ok(VersionReq { comparators })
}

fn parse_one<'a>(
    lexer: &mut Lexer<'a, Token>,
    remainder: Option<Token>,
) -> Result<Option<(Comparator, Option<Token>)>, Error<'a>> {
    let (op, major) = match remainder.or_else(|| lexer.next()) {
        Some(Token::Exact) => (Op::Exact, None),
        Some(Token::Greater) => (Op::Greater, None),
        Some(Token::GreaterEq) => (Op::GreaterEq, None),
        Some(Token::Less) => (Op::Less, None),
        Some(Token::LessEq) => (Op::LessEq, None),
        Some(Token::Tilde) => (Op::Tilde, None),
        Some(Token::Caret) => (Op::Caret, None),
        Some(Token::Wildcard) | None => return Ok(None),
        Some(token) => (Op::Exact, Some(token)),
    };

    let major = major.or_else(|| lexer.next()).ok_or(Error::UnexpectedEoi)?;
    let major = match major {
        Token::Num => lexer.slice().parse()?,
        _ => return Err(Error::UnexpectedInput(lexer.slice())),
    };

    macro_rules! matcher {
        {
            ($minor:expr, $patch:expr) => $expr:expr
        } => {
            match lexer.next() {
                Some(Token::Dot) => match lexer.next() {
                    Some(Token::Num) => $expr,
                    Some(_) => return Err(Error::UnexpectedInput(lexer.slice())),
                    None => return Err(Error::UnexpectedEoi),
                },
                Some(Token::Pre) => ($minor, $patch, Prerelease::new(&lexer.slice()[1..])?, None),
                Some(token) => ($minor, $patch, Prerelease::EMPTY, Some(token)),
                None => ($minor, $patch, Prerelease::EMPTY, None),
            }
        }
    }

    let (minor, patch, pre, remainder) = matcher! {
        (None, None) => {
            let minor = lexer.slice().parse()?;
            matcher! {
                (Some(minor), None) => {
                    let patch = lexer.slice().parse()?;
                    match lexer.next() {
                        Some(Token::Pre) => (Some(minor), Some(patch), Prerelease::new(&lexer.slice()[1..])?, None),
                        Some(token) => (Some(minor), Some(patch), Prerelease::EMPTY, Some(token)),
                        None => (Some(minor), Some(patch), Prerelease::EMPTY, None),
                    }
                }
            }
        }
    };

    Ok(Some((
        Comparator {
            op,
            major,
            minor,
            patch,
            pre,
        },
        remainder,
    )))
}

#[derive(Logos)]
enum Token {
    #[regex("[0-9]+")]
    Num,
    #[regex("-[A-Za-z0-9-.]+")]
    Pre,

    #[token(".")]
    Dot,

    #[token("=")]
    Exact,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEq,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEq,
    #[token("~")]
    Tilde,
    #[token("^")]
    Caret,
    #[token("*")]
    Wildcard,

    #[regex(r"[ \t,]", logos::skip)]
    #[error]
    Error,
}

#[derive(thiserror::Error, Debug)]
pub enum Error<'a> {
    #[error(transparent)]
    Semver(#[from] semver::Error),
    #[error(transparent)]
    Num(#[from] std::num::ParseIntError),

    #[error("unexpected end of input")]
    UnexpectedEoi,
    #[error("unexpected input: {0}")]
    UnexpectedInput(&'a str),
}

#[cfg(test)]
mod tests {
    #[test]
    fn not_separated() {
        let separated = ">=0.6.5, <0.8";
        let not_separated = ">=0.6.5<0.8";

        assert_eq!(
            super::parse(not_separated).unwrap(),
            super::VersionReq::parse(separated).unwrap(),
        );
    }
}

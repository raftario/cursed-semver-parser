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
    let (mut op, major) = match remainder.or_else(|| lexer.next()) {
        Some(Token::Exact) => (Op::Exact, None),
        Some(Token::Greater) => (Op::Greater, None),
        Some(Token::GreaterEq) => (Op::GreaterEq, None),
        Some(Token::Less) => (Op::Less, None),
        Some(Token::LessEq) => (Op::LessEq, None),
        Some(Token::Tilde) => (Op::Tilde, None),
        Some(Token::Caret) => (Op::Caret, None),
        Some(Token::Wildcard) | None => {
            return if lexer.next().is_none() {
                Ok(None)
            } else {
                Err(Error::UnexpectedInput(lexer.slice()))
            }
        }
        Some(token) => (Op::Exact, Some(token)),
    };

    let major = major.or_else(|| lexer.next()).ok_or(Error::UnexpectedEoi)?;
    let major = match major {
        Token::Num => lexer.slice().parse()?,
        _ => return Err(Error::UnexpectedInput(lexer.slice())),
    };

    macro_rules! matcher {
        ($minor:expr, $patch:expr $(, $expr:expr)?) => {
            match lexer.next() {
                $(Some(Token::Dot) => match lexer.next() {
                    Some(Token::Num) => $expr,
                    Some(Token::Wildcard) => {
                        op = Op::Wildcard;
                        matcher!($minor, $patch)
                    }
                    Some(_) => return Err(Error::UnexpectedInput(lexer.slice())),
                    None => return Err(Error::UnexpectedEoi),
                },)?
                Some(Token::Pre) => ($minor, $patch, Prerelease::new(&lexer.slice()[1..])?, None),
                Some(token) => ($minor, $patch, Prerelease::EMPTY, Some(token)),
                None => ($minor, $patch, Prerelease::EMPTY, None),
            }
        };
    }

    let (minor, patch, pre, remainder) = matcher!(None, None, {
        let minor = lexer.slice().parse()?;
        matcher!(Some(minor), None, {
            let patch = lexer.slice().parse()?;
            matcher!(Some(minor), Some(patch))
        })
    });

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
    #[regex("0|([1-9][0-9]*)")]
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

#[cfg(feature = "serde")]
pub fn deserialize<'de, D>(deserializer: D) -> Result<VersionReq, D::Error>
where
    D: serde::de::Deserializer<'de>,
{
    let text = <&str as serde::de::Deserialize>::deserialize(deserializer)?;
    parse(text).map_err(serde::de::Error::custom)
}

#[cfg(test)]
mod tests {
    use quickcheck::{Gen, QuickCheck};
    use semver::{Comparator, Op, Prerelease, VersionReq};
    use std::iter;

    use super::parse;

    #[test]
    fn not_separated() {
        let separated = ">=0.6.5, <0.8";
        let not_separated = ">=0.6.5<0.8";

        assert_eq!(
            parse(not_separated).unwrap(),
            VersionReq::parse(separated).unwrap(),
        );
    }

    #[test]
    fn wildcard() {
        let cases = ["*", "0.*", "0.0.*"];
        for text in cases {
            assert_eq!(parse(text).unwrap(), VersionReq::parse(text).unwrap(),);
        }
    }

    #[test]
    fn follows_semver_crate() {
        fn t(Arbitrary(version): Arbitrary<VersionReq>) -> bool {
            let text = version.to_string();
            parse(&text).unwrap() == VersionReq::parse(&text).unwrap()
        }

        QuickCheck::new()
            .tests(2048)
            .quickcheck(t as fn(Arbitrary<VersionReq>) -> bool);
    }

    #[derive(Debug, Clone)]
    struct Arbitrary<T>(T);

    impl quickcheck::Arbitrary for Arbitrary<VersionReq> {
        fn arbitrary(gen: &mut Gen) -> Self {
            let comparator_counts: Box<[usize]> = (0..10).collect();
            let comparator_count = *gen.choose(&comparator_counts).unwrap();

            let comparators = (0..comparator_count)
                .map(|_| <Arbitrary<Comparator>>::arbitrary(gen).0)
                .collect();

            Self(VersionReq { comparators })
        }

        fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
            let comparators = self.0.comparators.iter().cloned().map(Arbitrary).collect();
            let iterator = <Vec<Arbitrary<Comparator>>>::shrink(&comparators).map(|comparators| {
                let comparators = comparators
                    .into_iter()
                    .map(|comparator| comparator.0)
                    .collect();
                Self(VersionReq { comparators })
            });
            Box::new(iterator)
        }
    }

    impl quickcheck::Arbitrary for Arbitrary<Comparator> {
        fn arbitrary(gen: &mut Gen) -> Self {
            let ops = [
                Op::Exact,
                Op::Greater,
                Op::GreaterEq,
                Op::Less,
                Op::LessEq,
                Op::Tilde,
                Op::Caret,
                Op::Wildcard,
            ];
            let majors: Box<[u64]> = (0..10).collect();
            let minors_and_patches: Box<[Option<u64>]> = (0..100)
                .map(Some)
                .chain(iter::repeat(None).take(50))
                .collect();
            let pres_1: Box<[Option<&str>]> = ["alpha", "beta", "rc"]
                .iter()
                .copied()
                .map(Some)
                .chain(iter::repeat(None).take(3))
                .collect();
            let pres_2: Box<[u64]> = (1..10).collect();

            let mut op = *gen.choose(&ops).unwrap();
            let major = *gen.choose(&majors).unwrap();
            let minor = *gen.choose(&minors_and_patches).unwrap();
            let patch = minor.and_then(|_| *gen.choose(&minors_and_patches).unwrap());
            let pre = patch.map_or(Prerelease::EMPTY, |_| {
                gen.choose(&pres_1)
                    .unwrap()
                    .map_or(Prerelease::EMPTY, |p1| {
                        let p2 = *gen.choose(&pres_2).unwrap();
                        let s = format!("{}.{}", p1, p2);
                        Prerelease::new(&s).unwrap()
                    })
            });

            if matches!((op, patch), (Op::Wildcard, Some(_))) {
                op = Op::Tilde;
            }

            Self(Comparator {
                op,
                major,
                minor,
                patch,
                pre,
            })
        }
    }
}

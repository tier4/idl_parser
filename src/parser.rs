use nom::{branch::alt, bytes::complete::tag, error::VerboseError, multi::many0, IResult};

use crate::character::SP;

type PResult<'a, OUT> = IResult<&'a [u8], OUT, VerboseError<&'a [u8]>>;

// fn parse_comment(input: &str) -> PResult<Expr> {}

fn parse_comment(input: &[u8]) -> PResult<String> {
    skip_space0(input)?;

    let (input, c) = alt((tag("//"), tag("/*")))(input)?;

    todo!();
    // Ok("a".to_string())
}

fn skip_space0(input: &[u8]) -> PResult<()> {
    let (input, _) = many0(tag(" "))(input)?;
    Ok((input, ()))
}

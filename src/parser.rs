mod core;
mod interfaces;

use crate::expr::Definition;
use nom::{error::VerboseError, IResult};

type PResult<'a, OUT> = IResult<&'a str, OUT, VerboseError<&'a str>>;

pub fn parse(input: &str) -> PResult<Definition> {
    core::parse_definition(input)
}

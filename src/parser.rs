mod components;
mod core;
mod extended_data_types;
mod interfaces;
mod template;
mod value_types;

use crate::expr::Definition;
use nom::{error::VerboseError, IResult};

type PResult<'a, OUT> = IResult<&'a str, OUT, VerboseError<&'a str>>;

pub fn parse(input: &str) -> PResult<Definition> {
    core::parse_definition(input)
}

use super::{
    core::{parse_id, parse_member, skip_space_and_comment0, skip_space_and_comment1},
    PResult,
};
use crate::expr::ExceptDcl;
use nom::{
    bytes::complete::tag,
    multi::many1,
    sequence::{delimited, tuple},
};

/// ```text
/// (72) <except_dcl> ::= "exception" <identifier> "{" <member>*  "}"
/// ```
fn parse_except_dcl(input: &str) -> PResult<ExceptDcl> {
    let (input, _) = tuple((tag("exception"), skip_space_and_comment1))(input)?;
    let (input, id) = parse_id(input)?;

    let (input, members) = delimited(
        tuple((skip_space_and_comment0, tag("{"), skip_space_and_comment0)),
        many1(parse_member),
        tuple((skip_space_and_comment0, tag("}"))),
    )(input)?;

    Ok((input, ExceptDcl { id, members }))
}

/// ```text
/// (73) <interface_dcl> ::= <interface_def>
///                        | <interface_forward_dcl>
/// ```
fn parse_interface_dcl(input: &str) -> PResult<()> {
    todo!()
}

/// ```text
/// (74) <interface_def> ::= <interface_header> "{" <interface_body> "}"
/// ```
fn parse_interface_def(input: &str) -> PResult<()> {
    todo!()
}

/// ```text
/// (75) <interface_forward_dcl> ::= <interface_kind> <identifier>
/// ```
fn parse_interface_foward_dcl(input: &str) -> PResult<()> {
    todo!()
}

/// ```text
/// (76) <interface_header> ::= <interface_kind> <identifier> [ <interface_inheritance_spec> ]
/// ```
fn parse_interface_header(input: &str) -> PResult<()> {
    todo!()
}

/// ```text
/// (77) <interface_kind> ::= "interface"
/// ```
fn parse_interface_kind(input: &str) -> PResult<&str> {
    tag("interface")(input)
}

use super::{
    core::{
        parse_const_expr, parse_id, parse_int_words, parse_scoped_name, parse_type_spec,
        skip_space_and_comment0, skip_space_and_comment1,
    },
    PResult,
};
use crate::expr::{
    BitValue, Bitfield, BitfieldSpec, BitmaskDcl, BitsetDcl, MapType, PrimitiveType,
};
use nom::{
    branch::alt,
    bytes::complete::tag,
    error::Error,
    multi::{many0, separated_list0, separated_list1},
    sequence::{delimited, tuple},
};

/// ```text
/// (199) <map_type> ::= "map" "<" <type_spec> "," <type_spec> "," <positive_int_const>  ">"
///                    | "map" "<" <type_spec> "," <type_spec> " ">"
/// ```
pub fn parse_map_type(input: &str) -> PResult<MapType> {
    let (input, _) = tuple((
        tag("map"),
        skip_space_and_comment0,
        tag("<"),
        skip_space_and_comment0,
    ))(input)?;

    let (input, key) = parse_type_spec(input)?;
    let (input, _) = tuple((skip_space_and_comment0, tag(","), skip_space_and_comment0))(input)?;
    let (input, value) = parse_type_spec(input)?;

    let (input, _) = skip_space_and_comment0(input)?;

    let (input, c) = alt((tag(">"), tag(",")))(input)?;

    if c == ">" {
        return Ok((
            input,
            MapType {
                key,
                value,
                size: None,
            },
        ));
    }

    let (input, size) = delimited(
        skip_space_and_comment0,
        parse_const_expr,
        tuple((skip_space_and_comment0, tag(">"))),
    )(input)?;

    Ok((
        input,
        MapType {
            key,
            value,
            size: Some(size),
        },
    ))
}

/// ```text
/// (200) <bitset_dcl> ::= "bitset" <identifier> [":" <scoped_name> ] "{" <bitfield>* "}"
/// ```
pub fn parse_bitset_dcl(input: &str) -> PResult<BitsetDcl> {
    let (input, (_, _, id, _)) = tuple((
        tag("bitset"),
        skip_space_and_comment1,
        parse_id,
        skip_space_and_comment0,
    ))(input)?;

    let (input, name) = if let Ok((input, _)) = tag::<&str, &str, Error<&str>>(":")(input) {
        let (input, (_, name)) = tuple((skip_space_and_comment0, parse_scoped_name))(input)?;
        (input, Some(name))
    } else {
        (input, None)
    };

    let (input, fields) = delimited(
        tuple((skip_space_and_comment0, tag("{"), skip_space_and_comment0)),
        many0(parse_bitfield),
        tuple((skip_space_and_comment0, tag("}"))),
    )(input)?;

    Ok((input, BitsetDcl { id, name, fields }))
}

/// ```text
/// (201) <bitfield> ::= <bitfield_spec> <identifier>* ";"
/// ```
fn parse_bitfield(input: &str) -> PResult<Bitfield> {
    let (input, _) = skip_space_and_comment0(input)?;
    let (input, spec) = parse_bitfield_spec(input)?;
    let (input, _) = skip_space_and_comment0(input)?;
    let (input, ids) = separated_list0(skip_space_and_comment0, parse_id)(input)?;
    let (input, _) = tuple((skip_space_and_comment0, tag(";")))(input)?;
    Ok((input, Bitfield { spec, ids }))
}

/// ```text
/// (202) <bitfield_spec> ::= "bitfield" "<" <positive_int_const> ">"
///                         | "bitfield" "<" <positive_int_const> "," <destination_type> ">"
/// ```
fn parse_bitfield_spec(input: &str) -> PResult<BitfieldSpec> {
    let (input, _) = tuple((
        tag("bitfield"),
        skip_space_and_comment0,
        tag("<"),
        skip_space_and_comment0,
    ))(input)?;

    let (input, bits) = parse_const_expr(input)?;
    let (input, _) = skip_space_and_comment0(input)?;

    let (input, c) = alt((tag(">"), tag(",")))(input)?;

    if c == ">" {
        return Ok((
            input,
            BitfieldSpec {
                bits,
                destination_type: None,
            },
        ));
    }

    let (input, destination_type) = delimited(
        skip_space_and_comment0,
        parse_destination_type,
        tuple((skip_space_and_comment0, tag(">"))),
    )(input)?;

    Ok((
        input,
        BitfieldSpec {
            bits,
            destination_type: Some(destination_type),
        },
    ))
}

/// ```text
/// (203) <destination_type> ::= <boolean_type> | <octet_type> | <integer_type>
/// ```
fn parse_destination_type(input: &str) -> PResult<PrimitiveType> {
    parse_int_words(input)?;

    fn one_word(input: &str) -> PResult<PrimitiveType> {
        let (input, c) = alt((
            tag("boolean"),
            tag("octet"),
            tag("short"),
            tag("long"),
            tag("int8"),
            tag("uint8"),
            tag("int16"),
            tag("uint16"),
            tag("int32"),
            tag("uint32"),
            tag("int64"),
            tag("uint64"),
        ))(input)?;

        Ok((input, PrimitiveType::new(c).unwrap()))
    }

    alt((parse_int_words, one_word))(input)
}

/// ```text
/// (204) <bitmask_dcl> ::= "bitmask" <identifier> "{" <bit_value> { "," <bit_value> }* "}"
/// ```
pub fn parse_bitmask_dcl(input: &str) -> PResult<BitmaskDcl> {
    let (input, (_, _, id, _)) = tuple((
        tag("bitmask"),
        skip_space_and_comment0,
        parse_id,
        skip_space_and_comment0,
    ))(input)?;

    let (input, values) = delimited(
        tuple((skip_space_and_comment0, tag("{"), skip_space_and_comment0)),
        separated_list1(
            tuple((skip_space_and_comment0, tag(","), skip_space_and_comment0)),
            parse_bit_value,
        ),
        tuple((skip_space_and_comment0, tag("}"))),
    )(input)?;

    Ok((input, BitmaskDcl { id, values }))
}

/// ```text
/// (205) <bit_value> ::= <identifier>
/// ```
fn parse_bit_value(input: &str) -> PResult<BitValue> {
    let (input, id) = parse_id(input)?;
    Ok((input, BitValue(id)))
}

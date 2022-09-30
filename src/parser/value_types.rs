use super::{
    core::{
        parse_declarators, parse_id, parse_scoped_name, parse_simple_declarator, parse_type_spec,
        skip_space_and_comment0, skip_space_and_comment1,
    },
    interfaces::parse_raises_expr,
    PResult,
};
use crate::{
    expr::{
        InitDcl, InitParamDcl, ScopedName, StateMember, ValueDcl, ValueDef, ValueElement,
        ValueForwardDcl, ValueHeader, ValueInheritanceSpec, Visibility,
    },
    parser::interfaces::{parse_export, parse_interface_name},
};
use nom::{
    branch::alt,
    bytes::complete::tag,
    multi::{many0, separated_list0},
    sequence::{delimited, tuple},
};

/// ```text
/// (99) <value_dcl> ::= <value_def>
///                    | <value_forward_dcl>
/// ```
pub fn parse_value_dcl(input: &str) -> PResult<ValueDcl> {
    fn def(input: &str) -> PResult<ValueDcl> {
        let (input, result) = parse_value_def(input)?;
        Ok((input, ValueDcl::Def(result)))
    }

    fn forward_dcl(input: &str) -> PResult<ValueDcl> {
        let (input, result) = parse_value_forward_dcl(input)?;
        Ok((input, ValueDcl::ForwardDcl(result)))
    }

    alt((def, forward_dcl))(input)
}

/// ```text
/// (100) <value_def> ::= <value_header> "{" <value_element>* "}"
/// ```
fn parse_value_def(input: &str) -> PResult<ValueDef> {
    let (input, header) = parse_value_header(input)?;
    let (input, _) = tuple((skip_space_and_comment0, tag("{"), skip_space_and_comment0))(input)?;
    let (input, elements) = many0(parse_value_element)(input)?;
    let (input, _) = tuple((skip_space_and_comment0, tag("}")))(input)?;
    Ok((input, ValueDef { header, elements }))
}

/// ```text
/// (101) <value_header> ::= <value_kind> <identifier> [ <value_inheritance_spec> ]
/// ```
fn parse_value_header(input: &str) -> PResult<ValueHeader> {
    let (input, _) = tuple((parse_value_kind, skip_space_and_comment1))(input)?;
    let (input, id) = parse_id(input)?;
    let (input, inheritance) = parse_value_inheritance_spec(input)?;
    Ok((input, ValueHeader { id, inheritance }))
}

/// ```text
/// (102) <value_kind> ::= "valuetype"
/// ```
fn parse_value_kind(input: &str) -> PResult<&str> {
    tag("valuetype")(input)
}

/// ```text
/// (103) <value_inheritance_spec> ::= [ ":" <value_name> ] [ "supports" <interface_name> ]
/// ```
fn parse_value_inheritance_spec(input: &str) -> PResult<ValueInheritanceSpec> {
    fn value_name(input: &str) -> PResult<ScopedName> {
        let (input, _) =
            tuple((skip_space_and_comment0, tag(":"), skip_space_and_comment0))(input)?;
        parse_value_name(input)
    }

    fn supports(input: &str) -> PResult<ScopedName> {
        let (input, _) = tuple((tag("supports"), skip_space_and_comment0))(input)?;
        parse_interface_name(input)
    }

    let (input, value_name) = if let Ok((input, name)) = value_name(input) {
        (input, Some(name))
    } else {
        (input, None)
    };

    let (input, _) = skip_space_and_comment0(input)?;

    let (input, interface_name) = if let Ok((input, name)) = supports(input) {
        (input, Some(name))
    } else {
        (input, None)
    };

    Ok((
        input,
        ValueInheritanceSpec {
            value_name,
            interface_name,
        },
    ))
}

/// ```text
/// (104) <value_name> ::= <scoped_name>
/// ```
fn parse_value_name(input: &str) -> PResult<ScopedName> {
    parse_scoped_name(input)
}

/// ```text
/// (105) <value_element> ::= <export>
///                         | <state_member>
///                         | <init_dcl>
/// ```
fn parse_value_element(input: &str) -> PResult<ValueElement> {
    fn export(input: &str) -> PResult<ValueElement> {
        let (input, result) = parse_export(input)?;
        Ok((input, ValueElement::Export(result)))
    }

    fn state_member(input: &str) -> PResult<ValueElement> {
        let (input, result) = parse_state_member(input)?;
        Ok((input, ValueElement::StateMember(result)))
    }

    fn init_dcl(input: &str) -> PResult<ValueElement> {
        let (input, result) = parse_init_dcl(input)?;
        Ok((input, ValueElement::InitDcl(result)))
    }

    let (input, _) = skip_space_and_comment0(input)?;
    alt((export, state_member, init_dcl))(input)
}

/// ```text
/// (106) <state_member> ::= ( "public" | "private" ) <type_spec> <declarators> ";"
/// ```
fn parse_state_member(input: &str) -> PResult<StateMember> {
    let (input, visibility) = alt((tag("public"), tag("private")))(input)?;
    let (input, _) = skip_space_and_comment1(input)?;

    let (input, type_spec) = parse_type_spec(input)?;

    let (input, _) = skip_space_and_comment1(input)?;
    let (input, ids) = parse_declarators(input)?;

    let (input, _) = tuple((skip_space_and_comment0, tag(";")))(input)?;

    let visibility = match visibility {
        "public" => Visibility::Public,
        "private" => Visibility::Private,
        _ => unreachable!(),
    };

    Ok((
        input,
        StateMember {
            visibility,
            type_spec,
            ids,
        },
    ))
}

/// ```text
/// (107) <init_dcl> ::= "factory" <identifier> "(" [ init_param_dcls ] ")" [ <raises_expr> ] ";"
/// ```
fn parse_init_dcl(input: &str) -> PResult<InitDcl> {
    let (input, _) = tuple((tag("factory"), skip_space_and_comment1))(input)?;
    let (input, id) = parse_id(input)?;

    let (input, params) = delimited(
        tuple((skip_space_and_comment0, tag("("), skip_space_and_comment0)),
        separated_list0(
            tuple((skip_space_and_comment0, tag(","), skip_space_and_comment0)),
            parse_init_param_dcl,
        ),
        tuple((skip_space_and_comment0, tag(")"))),
    )(input)?;

    if let Ok((input, raises)) = parse_raises_expr(input) {
        Ok((
            input,
            InitDcl {
                id,
                params,
                raises: Some(raises),
            },
        ))
    } else {
        Ok((
            input,
            InitDcl {
                id,
                params,
                raises: None,
            },
        ))
    }
}

// ```text
// (108) <init_param_dcls> ::= <init_param_dcl> { "," <init_param_dcl> }*
// ```

/// ```text
/// (109) <init_param_dcl> ::= "in" <type_spec> <simple_declarator>
/// ```
fn parse_init_param_dcl(input: &str) -> PResult<InitParamDcl> {
    let (input, _) = tag("in")(input)?;

    let (input, type_spec) = delimited(
        skip_space_and_comment1,
        parse_type_spec,
        skip_space_and_comment1,
    )(input)?;

    let (input, id) = parse_simple_declarator(input)?;

    Ok((input, InitParamDcl { type_spec, id }))
}

/// ```text
/// (110) <value_forward_dcl> ::= <value_kind> <identifier>
/// ```
fn parse_value_forward_dcl(input: &str) -> PResult<ValueForwardDcl> {
    let (input, _) = tuple((parse_value_kind, skip_space_and_comment1))(input)?;
    let (input, id) = parse_id(input)?;
    Ok((input, ValueForwardDcl(id)))
}

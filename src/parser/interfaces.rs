use super::{
    core::{
        parse_id, parse_member, parse_scoped_name, parse_simple_declarator, parse_type_spec,
        skip_space_and_comment0, skip_space_and_comment1,
    },
    PResult,
};
use crate::expr::{
    AttrDcl, AttrDeclarator, AttrRaises, AttrSpec, ExceptDcl, Export, GetExcep, InterfaceDcl,
    InterfaceDef, InterfaceForwardDcl, InterfaceHeader, OpDcl, OpTypeSpec, ParamAttribute,
    ParamDcl, Raises, ReadonlyAttrDeclarator, ReadonlyAttrSpec, ScopedName, SetExcep, TypeSpec,
};
use nom::{
    branch::alt,
    bytes::complete::tag,
    multi::{many0, many1, separated_list1},
    sequence::{delimited, tuple},
};
use std::mem::take;

/// ```text
/// (72) <except_dcl> ::= "exception" <identifier> "{" <member>*  "}"
/// ```
pub fn parse_except_dcl(input: &str) -> PResult<ExceptDcl> {
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
pub fn parse_interface_dcl(input: &str) -> PResult<InterfaceDcl> {
    fn def(input: &str) -> PResult<InterfaceDcl> {
        let (input, result) = parse_interface_def(input)?;
        Ok((input, InterfaceDcl::Def(result)))
    }

    fn forward_dcl(input: &str) -> PResult<InterfaceDcl> {
        let (input, result) = parse_interface_forward_dcl(input)?;
        Ok((input, InterfaceDcl::ForwardDcl(result)))
    }

    alt((def, forward_dcl))(input)
}

/// ```text
/// (74) <interface_def> ::= <interface_header> "{" <interface_body> "}"
/// ```
fn parse_interface_def(input: &str) -> PResult<InterfaceDef> {
    let (input, header) = parse_interface_header(input)?;

    let (input, body) = delimited(
        tuple((skip_space_and_comment0, tag("{"), skip_space_and_comment0)),
        parse_interface_body,
        tuple((skip_space_and_comment0, tag("}"))),
    )(input)?;

    Ok((input, InterfaceDef { header, body }))
}

/// ```text
/// (75) <interface_forward_dcl> ::= <interface_kind> <identifier>
/// ```
fn parse_interface_forward_dcl(input: &str) -> PResult<InterfaceForwardDcl> {
    let (input, _) = tuple((parse_interface_kind, skip_space_and_comment0))(input)?;
    let (input, id) = parse_id(input)?;
    Ok((input, InterfaceForwardDcl(id)))
}

/// ```text
/// (76) <interface_header> ::= <interface_kind> <identifier> [ <interface_inheritance_spec> ]
/// ```
fn parse_interface_header(input: &str) -> PResult<InterfaceHeader> {
    let (input, _) = tuple((parse_interface_kind, skip_space_and_comment1))(input)?;
    let (input, id) = parse_id(input)?;

    let (input, (_, parents)) =
        tuple((skip_space_and_comment0, parse_interface_inheritance_spec))(input)?;

    Ok((input, InterfaceHeader { id, parents }))
}

/// ```text
/// (77) <interface_kind> ::= "interface"
/// ```
fn parse_interface_kind(input: &str) -> PResult<&str> {
    tag("interface")(input)
}

/// ```text
/// (78) <interface_inheritance_spec> ::= ":" <interface_name> { "," <interface_name> }*
/// ```
fn parse_interface_inheritance_spec(input: &str) -> PResult<Vec<ScopedName>> {
    let (input, _) = tuple((tag(":"), skip_space_and_comment0))(input)?;

    separated_list1(
        tuple((skip_space_and_comment0, tag(","), skip_space_and_comment0)),
        parse_interface_name,
    )(input)
}

/// ```text
/// (79) <interface_name> ::= <scoped_name>
/// ```
fn parse_interface_name(input: &str) -> PResult<ScopedName> {
    parse_scoped_name(input)
}

/// ```text
/// (80) <interface_body> ::= <export>*
/// ```
fn parse_interface_body(input: &str) -> PResult<Vec<Export>> {
    many0(parse_export)(input)
}

/// ```text
/// (81) <export> ::= <op_dcl> ";"
///                 | <attr_dcl> ";"
/// ```
fn parse_export(input: &str) -> PResult<Export> {
    fn op(input: &str) -> PResult<Export> {
        let (input, op) = parse_op_dcl(input)?;
        Ok((input, Export::Op(op)))
    }

    fn attr(input: &str) -> PResult<Export> {
        let (input, attr) = parse_attr_dcl(input)?;
        Ok((input, Export::Attr(attr)))
    }

    let (input, result) = alt((op, attr))(input)?;
    let (input, _) = tuple((skip_space_and_comment0, tag(";")))(input)?;

    Ok((input, result))
}

/// ```text
/// (82) <op_dcl> ::= <op_type_spec> <identifier> "(" [ <parameter_dcls> ] ")" [ <raises_expr> ]
/// ```
fn parse_op_dcl(input: &str) -> PResult<OpDcl> {
    let (input, type_spec) = parse_op_type_spec(input)?;

    let (input, _) = skip_space_and_comment1(input)?;
    let (input, id) = parse_id(input)?;

    let (input, params) = delimited(
        tuple((skip_space_and_comment0, tag("("), skip_space_and_comment0)),
        parse_parameter_dcls,
        tuple((skip_space_and_comment0, tag(")"), skip_space_and_comment0)),
    )(input)?;

    if let Ok((input, raises)) = parse_raises_expr(input) {
        Ok((
            input,
            OpDcl {
                id,
                type_spec,
                params,
                raises: Some(raises),
            },
        ))
    } else {
        Ok((
            input,
            OpDcl {
                id,
                type_spec,
                params,
                raises: None,
            },
        ))
    }
}

/// ```text
/// (83) <op_type_spec> ::= <type_spec>
///                       | "void"
/// ```
fn parse_op_type_spec(input: &str) -> PResult<OpTypeSpec> {
    let (input, type_spec) = parse_type_spec(input)?;

    if let TypeSpec::ScopedName(name) = &type_spec {
        if name.ids.len() == 1 && name.ids[0] == "void" {
            return Ok((input, OpTypeSpec::Void));
        }
    }

    Ok((input, OpTypeSpec::TypeSpec(type_spec)))
}

/// ```text
/// (84) <parameter_dcls> ::= <param_dcl> { "," <param_dcl> }*
/// ```
fn parse_parameter_dcls(input: &str) -> PResult<Vec<ParamDcl>> {
    separated_list1(
        tuple((skip_space_and_comment0, tag(","), skip_space_and_comment0)),
        parse_param_dcl,
    )(input)
}

/// ```text
/// (85) <param_dcl> ::= <param_attribute> <type_spec> <simple_declarator>
/// ```
fn parse_param_dcl(input: &str) -> PResult<ParamDcl> {
    let (input, attr) = parse_param_attribute(input)?;

    let (input, _) = skip_space_and_comment1(input)?;
    let (input, type_spec) = parse_type_spec(input)?;

    let (input, _) = skip_space_and_comment1(input)?;
    let (input, id) = parse_simple_declarator(input)?;

    Ok((
        input,
        ParamDcl {
            attr,
            type_spec,
            id,
        },
    ))
}

/// ```text
/// (86) <param_attribute> ::= "in"
///                          | "out"
///                          | "inout"
/// ```
fn parse_param_attribute(input: &str) -> PResult<ParamAttribute> {
    let (input, c) = alt((tag("in"), tag("out"), tag("inout")))(input)?;
    match c {
        "in" => Ok((input, ParamAttribute::In)),
        "out" => Ok((input, ParamAttribute::Out)),
        "inout" => Ok((input, ParamAttribute::InOut)),
        _ => unreachable!(),
    }
}

/// ```text
/// (87) <raises_expr> ::= "raises" "(" <scoped_name> { "," <scoped_name> }* ")"
/// ```
fn parse_raises_expr(input: &str) -> PResult<Raises> {
    let (input, _) = tuple((
        tag("raises"),
        skip_space_and_comment0,
        tag("("),
        skip_space_and_comment0,
    ))(input)?;

    let (input, names) = separated_list1(
        tuple((skip_space_and_comment0, tag(","), skip_space_and_comment0)),
        parse_scoped_name,
    )(input)?;

    Ok((input, Raises(names)))
}

/// ```text
/// (88) <attr_dcl> ::= <readonly_attr_spec>
///                   | <attr_spec>
/// ```
fn parse_attr_dcl(input: &str) -> PResult<AttrDcl> {
    fn readonly(input: &str) -> PResult<AttrDcl> {
        let (input, spec) = parse_readonly_attr_spec(input)?;
        Ok((input, AttrDcl::Readonly(spec)))
    }

    fn read_write(input: &str) -> PResult<AttrDcl> {
        let (input, spec) = parse_attr_spec(input)?;
        Ok((input, AttrDcl::ReadWrite(spec)))
    }

    alt((readonly, read_write))(input)
}

/// ```text
/// (89) <readonly_attr_spec> ::= "readonly" "attribute" <type_spec> <readonly_attr_declarator>
/// ```
fn parse_readonly_attr_spec(input: &str) -> PResult<ReadonlyAttrSpec> {
    let (input, _) = tuple((
        tag("readonly"),
        skip_space_and_comment1,
        tag("attribute"),
        skip_space_and_comment1,
    ))(input)?;

    let (input, type_spec) = parse_type_spec(input)?;

    let (input, _) = skip_space_and_comment1(input)?;
    let (input, declarator) = parse_readonly_attr_declarator(input)?;

    Ok((
        input,
        ReadonlyAttrSpec {
            type_spec,
            declarator,
        },
    ))
}

/// ```text
/// (90) <readonly_attr_declarator> ::= <simple_declarator> <raises_expr>
///                                   | <simple_declarator> { "," <simple_declarator> }*
/// ```
fn parse_readonly_attr_declarator(input: &str) -> PResult<ReadonlyAttrDeclarator> {
    let (input, mut ids) = separated_list1(
        tuple((skip_space_and_comment0, tag(","), skip_space_and_comment0)),
        parse_simple_declarator,
    )(input)?;

    if ids.len() == 1 {
        if let Ok((input, raises)) = parse_raises_expr(input) {
            let s = take(&mut ids[0]);
            return Ok((input, ReadonlyAttrDeclarator::WithRaises(s, raises)));
        }
    }

    Ok((input, ReadonlyAttrDeclarator::IDs(ids)))
}

/// ```text
/// (91) <attr_spec> ::= "attribute" <type_spec> <attr_declarator>
/// ```
fn parse_attr_spec(input: &str) -> PResult<AttrSpec> {
    let (input, _) = tuple((tag("attribute"), skip_space_and_comment1))(input)?;
    let (input, type_spec) = parse_type_spec(input)?;

    let (input, _) = skip_space_and_comment1(input)?;
    let (input, declarator) = parse_attr_declarator(input)?;

    Ok((
        input,
        AttrSpec {
            type_spec,
            declarator,
        },
    ))
}

/// ```text
/// (92) <attr_declarator> ::= <simple_declarator> <attr_raises_expr>
///                          | <simple_declarator> { "," <simple_declarator> }*
/// ```
fn parse_attr_declarator(input: &str) -> PResult<AttrDeclarator> {
    let (input, mut ids) = separated_list1(
        tuple((skip_space_and_comment0, tag(","), skip_space_and_comment0)),
        parse_simple_declarator,
    )(input)?;

    if ids.len() == 1 {
        if let Ok((input, (_, raises))) =
            tuple((skip_space_and_comment1, parse_attr_raises_expr))(input)
        {
            let s = take(&mut ids[0]);
            return Ok((input, AttrDeclarator::WithRaises(s, raises)));
        }
    }

    Ok((input, AttrDeclarator::IDs(ids)))
}

/// ```text
/// (93) <attr_raises_expr> ::= <get_excep_expr> [ <set_excep_expr> ]
///                           | <set_excep_expr>
/// ```
fn parse_attr_raises_expr(input: &str) -> PResult<AttrRaises> {
    fn get_excep(input: &str) -> PResult<AttrRaises> {
        let (input, get_excep) = parse_get_excep_expr(input)?;
        let (input, _) = skip_space_and_comment0(input)?;
        if let Ok((input, set_excep)) = parse_set_excep_expr(input) {
            Ok((input, AttrRaises::GetExcep(get_excep, Some(set_excep))))
        } else {
            Ok((input, AttrRaises::GetExcep(get_excep, None)))
        }
    }

    fn set_excep(input: &str) -> PResult<AttrRaises> {
        let (input, set_excep) = parse_set_excep_expr(input)?;
        Ok((input, AttrRaises::SetExcep(set_excep)))
    }

    alt((get_excep, set_excep))(input)
}

/// ```text
/// (94) <get_excep_expr> ::= "getraises" <exception_list>
/// ```
fn parse_get_excep_expr(input: &str) -> PResult<GetExcep> {
    let (input, _) = tuple((tag("getraises"), skip_space_and_comment0))(input)?;
    let (input, names) = parse_exception_list(input)?;
    Ok((input, GetExcep(names)))
}

/// ```text
/// (95) <set_excep_expr> ::= "setraises" <exception_list>
/// ```
fn parse_set_excep_expr(input: &str) -> PResult<SetExcep> {
    let (input, _) = tuple((tag("setraises"), skip_space_and_comment0))(input)?;
    let (input, names) = parse_exception_list(input)?;
    Ok((input, SetExcep(names)))
}

/// ```text
/// (96) <exception_list> ::= "(" <scoped_name> { "," <scoped_name> }* ")"
/// ```
fn parse_exception_list(input: &str) -> PResult<Vec<ScopedName>> {
    delimited(
        tuple((tag("("), skip_space_and_comment0)),
        separated_list1(
            tuple((skip_space_and_comment0, tag(","), skip_space_and_comment0)),
            parse_scoped_name,
        ),
        tuple((skip_space_and_comment0, tag(")"))),
    )(input)
}
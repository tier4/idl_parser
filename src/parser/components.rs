use super::{
    core::{
        parse_id, parse_scoped_name, parse_simple_declarator, parse_type_spec,
        skip_space_and_comment0, skip_space_and_comment1,
    },
    interfaces::{parse_export, parse_raises_expr},
    PResult,
};
use crate::{
    expr::{
        CompoentExport, ComponentDcl, ComponentDef, ComponentForwardDcl, ComponentHeader,
        ComponentInheritanceSpec, FactoryDcl, FactoryParamDcl, HomeDcl, HomeExport, HomeHeader,
        HomeInheritanceSpec, InterfaceType, ProvidesDcl, UsesDcl,
    },
    parser::interfaces::parse_attr_dcl,
};
use nom::{
    branch::alt,
    bytes::complete::tag,
    multi::{many0, separated_list0},
    sequence::{delimited, tuple},
};

/// ```text
/// (134) <component_dcl> ::= <component_def>
///                         | <component_forward_dcl>
/// ```
pub fn parse_component_dcl(input: &str) -> PResult<ComponentDcl> {
    fn def(input: &str) -> PResult<ComponentDcl> {
        let (input, result) = parse_component_def(input)?;
        Ok((input, ComponentDcl::Def(result)))
    }

    fn forward_dcl(input: &str) -> PResult<ComponentDcl> {
        let (input, result) = parse_component_forward_dcl(input)?;
        Ok((input, ComponentDcl::ForwardDcl(result)))
    }

    alt((def, forward_dcl))(input)
}

/// ```text
/// (135) <component_forward_dcl> ::= "component" <identifier>
/// ```
fn parse_component_forward_dcl(input: &str) -> PResult<ComponentForwardDcl> {
    let (input, _) = tuple((tag("component"), skip_space_and_comment1))(input)?;
    let (input, id) = parse_id(input)?;
    Ok((input, ComponentForwardDcl(id)))
}

/// ```text
/// (136) <component_def> ::= <component_header> "{" <component_body> "}"
/// ```
fn parse_component_def(input: &str) -> PResult<ComponentDef> {
    let (input, header) = parse_component_header(input)?;

    let (input, body) = delimited(
        tuple((skip_space_and_comment0, tag("{"), skip_space_and_comment0)),
        parse_componet_body,
        tuple((skip_space_and_comment0, tag("}"))),
    )(input)?;

    Ok((input, ComponentDef { header, body }))
}

/// ```text
/// (137) <component_header> ::= "component" <identifier> [ <component_inheritance_spec> ]
/// ```
fn parse_component_header(input: &str) -> PResult<ComponentHeader> {
    let (input, _) = tuple((tag("component"), skip_space_and_comment1))(input)?;
    let (input, id) = parse_id(input)?;

    let (input, inheritance) = if let Ok((input, (_, result))) =
        tuple((skip_space_and_comment0, parse_component_inheritance_spec))(input)
    {
        (input, Some(result))
    } else {
        (input, None)
    };

    Ok((input, ComponentHeader { id, inheritance }))
}

/// ```text
/// (138) <component_inheritance_spec> ::= ":" <scoped_name>
/// ```
fn parse_component_inheritance_spec(input: &str) -> PResult<ComponentInheritanceSpec> {
    let (input, _) = tuple((tag(":"), skip_space_and_comment0))(input)?;
    let (input, name) = parse_scoped_name(input)?;
    Ok((input, ComponentInheritanceSpec(name)))
}

/// ```text
/// (139) <component_body> ::= <component_export>*
/// ```
fn parse_componet_body(input: &str) -> PResult<Vec<CompoentExport>> {
    many0(parse_compoent_export)(input)
}

/// ```text
/// (140) <component_export> ::= <provides_dcl> ";"
///                            | <uses_dcl> ";"
///                            | <attr_dcl> ";"
/// ```
fn parse_compoent_export(input: &str) -> PResult<CompoentExport> {
    fn provides(input: &str) -> PResult<CompoentExport> {
        let (input, result) = parse_provides_dcl(input)?;
        let (input, _) = tuple((skip_space_and_comment0, tag(";")))(input)?;
        Ok((input, CompoentExport::Provides(result)))
    }

    fn uses(input: &str) -> PResult<CompoentExport> {
        let (input, result) = parse_uses_dcl(input)?;
        let (input, _) = tuple((skip_space_and_comment0, tag(";")))(input)?;
        Ok((input, CompoentExport::Uses(result)))
    }

    fn attr(input: &str) -> PResult<CompoentExport> {
        let (input, result) = parse_attr_dcl(input)?;
        let (input, _) = tuple((skip_space_and_comment0, tag(";")))(input)?;
        Ok((input, CompoentExport::Attr(result)))
    }

    let (input, _) = skip_space_and_comment0(input)?;
    alt((provides, uses, attr))(input)
}

/// ```text
/// (141) <provides_dcl> ::= "provides" <interface_type> <identifier>
/// ```
fn parse_provides_dcl(input: &str) -> PResult<ProvidesDcl> {
    let (input, _) = tuple((tag("provides"), skip_space_and_comment1))(input)?;
    let (input, interface_type) = parse_interface_type(input)?;
    let (input, (_, id)) = tuple((skip_space_and_comment1, parse_id))(input)?;

    Ok((input, ProvidesDcl { interface_type, id }))
}

/// ```text
/// (142) <interface_type> ::= <scoped_name>
/// ```
fn parse_interface_type(input: &str) -> PResult<InterfaceType> {
    let (input, name) = parse_scoped_name(input)?;
    Ok((input, InterfaceType(name)))
}

/// ```text
/// (143) <uses_dcl> ::= "uses" <interface_type> <identifier>
/// ```
fn parse_uses_dcl(input: &str) -> PResult<UsesDcl> {
    let (input, (_, _, interface_type, _, id)) = tuple((
        tag("uses"),
        skip_space_and_comment1,
        parse_interface_type,
        skip_space_and_comment1,
        parse_id,
    ))(input)?;

    Ok((input, UsesDcl { interface_type, id }))
}

/// ```text
/// (145) <home_dcl> ::= <home_header> "{" <home_body> "}"
/// ```
pub fn parse_home_dcl(input: &str) -> PResult<HomeDcl> {
    let (input, header) = parse_home_header(input)?;

    let (input, body) = delimited(
        tuple((skip_space_and_comment0, tag("{"), skip_space_and_comment0)),
        parse_home_body,
        tuple((skip_space_and_comment0, tag("}"))),
    )(input)?;

    Ok((input, HomeDcl { header, body }))
}

/// ```text
/// (146) <home_header> ::= "home" <identifier> [ <home_inheritance_spec> ] "manages" <scoped_name>
/// ```
fn parse_home_header(input: &str) -> PResult<HomeHeader> {
    let (input, _) = tuple((tag("home"), skip_space_and_comment1))(input)?;

    let (input, id) = parse_id(input)?;

    let (input, inheritance) = if let Ok((input, (_, result))) =
        tuple((skip_space_and_comment0, parse_home_inheritance_spec))(input)
    {
        (input, Some(result))
    } else {
        (input, None)
    };

    let (input, _) = tuple((
        skip_space_and_comment1,
        tag("manages"),
        skip_space_and_comment1,
    ))(input)?;

    let (input, manages) = parse_scoped_name(input)?;

    Ok((
        input,
        HomeHeader {
            id,
            inheritance,
            manages,
        },
    ))
}

/// ```text
/// (147) <home_inheritance_spec> ::= ":" <scoped_name>
/// ```
fn parse_home_inheritance_spec(input: &str) -> PResult<HomeInheritanceSpec> {
    let (input, _) = tuple((tag(":"), skip_space_and_comment0))(input)?;
    let (input, name) = parse_scoped_name(input)?;
    Ok((input, HomeInheritanceSpec(name)))
}

/// ```text
/// (148) <home_body> ::= <home_export>*
/// ```
fn parse_home_body(input: &str) -> PResult<Vec<HomeExport>> {
    many0(parse_home_export)(input)
}

/// ```text
/// (149) <home_export> ::= <export>
///                       | <factory_dcl> ";"
/// ```
fn parse_home_export(input: &str) -> PResult<HomeExport> {
    fn export(input: &str) -> PResult<HomeExport> {
        let (input, result) = parse_export(input)?;
        Ok((input, HomeExport::Export(result)))
    }

    fn factory_dcl(input: &str) -> PResult<HomeExport> {
        let (input, result) = parse_factory_dcl(input)?;
        Ok((input, HomeExport::FactoryDcl(result)))
    }

    let (input, _) = skip_space_and_comment0(input)?;
    alt((export, factory_dcl))(input)
}

/// ```text
/// (150) <factory_dcl> ::= "factory" <identifier> "(" [ <factory_param_dcls> ] ")" [ <raises_expr> ]
/// ```
fn parse_factory_dcl(input: &str) -> PResult<FactoryDcl> {
    let (input, _) = tuple((tag("factory"), skip_space_and_comment1))(input)?;

    let (input, id) = parse_simple_declarator(input)?;

    let (input, params) = delimited(
        tuple((skip_space_and_comment0, tag("("), skip_space_and_comment0)),
        separated_list0(
            tuple((skip_space_and_comment0, tag(","), skip_space_and_comment0)),
            parse_factory_param_dcl,
        ),
        tuple((skip_space_and_comment0, tag(")"))),
    )(input)?;

    let (input, raises) = if let Ok((input, raises)) = parse_raises_expr(input) {
        (input, Some(raises))
    } else {
        (input, None)
    };

    Ok((input, FactoryDcl { id, params, raises }))
}

// ```text
// (151) <factory_param_dcls> ::= <factory_param_dcl> { ","  <factory_param_dcl> }*
// ```

/// ```text
/// (152) <factory_param_dcl> ::= "in" <type_spec> <simple_declarator>
/// ```
fn parse_factory_param_dcl(input: &str) -> PResult<FactoryParamDcl> {
    let (input, _) = tuple((tag("in"), skip_space_and_comment1))(input)?;
    let (input, type_spec) = parse_type_spec(input)?;
    let (input, id) = parse_simple_declarator(input)?;
    Ok((input, FactoryParamDcl { type_spec, id }))
}

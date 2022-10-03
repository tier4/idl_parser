use super::{
    core::{parse_scoped_name, skip_space_and_comment0},
    PResult,
};
use crate::{
    expr::{
        ActualParameter, FormalParameter, FormalParameterType, TemplateModuleDcl,
        TemplateModuleInst, TemplateModuleRef, TplDefinition,
    },
    parser::core::{
        parse_const_expr, parse_const_type, parse_definition, parse_id, parse_sequence_type,
        parse_type_spec, skip_space_and_comment1,
    },
};
use nom::{
    branch::alt,
    bytes::complete::tag,
    multi::{many1, separated_list1},
    sequence::{delimited, tuple},
};

/// ```text
/// (185) <template_module_dcl> ::= "module" <identifier> "<" <formal_parameters> ">" "{" <tpl_definition>+ "}"
/// ```
pub fn parse_template_module_dcl(input: &str) -> PResult<TemplateModuleDcl> {
    let (input, (_, _, id)) = tuple((tag("module"), skip_space_and_comment1, parse_id))(input)?;

    let (input, parameters) = delimited(
        tuple((skip_space_and_comment0, tag("<"), skip_space_and_comment0)),
        parse_formal_parameters,
        tuple((skip_space_and_comment0, tag(">"), skip_space_and_comment0)),
    )(input)?;

    let (input, definitions) = delimited(
        tuple((tag("{"), skip_space_and_comment0)),
        many1(parse_tpl_definition),
        tuple((skip_space_and_comment0, tag("}"))),
    )(input)?;

    Ok((
        input,
        TemplateModuleDcl {
            id,
            parameters,
            definitions,
        },
    ))
}

/// ```text
/// (186) <formal_parameters> ::= <formal_parameter> {"," <formal_parameter>}*
/// ```
fn parse_formal_parameters(input: &str) -> PResult<Vec<FormalParameter>> {
    separated_list1(
        tuple((skip_space_and_comment0, tag(","), skip_space_and_comment0)),
        parse_formal_parameter,
    )(input)
}

/// ```text
/// (187) <formal_parameter> ::= <formal_parameter_type> <identifier>
/// ```
fn parse_formal_parameter(input: &str) -> PResult<FormalParameter> {
    let (input, parameter_type) = parse_formal_parameter_type(input)?;
    let (input, _) = skip_space_and_comment1(input)?;
    let (input, id) = parse_id(input)?;
    Ok((input, FormalParameter { id, parameter_type }))
}

/// ```text
/// (188) <formal_parameter_type> ::= "typename" | "interface" | "valuetype" | "eventtype"
///                                 | "struct" | "union" | "exception" | "enum" | "sequence"
///                                 | "const <const_type>"
///                                 | <sequence_type>
/// ```
fn parse_formal_parameter_type(input: &str) -> PResult<FormalParameterType> {
    fn const_type(input: &str) -> PResult<FormalParameterType> {
        let (input, _) = tuple((tag("const"), skip_space_and_comment1))(input)?;
        let (input, const_type) = parse_const_type(input)?;
        Ok((input, FormalParameterType::Const(const_type)))
    }

    fn sequence_type(input: &str) -> PResult<FormalParameterType> {
        let (input, sequence_type) = parse_sequence_type(input)?;
        Ok((input, FormalParameterType::SequenceType(sequence_type)))
    }

    fn types(input: &str) -> PResult<FormalParameterType> {
        let (input, c) = alt((
            tag("typename"),
            tag("interface"),
            tag("valuetype"),
            tag("eventtype"),
            tag("struct"),
            tag("union"),
            tag("exception"),
            tag("enum"),
            tag("sequence"),
        ))(input)?;

        match c {
            "typename" => Ok((input, FormalParameterType::Typename)),
            "interface" => Ok((input, FormalParameterType::Interface)),
            "valuetype" => Ok((input, FormalParameterType::ValueType)),
            "eventtype" => Ok((input, FormalParameterType::EventType)),
            "struct" => Ok((input, FormalParameterType::Struct)),
            "union" => Ok((input, FormalParameterType::Union)),
            "exception" => Ok((input, FormalParameterType::Exception)),
            "enum" => Ok((input, FormalParameterType::Enum)),
            "sequence" => Ok((input, FormalParameterType::Sequence)),
            _ => unreachable!(),
        }
    }

    alt((const_type, sequence_type, types))(input)
}

/// ```text
/// (189) <tpl_definition> ::= <definition>
///                          | <template_module_ref> ";"
/// ```
fn parse_tpl_definition(input: &str) -> PResult<TplDefinition> {
    fn definition(input: &str) -> PResult<TplDefinition> {
        let (input, result) = parse_definition(input)?;
        Ok((input, TplDefinition::Definition(result)))
    }

    fn template_module_ref(input: &str) -> PResult<TplDefinition> {
        let (input, result) = parse_template_module_ref(input)?;
        let (input, _) = tuple((skip_space_and_comment0, tag(";")))(input)?;
        Ok((input, TplDefinition::TemplateModuleRef(result)))
    }

    let (input, _) = skip_space_and_comment0(input)?;
    alt((definition, template_module_ref))(input)
}

/// ```text
/// (190) <template_module_inst> ::= "module" <scoped_name> "<" <actual_parameters> ">" <identifier>
/// ```
pub fn parse_template_module_inst(input: &str) -> PResult<TemplateModuleInst> {
    let (input, name) = delimited(
        tuple((tag("module"), skip_space_and_comment1)),
        parse_scoped_name,
        skip_space_and_comment0,
    )(input)?;

    let (input, parameters) = delimited(
        tuple((tag("<"), skip_space_and_comment0)),
        parse_actual_parameters,
        tuple((skip_space_and_comment0, tag(">"))),
    )(input)?;

    Ok((input, TemplateModuleInst { name, parameters }))
}

/// ```text
/// (191) <actual_parameters> ::= <actual_parameter> { "," <actual_parameter> }*
/// ```
fn parse_actual_parameters(input: &str) -> PResult<Vec<ActualParameter>> {
    separated_list1(
        tuple((skip_space_and_comment0, tag(","), skip_space_and_comment0)),
        parse_actual_parameter,
    )(input)
}

/// ```text
/// (192) <actual_parameter> ::= <type_spec>
///                            | <const_expr>
/// ```
fn parse_actual_parameter(input: &str) -> PResult<ActualParameter> {
    fn type_spec(input: &str) -> PResult<ActualParameter> {
        let (input, result) = parse_type_spec(input)?;
        Ok((input, ActualParameter::TypeSpec(result)))
    }

    fn const_expr(input: &str) -> PResult<ActualParameter> {
        let (input, result) = parse_const_expr(input)?;
        Ok((input, ActualParameter::ConstExpr(result)))
    }

    alt((type_spec, const_expr))(input)
}

/// ```text
/// (193) <template_module_ref> ::= "alias" <scoped_name> "<" <formal_parameter_names> ">" <identifier>
/// ```
fn parse_template_module_ref(input: &str) -> PResult<TemplateModuleRef> {
    let (input, name) = delimited(
        tuple((tag("alias"), skip_space_and_comment1)),
        parse_scoped_name,
        skip_space_and_comment0,
    )(input)?;

    let (input, parameters) = parse_formal_parameter_names(input)?;
    let (input, _) = skip_space_and_comment0(input)?;
    let (input, id) = parse_id(input)?;

    Ok((
        input,
        TemplateModuleRef {
            name,
            parameters,
            id,
        },
    ))
}

/// ```text
/// (194) <formal_parameter_names> ::= <identifier> {"," <identifier>}*
/// ```
fn parse_formal_parameter_names(input: &str) -> PResult<Vec<String>> {
    separated_list1(
        tuple((skip_space_and_comment0, tag(","), skip_space_and_comment0)),
        parse_id,
    )(input)
}

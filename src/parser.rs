use crate::{
    character::{BELL, BS, CR, CR_S, FF, HT, HT_S, LF, LF_S, VT, VT_S},
    expr::{
        AnyDeclarator, ArrayDeclarator, BaseType, Case, CaseLabel, ConstDcl, ConstExpr, ConstType,
        ConstrTypeDcl, Definition, ElementSpec, EnumDcl, FixedPoint, FixedPtType,
        FloatingPointType, IntegerType, Literal, Member, Module, ScopedName, SequenceType,
        StringType, StructDcl, StructDef, StructForwardDcl, SwitchTypeSpec, TemplateTypeSpec,
        TypeSpec, Typedef, TypedefType, UnaryOpExpr, UnionDcl, UnionDef, UnionForwardDcl,
        WStringType,
    },
};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while},
    character::{
        complete::{anychar, digit0, digit1, hex_digit1, oct_digit0, one_of, satisfy},
        is_alphabetic, is_alphanumeric, is_hex_digit, is_oct_digit,
    },
    combinator::{eof, fail},
    error::{Error, VerboseError},
    multi::{many0, many1, separated_list1},
    sequence::{delimited, tuple},
    IResult,
};
use num_bigint::BigInt;
use num_traits::Zero;

type PResult<'a, OUT> = IResult<&'a str, OUT, VerboseError<&'a str>>;

pub fn parse(input: &str) -> PResult<Literal> {
    parse_literal(input)
}

fn parse_comment(input: &str) -> PResult<&str> {
    let (input, c) = alt((tag("//"), tag("/*")))(input)?;

    let (input, comment) = match c {
        "//" => alt((tag("\n"), eof))(input)?,
        "/*" => {
            let (input, c) = take_until("*/")(input)?;
            let (input, _) = tag("*/")(input)?;
            (input, c)
        }
        _ => unreachable!(),
    };

    Ok((input, comment))
}

/// ```text
/// <definition> ::= <module_dcl> ";"
///                | <const_dcl> ";"
///                | <type_dcl> ";"
/// ```
fn parse_definition(input: &str) -> PResult<Definition> {
    todo!()
}

/// ```text
/// (64) <type_declarator> ::= { <simple_type_spec> | <template_type_spec> | <constr_type_dcl> } <any_declarators>
/// (65) <any_declalators> ::= <any_declarator> { "," <any_declarator> }*
/// ```
fn parse_type_declarator(input: &str) -> PResult<Typedef> {
    fn simple(input: &str) -> PResult<TypedefType> {
        let (input, t) = parse_simple_type_spec(input)?;
        Ok((input, TypedefType::Simple(t)))
    }

    fn template(input: &str) -> PResult<TypedefType> {
        let (input, t) = parse_template_type_spec(input)?;
        Ok((input, TypedefType::Template(t)))
    }

    fn constr(input: &str) -> PResult<TypedefType> {
        let (input, t) = parse_constr_type_dcl(input)?;
        Ok((input, TypedefType::Constr(t)))
    }

    // <simple_type_spec> | <template_type_spec> | <constr_type_dcl>
    let (input, type_dcl) = alt((simple, template, constr))(input)?;

    let (input, _) = skip_space_and_comment1(input)?;

    // <any_declalators>
    let (input, declarators) = separated_list1(tag(","), parse_any_declarator)(input)?;

    Ok((
        input,
        Typedef {
            type_dcl,
            declarators,
        },
    ))
}

/// ```text
/// (66) <any_declarator> ::= <simple_declarator>
///                         | <array_declarator>
/// (59) <array_declarator> ::= <identifier> <fixed_array_size>+
/// ```
fn parse_any_declarator(input: &str) -> PResult<AnyDeclarator> {
    let (input, id) = parse_id(input)?;

    let (input, array_size) = many0(parse_fixed_array_size)(input)?;

    if array_size.is_empty() {
        Ok((input, AnyDeclarator::Simple(id)))
    } else {
        Ok((
            input,
            AnyDeclarator::Array(ArrayDeclarator { id, array_size }),
        ))
    }
}

/// ```text
/// (60) <fixed_array_size> ::= "[" <positive_int_const> "]"
/// ```
fn parse_fixed_array_size(input: &str) -> PResult<ConstExpr> {
    delimited(
        tuple((skip_space_and_comment0, tag("["), skip_space_and_comment0)),
        parse_const_expr,
        tuple((skip_space_and_comment0, tag("]"))),
    )(input)
}

/// ```text
/// (38) <template_type_spec> ::= <sequence_type>
///                             | <string_type>
///                             | <wide_string_type>
///                             | <fixed_pt_type>
/// ```
fn parse_template_type_spec(input: &str) -> PResult<TemplateTypeSpec> {
    fn sequence(input: &str) -> PResult<TemplateTypeSpec> {
        let (input, t) = parse_sequence_type(input)?;
        Ok((input, TemplateTypeSpec::Sequence(t)))
    }

    fn string(input: &str) -> PResult<TemplateTypeSpec> {
        let (input, t) = parse_string_type(input)?;
        Ok((input, TemplateTypeSpec::String(t)))
    }

    fn wide_string(input: &str) -> PResult<TemplateTypeSpec> {
        let (input, t) = parse_wstring_type(input)?;
        Ok((input, TemplateTypeSpec::WString(t)))
    }

    fn fixed_pt(input: &str) -> PResult<TemplateTypeSpec> {
        let (input, t) = parse_fixed_pt_type(input)?;
        Ok((input, TemplateTypeSpec::FixedPoint(t)))
    }

    alt((sequence, string, wide_string, fixed_pt))(input)
}

/// ```text
/// <fixed_pt_type> ::= "fixed" "<" <positive_int_const> "," <positive_int_const> ">"
/// ```
fn parse_fixed_pt_type(input: &str) -> PResult<FixedPtType> {
    let (input, _) = tag("fixed")(input)?;

    let (input, _) = skip_space_and_comment0(input)?;
    let (input, _) = tag("<")(input)?;

    let (input, _) = skip_space_and_comment0(input)?;
    let (input, total_digits) = parse_const_expr(input)?;

    let (input, _) = skip_space_and_comment0(input)?;
    let (input, _) = tag(",")(input)?;

    let (input, _) = skip_space_and_comment0(input)?;
    let (input, fractional_digits) = parse_const_expr(input)?;

    let (input, _) = skip_space_and_comment0(input)?;
    let (input, _) = tag(">")(input)?;

    Ok((
        input,
        FixedPtType {
            total_digits,
            fractional_digits,
        },
    ))
}

/// ```text
/// <sequence_type> ::= "sequence" "<" <type_spec> "," <positive_int_const> ">"
///                   | "sequence" "<" <type_spec> ">"
/// ```
fn parse_sequence_type(input: &str) -> PResult<SequenceType> {
    let (input, _) = tag("sequence")(input)?;

    let (input, _) = skip_space_and_comment0(input)?;
    let (input, type_spec) = parse_type_spec(input)?;

    let (input, c) = alt((tag(","), tag(">")))(input)?;

    match c {
        "," => {
            let (input, _) = skip_space_and_comment0(input)?;
            let (input, expr) = parse_const_expr(input)?;

            let (input, _) = skip_space_and_comment0(input)?;
            let (input, _) = tag(">")(input)?;

            Ok((input, SequenceType::Limited(type_spec, expr)))
        }
        ">" => {
            let (input, _) = skip_space_and_comment0(input)?;
            let (input, _) = tag(">")(input)?;

            Ok((input, SequenceType::Unlimited(type_spec)))
        }
        _ => unreachable!(),
    }
}

/// ```text
/// <constr_type_dcl> ::= <struct_dcl>
///                     | <union_dcl>
///                     | <enum_dcl>
/// ```
fn parse_constr_type_dcl(input: &str) -> PResult<ConstrTypeDcl> {
    fn struct_type(input: &str) -> PResult<ConstrTypeDcl> {
        let (input, t) = parse_struct_dcl(input)?;
        Ok((input, ConstrTypeDcl::Struct(t)))
    }

    fn enum_type(input: &str) -> PResult<ConstrTypeDcl> {
        let (input, t) = parse_enum_dcl(input)?;
        Ok((input, ConstrTypeDcl::Enum(t)))
    }

    fn union_type(input: &str) -> PResult<ConstrTypeDcl> {
        let (input, t) = parse_union_dcl(input)?;
        Ok((input, ConstrTypeDcl::Union(t)))
    }

    alt((struct_type, enum_type, union_type))(input)
}

/// ```text
/// <union_dcl> ::= <union_def>
///               | <union_forward_dcl>
/// ```
fn parse_union_dcl(input: &str) -> PResult<UnionDcl> {
    fn union_def(input: &str) -> PResult<UnionDcl> {
        let (input, def) = parse_union_def(input)?;
        Ok((input, UnionDcl::Def(def)))
    }

    fn forward_dcl(input: &str) -> PResult<UnionDcl> {
        let (input, dcl) = parse_union_forward_dcl(input)?;
        Ok((input, UnionDcl::ForwardDcl(dcl)))
    }

    alt((union_def, forward_dcl))(input)
}

/// ```text
/// <union_forward_dcl> ::= "union" <identifier>
/// ```
fn parse_union_forward_dcl(input: &str) -> PResult<UnionForwardDcl> {
    // "union"
    let (input, _) = tag("union")(input)?;

    let (input, _) = skip_space_and_comment1(input)?;
    let (input, id) = parse_id(input)?;

    Ok((input, UnionForwardDcl(id)))
}

/// ```text
/// <union_def> ::= "union" <identifier> "switch" "(" <switch_type_spec> ")" "{" <switch_body> "}"
/// ```
fn parse_union_def(input: &str) -> PResult<UnionDef> {
    // "union"
    let (input, _) = tag("union")(input)?;

    // <identifier>
    let (input, _) = skip_space_and_comment1(input)?;
    let (input, id) = parse_id(input)?;

    // "switch"
    let (input, _) = skip_space_and_comment1(input)?;
    let (input, _) = tag("switch")(input)?;

    // "(" <switch_type_spec> ")"
    let (input, switch_type_spec) = delimited(
        tuple((skip_space_and_comment0, tag("("), skip_space_and_comment0)),
        parse_switch_type_spec,
        tuple((skip_space_and_comment0, tag(")"))),
    )(input)?;

    // "{" <switch_body> "}"
    let (input, body) = delimited(
        tuple((skip_space_and_comment0, tag("{"), skip_space_and_comment0)),
        parse_switch_body,
        tuple((skip_space_and_comment0, tag("}"))),
    )(input)?;

    Ok((
        input,
        UnionDef {
            id,
            switch_type_spec,
            body,
        },
    ))
}

/// ```text
/// <switch_type_spec> ::= <integer_type>
///                      | <char_type>
///                      | <boolean_type>
///                      | <scoped_name>
/// ```
fn parse_switch_type_spec(input: &str) -> PResult<SwitchTypeSpec> {
    fn integer_type(input: &str) -> PResult<SwitchTypeSpec> {
        let (input, int_type) = parse_integer_type(input)?;
        Ok((input, SwitchTypeSpec::Integer(int_type)))
    }

    fn char_type(input: &str) -> PResult<SwitchTypeSpec> {
        let (input, _) = tag("char")(input)?;
        Ok((input, SwitchTypeSpec::Char))
    }

    fn boolean_type(input: &str) -> PResult<SwitchTypeSpec> {
        let (input, _) = tag("boolean")(input)?;
        Ok((input, SwitchTypeSpec::Boolean))
    }

    fn scoped_name(input: &str) -> PResult<SwitchTypeSpec> {
        let (input, name) = parse_scoped_name(input)?;
        Ok((input, SwitchTypeSpec::ScopedName(name)))
    }

    alt((integer_type, char_type, boolean_type, scoped_name))(input)
}

/// ```text
/// <switch_body> ::= <case>+
/// ```
fn parse_switch_body(input: &str) -> PResult<Vec<Case>> {
    many1(parse_case)(input)
}

/// ```text
/// <case> ::= <case_label>+ <element_spec> ";"
/// ```
fn parse_case(input: &str) -> PResult<Case> {
    let (input, labels) = many1(parse_case_label)(input)?;

    let (input, _) = skip_space_and_comment0(input)?;
    let (input, spec) = parse_element_spec(input)?;

    Ok((input, Case { labels, spec }))
}

/// ```text
/// <element_spec> ::= <type_spec> <declarator>
/// ```
fn parse_element_spec(input: &str) -> PResult<ElementSpec> {
    let (input, type_spec) = parse_type_spec(input)?;

    let (input, _) = skip_space_and_comment1(input)?;
    let (input, id) = parse_declarator(input)?;

    Ok((input, ElementSpec { type_spec, id }))
}

/// ```text
/// <case_label> ::= "case" <const_expr> ":"
///                | "default" ":"
/// ```
fn parse_case_label(input: &str) -> PResult<CaseLabel> {
    fn case(input: &str) -> PResult<CaseLabel> {
        let (input, _) = tag("case")(input)?;
        let (input, _) = skip_space_and_comment1(input)?;

        let (input, expr) = parse_const_expr(input)?;

        let (input, _) = skip_space_and_comment0(input)?;
        let (input, _) = tag(":")(input)?;

        Ok((input, CaseLabel::Case(expr)))
    }

    fn default(input: &str) -> PResult<CaseLabel> {
        let (input, _) = tag("default")(input)?;
        let (input, _) = skip_space_and_comment0(input)?;
        let (input, _) = tag(":")(input)?;
        Ok((input, CaseLabel::Default))
    }

    let (input, _) = skip_space_and_comment0(input)?;
    alt((case, default))(input)
}

/// ```text
/// <const_dcl> ::= "const" <const_type> <identifier> "=" <const_expr>
/// ```
fn parse_const_dcl(input: &str) -> PResult<ConstDcl> {
    // "const"
    let (input, _) = skip_space_and_comment0(input)?;
    let (input, _) = tag("const")(input)?;

    // <const_type>
    let (input, _) = skip_space_and_comment1(input)?;
    let (input, const_type) = parse_const_type(input)?;

    // <identifier>
    let (input, _) = skip_space_and_comment1(input)?;
    let (input, id) = parse_id(input)?;

    // "="
    let (input, _) = skip_space_and_comment0(input)?;
    let (input, _) = tag("=")(input)?;

    // <const_expr>
    let (input, _) = skip_space_and_comment0(input)?;
    let (input, expr) = parse_const_expr(input)?;

    Ok((
        input,
        ConstDcl {
            const_type,
            id,
            expr,
        },
    ))
}

/// ```text
/// <enum_dcl> ::= "enum" <identifier> "{" <enumerator> { "," <enumerator> }* "}"
/// ```
fn parse_enum_dcl(input: &str) -> PResult<EnumDcl> {
    // "enum"
    let (input, _) = skip_space_and_comment0(input)?;
    let (input, _) = tag("enum")(input)?;

    // <identifier>
    let (input, _) = skip_space_and_comment1(input)?;
    let (input, id) = parse_id(input)?;

    // "{" <enumerator> { "," <enumerator> }* "}"
    let (input, variants) = delimited(
        tuple((skip_space_and_comment0, tag("{"), skip_space_and_comment0)),
        separated_list1(
            tuple((skip_space_and_comment0, tag(","), skip_space_and_comment0)),
            parse_enumerator,
        ),
        tuple((skip_space_and_comment0, tag("}"))),
    )(input)?;

    Ok((input, EnumDcl { id, variants }))
}

/// ```text
/// <enumerator> ::= <identifier>
/// ```
fn parse_enumerator(input: &str) -> PResult<String> {
    parse_id(input)
}

/// ```text
/// <struct_dcl> ::= <struct_def>
///                | <struct_forward_dcl>
/// ```
fn parse_struct_dcl(input: &str) -> PResult<StructDcl> {
    fn forward_dcl(input: &str) -> PResult<StructDcl> {
        let (input, dcl) = parse_struct_forward_dcl(input)?;
        Ok((input, StructDcl::ForwardDcl(dcl)))
    }

    fn struct_def(input: &str) -> PResult<StructDcl> {
        let (input, def) = parse_struct_def(input)?;
        Ok((input, StructDcl::Def(def)))
    }

    let (input, _) = skip_space_and_comment0(input)?;
    alt((struct_def, forward_dcl))(input)
}

/// ```text
/// <struct_forward_dcl> ::= "struct" <identifier>
/// ```
fn parse_struct_forward_dcl(input: &str) -> PResult<StructForwardDcl> {
    let (input, _) = tag("struct")(input)?;

    let (input, _) = skip_space_and_comment1(input)?;
    let (input, id) = parse_id(input)?;

    Ok((input, StructForwardDcl(id)))
}

/// ```text
/// <struct_def> ::= "struct" <identifier> "{" <member>+ "}"
/// ```
fn parse_struct_def(input: &str) -> PResult<StructDef> {
    // "struct"
    let (input, _) = tag("struct")(input)?;

    // <identifier>
    let (input, _) = skip_space_and_comment1(input)?;
    let (input, id) = parse_id(input)?;

    // "{" <member>+ "}"
    let (input, members) = delimited(
        tuple((skip_space_and_comment0, tag("{"), skip_space_and_comment0)),
        many1(parse_member),
        tuple((skip_space_and_comment0, tag("}"))),
    )(input)?;

    Ok((input, StructDef { id, members }))
}

/// ```text
/// <member> ::= <type_spec> <declarators> ";"
/// ```
fn parse_member(input: &str) -> PResult<Member> {
    // <type_spec>
    let (input, _) = skip_space_and_comment0(input)?;
    let (input, type_spec) = parse_type_spec(input)?;

    // <declarators>
    let (input, _) = skip_space_and_comment1(input)?;
    let (input, ids) = parse_declarators(input)?;

    // ";"
    let (input, _) = skip_space_and_comment0(input)?;
    let (input, _) = tag(";")(input)?;

    Ok((input, Member { type_spec, ids }))
}

/// ```text
/// <type_spec> ::= <simple_type_spec>
/// ```
fn parse_type_spec(input: &str) -> PResult<TypeSpec> {
    parse_simple_type_spec(input)
}

/// ```text
/// <simple_type_spec> ::= <base_type_spec> | <scoped_name>
/// ```
fn parse_simple_type_spec(input: &str) -> PResult<TypeSpec> {
    fn scoped_name(input: &str) -> PResult<TypeSpec> {
        let (input, name) = parse_scoped_name(input)?;
        Ok((input, TypeSpec::ScopedName(name)))
    }

    fn base_type_spec(input: &str) -> PResult<TypeSpec> {
        let (input, base_type) = parse_base_type_spec(input)?;
        Ok((input, TypeSpec::BaseType(base_type)))
    }

    alt((scoped_name, base_type_spec))(input)
}

/// ```text
/// <declarators> ::= <declarator> { "," <declarator> }*
/// ```
fn parse_declarators(input: &str) -> PResult<Vec<String>> {
    separated_list1(
        tuple((skip_space_and_comment0, tag(","), skip_space_and_comment0)),
        parse_declarator,
    )(input)
}

/// ```text
/// <simple_declarator> ::= <identifier>
/// ```
fn parse_simple_declarator(input: &str) -> PResult<String> {
    parse_id(input)
}

/// ```text
/// <declarator> ::= <simple_declarator>
/// ```
fn parse_declarator(input: &str) -> PResult<String> {
    parse_simple_declarator(input)
}

/// ```text
/// <module_decl> ::= "module" <identifier> "{" <definition>+ "}"
/// ```
fn parse_module(input: &str) -> PResult<Module> {
    let (input, _) = skip_space_and_comment0(input)?;
    let (input, _) = tag("module")(input)?;
    let (input, _) = skip_space_and_comment1(input)?;

    // <identifier>
    let (input, id) = parse_id(input)?;

    // "{" <definition>+ "}"
    let (input, definitions) = delimited(
        tuple((skip_space_and_comment0, tag("{"), skip_space_and_comment0)),
        many1(parse_definition),
        tuple((skip_space_and_comment0, tag("}"))),
    )(input)?;

    Ok((input, Module { id, definitions }))
}

/// ```text
/// <identifier>
/// ```
fn parse_id(input: &str) -> PResult<String> {
    let (input, head) = satisfy(|c| c == '_' || is_alphabetic(c as u8))(input)?;
    let (input, tail) = take_while(|c| c == '_' || is_alphanumeric(c as u8))(input)?;

    let mut result = String::new();
    result.push(head);
    result.push_str(tail);
    Ok((input, result))
}

/// ```text
/// <scoped_name> ::= <identifier>
///                 | "::" <identifier>
///                 | <scoped_name> "::" <identifier>
/// ```
fn parse_scoped_name(mut input: &str) -> PResult<ScopedName> {
    let mut ids = Vec::new();
    loop {
        let (next, id) = parse_id(input)?;
        ids.push(id);

        if let Ok((next, _)) = tag::<&str, &str, Error<&str>>("::")(next) {
            input = next;
        } else {
            return Ok((next, ScopedName { ids }));
        }
    }
}

/// ```text
/// <const_type> ::= <integer_type>
///                | <floating_pt_type>
///                | <fixed_pt_const_type>
///                | <char_type>
///                | <wide_char_type>
///                | <boolean_type>
///                | <octet_type>
///                | <string_type>
///                | <wide_string_type>
///                | <scoped_name>
/// ```
fn parse_const_type(input: &str) -> PResult<ConstType> {
    fn base_type(input: &str) -> PResult<ConstType> {
        let (input, t) = parse_base_type_spec(input)?;
        Ok((input, ConstType::BaseType(t)))
    }

    fn string_type(input: &str) -> PResult<ConstType> {
        let (input, t) = parse_string_type(input)?;
        Ok((input, ConstType::StringType(t)))
    }

    fn wstring_type(input: &str) -> PResult<ConstType> {
        let (input, t) = parse_wstring_type(input)?;
        Ok((input, ConstType::WStringType(t)))
    }

    fn scoped_name(input: &str) -> PResult<ConstType> {
        let (input, t) = parse_scoped_name(input)?;
        Ok((input, ConstType::ScopedName(t)))
    }

    alt((base_type, string_type, wstring_type, scoped_name))(input)
}

/// ```text
/// <const_expr> ::= <or_expr>
/// <positive_int_const> ::= <const_expr>
/// ```
fn parse_const_expr(input: &str) -> PResult<ConstExpr> {
    parse_or_expr(input)
}

/// ```text
/// <or_expr> ::= <xor_expr>
///             | <or_expr> "&" <xor_expr>
/// ```
fn parse_or_expr(input: &str) -> PResult<ConstExpr> {
    let (input, left) = parse_xor_expr(input)?;

    if let Ok((input, _)) = tuple((skip_space_and_comment0, tag("|")))(input) {
        let (input, right) = parse_or_expr(input)?;
        Ok((input, ConstExpr::Or(Box::new(left), Box::new(right))))
    } else {
        Ok((input, left))
    }
}

/// ```text
/// <xor_expr> ::= <and_expr>
///              | <xor_expr> "&" <and_expr>
/// ```
fn parse_xor_expr(input: &str) -> PResult<ConstExpr> {
    let (input, left) = parse_and_expr(input)?;

    if let Ok((input, _)) = tuple((skip_space_and_comment0, tag("^")))(input) {
        let (input, right) = parse_xor_expr(input)?;
        Ok((input, ConstExpr::Xor(Box::new(left), Box::new(right))))
    } else {
        Ok((input, left))
    }
}

/// ```text
/// <and_expr> ::= <shift_expr>
///              | <and_expr> "&" <shift_expr>
/// ```
fn parse_and_expr(input: &str) -> PResult<ConstExpr> {
    let (input, left) = parse_shift_expr(input)?;

    if let Ok((input, _)) = tuple((skip_space_and_comment0, tag("&")))(input) {
        let (input, right) = parse_and_expr(input)?;
        Ok((input, ConstExpr::And(Box::new(left), Box::new(right))))
    } else {
        Ok((input, left))
    }
}

/// ```text
/// <shift_expr> ::= <add_expr>
///                | <shift_expr> ">>" <add_expr>
///                | <shift_expr> "<<" <add_expr>
/// ```
fn parse_shift_expr(input: &str) -> PResult<ConstExpr> {
    let (input, left) = parse_add_expr(input)?;

    if let Ok((input, (_, op))) =
        tuple((skip_space_and_comment0, alt((tag(">>"), tag("<<")))))(input)
    {
        let (input, right) = parse_shift_expr(input)?;

        match op {
            ">>" => Ok((input, ConstExpr::RShift(Box::new(left), Box::new(right)))),
            "<<" => Ok((input, ConstExpr::LShift(Box::new(left), Box::new(right)))),
            _ => unreachable!(),
        }
    } else {
        Ok((input, left))
    }
}

/// ```text
/// <add_expr> ::= <mult_expr>
///              | <add_expr> "+" <mult_expr>
///              | <add_expr> "-" <mult_expr>
/// ```
fn parse_add_expr(input: &str) -> PResult<ConstExpr> {
    let (input, left) = parse_mult_expr(input)?;

    if let Ok((input, (_, op))) = tuple((skip_space_and_comment0, alt((tag("+"), tag("-")))))(input)
    {
        let (input, right) = parse_add_expr(input)?;

        match op {
            "+" => Ok((input, ConstExpr::Add(Box::new(left), Box::new(right)))),
            "-" => Ok((input, ConstExpr::Sub(Box::new(left), Box::new(right)))),
            _ => unreachable!(),
        }
    } else {
        Ok((input, left))
    }
}

/// ```text
/// <mult_expr> ::= <unary_expr>
///               | <mult_expr> "*" <unary_expr>
///               | <mult_expr> "/" <unary_expr>
///               | <mult_expr> "%" <unary_expr>
/// ```
fn parse_mult_expr(input: &str) -> PResult<ConstExpr> {
    let (input, left) = parse_unary_expr(input)?;

    if let Ok((input, (_, op))) =
        tuple((skip_space_and_comment0, alt((tag("*"), tag("/"), tag("%")))))(input)
    {
        let (input, right) = parse_mult_expr(input)?;

        match op {
            "*" => Ok((input, ConstExpr::Mul(Box::new(left), Box::new(right)))),
            "/" => Ok((input, ConstExpr::Div(Box::new(left), Box::new(right)))),
            "%" => Ok((input, ConstExpr::Mod(Box::new(left), Box::new(right)))),
            _ => unreachable!(),
        }
    } else {
        Ok((input, left))
    }
}

/// ```text
/// <unary_expr> ::= <unary_operator> <primary_expr>
///                | <primary_expr>
///
/// <unary_operator> ::= "-" | "+" | "~"
/// ```
fn parse_unary_expr(input: &str) -> PResult<ConstExpr> {
    let (input, _) = skip_space_and_comment0(input)?;

    let (input, op) = if let Ok((input, op)) =
        alt((tag::<&str, &str, Error<&str>>("+"), tag("-"), tag("~")))(input)
    {
        (input, op)
    } else {
        (input, "")
    };

    let (input, expr) = parse_primary_expr(input)?;

    let expr = match op {
        "-" => UnaryOpExpr::Minus(Box::new(expr)),
        "~" => UnaryOpExpr::Negate(Box::new(expr)),
        "+" => UnaryOpExpr::Plus(Box::new(expr)),
        "" => return Ok((input, expr)),
        _ => unreachable!(),
    };

    Ok((input, ConstExpr::UnaryOp(expr)))
}

/// ```text
/// <primary_expr> ::= <scoped_name>
///                  | <literal>
///                  | "(" <const_expr> ")"
/// ```
fn parse_primary_expr(input: &str) -> PResult<ConstExpr> {
    fn expr_literal(input: &str) -> PResult<ConstExpr> {
        let (input, literal) = parse_literal(input)?;
        Ok((input, ConstExpr::Literal(literal)))
    }

    fn expr_scoped_name(input: &str) -> PResult<ConstExpr> {
        let (input, name) = parse_scoped_name(input)?;
        Ok((input, ConstExpr::ScopedName(name)))
    }

    let (input, _) = skip_space_and_comment0(input)?;

    alt((
        expr_literal,
        expr_scoped_name,
        delimited(
            tuple((tag("("), skip_space_and_comment0)),
            parse_const_expr,
            tuple((skip_space_and_comment0, tag(")"))),
        ),
    ))(input)
}

fn parse_max_size(input: &str) -> PResult<ConstExpr> {
    delimited(
        tuple((skip_space_and_comment0, tag("<"), skip_space_and_comment0)),
        parse_const_expr,
        tuple((skip_space_and_comment0, tag(">"))),
    )(input)
}

fn parse_string_type(input: &str) -> PResult<StringType> {
    let (input, _) = tag("string")(input)?;
    if let Ok((input, size)) = parse_max_size(input) {
        Ok((input, StringType::Sized(size)))
    } else {
        Ok((input, StringType::UnlimitedSize))
    }
}

fn parse_wstring_type(input: &str) -> PResult<WStringType> {
    let (input, _) = tag("wstring")(input)?;
    if let Ok((input, size)) = parse_max_size(input) {
        Ok((input, WStringType::Sized(size)))
    } else {
        Ok((input, WStringType::UnlimitedSize))
    }
}

/// ```text
/// <base_type_spec> ::= <integer_type>
///                    | <floating_pt_type>
///                    | <char_type>
///                    | <wide_char_type>
///                    | <boolean_type>
///                    | <octet_type>
///
/// <char_type> ::= "char"
/// <wide_char_type> ::= "wchar"
/// <boolean_type> ::= "boolean"
/// <octet_type> ::= "octet"
/// ```
fn parse_base_type_spec(input: &str) -> PResult<BaseType> {
    fn parse_char(input: &str) -> PResult<BaseType> {
        let (input, _) = tag("char")(input)?;
        Ok((input, BaseType::Char))
    }

    fn parse_wchar(input: &str) -> PResult<BaseType> {
        let (input, _) = tag("wchar")(input)?;
        Ok((input, BaseType::WChar))
    }

    fn parse_boolean(input: &str) -> PResult<BaseType> {
        let (input, _) = tag("boolean")(input)?;
        Ok((input, BaseType::Boolean))
    }

    fn parse_octet(input: &str) -> PResult<BaseType> {
        let (input, _) = tag("octet")(input)?;
        Ok((input, BaseType::Octet))
    }

    fn integer_type(input: &str) -> PResult<BaseType> {
        let (input, int_type) = parse_integer_type(input)?;
        Ok((input, BaseType::Integer(int_type)))
    }

    fn floating_type(input: &str) -> PResult<BaseType> {
        let (input, float_type) = parse_floating_pt_type(input)?;
        Ok((input, BaseType::Float(float_type)))
    }

    alt((
        integer_type,
        floating_type,
        parse_char,
        parse_wchar,
        parse_boolean,
        parse_octet,
    ))(input)
}

/// ```text
/// <floating_pt_type> ::= "float"
///                      | "double"
///                      | "long" "double"
/// ```
fn parse_floating_pt_type(input: &str) -> PResult<FloatingPointType> {
    fn parse_float(input: &str) -> PResult<FloatingPointType> {
        let (input, _) = tag("float")(input)?;
        Ok((input, FloatingPointType::Float))
    }

    fn parse_double(input: &str) -> PResult<FloatingPointType> {
        let (input, _) = tag("double")(input)?;
        Ok((input, FloatingPointType::Float))
    }

    fn parse_long_double(input: &str) -> PResult<FloatingPointType> {
        let (input, _) = tuple((tag("long"), skip_space_and_comment1, tag("double")))(input)?;
        Ok((input, FloatingPointType::LongDouble))
    }

    alt((parse_float, parse_double, parse_long_double))(input)
}

/// ```text
/// <integer_type> ::= <signed_int>
///                  | <unsigned_int>
///
/// <signed_int> ::= <signed_short_int>
///                | <signed_long_int>
///                | <signed_longlong_int>
///
/// <signed_short_int> ::= "short"
/// <signed_long_int> ::= "long"
/// <signed_longlong_int> ::= "long" "long"
///
/// <unsigned_int> ::= <unsigned_short_int>
///                  | <unsigned_long_int>
///                  | <unsigned_longlong_int>
///
/// <unsigned_short_int> ::= "unsigned" "short"
/// <unsigned_long_int> ::= "unsigned" "long"
/// <unsigned_longlong_int> ::= "unsigned" "long" "long"
/// ```
fn parse_integer_type(input: &str) -> PResult<IntegerType> {
    fn short(input: &str) -> PResult<IntegerType> {
        let (input, _) = tag("short")(input)?;
        Ok((input, IntegerType::Short))
    }

    fn long_long(input: &str) -> PResult<IntegerType> {
        let (input, _) = tuple((tag("long"), skip_space_and_comment1, tag("long")))(input)?;
        Ok((input, IntegerType::LongLong))
    }

    fn long(input: &str) -> PResult<IntegerType> {
        let (input, _) = tag("long")(input)?;
        Ok((input, IntegerType::Long))
    }

    fn unsigned_short(input: &str) -> PResult<IntegerType> {
        let (input, _) = tuple((tag("unsigned"), skip_space_and_comment1, tag("long")))(input)?;
        Ok((input, IntegerType::UnsignedLong))
    }

    fn unsigned_long_long(input: &str) -> PResult<IntegerType> {
        let (input, _) = tuple((
            tag("unsigned"),
            skip_space_and_comment1,
            tag("long"),
            skip_space_and_comment1,
            tag("long"),
        ))(input)?;
        Ok((input, IntegerType::UnsignedLongLong))
    }

    fn unsigned_long(input: &str) -> PResult<IntegerType> {
        let (input, _) = tuple((tag("unsigned"), skip_space_and_comment1, tag("long")))(input)?;
        Ok((input, IntegerType::UnsignedLong))
    }

    alt((
        short,
        long_long,
        long,
        unsigned_short,
        unsigned_long_long,
        unsigned_long,
    ))(input)
}

fn skip_space_and_comment0(input: &str) -> PResult<()> {
    let (input, _) = many0(alt((
        tag(" "),
        tag(LF_S),
        tag(CR_S),
        tag(HT_S),
        tag(VT_S),
        parse_comment,
    )))(input)?;

    Ok((input, ()))
}

fn skip_space_and_comment1(input: &str) -> PResult<()> {
    let (input, _) = many1(alt((
        tag(" "),
        tag(LF_S),
        tag(CR_S),
        tag(HT_S),
        tag(VT_S),
        parse_comment,
    )))(input)?;

    Ok((input, ()))
}

fn parse_keywards(input: &str) -> PResult<&str> {
    alt((
        alt((
            tag("abstract"),
            tag("any"),
            tag("alias"),
            tag("attribute"),
            tag("bitfield"),
            tag("bitmask"),
            tag("bitset"),
            tag("boolean"),
            tag("case"),
            tag("char"),
            tag("component"),
            tag("connector"),
            tag("const"),
            tag("consumes"),
            tag("context"),
            tag("custom"),
        )),
        alt((
            tag("default"),
            tag("double"),
            tag("exception"),
            tag("emits"),
            tag("enum"),
            tag("eventype"),
            tag("factory"),
            tag("FALSE"),
            tag("finder"),
            tag("fixed"),
            tag("float"),
            tag("getraises"),
            tag("home"),
            tag("import"),
            tag("in"),
            tag("inout"),
        )),
        alt((
            tag("interface"),
            tag("local"),
            tag("long"),
            tag("manages"),
            tag("map"),
            tag("mirrorport"),
            tag("module"),
            tag("multiple"),
            tag("native"),
            tag("Object"),
            tag("octet"),
            tag("oneway"),
            tag("out"),
            tag("primarykey"),
            tag("private"),
            tag("port"),
        )),
        alt((
            tag("porttype"),
            tag("provides"),
            tag("public"),
            tag("publishes"),
            tag("raises"),
            tag("readonly"),
            tag("setraises"),
            tag("sequence"),
            tag("short"),
            tag("string"),
            tag("struct"),
            tag("supports"),
            tag("switch"),
            tag("TRUE"),
            tag("truncatable"),
            tag("typedef"),
        )),
        alt((
            tag("typeid"),
            tag("typename"),
            tag("typeprefix"),
            tag("unsigned"),
            tag("union"),
            tag("uses"),
            tag("ValueBase"),
            tag("valuetype"),
            tag("void"),
            tag("wchar"),
            tag("wstring"),
            tag("int8"),
            tag("uint8"),
            tag("int16"),
            tag("int32"),
            tag("int64"),
            tag("uint16"),
            tag("uint32"),
            tag("uint64"),
        )),
    ))(input)
}

/// ```text
/// <literal> ::= <integer_literal>
///             | <floating_pt_literal>
///             | <fixed_pt_literal>
///             | <character_literal>
///             | <wide_character_literal>
///             | <boolean_literal>
///             | <string_literal>
///             | <wide_string_literal>
/// ```
fn parse_literal(input: &str) -> PResult<Literal> {
    alt((parse_num, parse_hex, parse_octal, parse_char, parse_string))(input)
}

fn parse_sign(input: &str) -> PResult<bool> {
    let (input, sign) = tag::<&str, &str, Error<&str>>("-")(input).unwrap_or((input, ""));
    match sign {
        "-" => Ok((input, true)),
        "" => Ok((input, false)),
        _ => unreachable!(),
    }
}

fn parse_num(input: &str) -> PResult<Literal> {
    let (input, sign) = parse_sign(input)?;

    let (input, mut integer) = if let Ok((input, _)) = tag::<&str, &str, Error<&str>>("0.")(input) {
        (input, BigInt::zero())
    } else {
        let (input, num) = parse_decimal(input)?;
        if let Ok((input, _)) = tag::<&str, &str, Error<&str>>(".")(input) {
            (input, num)
        } else {
            // decimal
            let result = if sign { num * -1 } else { num };
            return Ok((input, Literal::Integer(result)));
        }
    };

    // floating- or fixed-point
    let (input, fraction) = digit1(input)?;
    let (input, c) = alt((tag("d"), tag("D"), tag("e"), tag("E")))(input)?;
    match c {
        "d" | "D" => {
            let mut scale = 0;
            for n in fraction.bytes() {
                let val = n - '0' as u8;
                integer *= 10;
                integer += val;
                scale += 1;
            }
            let value = if sign { integer * -1 } else { integer };
            let result = Literal::FixedPoint(FixedPoint { value, scale });
            Ok((input, result))
        }
        "e" | "E" => {
            let (input, exp_sign) =
                tag::<&str, &str, VerboseError<&str>>("-")(input).unwrap_or((input, ""));
            let sign = if sign { "-" } else { "" };

            let (input, float_num) = if exp_sign == "-" {
                let (input, exp) = parse_decimal(input)?;
                (input, format!("{sign}{integer}.{fraction}e-{exp}"))
            } else if let Ok((input, exp)) = parse_decimal(input) {
                (input, format!("{sign}{integer}.{fraction}e{exp}"))
            } else {
                (input, format!("{sign}{integer}.{fraction}"))
            };

            let result = float_num
                .parse::<f64>()
                .expect("failed to parse a floating point number");
            Ok((input, Literal::FloatingPoint(result)))
        }
        _ => unreachable!(),
    }
}

/// [1-0][0-9]*
fn parse_decimal(input: &str) -> PResult<BigInt> {
    let (input, head) = one_of("123456789")(input)?;
    let (input, tail) = digit0(input)?;

    let mut result = BigInt::zero();
    let n = head as u8 - '0' as u8;
    result += n;

    for n in tail.as_bytes().iter() {
        let val = *n - '0' as u8;
        result *= 10;
        result += val;
    }

    Ok((input, result))
}

/// 0[0-8]*
fn parse_octal(input: &str) -> PResult<Literal> {
    let (input, _) = tag("0")(input)?;
    let (input, tail) = oct_digit0(input)?;

    let mut result = BigInt::zero();
    for n in tail.as_bytes().iter() {
        let val = *n - '0' as u8;
        result *= 8;
        result += val;
    }

    Ok((input, Literal::Integer(result)))
}

fn hex_to_num(hex: char) -> u8 {
    match hex {
        'a' | 'A' => 10,
        'b' | 'B' => 11,
        'c' | 'C' => 12,
        'd' | 'D' => 13,
        'e' | 'E' => 14,
        'f' | 'F' => 15,
        _ => hex as u8 - '0' as u8,
    }
}

/// (0x|0X)[0-9a-fA-F]+
fn parse_hex(input: &str) -> PResult<Literal> {
    let (input, _) = alt((tag("0x"), tag("0X")))(input)?;
    let (input, tail) = hex_digit1(input)?;

    let mut result = BigInt::zero();
    for n in tail.as_bytes().iter() {
        result *= 16;
        result += hex_to_num(*n as char);
    }

    Ok((input, Literal::Integer(result)))
}

fn parse_string(input: &str) -> PResult<Literal> {
    let (mut input, _) = alt((tag("\""), tag("L\"")))(input)?;
    let mut result = String::new();

    loop {
        if let Ok((input, _)) = tag::<&str, &str, Error<&str>>("\"")(input) {
            return Ok((input, Literal::String(result)));
        };

        let (next, c) = parse_char_escape(input)?;
        result.push(c);
        input = next;
    }
}

/// 'c', where `c` is a character or escaped character
fn parse_char(input: &str) -> PResult<Literal> {
    let (input, _) = alt((tag("'"), tag("L'")))(input)?;
    let (input, result) = parse_char_escape(input)?;
    let (input, _) = tag("'")(input)?;

    Ok((input, Literal::Char(result)))
}

fn parse_char_escape(input: &str) -> PResult<char> {
    if let Ok((input, _)) = tag::<&str, &str, Error<&str>>("\\")(input) {
        let (input, c) = one_of("ntvbrfa?\\'\"oxu")(input)?;
        match c {
            'n' => Ok((input, LF)),
            't' => Ok((input, HT)),
            'v' => Ok((input, VT)),
            'b' => Ok((input, BS)),
            'r' => Ok((input, CR)),
            'f' => Ok((input, FF)),
            'a' => Ok((input, BELL)),
            '\\' => Ok((input, '\\')),
            '?' => Ok((input, '?')),
            '\'' => Ok((input, '\'')),
            '"' => Ok((input, '"')),
            'o' => {
                // \ooo
                let original = input;
                let (input, high) = satisfy(|n| is_oct_digit(n as u8))(input)?;
                let (input, mid) = satisfy(|n| is_oct_digit(n as u8))(input)?;
                let (input, low) = satisfy(|n| is_oct_digit(n as u8))(input)?;

                let high = high as u32 - '0' as u32;
                let mid = mid as u32 - '0' as u32;
                let low = low as u32 - '0' as u32;

                let val = high * 8 * 8 + mid * 8 + low;
                if let Some(result) = char::from_u32(val) {
                    Ok((input, result))
                } else {
                    fail(original)
                }
            }
            'x' => {
                // \xhh
                let (input, high) = satisfy(|n| is_hex_digit(n as u8))(input)?;
                let (input, low) = satisfy(|n| is_hex_digit(n as u8))(input)?;

                let high = hex_to_num(high);
                let low = hex_to_num(low);

                let val = high * 16 + low;
                Ok((input, val as char))
            }
            'u' => {
                // \uhhhh
                let original = input;
                let (input, hex1) = satisfy(|n| is_hex_digit(n as u8))(input)?;
                let (input, hex2) = satisfy(|n| is_hex_digit(n as u8))(input)?;
                let (input, hex3) = satisfy(|n| is_hex_digit(n as u8))(input)?;
                let (input, hex4) = satisfy(|n| is_hex_digit(n as u8))(input)?;

                let hex1 = hex_to_num(hex1) as u32;
                let hex2 = hex_to_num(hex2) as u32;
                let hex3 = hex_to_num(hex3) as u32;
                let hex4 = hex_to_num(hex4) as u32;

                let val = hex1 * 16 * 16 * 16 + hex2 * 16 * 16 + hex3 * 16 + hex4;

                if let Some(result) = char::from_u32(val) {
                    Ok((input, result))
                } else {
                    fail(original)
                }
            }
            _ => unreachable!(),
        }
    } else {
        anychar(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::*;
    use num_bigint::BigInt;
    use num_traits::FromPrimitive;

    #[test]
    fn literal() {
        let input = "1234";
        assert_eq!(
            parse_literal(input).unwrap().1,
            Literal::Integer(BigInt::from_usize(1234).unwrap())
        );

        let input = "0xabcd";
        assert_eq!(
            parse_literal(input).unwrap().1,
            Literal::Integer(BigInt::from_usize(0xabcd).unwrap())
        );

        let input = "0XEF45";
        assert_eq!(
            parse_literal(input).unwrap().1,
            Literal::Integer(BigInt::from_usize(0xef45).unwrap())
        );

        let input = "0127";
        assert_eq!(
            parse_literal(input).unwrap().1,
            Literal::Integer(BigInt::from_usize(0o127).unwrap())
        );

        let input = "-567";
        assert_eq!(
            parse_literal(input).unwrap().1,
            Literal::Integer(BigInt::from_isize(-567).unwrap())
        );

        let input = "-567.04e";
        assert_eq!(
            parse_literal(input).unwrap().1,
            Literal::FloatingPoint(-567.04)
        );

        let input = "-567.04e4";
        assert_eq!(
            parse_literal(input).unwrap().1,
            Literal::FloatingPoint(-567.04e4)
        );

        let input = "-567.04e-4";
        assert_eq!(
            parse_literal(input).unwrap().1,
            Literal::FloatingPoint(-567.04e-4)
        );

        let input = "567.04d";
        assert_eq!(
            parse_literal(input).unwrap().1,
            Literal::FixedPoint(FixedPoint {
                value: BigInt::from_isize(56704).unwrap(),
                scale: 2
            })
        );

        let input = "-567.04d";
        assert_eq!(
            parse_literal(input).unwrap().1,
            Literal::FixedPoint(FixedPoint {
                value: BigInt::from_isize(-56704).unwrap(),
                scale: 2
            })
        );

        let input = "'\n'";
        assert_eq!(parse_literal(input).unwrap().1, Literal::Char('\n'));
    }
}

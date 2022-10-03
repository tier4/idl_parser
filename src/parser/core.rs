use super::PResult;
use crate::{
    character::{BELL, BS, CR, CR_S, FF, HT, HT_S, LF, LF_S, VT, VT_S},
    expr::{
        AnyDeclarator, ArrayDeclarator, Case, CaseLabel, ConstDcl, ConstExpr, ConstType,
        ConstrTypeDcl, Definition, ElementSpec, EnumDcl, FixedPoint, FixedPtType, Literal, Member,
        Module, NativeDcl, PrimitiveType, ScopedName, SequenceType, StringType, StructDcl,
        StructDef, StructForwardDcl, SwitchTypeSpec, TemplateTypeSpec, TypeDcl, TypeSpec, Typedef,
        TypedefType, UnaryOpExpr, UnionDcl, UnionDef, UnionForwardDcl, WStringType,
    },
    parser::{
        components::{
            parse_component_dcl, parse_connector_dcl, parse_home_dcl, parse_porttype_dcl,
        },
        extended_data_types::{parse_bitmask_dcl, parse_bitset_dcl, parse_map_type},
        interfaces::{parse_except_dcl, parse_interface_dcl},
        template::{parse_template_module_dcl, parse_template_module_inst},
        value_types::parse_value_dcl,
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
};
use num_bigint::BigInt;
use num_traits::Zero;

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

pub fn skip_space_and_comment0(input: &str) -> PResult<()> {
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

pub fn skip_space_and_comment1(input: &str) -> PResult<()> {
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

/// ```text
/// <identifier>
/// ```
pub fn parse_id(input: &str) -> PResult<String> {
    let (input, head) = satisfy(|c| c == '_' || is_alphabetic(c as u8))(input)?;
    let (input, tail) = take_while(|c| c == '_' || is_alphanumeric(c as u8))(input)?;

    let mut result = String::new();
    result.push(head);
    result.push_str(tail);
    Ok((input, result))
}

/// ```text
/// (  2) <definition> ::= <module_dcl> ";"
///                      | <const_dcl> ";"
///                      | <type_dcl> ";"
///
/// ( 71) <definition> ::+ <except_dcl> ";"
///                      | <interface_dcl> ";"
///
/// ( 98) <definition> ::+ <value_dcl> ";"
///
/// (111) <definition> ::+ <type_id_dcl> ";"
///                      | <type_prefix_dcl> ";"
///                      | <import_dcl> ";"
///
/// (133) <definition> ::+ <component_dcl> ";"
///
/// (144) <definition> ::+ <home_dcl> ";"
///
/// (153) <definition> ::+ <event_dcl> ";"
///
/// (171) <definition> ::+ <porttype_dcl> ";"
///                      | <connector_dcl> ";"
///
/// (184) <definition> ::+ <template_module_dcl> ";"
///                      | <template_module_inst> ";"
/// ```
pub fn parse_definition(input: &str) -> PResult<Definition> {
    fn module(input: &str) -> PResult<Definition> {
        let (input, def) = parse_module_dcl(input)?;
        Ok((input, Definition::Module(def)))
    }

    fn const_dcl(input: &str) -> PResult<Definition> {
        let (input, def) = parse_const_dcl(input)?;
        Ok((input, Definition::Const(def)))
    }

    fn type_dcl(input: &str) -> PResult<Definition> {
        let (input, def) = parse_type_dcl(input)?;
        Ok((input, Definition::Type(def)))
    }

    fn except_dcl(input: &str) -> PResult<Definition> {
        let (input, def) = parse_except_dcl(input)?;
        Ok((input, Definition::Except(def)))
    }

    fn interface_dcl(input: &str) -> PResult<Definition> {
        let (input, def) = parse_interface_dcl(input)?;
        Ok((input, Definition::Interface(def)))
    }

    fn value_dcl(input: &str) -> PResult<Definition> {
        let (input, def) = parse_value_dcl(input)?;
        Ok((input, Definition::Value(def)))
    }

    fn component_dcl(input: &str) -> PResult<Definition> {
        let (input, def) = parse_component_dcl(input)?;
        Ok((input, Definition::Component(def)))
    }

    fn home_dcl(input: &str) -> PResult<Definition> {
        let (input, def) = parse_home_dcl(input)?;
        Ok((input, Definition::Home(def)))
    }

    fn port_type_dcl(input: &str) -> PResult<Definition> {
        let (input, def) = parse_porttype_dcl(input)?;
        Ok((input, Definition::PortType(def)))
    }

    fn connector_dcl(input: &str) -> PResult<Definition> {
        let (input, def) = parse_connector_dcl(input)?;
        Ok((input, Definition::Connector(def)))
    }

    fn template_module_dcl(input: &str) -> PResult<Definition> {
        let (input, def) = parse_template_module_dcl(input)?;
        Ok((input, Definition::TemplateModuleDcl(def)))
    }

    fn template_module_inst(input: &str) -> PResult<Definition> {
        let (input, def) = parse_template_module_inst(input)?;
        Ok((input, Definition::TemplateModuleInst(def)))
    }

    let (input, def) = alt((
        module,
        const_dcl,
        type_dcl,
        except_dcl,
        interface_dcl,
        value_dcl,
        component_dcl,
        home_dcl,
        port_type_dcl,
        connector_dcl,
        template_module_dcl,
        template_module_inst,
    ))(input)?;
    let (input, _) = tuple((skip_space_and_comment0, tag(";")))(input)?;

    Ok((input, def))
}

/// ```text
/// (3) <module_dcl> ::= "module" <identifier> "{" <definition>+ "}"
/// ```
fn parse_module_dcl(input: &str) -> PResult<Module> {
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
/// (4) <scoped_name> ::= <identifier>
///                     | "::" <identifier>
///                     | <scoped_name> "::" <identifier>
/// ```
pub fn parse_scoped_name(mut input: &str) -> PResult<ScopedName> {
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
/// (5) <const_dcl> ::= "const" <const_type> <identifier> "=" <const_expr>
/// ```
pub fn parse_const_dcl(input: &str) -> PResult<ConstDcl> {
    // "const"
    let (input, _) = tuple((skip_space_and_comment0, tag("const")))(input)?;

    // <const_type>
    let (input, _) = skip_space_and_comment1(input)?;
    let (input, const_type) = parse_const_type(input)?;

    // <identifier>
    let (input, _) = skip_space_and_comment1(input)?;
    let (input, id) = parse_id(input)?;

    // "="
    let (input, _) = tuple((skip_space_and_comment0, tag("=")))(input)?;

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
/// (6) <const_type> ::= <integer_type>
///                    | <floating_pt_type>
///                    | <fixed_pt_const_type>
///                    | <char_type>
///                    | <wide_char_type>
///                    | <boolean_type>
///                    | <octet_type>
///                    | <string_type>
///                    | <wide_string_type>
///                    | <scoped_name>
/// ```
pub fn parse_const_type(input: &str) -> PResult<ConstType> {
    if let Ok((input, result)) = alt((parse_int_words, parse_long_double))(input) {
        return Ok((input, ConstType::PrimitiveType(result)));
    }

    if let Ok((input, result)) = parse_string_type(input) {
        return Ok((input, ConstType::StringType(result)));
    }

    if let Ok((input, result)) = parse_wide_string_type(input) {
        return Ok((input, ConstType::WStringType(result)));
    }

    let (input, name) = parse_scoped_name(input)?;

    if let Some(p) = name.to_primitive() {
        return Ok((input, ConstType::PrimitiveType(p)));
    }

    if name.ids.len() >= 2 {
        return Ok((input, ConstType::ScopedName(name)));
    }

    match name.ids[0].as_str() {
        "string" => Ok((input, ConstType::StringType(StringType::UnlimitedSize))),
        "wstring" => Ok((input, ConstType::WStringType(WStringType::UnlimitedSize))),
        "fixed" => Ok((input, ConstType::FixedPointConst)),
        _ => Ok((input, ConstType::ScopedName(name))),
    }
}

/// ```text
/// ( 7) <const_expr> ::= <or_expr>
/// (19) <positive_int_const> ::= <const_expr>
/// ```
pub fn parse_const_expr(input: &str) -> PResult<ConstExpr> {
    parse_or_expr(input)
}

/// ```text
/// (8) <or_expr> ::= <xor_expr>
///                 | <or_expr> "&" <xor_expr>
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
/// (9) <xor_expr> ::= <and_expr>
///                  | <xor_expr> "&" <and_expr>
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
/// (10) <and_expr> ::= <shift_expr>
///                   | <and_expr> "&" <shift_expr>
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
/// (11) <shift_expr> ::= <add_expr>
///                     | <shift_expr> ">>" <add_expr>
///                     | <shift_expr> "<<" <add_expr>
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
/// (12) <add_expr> ::= <mult_expr>
///                   | <add_expr> "+" <mult_expr>
///                   | <add_expr> "-" <mult_expr>
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
/// (13) <mult_expr> ::= <unary_expr>
///                    | <mult_expr> "*" <unary_expr>
///                    | <mult_expr> "/" <unary_expr>
///                    | <mult_expr> "%" <unary_expr>
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
/// (14) <unary_expr> ::= <unary_operator> <primary_expr>
///                     | <primary_expr>
/// (15) <unary_operator> ::= "-" | "+" | "~"
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
/// (16) <primary_expr> ::= <scoped_name>
///                       | <literal>
///                       | "(" <const_expr> ")"
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

/// ```text
/// (17) <literal> ::= <integer_literal>
///                  | <floating_pt_literal>
///                  | <fixed_pt_literal>
///                  | <character_literal>
///                  | <wide_character_literal>
///                  | <boolean_literal>
///                  | <string_literal>
///                  | <wide_string_literal>
/// ```
fn parse_literal(input: &str) -> PResult<Literal> {
    alt((
        parse_num,
        parse_hex,
        parse_octal,
        parse_boolean_literal,
        parse_char,
        parse_string,
    ))(input)
}

/// ```test
/// (18) <boolean_literal> ::= "TRUE" | "FALSE"
/// ```
fn parse_boolean_literal(input: &str) -> PResult<Literal> {
    let (input, value) = alt((tag("TRUE"), tag("FALSE")))(input)?;

    match value {
        "TRUE" => Ok((input, Literal::Boolean(true))),
        "FALSE" => Ok((input, Literal::Boolean(false))),
        _ => unreachable!(),
    }
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

/// ```text
/// (20) <type_dcl> ::= <constr_type_dcl>
///                   | <native_dcl>
///                   | <typedef_dcl>
/// ```
pub fn parse_type_dcl(input: &str) -> PResult<TypeDcl> {
    fn constr_type(input: &str) -> PResult<TypeDcl> {
        let (input, dcl) = parse_constr_type_dcl(input)?;
        Ok((input, TypeDcl::ConstrType(dcl)))
    }

    fn native(input: &str) -> PResult<TypeDcl> {
        let (input, dcl) = parse_native_dcl(input)?;
        Ok((input, TypeDcl::Native(dcl)))
    }

    fn typedef(input: &str) -> PResult<TypeDcl> {
        let (input, dcl) = parse_typedef_dcl(input)?;
        Ok((input, TypeDcl::Typedef(dcl)))
    }

    alt((constr_type, native, typedef))(input)
}

/// ```text
/// ( 21) <type_spec> ::= <simple_type_spec>
/// (216) <type_spec> ::+ <template_type_spec>
/// ```
pub fn parse_type_spec(input: &str) -> PResult<TypeSpec> {
    fn template_type_spec(input: &str) -> PResult<TypeSpec> {
        let (input, t) = parse_template_type_spec(input)?;
        Ok((input, TypeSpec::Template(Box::new(t))))
    }

    alt((template_type_spec, parse_simple_type_spec))(input)
}

/// ```text
/// (22) <simple_type_spec> ::= <base_type_spec> | <scoped_name>
/// ```
fn parse_simple_type_spec(input: &str) -> PResult<TypeSpec> {
    if let Ok((input, result)) = alt((parse_int_words, parse_long_double))(input) {
        return Ok((input, TypeSpec::PrimitiveType(result)));
    }

    let (input, name) = parse_scoped_name(input)?;

    if let Some(p) = name.to_primitive() {
        Ok((input, TypeSpec::PrimitiveType(p)))
    } else {
        Ok((input, TypeSpec::ScopedName(name)))
    }
}

// ```text
// (23) <base_type_spec> ::= <integer_type>
//                         | <floating_pt_type>
//                         | <char_type>
//                         | <wide_char_type>
//                         | <boolean_type>
//                         | <octet_type>
//
// (69) <base_type_spec> ::+ <any_type>
// (70) <any_type> ::= "any"
// ```

/// Parse integer types consisting more than or equal to 2 words.
pub fn parse_int_words(input: &str) -> PResult<PrimitiveType> {
    fn long_long(input: &str) -> PResult<PrimitiveType> {
        let (input, _) = tuple((tag("long"), skip_space_and_comment1, tag("long")))(input)?;
        Ok((input, PrimitiveType::LongLong))
    }

    fn unsigned_short(input: &str) -> PResult<PrimitiveType> {
        let (input, _) = tuple((tag("unsigned"), skip_space_and_comment1, tag("short")))(input)?;
        Ok((input, PrimitiveType::UnsignedLong))
    }

    fn unsigned_long_long(input: &str) -> PResult<PrimitiveType> {
        let (input, _) = tuple((
            tag("unsigned"),
            skip_space_and_comment1,
            tag("long"),
            skip_space_and_comment1,
            tag("long"),
        ))(input)?;
        Ok((input, PrimitiveType::UnsignedLongLong))
    }

    fn unsigned_long(input: &str) -> PResult<PrimitiveType> {
        let (input, _) = tuple((tag("unsigned"), skip_space_and_comment1, tag("long")))(input)?;
        Ok((input, PrimitiveType::UnsignedLong))
    }

    alt((long_long, unsigned_short, unsigned_long_long, unsigned_long))(input)
}

fn parse_long_double(input: &str) -> PResult<PrimitiveType> {
    let (input, _) = tuple((tag("long"), skip_space_and_comment1, tag("double")))(input)?;
    Ok((input, PrimitiveType::LongDouble))
}

// ```text
// (24) <floating_pt_type> ::= "float"
//                           | "double"
//                           | "long" "double"
//
// (25) <integer_type> ::= <signed_int>
//                       | <unsigned_int>
//
// (26) <signed_int> ::= <signed_short_int>
//                     | <signed_long_int>
//                     | <signed_longlong_int>
//
// (27) <signed_short_int> ::= "short"
// (28) <signed_long_int> ::= "long"
// (29) <signed_longlong_int> ::= "long" "long"
//
// (30) <unsigned_int> ::= <unsigned_short_int>
//                       | <unsigned_long_int>
//                       | <unsigned_longlong_int>
//
// (31) <unsigned_short_int> ::= "unsigned" "short"
// (32) <unsigned_long_int> ::= "unsigned" "long"
// (33) <unsigned_longlong_int> ::= "unsigned" "long" "long"
//
// (34) <char_type> ::= "char"
// (35) <wide_char_type> ::= "wchar"
// (36) <boolean_type> ::= "boolean"
// (37) <octet_type> ::= "octet"
//
// (206) <signed_int> ::+ <signed_tiny_int>
// (207) <unsigned_int> ::+ <unsigned_tiny_int>
// (208) <signed_tiny_int>     ::= "int8"
// (209) <unsigned_tiny_int>   ::= "uint8"
// (210) <signed_short_int>    ::+ "int16"
// (211) <signed_long_int>     ::+ "int32"
// (212) <signed_longlong_int> ::+ "int64"
// (213) <signed_short_int>    ::+ "int16"
// (214) <signed_long_int>     ::+ "int32"
// (215) <signed_longlong_int> ::+ "int64"
// ```

/// ```text
/// ( 38) <template_type_spec> ::= <sequence_type>
///                              | <string_type>
///                              | <wide_string_type>
///                              | <fixed_pt_type>
///
/// (197) <template_type_spec> ::+ <map_type>
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
        let (input, t) = parse_wide_string_type(input)?;
        Ok((input, TemplateTypeSpec::WString(t)))
    }

    fn fixed_pt(input: &str) -> PResult<TemplateTypeSpec> {
        let (input, t) = parse_fixed_pt_type(input)?;
        Ok((input, TemplateTypeSpec::FixedPoint(t)))
    }

    fn map_type(input: &str) -> PResult<TemplateTypeSpec> {
        let (input, t) = parse_map_type(input)?;
        Ok((input, TemplateTypeSpec::Map(t)))
    }

    alt((sequence, string, wide_string, fixed_pt, map_type))(input)
}

/// ```text
/// (39) <sequence_type> ::= "sequence" "<" <type_spec> "," <positive_int_const> ">"
///                        | "sequence" "<" <type_spec> ">"
/// ```
pub fn parse_sequence_type(input: &str) -> PResult<SequenceType> {
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
/// "<" <positive_int_const> ">"
/// ```
fn parse_max_size(input: &str) -> PResult<ConstExpr> {
    delimited(
        tuple((skip_space_and_comment0, tag("<"), skip_space_and_comment0)),
        parse_const_expr,
        tuple((skip_space_and_comment0, tag(">"))),
    )(input)
}

/// ```text
/// (40) <string_type> ::= "string" "<" <positive_int_const> ">"
///                      | "string"
/// ```
///
/// This function parses only `"string" "<" <positive_int_const> ">"`.
fn parse_string_type(input: &str) -> PResult<StringType> {
    let (input, _) = tag("string")(input)?;
    let (input, size) = parse_max_size(input)?;
    Ok((input, StringType::Sized(size)))
}

/// ```text
/// (41) <wide_string_type> ::= "wstring" "<" <positive_int_const> ">"
///                           | "wstring"
/// ```
///
/// This function parses only `"string" "<" <positive_int_const> ">"`.
fn parse_wide_string_type(input: &str) -> PResult<WStringType> {
    let (input, _) = tag("wstring")(input)?;
    let (input, size) = parse_max_size(input)?;
    Ok((input, WStringType::Sized(size)))
}

/// ```text
/// (42) <fixed_pt_type> ::= "fixed" "<" <positive_int_const> "," <positive_int_const> ">"
/// ```
fn parse_fixed_pt_type(input: &str) -> PResult<FixedPtType> {
    let (input, _) = tuple((
        tag("fixed"),
        skip_space_and_comment0,
        tag("<"),
        skip_space_and_comment0,
    ))(input)?;

    let (input, total_digits) = parse_const_expr(input)?;

    let (input, _) = tuple((skip_space_and_comment0, tag(","), skip_space_and_comment0))(input)?;

    let (input, fractional_digits) = parse_const_expr(input)?;

    let (input, _) = tuple((skip_space_and_comment0, tag(">")))(input)?;

    Ok((
        input,
        FixedPtType {
            total_digits,
            fractional_digits,
        },
    ))
}

// ```text
// (43) <fixed_pt_const_type> ::= "fixed"
// ```

/// ```text
/// ( 44) <constr_type_dcl> ::= <struct_dcl>
///                           | <union_dcl>
///                           | <enum_dcl>
///
/// (198) <constr_type_dcl> ::+ <bitset_dcl>
///                           | <bitmask_dcl>
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

    fn bitset(input: &str) -> PResult<ConstrTypeDcl> {
        let (input, t) = parse_bitset_dcl(input)?;
        Ok((input, ConstrTypeDcl::Bitset(t)))
    }

    fn bitmask(input: &str) -> PResult<ConstrTypeDcl> {
        let (input, t) = parse_bitmask_dcl(input)?;
        Ok((input, ConstrTypeDcl::Bitmask(t)))
    }

    alt((struct_type, enum_type, union_type, bitset, bitmask))(input)
}

/// ```text
/// (45) <struct_dcl> ::= <struct_def>
///                     | <struct_forward_dcl>
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
/// ( 46) <struct_def> ::= "struct" <identifier> "{" <member>+ "}"
///
/// (195) <struct_def> ::+ "struct" <identifier> ":" <scoped_name> "{" <member>* "}"
///                      | "struct" <identifier> "{" "}"
/// ```
fn parse_struct_def(input: &str) -> PResult<StructDef> {
    // "struct"
    let (input, _) = tuple((tag("struct"), skip_space_and_comment1))(input)?;

    // <identifier>
    let (input, id) = parse_id(input)?;

    let (input, _) = skip_space_and_comment0(input)?;

    let (input, inheritance) = if let Ok((input, _)) = tag::<&str, &str, Error<&str>>(":")(input) {
        let (input, (_, name)) = tuple((skip_space_and_comment0, parse_scoped_name))(input)?;
        (input, Some(name))
    } else {
        (input, None)
    };

    // "{" <member>* "}"
    let (input, members) = delimited(
        tuple((skip_space_and_comment0, tag("{"), skip_space_and_comment0)),
        many0(parse_member),
        tuple((skip_space_and_comment0, tag("}"))),
    )(input)?;

    Ok((
        input,
        StructDef {
            id,
            members,
            inheritance,
        },
    ))
}

/// ```text
/// (47) <member> ::= <type_spec> <declarators> ";"
/// ```
pub fn parse_member(input: &str) -> PResult<Member> {
    // <type_spec>
    let (input, _) = skip_space_and_comment0(input)?;
    let (input, type_spec) = parse_type_spec(input)?;

    // <declarators>
    let (input, _) = skip_space_and_comment1(input)?;
    let (input, declarators) = parse_declarators(input)?;

    // ";"
    let (input, _) = tuple((skip_space_and_comment0, tag(";")))(input)?;

    Ok((
        input,
        Member {
            type_spec,
            declarators,
        },
    ))
}

/// ```text
/// (48) <struct_forward_dcl> ::= "struct" <identifier>
/// ```
fn parse_struct_forward_dcl(input: &str) -> PResult<StructForwardDcl> {
    let (input, _) = tuple((tag("struct"), skip_space_and_comment1))(input)?;
    let (input, id) = parse_id(input)?;

    Ok((input, StructForwardDcl(id)))
}

/// ```text
/// (49) <union_dcl> ::= <union_def>
///                    | <union_forward_dcl>
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
/// (50) <union_def> ::= "union" <identifier> "switch" "(" <switch_type_spec> ")" "{" <switch_body> "}"
/// ```
fn parse_union_def(input: &str) -> PResult<UnionDef> {
    // "union"
    let (input, _) = tuple((tag("union"), skip_space_and_comment1))(input)?;

    // <identifier>
    let (input, id) = parse_id(input)?;

    // "switch"
    let (input, _) = tuple((skip_space_and_comment1, tag("switch")))(input)?;

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
/// ( 51) <switch_type_spec> ::= <integer_type>
///                            | <char_type>
///                            | <boolean_type>
///                            | <scoped_name>
///
/// (196) <switch_type_spec> ::+ <wide_char_type>
///                            | <octet_type>
/// ```
fn parse_switch_type_spec(input: &str) -> PResult<SwitchTypeSpec> {
    if let Ok((input, result)) = parse_int_words(input) {
        return Ok((input, SwitchTypeSpec::PrimitiveType(result)));
    }

    let (input, name) = parse_scoped_name(input)?;

    if name.ids.len() >= 2 {
        return Ok((input, SwitchTypeSpec::ScopedName(name)));
    }

    match name.ids[0].as_str() {
        "short" => Ok((input, SwitchTypeSpec::PrimitiveType(PrimitiveType::Short))),
        "long" => Ok((input, SwitchTypeSpec::PrimitiveType(PrimitiveType::Long))),
        "char" => Ok((input, SwitchTypeSpec::PrimitiveType(PrimitiveType::Char))),
        "boolean" => Ok((input, SwitchTypeSpec::PrimitiveType(PrimitiveType::Boolean))),
        "int8" => Ok((input, SwitchTypeSpec::PrimitiveType(PrimitiveType::Int8))),
        "uint8" => Ok((input, SwitchTypeSpec::PrimitiveType(PrimitiveType::Uint8))),
        "int16" => Ok((input, SwitchTypeSpec::PrimitiveType(PrimitiveType::Int16))),
        "uint16" => Ok((input, SwitchTypeSpec::PrimitiveType(PrimitiveType::Uint16))),
        "int32" => Ok((input, SwitchTypeSpec::PrimitiveType(PrimitiveType::Int32))),
        "uint32" => Ok((input, SwitchTypeSpec::PrimitiveType(PrimitiveType::Uint32))),
        "int64" => Ok((input, SwitchTypeSpec::PrimitiveType(PrimitiveType::Int64))),
        "uint64" => Ok((input, SwitchTypeSpec::PrimitiveType(PrimitiveType::Uint64))),
        "wchar" => Ok((input, SwitchTypeSpec::PrimitiveType(PrimitiveType::WChar))),
        "octet" => Ok((input, SwitchTypeSpec::PrimitiveType(PrimitiveType::Octet))),
        _ => Ok((input, SwitchTypeSpec::ScopedName(name))),
    }
}

/// ```text
/// (52) <switch_body> ::= <case>+
/// ```
fn parse_switch_body(input: &str) -> PResult<Vec<Case>> {
    many1(parse_case)(input)
}

/// ```text
/// (53) <case> ::= <case_label>+ <element_spec> ";"
/// ```
fn parse_case(input: &str) -> PResult<Case> {
    let (input, labels) = many1(parse_case_label)(input)?;

    let (input, _) = skip_space_and_comment0(input)?;
    let (input, spec) = parse_element_spec(input)?;

    Ok((input, Case { labels, spec }))
}

/// ```text
/// (54) <case_label> ::= "case" <const_expr> ":"
///                     | "default" ":"
/// ```
fn parse_case_label(input: &str) -> PResult<CaseLabel> {
    fn case(input: &str) -> PResult<CaseLabel> {
        let (input, _) = tuple((tag("case"), skip_space_and_comment1))(input)?;

        let (input, expr) = parse_const_expr(input)?;

        let (input, _) = tuple((skip_space_and_comment0, tag(":")))(input)?;

        Ok((input, CaseLabel::Case(expr)))
    }

    fn default(input: &str) -> PResult<CaseLabel> {
        let (input, _) = tuple((tag("default"), skip_space_and_comment0, tag(":")))(input)?;
        Ok((input, CaseLabel::Default))
    }

    let (input, _) = skip_space_and_comment0(input)?;
    alt((case, default))(input)
}

/// ```text
/// (55) <element_spec> ::= <type_spec> <declarator>
/// ```
fn parse_element_spec(input: &str) -> PResult<ElementSpec> {
    let (input, type_spec) = parse_type_spec(input)?;

    let (input, _) = skip_space_and_comment1(input)?;
    let (input, declarator) = parse_declarator(input)?;

    Ok((
        input,
        ElementSpec {
            type_spec,
            declarator,
        },
    ))
}

/// ```text
/// (56) <union_forward_dcl> ::= "union" <identifier>
/// ```
fn parse_union_forward_dcl(input: &str) -> PResult<UnionForwardDcl> {
    // "union"
    let (input, _) = tuple((tag("union"), skip_space_and_comment1))(input)?;
    let (input, id) = parse_id(input)?;

    Ok((input, UnionForwardDcl(id)))
}

/// ```text
/// (57) <enum_dcl> ::= "enum" <identifier> "{" <enumerator> { "," <enumerator> }* "}"
/// ```
fn parse_enum_dcl(input: &str) -> PResult<EnumDcl> {
    // "enum"
    let (input, _) = tuple((
        skip_space_and_comment0,
        tag("enum"),
        skip_space_and_comment1,
    ))(input)?;

    // <identifier>
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
/// (58) <enumerator> ::= <identifier>
/// ```
fn parse_enumerator(input: &str) -> PResult<String> {
    parse_id(input)
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
/// (61) <native_dcl> ::= "native" <simple_declarator>
/// ```
fn parse_native_dcl(input: &str) -> PResult<NativeDcl> {
    let (input, _) = tuple((tag("native"), skip_space_and_comment1))(input)?;
    let (input, id) = parse_simple_declarator(input)?;

    Ok((input, NativeDcl(id)))
}

/// ```text
/// (62) <simple_declarator> ::= <identifier>
/// ```
pub fn parse_simple_declarator(input: &str) -> PResult<String> {
    parse_id(input)
}

/// ```text
/// (63) <typedef_dcl> ::= "typedef" <type_declarator>
/// ```
fn parse_typedef_dcl(input: &str) -> PResult<Typedef> {
    let (input, _) = tuple((tag("typedef"), skip_space_and_comment1))(input)?;
    parse_type_declarator(input)
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
/// (67) <declarators> ::= <declarator> { "," <declarator> }*
/// ```
pub fn parse_declarators(input: &str) -> PResult<Vec<AnyDeclarator>> {
    separated_list1(
        tuple((skip_space_and_comment0, tag(","), skip_space_and_comment0)),
        parse_declarator,
    )(input)
}

/// ```text
/// ( 68) <declarator> ::= <simple_declarator>
/// (217) <declarator> ::+ <array_declarator>
/// ```
fn parse_declarator(input: &str) -> PResult<AnyDeclarator> {
    parse_any_declarator(input)
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

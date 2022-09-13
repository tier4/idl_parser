use nom::{
    branch::alt,
    bytes::complete::{tag, take_until1, take_while1},
    character::{
        complete::{anychar, digit0, digit1, hex_digit1, oct_digit0, one_of, satisfy},
        is_alphanumeric, is_hex_digit, is_oct_digit,
    },
    combinator::{eof, fail},
    error::VerboseError,
    multi::many0,
    IResult,
};
use num_bigint::BigInt;
use num_traits::Zero;

use crate::{
    character::{BELL, BS, CR, FF, HT, LF, VT},
    expr::{Expr, Literal},
};

type PResult<'a, OUT> = IResult<&'a str, OUT, VerboseError<&'a str>>;

// fn parse_comment(input: &str) -> PResult<Expr> {}

pub fn parse(input: &str) -> PResult<Expr> {
    parse_literal(input)
}

fn parse_comment(input: &str) -> PResult<&str> {
    skip_space0(input)?;

    let (input, c) = alt((tag("//"), tag("/*")))(input)?;

    let (input, comment) = match c {
        "//" => alt((tag("\n"), eof))(input)?,
        "/*" => {
            let (input, c) = take_until1("*/")(input)?;
            let (input, _) = tag("*/")(input)?;
            (input, c)
        }
        _ => unreachable!(),
    };

    Ok((input, comment))
}

fn parse_id(input: &str) -> PResult<&str> {
    take_while1(|c| c == '_' || is_alphanumeric(c as u8))(input)
}

fn skip_space0(input: &str) -> PResult<()> {
    let (input, _) = many0(tag(" "))(input)?;
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

fn parse_literal(input: &str) -> PResult<Expr> {
    let (input, literal) = alt((parse_num, parse_hex, parse_octal, parse_char))(input)?;
    Ok((input, Expr::Literal(literal)))
}

fn parse_num(input: &str) -> PResult<Literal> {
    let (input, integer) =
        if let Ok((input, _)) = tag::<&str, &str, VerboseError<&str>>("0.")(input) {
            (input, BigInt::zero())
        } else {
            let (input, num) = parse_decimal(input)?;
            if let Ok((input, _)) = tag::<&str, &str, VerboseError<&str>>(".")(input) {
                (input, num)
            } else {
                // decimal
                return Ok((input, Literal::Integer(num)));
            }
        };

    // floating- or fixed-point
    let (input, fraction) = digit1(input)?;
    todo!()
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

/// 'c', where `c` is a character or escaped character
fn parse_char(input: &str) -> PResult<Literal> {
    let (input, _) = tag("'")(input)?;
    let (input, result) = parse_char_escape(input)?;
    let (input, _) = tag("'")(input)?;

    Ok((input, Literal::Char(result)))
}

fn parse_char_escape(input: &str) -> PResult<char> {
    if let Ok((input, _)) = tag::<&str, &str, VerboseError<&str>>("\\")(input) {
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
            Expr::Literal(Literal::Integer(BigInt::from_usize(1234).unwrap()))
        );

        let input = "0xabcd";
        assert_eq!(
            parse_literal(input).unwrap().1,
            Expr::Literal(Literal::Integer(BigInt::from_usize(0xabcd).unwrap()))
        );

        let input = "0XEF45";
        assert_eq!(
            parse_literal(input).unwrap().1,
            Expr::Literal(Literal::Integer(BigInt::from_usize(0xef45).unwrap()))
        );

        let input = "0127";
        assert_eq!(
            parse_literal(input).unwrap().1,
            Expr::Literal(Literal::Integer(BigInt::from_usize(0o127).unwrap()))
        );

        let input = "'\n'";
        assert_eq!(
            parse_literal(input).unwrap().1,
            Expr::Literal(Literal::Char('\n'))
        );
    }
}

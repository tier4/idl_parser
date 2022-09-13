use num_bigint::BigInt;

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Expr {
    Literal(Literal),
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Literal {
    Char(char),
    String(String),
    Integer(BigInt),
}

/// `FixedPoint` represents a fixed point number.
/// `FixedPoint::value / 10^FixedPoint::scale`
#[derive(Debug, PartialEq, PartialOrd)]
pub struct FixedPoint {
    value: BigInt,
    scale: u64,
}

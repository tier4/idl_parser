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
    FixedPoint(FixedPoint),
    FloatingPoint(f64),
}

/// `FixedPoint` represents a fixed point number.
/// The actual value is `value / 10^scale`.
#[derive(Debug, PartialEq, PartialOrd)]
pub struct FixedPoint {
    pub value: BigInt,
    pub scale: u64,
}

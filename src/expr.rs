use num_bigint::BigInt;

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Module {
    pub id: String,
    pub definitions: Vec<Definition>,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Const {
    pub id: String,
    //    pub expr: Expr,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Definition {
    Module(Module),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct ScopedName {
    pub ids: Vec<String>,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum BaseType {
    Char,             // 8-bit character
    WChar,            // implementation dependent wide character
    Boolean,          // boolean
    Float,            // 32-bit floating point number
    Double,           // 64-bit floating point number
    LongDouble,       // 128 bits floating point number
    Short,            // signed 2^16
    Long,             // signed 2^32
    LongLong,         // signed 2^64
    UnsignedShort,    // unsigned 2^16
    UnsignedLong,     // unsigned 2^32
    UnsignedLongLong, // unsigned 2^64

    // an opaque 8-bit quantity that is guaranteed not to undergo any change by the middleware
    Octet,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Literal {
    Char(char),
    String(String),
    Integer(BigInt),
    FixedPoint(FixedPoint),
    FloatingPoint(f64),
}

/// `FixedPoint` represents a fixed point number.
/// The actual value is `value / 10^scale`.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct FixedPoint {
    pub value: BigInt,
    pub scale: u64,
}

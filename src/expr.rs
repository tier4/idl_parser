use num_bigint::BigInt;

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Module {
    pub id: String,
    pub definitions: Vec<Definition>,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Const {
    pub id: String,
    pub expr: ConstExpr,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Definition {
    Module(Module),
    Const(ConstDcl),
    Type(TypeDcl),
    Except(ExceptDcl),
    Interface(InterfaceDcl),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct ScopedName {
    pub ids: Vec<String>,
}

impl ScopedName {
    pub fn to_primitive(&self) -> Option<PrimitiveType> {
        if self.ids.len() == 1 {
            match self.ids[0].as_str() {
                "char" => Some(PrimitiveType::Char),
                "wchar" => Some(PrimitiveType::WChar),
                "boolean" => Some(PrimitiveType::Boolean),
                "octet" => Some(PrimitiveType::Octet),
                "short" => Some(PrimitiveType::Short),
                "long" => Some(PrimitiveType::Long),
                "double" => Some(PrimitiveType::Double),
                "float" => Some(PrimitiveType::Float),
                "any" => Some(PrimitiveType::Any),
                _ => None,
            }
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum PrimitiveType {
    Short,            // signed 2^16
    Long,             // signed 2^32
    LongLong,         // signed 2^64
    UnsignedShort,    // unsigned 2^16
    UnsignedLong,     // unsigned 2^32
    UnsignedLongLong, // unsigned 2^64
    Float,            // 32-bit floating point number
    Double,           // 64-bit floating point number
    LongDouble,       // 128 bits floating point number
    Char,             // 8-bit character
    WChar,            // implementation dependent wide character
    Boolean,          // boolean
    Any,              // any type

    // an opaque 8-bit quantity that is guaranteed not to undergo any change by the middleware
    Octet,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum ConstExpr {
    Or(Box<ConstExpr>, Box<ConstExpr>),
    Xor(Box<ConstExpr>, Box<ConstExpr>),
    And(Box<ConstExpr>, Box<ConstExpr>),
    RShift(Box<ConstExpr>, Box<ConstExpr>),
    LShift(Box<ConstExpr>, Box<ConstExpr>),
    Add(Box<ConstExpr>, Box<ConstExpr>),
    Sub(Box<ConstExpr>, Box<ConstExpr>),
    Mul(Box<ConstExpr>, Box<ConstExpr>),
    Div(Box<ConstExpr>, Box<ConstExpr>),
    Mod(Box<ConstExpr>, Box<ConstExpr>),
    UnaryOp(UnaryOpExpr),
    Literal(Literal),
    ScopedName(ScopedName),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum UnaryOpExpr {
    Minus(Box<ConstExpr>),
    Plus(Box<ConstExpr>),
    Negate(Box<ConstExpr>),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum StringType {
    UnlimitedSize,
    Sized(ConstExpr),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum WStringType {
    UnlimitedSize,
    Sized(ConstExpr),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum ConstType {
    PrimitiveType(PrimitiveType),
    ScopedName(ScopedName),
    StringType(StringType),
    WStringType(WStringType),
    FixedPointConst, // constant fixed point number
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Literal {
    Char(char),
    String(String),
    Integer(BigInt),
    FixedPoint(FixedPoint),
    FloatingPoint(f64),
    Boolean(bool),
}

/// `FixedPoint` represents a fixed point number.
/// The actual value is `value / 10^scale`.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct FixedPoint {
    pub value: BigInt,
    pub scale: u64,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct ConstDcl {
    pub const_type: ConstType,
    pub id: String,
    pub expr: ConstExpr,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct StructForwardDcl(pub String);

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum TypeSpec {
    PrimitiveType(PrimitiveType),
    ScopedName(ScopedName),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Member {
    pub type_spec: TypeSpec,
    pub ids: Vec<String>,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct StructDef {
    pub id: String,
    pub members: Vec<Member>,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum StructDcl {
    Def(StructDef),
    ForwardDcl(StructForwardDcl),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct EnumDcl {
    pub id: String,
    pub variants: Vec<String>,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum CaseLabel {
    Case(ConstExpr),
    Default,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Case {
    pub labels: Vec<CaseLabel>,
    pub spec: ElementSpec,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct ElementSpec {
    pub type_spec: TypeSpec,
    pub id: String,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum SwitchTypeSpec {
    PrimitiveType(PrimitiveType),
    ScopedName(ScopedName),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct UnionDef {
    pub id: String,
    pub switch_type_spec: SwitchTypeSpec,
    pub body: Vec<Case>,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum UnionDcl {
    Def(UnionDef),
    ForwardDcl(UnionForwardDcl),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct UnionForwardDcl(pub String);

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum ConstrTypeDcl {
    Struct(StructDcl),
    Union(UnionDcl),
    Enum(EnumDcl),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum TypeDcl {
    ConstrType(ConstrTypeDcl),
    Native(NativeDcl),
    Typedef(Typedef),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct NativeDcl(pub String);

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Typedef {
    pub type_dcl: TypedefType,
    pub declarators: Vec<AnyDeclarator>,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum TypedefType {
    Simple(TypeSpec),
    Constr(ConstrTypeDcl),
    Template(TemplateTypeSpec),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum SequenceType {
    Unlimited(TypeSpec),
    Limited(TypeSpec, ConstExpr),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum TemplateTypeSpec {
    Sequence(SequenceType),
    String(StringType),
    WString(WStringType),
    FixedPoint(FixedPtType),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct FixedPtType {
    pub total_digits: ConstExpr,      // up to 31
    pub fractional_digits: ConstExpr, // less than or equal to total_digits
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum AnyDeclarator {
    Simple(String),
    Array(ArrayDeclarator),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct ArrayDeclarator {
    pub id: String,
    pub array_size: Vec<ConstExpr>,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct ExceptDcl {
    pub id: String,
    pub members: Vec<Member>,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct InterfaceHeader {
    pub id: String,
    pub parents: Vec<ScopedName>,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct InterfaceForwardDcl(pub String);

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum OpTypeSpec {
    TypeSpec(TypeSpec),
    Void,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum ParamAttribute {
    In,
    Out,
    InOut,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct ParamDcl {
    pub attr: ParamAttribute,
    pub type_spec: TypeSpec,
    pub id: String,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Raises(pub Vec<ScopedName>);

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct OpDcl {
    pub id: String,
    pub type_spec: OpTypeSpec,
    pub params: Vec<ParamDcl>,
    pub raises: Option<Raises>,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum ReadonlyAttrDeclarator {
    WithRaises(String, Raises),
    IDs(Vec<String>),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct ReadonlyAttrSpec {
    pub type_spec: TypeSpec,
    pub declarator: ReadonlyAttrDeclarator,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct SetExcep(pub Vec<ScopedName>);

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct GetExcep(pub Vec<ScopedName>);

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum AttrRaises {
    GetExcep(GetExcep, Option<SetExcep>),
    SetExcep(SetExcep),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum AttrDeclarator {
    WithRaises(String, AttrRaises),
    IDs(Vec<String>),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct AttrSpec {
    pub type_spec: TypeSpec,
    pub declarator: AttrDeclarator,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum AttrDcl {
    Readonly(ReadonlyAttrSpec),
    ReadWrite(AttrSpec),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Export {
    Op(OpDcl),
    Attr(AttrDcl),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct InterfaceDef {
    pub header: InterfaceHeader,
    pub body: Vec<Export>,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum InterfaceDcl {
    Def(InterfaceDef),
    ForwardDcl(InterfaceForwardDcl),
}

pub const HT: u8 = 0x09; // horizontal tab
pub const VT: u8 = 0x0b; // vertical tab
pub const LF: u8 = 0x0a; // line feed, new line
pub const FF: u8 = 0x0c; // form feed
pub const SP: u8 = 0x20; // white space

pub fn is_latin1(c: u8) -> bool {
    matches!(c, 0x20..=0x7e | 0xa0..=0xff)
}

pub fn is_token(c: u8) -> bool {
    matches!(c, SP | HT | VT | FF)
}

pub fn is_space(c: u8) -> bool {
    c == SP
}

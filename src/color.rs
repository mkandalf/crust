#[deriving(PartialEq, Eq, Show)]
pub struct Color(pub uint);

pub static WHITE : Color = Color(0);
pub static BLACK : Color = Color(1);

pub fn to_int (Color(color) : Color) -> uint {
    return color;
}

pub fn flip (Color(color) : Color) -> Color {
    return Color(color ^ 1);
}

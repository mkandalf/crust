#[derive(PartialEq, Eq, Show, Copy)]
pub struct Color(pub u8);

pub const WHITE : Color = Color(0);
pub const BLACK : Color = Color(1);

pub fn to_int (Color(color) : Color) -> u8 {
    return color;
}

pub fn to_idx (Color(color) : Color) -> usize {
    return color as usize;
}

pub fn flip (Color(color) : Color) -> Color {
    return Color(color ^ 1);
}

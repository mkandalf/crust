use std::char;

use color::{Color, WHITE, BLACK};
use piece_type;
use piece_type::PieceType;

#[derive(PartialEq, Eq, Show, Copy)]
pub struct Piece(pub uint);

pub static NP : Piece = Piece(0); // No Piece

pub static WP : Piece = Piece(1);
pub static WN : Piece = Piece(2);
pub static WB : Piece = Piece(3);
pub static WR : Piece = Piece(4);
pub static WQ : Piece = Piece(5);
pub static WK : Piece = Piece(6);

pub static BP : Piece = Piece(9);
pub static BN : Piece = Piece(10);
pub static BB : Piece = Piece(11);
pub static BR : Piece = Piece(12);
pub static BQ : Piece = Piece(13);
pub static BK : Piece = Piece(14);

impl Piece {
    pub fn new (PieceType(t): PieceType, Color(c): Color) -> Piece {
        return Piece((c << 3) | t);
    }
}

pub fn to_type (Piece(piece): Piece) -> PieceType {
    PieceType(piece & 7)
}

pub fn to_color (Piece(piece): Piece) -> Color {
    Color((piece & 8) >> 3)
}

pub fn to_char (piece: Piece) -> char {
    let type_char = piece_type::to_char(to_type(piece));
    match to_color(piece) {
        WHITE => type_char.to_uppercase(),
        BLACK => type_char,
        _ => ' '
    }
}

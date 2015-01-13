use std::fmt;

use square;
use square::Square;
use piece_type::PieceType;
use std::cmp::Ordering::{self, Less, Equal, Greater};

pub const NULL : Move = Move(0);

#[derive(Eq, Hash, PartialOrd, Copy, Clone)]
pub struct Move(pub uint);

impl Move {
    #[inline(always)]
    pub fn new (Square(from):Square, Square(to):Square, attack: uint) -> Move {
        Move(to | from << 6 | (attack & 0x1) << 16)
    }
    #[inline(always)]
    pub fn new_castle(Square(from):Square, Square(to):Square) -> Move {
        Move(to | from << 6 | (1 << 12))
    }
    #[inline(always)]
    pub fn new_promotion(Square(from):Square, Square(to):Square, PieceType(p):PieceType, attack: uint) -> Move {
        Move(to | (from << 6) | ((p & 0x7) << 13) | ((attack & 0x1) << 16))
    }

    pub fn set_score(&mut self, score: int) -> () {
        let Move(m) = *self;
        *self = Move(m | ((score as i16 as u16 as uint) << 17));
    }

    pub fn get_score(&self) -> int {
        let Move(m) = *self;
        return ((m >> 17) & 0xffff) as u16 as i16 as int;
    }
}

impl PartialEq for Move {
    fn eq(&self, &Move(other):&Move) -> bool {
        let Move(m) = *self;
        return m & 0x1fff == other & 0x1fff;
    }
    fn ne(&self, &Move(other):&Move) -> bool {
        let Move(m) = *self;
        return m & 0x1fff != other & 0x1fff;
    }
}

impl Ord for Move {
    fn cmp(&self, other: &Move) -> Ordering {
        match get_from(*self).cmp(&get_from(*other)) {
            Less => Less,
            Equal => get_to(*self).cmp(&get_to(*other)),
            Greater => Greater
        }
    }
}

impl fmt::Show for Move {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if (*self) == NULL {
            return write!(f, "NULL");
        }
        return write!(f, "{:?}{:?}", get_from(*self), get_to(*self));
    }
}

pub fn from_str (s: &str) -> Option<Move> {
    use piece_type;
    match s.len() {
        4 => {
            let from: &str = s.slice(0,2);
            let to: &str = s.slice(2,4);
            match (square::from_str(from), square::from_str(to)) {
                // TODO: Do this better
                (Some(f), Some(t)) => Some(Move::new(f, t, 0)),
                _ => None
            }
        },
        5 => {
            let from: &str = s.slice(0,2);
            let to: &str = s.slice(2,4);
            let promotion: char = s.char_at(4).to_lowercase();
            match (square::from_str(from), square::from_str(to), piece_type::from_char(promotion)) {
                (Some(f), Some(t), Some(p)) => {
                    Some(Move::new_promotion(f, t, p, 0))
                }
                _ => None
            }
        },
        _ => None
    }
}

pub fn to_int (Move(m): Move) -> uint {
    return m;
}

pub fn get_to (Move(m): Move) -> Square {
    Square(m & 0x3f)
}

pub fn get_from (Move(m): Move) -> Square {
    Square((m >> 6) & 0x3f)
}

pub fn get_promotion (Move(m): Move) -> PieceType {
    PieceType((m >> 13) & 7)
}

pub fn is_castle (Move(m): Move) -> bool {
    (m >> 12) & 1 > 0
}

pub fn is_attack (Move(m): Move) -> bool {
    (m >> 16) & 1 > 0
}

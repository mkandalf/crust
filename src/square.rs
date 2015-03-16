use std::ops;
use std::num::SignedInt;
use std::fmt;
use std::cmp::Ordering::{self, Less, Greater};

#[derive(PartialOrd, Eq, PartialEq, Copy)]
pub struct Square(pub u8);
impl Square {
    pub fn new (rank:u8, file:u8) -> Square {
        Square(file | (rank << 3))
    }
}

impl ops::Add<u8> for Square {
    type Output = Square;

    fn add (self, rhs: u8) -> Square {
        let Square(lhs) = self;
        return Square(lhs + rhs);
    }
}

impl ops::Sub<u8> for Square {
    type Output = Square;

    fn sub (self, rhs: u8) -> Square {
        let Square(lhs) = self;
        return Square(lhs - rhs);
    }
}

impl ops::Rem<u8> for Square {
    type Output = Square;

    fn rem (self, rhs: u8) -> Square {
        let Square(lhs) = self;
        return Square(lhs % rhs);
    }
}

impl Ord for Square {
    fn cmp(&self, &other: &Square) -> Ordering {
        if file(*self) < file(other) {
            return Less;
        } else if file(*self) == file(other) {
            return rank(*self).cmp(&rank(other));
        } else {
            return Greater;
        }
    }
}

impl fmt::Show for Square {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if (*self) == NULL {
            return write!(f, "NULL");
        }
        return write!(f, "{}{}", (file(*self) + 97) as u8 as char, rank(*self) + 1);
    }
}

pub fn from_str (s: &str) -> Option<Square> {
    if s.len() != 2 { return None }
    let chars: Vec<char> = s.chars().collect();
    let rank = chars[1] as u8 - 49;
    let file = chars[0] as u8 - 97;
    if rank > 7 || file > 7 { return None }
    return Some(Square::new(rank as u8, file as u8));
}

pub fn abs_diff (Square(s1): Square, Square(s2): Square) -> u8 {
    return SignedInt::abs(s1 as i8 - s2 as i8) as u8;
}

pub fn to_int (Square(s): Square) -> u8 {
    return s;
}

pub fn file (Square(s): Square) -> u8 {
    return s & 0x7;
}

pub fn rank (Square(s): Square) -> u8 {
    return (s >> 3) & 0x7;
}

pub static NULL : Square = Square(64);

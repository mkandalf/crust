use std::fmt;
use std::iter::range_step;
use std::ops;

use constants;
use piece::Piece;
use square::Square;

#[deriving(PartialEq, Eq)]
pub struct BitBoard(pub u64);

mod test {
    use bitboard;
    use bitboard::BitBoard;
    use square::Square;
    #[test]
    fn bit_scan_forward() {
        assert!(bitboard::bit_scan_forward(BitBoard(0x01)) == Square(0));
        assert!(bitboard::bit_scan_forward(BitBoard(0x10)) == Square(4));
        assert!(bitboard::bit_scan_forward(BitBoard(0x1000)) == Square(12));
    }

    #[test]
    fn popcnt() {
        assert!(bitboard::popcnt(BitBoard(0x01)) == 1);
        assert!(bitboard::popcnt(BitBoard(0xff0)) == 8);
        assert!(bitboard::popcnt(BitBoard(0x1f00)) == 5);
    }

    #[test]
    fn is_bit_set() {
        assert!(bitboard::is_bit_set(BitBoard(0x01), Square(0)) == true);
        assert!(bitboard::is_bit_set(BitBoard(0xff0), Square(5)) == true);
        assert!(bitboard::is_bit_set(BitBoard(0x1f00), Square(3)) == false);
    }

    #[test]
    fn clear_bit() {
        let mut b1 = &mut BitBoard(0x01);
        bitboard::clear_bit(b1, Square(0));
        assert!(*b1 == BitBoard(0x0));

        let mut b2 = &mut BitBoard(0xff0);
        bitboard::clear_bit(b2, Square(0));
        assert!(*b2 == BitBoard(0xff0));

        let mut b3 = &mut BitBoard(0x1f00);
        bitboard::clear_bit(b3, Square(8));
        assert!(*b3 == BitBoard(0x1e00));
    }

    #[test]
    fn set_bit() {
        let mut b1 = &mut BitBoard(0x01);
        bitboard::set_bit(b1, Square(0));
        assert!(*b1 == BitBoard(0x1));

        let mut b2 = &mut BitBoard(0xff0);
        bitboard::set_bit(b2, Square(0));
        assert!(*b2 == BitBoard(0xff1));

        let mut b3 = &mut BitBoard(0x1f00);
        bitboard::set_bit(b3, Square(8));
        assert!(*b3 == BitBoard(0x1f00));
    }
}

impl ops::BitAnd<BitBoard, BitBoard> for BitBoard {
    fn bitand (&self, &BitBoard(b1) : &BitBoard) -> BitBoard {
        let BitBoard(b2) = *self;
        return BitBoard(b1 & b2);
    }
}

impl ops::BitOr<BitBoard, BitBoard> for BitBoard {
    fn bitor (&self, &BitBoard(b1) : &BitBoard) -> BitBoard {
        let BitBoard(b2) = *self;
        return BitBoard(b1 | b2);
    }
}

impl ops::BitXor<BitBoard, BitBoard> for BitBoard {
    fn bitxor (&self, &BitBoard(b1) : &BitBoard) -> BitBoard {
        let BitBoard(b2) = *self;
        return BitBoard(b1 ^ b2);
    }
}

impl ops::Shr<uint, BitBoard> for BitBoard {
    fn shr (&self, rhs : &uint) -> BitBoard {
        let BitBoard(b) = *self;
        return BitBoard(b >> *rhs);
    }
}

impl ops::Shl<uint, BitBoard> for BitBoard {
    fn shl (&self, rhs : &uint) -> BitBoard {
        let BitBoard(b) = *self;
        return BitBoard(b << *rhs);
    }
}

impl ops::Sub<BitBoard, BitBoard> for BitBoard {
    fn sub (&self, &BitBoard(rhs) : &BitBoard) -> BitBoard {
        let BitBoard(b) = *self;
        return BitBoard(b - rhs);
    }
}

impl ops::Mul<u64, BitBoard> for BitBoard {
    fn mul (&self, &rhs : &u64) -> BitBoard {
        let BitBoard(b) = *self;
        return BitBoard(b * rhs);
    }
}

impl ops::Not<BitBoard> for BitBoard {
    fn not (&self) -> BitBoard {
        let BitBoard(b) = *self;
        return BitBoard(!b);
    }
}

pub fn clear_lsb(BitBoard(b) : BitBoard) -> BitBoard {
    return BitBoard(b & (b - 1));
}

pub fn from_pieces(pieces : [Piece, ..64], f:|Piece| -> bool) -> BitBoard {
    let mut b = BitBoard(0);
    for x in pieces.iter() {
        b = b >> 1;
        b = b | BitBoard(if f(*x) { 1 << 63 } else { 0 });
    }
    return b;
}

pub static NOT_FILE_A : BitBoard = BitBoard(0xfefefefefefefefe);
pub static NOT_FILE_H : BitBoard = BitBoard(0x7f7f7f7f7f7f7f7f);
pub static RANK_1 : BitBoard = BitBoard(0x00000000000000FF);
pub static RANK_3 : BitBoard = BitBoard(0x0000000000FF0000);
pub static RANK_6 : BitBoard = BitBoard(0x0000FF0000000000);
pub static RANK_8 : BitBoard = BitBoard(0xFF00000000000000);

pub fn east_one(b : BitBoard) -> BitBoard {
    return (b << 1) & NOT_FILE_A;
}
pub fn west_one(b : BitBoard) -> BitBoard {
    return (b >> 1) & NOT_FILE_H;
}
pub fn north_one(b : BitBoard) -> BitBoard {
    return b << 8;
}
pub fn south_one(b : BitBoard) -> BitBoard {
    return b >> 8;
}
pub fn no_ea_one(b : BitBoard) -> BitBoard {
    return (b << 9) & NOT_FILE_A;
}
pub fn so_ea_one(b : BitBoard) -> BitBoard {
    return (b >> 7) & NOT_FILE_A;
}
pub fn no_we_one(b : BitBoard) -> BitBoard {
    return (b << 7) & NOT_FILE_H;
}
pub fn so_we_one(b : BitBoard) -> BitBoard {
    return (b >> 9) & NOT_FILE_H;
}

pub fn single_bit(Square(s): Square) -> BitBoard {
    return BitBoard (1 << s);
}

pub fn circular_left_shift(BitBoard(b): BitBoard, shift: uint) -> BitBoard {
    return BitBoard(b << shift | b >> (64 - shift));
}

impl fmt::Show for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for rank in range_step(7, -1, -1 as int) {
            write!(f, "+---+---+---+---+---+---+---+---+\n");
            for file in range(0, 8) {
                let square = Square::new(rank as uint, file);
                write!(f, "| {} ", if is_bit_set(*self, square) {'X'} else {' '});
            }
            write!(f, "|\n");
        }
        return write!(f, "+---+---+---+---+---+---+---+---+\n");
    }
}

#[inline(always)]
pub fn bit_scan_forward(BitBoard(mask) : BitBoard) -> Square {
    unsafe {
        let mut ret;
        asm!(
            "bsfq $1, $0"
            :"=r"(ret)
            :"r"(mask)
        );
        return Square(ret);
    }
}

#[inline(always)]
pub fn popcnt(BitBoard(mask) : BitBoard) -> uint {
    unsafe {
        let mut ret;
        asm!(
            "popcnt $1, $0"
            :"=r"(ret)
            :"r"(mask)
        );
        return ret;
    }
}

pub fn is_bit_set (BitBoard(board): BitBoard, Square(square): Square) -> bool {
    ((1_u64 << square) & board) != 0
}

pub fn set_bit (bitboard: &mut BitBoard, Square(square): Square) -> () {
    let BitBoard(board) = *bitboard;
    *bitboard = BitBoard(board | (1 << square));
}

pub fn clear_bit (bitboard: &mut BitBoard, Square(square): Square) -> () {
    let BitBoard(board) = *bitboard;
    *bitboard = BitBoard(board ^ (1 << square));
}

pub fn in_between(Square(s1): Square, Square(s2): Square) -> BitBoard {
    return constants::get_between_bb()[s1][s2];
}

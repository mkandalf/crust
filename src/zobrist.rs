use std::ops;
use std::fmt;

use piece::Piece;
use piece_type::PieceType;
use color;
use color::Color;
use _move::Move;
use square::Square;

#[derive(Eq, PartialEq, Show, Copy)]
pub struct ZobristHash(u64);

static mut piece_keys : [ZobristHash; 768] = [ZobristHash(0); 768];
static mut castle_keys : [ZobristHash; 16] = [ZobristHash(0); 16];
static mut ep_keys : [ZobristHash; 8] = [ZobristHash(0); 8];
static mut black_key : ZobristHash = ZobristHash(0);

pub fn init() -> () {
    use std::rand::{SeedableRng, StdRng, Rng};
    let mut rng: StdRng = SeedableRng::from_seed([1us].as_slice());
    for i in 0..768 {
        unsafe {
            piece_keys[i] = ZobristHash(rng.gen::<u64>());
        }
    }
    for i in 0..16 {
        unsafe {
            castle_keys[i] = ZobristHash(rng.gen::<u64>());
        }
    }
    for i in 0..8 {
        unsafe {
            ep_keys[i] = ZobristHash(rng.gen::<u64>());
        }
    }
    unsafe {
        black_key = ZobristHash(rng.gen::<u64>());
    }
}

impl ops::BitXor<ZobristHash> for ZobristHash {
    type Output = ZobristHash;

    fn bitxor (self, ZobristHash(rhs) : ZobristHash) -> ZobristHash {
        let ZobristHash(lhs) = self;
        return ZobristHash(lhs ^ rhs);
    }
}

impl ops::Rem<usize> for ZobristHash {
    type Output = usize;

    fn rem (self, rhs: usize) -> usize {
        let ZobristHash(lhs) = self;
        return (lhs as usize % rhs);
    }
}

//fn get_hash_for_piece_square(PieceType(p): PieceType,
                             //Color(c): Color,
                             //Square(s): Square) -> &ZobristHash {
    //return piece_keys[(384 * c) + ((p - 1) * 64) + s];
//}
const TABLE_SIZE : usize = 1048583; 

pub struct Table {
    table: [Entry; TABLE_SIZE]
}

impl Table {
    pub fn new() -> Table {
        Table{ table: [NULL_ENTRY; TABLE_SIZE]}
    }

    pub fn probe(&self, hash: ZobristHash, depth: u8, alpha: i16, beta: i16) -> (Option<i16>, Move) {
        use _move;
        let entry: Entry = self.table[hash % TABLE_SIZE];
        if entry != NULL_ENTRY && entry.get_hash() == hash {
            if entry.get_depth() >= depth {
                match entry.get_type() {
                    ALPHA_BOUND =>  {
                        if alpha >= entry.get_score() {
                            return (Some(entry.get_score()), _move::NULL);
                        }
                    }
                    BETA_BOUND => {
                        if beta <= entry.get_score() {
                            return (Some(entry.get_score()), _move::NULL);
                        }
                    }
                    EXACT_BOUND => {
                        return (Some(entry.get_score()), _move::NULL);
                    }
                    _ => { }
                }
            }
            let _move = entry.get_move();
            return (None, _move);
        }
        return (None, _move::NULL);
    }

    pub fn best_move(&self, hash: ZobristHash) -> Option<Move> {
        use _move;
        let entry: Entry = self.table[hash % TABLE_SIZE];
        if entry != NULL_ENTRY && entry.get_hash() == hash {
            let _move = entry.get_move();
            match _move {
                _move::NULL => None,
                _ => Some(_move)
            }
        } else {
            return None;
        }
    }

    pub fn record(&mut self, hash: ZobristHash, score: i16, _move: Move, depth: u8, bound: Bound, ancient: u8) -> () {
        let entry: Entry = self.table[hash % TABLE_SIZE];
        if depth >= entry.get_depth() || (entry.get_ancient() != ancient && entry.get_hash() != hash) {
            self.table[hash % TABLE_SIZE] = Entry::new(hash, score, _move, depth, bound, ancient);
        }
    }

    pub fn slots_filled(&self) -> u64 {
        let mut c = 0;
        for i in (0..TABLE_SIZE) {
            if self.table[i] != NULL_ENTRY {
                c += 1;
            }
        }
        return c;
    }

    pub fn print_nonempty(&self) -> () {
        for i in (0..TABLE_SIZE) {
            if self.table[i] != NULL_ENTRY {
                print!("{:?}\n", self.table[i]);
            }
        }
    }
}

#[derive(Eq, PartialEq, Copy)]
pub struct Bound(u8);

pub const ALPHA_BOUND : Bound = Bound(0);
pub const BETA_BOUND : Bound = Bound(1);
pub const EXACT_BOUND : Bound = Bound(2);

impl fmt::Show for Bound {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match *self {
            ALPHA_BOUND => "ALPHA",
            BETA_BOUND => "BETA",
            EXACT_BOUND => "EXACT",
            _ => "NULL"
        };
        return write!(f, "{:?}", s);
    }
}

// [64 bits: zobrist]
// [18 bits: padding] [1: ancient] [21: score] [2: bound] [16: best move] [8: depth] 
#[derive(Eq, PartialEq, Copy)]
struct Entry {
    zobrist: ZobristHash,
    data: u64
}

static NULL_ENTRY : Entry = Entry {
    zobrist: ZobristHash(0),
    data: 0
};

impl fmt::Show for Entry {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "Depth: {:?}, Move: {:?}, Score: {:?}, Bound: {:?}, Ancient: {:?}, Hash: {:?}", self.get_depth(), self.get_move(), self.get_score(), self.get_type(), self.get_ancient(), self.get_hash());
    }
}

impl Entry {
    fn new(hash: ZobristHash, score: i16, Move(m): Move, depth: u8, Bound(b): Bound, ancient: u8) -> Entry {
        Entry {
            zobrist: hash,
            data: ((depth as u64 & 0xff)
                   | ((m as u64 & 0xffff) << 8)
                   | ((b as u64 & 0x3) << 24)
                   | ((score as i16 as u16 as u64) << 26)
                   | ((ancient as u64 & 0x1) << 42))
        }
    }

    fn get_depth(&self) -> u8 {
        return (self.data & 0xff) as u8;
    }

    fn get_move(&self) -> Move {
        return Move(((self.data >> 8) & 0xffff) as u64);
    }

    fn get_type(&self) -> Bound {
        return Bound(((self.data >> 24) & 0x3) as u8);
    }

    fn get_score(&self) -> i16 {
        return ((self.data >> 26) & 0xffffffff) as u16 as i16 as i16;
    }

    fn get_ancient(&self) -> u8 {
        return ((self.data >> 42) & 0x1) as u8;
    }

    fn get_hash(&self) -> ZobristHash {
        return self.zobrist;
    }
}


impl ZobristHash {
    pub fn to_int(&self) -> u64 {
        let ZobristHash(z) = *self;
        z
    }

    pub fn new() -> ZobristHash {
        return ZobristHash(0);
    }

    pub fn init(pieces: [Piece; 64],
                to_move: Color,
                castling: u8, 
                ep_square: Square)
                -> ZobristHash {
        use piece;
        use piece_type;

        let mut hash = ZobristHash::new();
        let mut sq = Square(0);

        for piece in pieces.iter() {
            if piece::to_type(*piece) != piece_type::NO_PIECE_TYPE {
                let piece_type = piece::to_type(*piece);
                let color = piece::to_color(*piece);
                hash.set_piece(piece_type, color, sq);
            }
            sq = sq + 1;
        }

        if to_move == color::BLACK {
            hash.flip_color();
        }

        hash.set_castling(castling);

        hash.set_ep(ep_square);

        return hash;
    }

    pub fn clear_piece(&mut self,
                        PieceType(p): PieceType,
                        Color(c): Color,
                        Square(s): Square)
                        -> () {
        debug_assert!(p <= 7);
        debug_assert!(p >= 1);
        debug_assert!(c == 0 || c == 1);
        debug_assert!(s <= 63);
        unsafe {
            *self = *self ^ piece_keys[((384 * c as usize) + ((p as usize - 1) * 64) + s as usize)];
        }
        //*self = self ^ get_hash_for_piece_square(p, c, s);
    }

    pub fn set_piece(&mut self,
                     PieceType(p): PieceType,
                     Color(c): Color,
                     Square(s): Square)
                     -> () {
        unsafe {
            *self = *self ^ piece_keys[((384 * c as usize) + ((p as usize - 1) * 64) + s as usize)];
        }
        //*self = self ^ get_hash_for_piece_square(p, c, s);
    }

    pub fn clear_ep(&mut self, s: Square) -> () {
        use square::file;
        unsafe {
            *self = *self ^ ep_keys[file(s) as usize];
        }
    }

    pub fn set_ep(&mut self, s: Square) -> () {
        use square::file;
        unsafe {
            *self = *self ^ ep_keys[file(s) as usize];
        }
    }

    pub fn flip_color(&mut self) -> () {
        unsafe {
            *self = *self ^ black_key;
        }
    }

    pub fn clear_castling(&mut self, castling: u8) -> () {
        unsafe {
            *self = *self ^ castle_keys[castling as usize];
        }
    }

    pub fn set_castling(&mut self, castling: u8) -> () {
        unsafe {
            *self = *self ^ castle_keys[castling as usize];
        }
    }
}

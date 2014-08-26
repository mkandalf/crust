#[deriving(PartialEq, Eq, Show)]
pub struct PieceType(pub uint);

pub static NO_PIECE_TYPE : PieceType = PieceType(0);
pub static PAWN : PieceType = PieceType(1);
pub static KNIGHT : PieceType = PieceType(2);
pub static BISHOP : PieceType = PieceType(3);
pub static ROOK : PieceType = PieceType(4);
pub static QUEEN : PieceType = PieceType(5);
pub static KING : PieceType = PieceType(6);

static PIECE_TYPE_CHARS : [char, ..7] = [' ', 'p', 'n', 'b', 'r', 'q', 'k'];
static CHAR_PIECE_TYPES : [Option<PieceType>, ..26] = 
    [None, Some(BISHOP), None, None, None, None, None, None, None, None, Some(KING), None, None,
     Some(KNIGHT), None, Some(PAWN), Some(QUEEN), Some(ROOK), None, None, None, None, None, None, None, None];

pub fn to_char (PieceType(piece_type): PieceType) -> char {
    PIECE_TYPE_CHARS[piece_type]
}

// from a lowercase char
pub fn from_char (c: char) -> Option<PieceType> {
    let idx = c as u8 - 97;
    if idx > 25 { return None }
    return CHAR_PIECE_TYPES[idx as uint];
}

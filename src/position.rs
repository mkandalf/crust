use std::collections::HashMap;
use std::fmt;

use bitboard;
use bitboard::{BitBoard, clear_bit, set_bit, bit_scan_forward, circular_left_shift, clear_lsb,
in_between, no_we_one, so_we_one, no_ea_one, so_ea_one, from_pieces};
use color;
use color::{Color, WHITE, BLACK};
use constants;
use _move::{Move, get_from, get_to, get_promotion, is_castle};
use piece;
use piece::{Piece, NP,
            WP, WN, WB, WR, WQ, WK,
            BP, BN, BB, BR, BQ, BK, to_color, to_type};
use piece_type::{PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING};
use piece_type::{PieceType, NO_PIECE_TYPE, from_char};
use square;
use square::{Square};
use zobrist::ZobristHash;

pub struct Position {
    pub occupied: BitBoard,
    pub to_move: Color,
    pub by_piece: [BitBoard, ..7],
    pub by_color: [BitBoard, ..2],
    pub castling_rights : u8,
    pub board: [Piece, ..64],
    pub ep_square: Square,
    pub ep_square_hist: Vec<Square>,
    pub castling_hist: Vec<u8>,
    pub half_moves_hist: Vec<u8>,
    pub capture_hist: Vec<PieceType>,
    pub hash: ZobristHash,
    pub half_moves: u8
}

impl Position {
    pub fn from_fen(fen: &str) -> Position {
        use std::char::{is_digit, is_alphabetic, to_digit};

        let parts: Vec<&str> = fen.split(' ').collect();
        assert!(parts.len() >= 5);
        assert!(parts[1] == "w" || parts[1] == "b");
        let mut rank_idx: uint = 7;
        let rank_strs: Vec<&str> = parts[0].split('/').collect();
        assert!(rank_strs.len() == 8);
        let mut board: [Piece, ..64] = [NP, ..64];
        for rank_str in rank_strs.iter() {
            let mut file_idx: uint = 0;
            for c in rank_str.chars() {
                if is_digit(c) {
                    file_idx += to_digit(c, 10).expect("");
                } else if is_alphabetic(c) {
                    let color = if c.is_lowercase() { BLACK } else { WHITE };
                    board[rank_idx * 8 + file_idx] = Piece::new(from_char(c.to_lowercase()).expect("fen failed"), color);
                    file_idx += 1;
                }
            }
            rank_idx -= 1;
            assert!(file_idx == 8);
        }

        let mut castling_rights = 0;
        let castling_vec: Vec<char> = parts[2].chars().collect();
        if castling_vec.contains(&'K') { castling_rights |= 1 }
        if castling_vec.contains(&'Q') { castling_rights |= 4 }
        if castling_vec.contains(&'k') { castling_rights |= 2 }
        if castling_vec.contains(&'q') { castling_rights |= 8 }

        let to_move = if parts[1] == "w" { WHITE } else { BLACK };

        let ep_square = if parts[3].is_alphanumeric() { square::from_str(parts[3]).expect("fen failed") } else { square::NULL };

        return Position {
            occupied: from_pieces(board, |piece| to_type(piece) != NO_PIECE_TYPE),
            by_piece: [
                from_pieces(board, |piece| to_type(piece) == NO_PIECE_TYPE),
                from_pieces(board, |piece| to_type(piece) == PAWN),
                from_pieces(board, |piece| to_type(piece) == KNIGHT),
                from_pieces(board, |piece| to_type(piece) == BISHOP),
                from_pieces(board, |piece| to_type(piece) == ROOK),
                from_pieces(board, |piece| to_type(piece) == QUEEN),
                from_pieces(board, |piece| to_type(piece) == KING),
            ],
            board: board,
            by_color: [from_pieces(board, |piece| to_color(piece) == WHITE && to_type(piece) != NO_PIECE_TYPE),
                       from_pieces(board, |piece| to_color(piece) == BLACK && to_type(piece) != NO_PIECE_TYPE)],
            to_move: to_move,
            castling_rights: castling_rights,
            ep_square: if parts[3].is_alphanumeric() { square::from_str(parts[3]).expect("fen failed") } else { square::NULL },
            ep_square_hist: vec![],
            castling_hist: vec![],
            half_moves_hist: vec![],
            capture_hist: vec![],
            hash: ZobristHash::init(board, to_move, castling_rights, ep_square),
            half_moves: from_str(parts[4]).unwrap()
        }
    }

    pub fn new () -> Position {
        let mut p = Position {
            occupied : BitBoard(0xffff00000000ffff_u64),
            by_piece: [
                BitBoard(0x0000ffffffff0000_u64),
                BitBoard(0x00ff00000000ff00_u64),
                BitBoard(0x4200000000000042_u64),
                BitBoard(0x2400000000000024_u64),
                BitBoard(0x8100000000000081_u64),
                BitBoard(0x0800000000000008_u64),
                BitBoard(0x1000000000000010_u64),
            ],
            board: [
                WR, WN, WB, WQ, WK, WB, WN, WR,
                WP, WP, WP, WP, WP, WP, WP, WP,
                NP, NP, NP, NP, NP, NP, NP, NP,
                NP, NP, NP, NP, NP, NP, NP, NP,
                NP, NP, NP, NP, NP, NP, NP, NP,
                NP, NP, NP, NP, NP, NP, NP, NP,
                BP, BP, BP, BP, BP, BP, BP, BP,
                BR, BN, BB, BQ, BK, BB, BN, BR,
            ],
            by_color: [ BitBoard(0xFFFF_u64), BitBoard(0xFFFF000000000000_u64) ],
            to_move: WHITE,
            castling_rights: 15,
            ep_square: square::NULL,
            ep_square_hist: vec![],
            castling_hist: vec![],
            half_moves_hist: vec![],
            capture_hist: vec![],
            hash: ZobristHash::new(),
            half_moves: 0
        };
        p.hash = ZobristHash::init(p.board, p.to_move, p.castling_rights, p.ep_square);
        p
    }

    pub fn piece_on (&self, Square(square): Square) -> Piece {
        self.board[square]
    }

    pub fn set_piece_on (&mut self, Square(square): Square, piece: Piece) -> () {
        self.board[square] = piece;
    }

    pub fn type_of_piece_on (&self, square: Square) -> PieceType {
        piece::to_type(self.piece_on(square))
    }

    pub fn color_of_piece_on (&self, square: Square) -> Color {
        piece::to_color(self.piece_on(square))
    }

    pub fn pieces_of_type (&self, PieceType(piece_type): PieceType) -> BitBoard {
        self.by_piece[piece_type]
    }

    pub fn mut_pieces_of_type (&mut self, PieceType(piece_type): PieceType) -> &mut BitBoard {
        &mut self.by_piece[piece_type]
    }

    pub fn pieces_of_color (&self, Color(color): Color) -> BitBoard {
        self.by_color[color]
    }

    pub fn mut_pieces_of_color (&mut self, Color(color): Color) -> &mut BitBoard {
        &mut self.by_color[color]
    }

    pub fn pieces (&self, Color(color): Color, PieceType(piece_type): PieceType) -> BitBoard {
        self.by_piece[piece_type] & self.by_color[color]
    }

    pub fn move_is_legal(&mut self, _move: Move, pinned: BitBoard, checkers: BitBoard) -> bool {
        let from = get_from(_move);
        let to = get_to(_move);
        let PieceType(piece) = self.type_of_piece_on(from);

        if checkers != BitBoard(0) {
            if clear_lsb(checkers) != BitBoard(0) {
                // Double check, so make sure we move the king out
                if PieceType(piece) != KING {
                    return false;
                }
            } else {
                // If there is only one checker
                if PieceType(piece) != KING {
                    // Stop the check, either by blocking it or by capturing
                    let checker_sq = bit_scan_forward(checkers);
                    let checker_type = self.type_of_piece_on(checker_sq);
                    if checker_type == KNIGHT {
                        if to != checker_sq {
                            return false;
                        }
                    } else if checker_type == PAWN {
                        // There is no way to evade check from a pawn
                        // without capturing it or moving the king
                        if to != checker_sq {
                            return false;
                        }
                    } else {
                        // The move must block the check, otherwise return false
                        let king_sq = bit_scan_forward(self.pieces(self.to_move, KING));
                        let blocks = (bitboard::in_between(king_sq, checker_sq) & bitboard::single_bit(to))
                                     != BitBoard(0);
                        let captures = to == checker_sq;
                        if !blocks & !captures {
                            return false;
                        }
                    }
                }
            }
            if is_castle(_move) {
                return false;
            }
        } else {
            // Only check for castling being valid in the no checkers case,
            // since we already invalidated it otherwise
            if is_castle(_move) {
                static castle_sqs : [[[Square, ..2], ..2], ..2] =
                    [[[Square(5), Square(6)], [Square(3), Square(2)]],
                     [[Square(61), Square(62)], [Square(59), Square(58)]]]; 
                let queenside_castle = square::file(to) == 2;
                let sqs = castle_sqs[color::to_int(self.to_move)][queenside_castle as uint];
                let without_king = self.occupied ^ self.pieces(self.to_move, KING);
                if self.attacks_for_occ(sqs[0], without_king) != BitBoard(0) {
                    return false;
                }
                static rook_sqs : [[Square, ..2], ..2] =
                    [[Square(7), Square(0)], [Square(63), Square(56)]];
                let without_king_or_rook =
                    self.occupied
                    ^ bitboard::single_bit(
                        rook_sqs[color::to_int(self.to_move)][queenside_castle as uint]
                      );
                if self.attacks_for_occ(sqs[1], without_king_or_rook) != BitBoard(0) {
                    return false;
                }
            }
        }
        // could switch this to checking that the direction between
        // squares is the same.
        if (bitboard::single_bit(from) & pinned) != BitBoard(0) {
            let king_sq = bit_scan_forward(self.pieces(self.to_move, KING));
            if ((bitboard::in_between(king_sq, from) & bitboard::single_bit(to))
                | (bitboard::in_between(king_sq, to) & bitboard::single_bit(from)))
               == BitBoard(0) {
                return false;
            }
        }
        if to == self.ep_square && PieceType(piece) == PAWN {
            if (bitboard::single_bit(to) & pinned) != BitBoard(0) {
                let king_sq = bit_scan_forward(self.pieces(self.to_move, KING));
                static single_push_diffs : [uint, ..2] = [8, 64 - 8];
                let target = ((from + 64) - single_push_diffs[color::to_int(self.to_move)]) % 64;
                if ((bitboard::in_between(king_sq, target) & bitboard::single_bit(to))
                    | (bitboard::in_between(king_sq, to) & bitboard::single_bit(target)))
                   == BitBoard(0) {
                    return false;
                }
            }
        }
        // No need to check here if it's a castle, we already did it above
        if PieceType(piece) == KING && !is_castle(_move) {
            if self.attacks_for_occ(to, self.occupied ^ self.pieces(self.to_move, KING)) != BitBoard(0) {
                return false;
            }
        }
        return true;
    }

    pub fn make_move(&mut self, _move: Move) -> () {
        self.ep_square_hist.push(self.ep_square);
        self.castling_hist.push(self.castling_rights);
        self.half_moves_hist.push(self.half_moves);

        self.half_moves += 1;

        let from = get_from(_move);
        let to = get_to(_move);

        let us = self.to_move;
        let Color(us_int) = us;
        let them = color::flip(self.to_move);
        let Color(them_int) = them;

        let piece = self.type_of_piece_on(from);
        let capture = self.type_of_piece_on(to);

        self.capture_hist.push(capture);

        if capture != NO_PIECE_TYPE {
            self.half_moves = 0;
            clear_bit(self.mut_pieces_of_type(capture), to);
            clear_bit(self.mut_pieces_of_color(them), to);
            self.hash.clear_piece(capture, them, to);
        }

        let on_from = self.piece_on(from);

        // Remove piece on old square
        clear_bit(self.mut_pieces_of_type(piece), from);
        clear_bit(self.mut_pieces_of_color(us), from);
        clear_bit(&mut self.occupied, from);
        clear_bit(&mut self.by_piece[0], to);
        self.set_piece_on(from, NP);
        self.hash.clear_piece(piece, us, from);

        // Place piece on new square
        set_bit(self.mut_pieces_of_type(piece), to);
        set_bit(self.mut_pieces_of_color(us), to);
        set_bit(&mut self.occupied, to);
        set_bit(&mut self.by_piece[0], from);
        self.set_piece_on(to, on_from);
        self.hash.set_piece(piece, us, to);

        self.hash.clear_ep(self.ep_square);
        let old_ep_square = self.ep_square;
        self.ep_square = square::NULL;
        if piece == PAWN {
            static single_push_diffs : [uint, ..2] = [64 - 8, 8];

            self.half_moves = 0;

            if square::abs_diff(to, from) == 16 {
                // Double push, set ep square
                self.ep_square = ((from + 64)
                                  - single_push_diffs[us_int])
                                 % 64;
            } else if to == old_ep_square {
                // En passant
                let target = Square::new(square::rank(from), square::file(to));
                clear_bit(self.mut_pieces_of_type(PAWN), target);
                clear_bit(self.mut_pieces_of_color(them), target);
                clear_bit(&mut self.occupied, target);
                set_bit(&mut self.by_piece[0], target);
                self.set_piece_on(target, NP);
                self.hash.clear_piece(PAWN, them, target);
            } else if get_promotion(_move) != NO_PIECE_TYPE {
                // Promotion
                clear_bit(self.mut_pieces_of_type(piece), to);
                self.set_piece_on(to, Piece::new(get_promotion(_move), us));
                set_bit(self.mut_pieces_of_type(get_promotion(_move)), to);
                self.hash.clear_piece(PAWN, us, to);
                self.hash.set_piece(get_promotion(_move), us, to);
            }
        } else if piece == KING {
            if is_castle(_move) {
                static rook_sqs : [[Square, ..2], ..2] =
                    [[Square(7), Square(0)], [Square(63), Square(56)]];
                static rook_dests : [[Square, ..2], ..2] =
                    [[Square(5), Square(3)], [Square(61), Square(59)]];
                let queenside_castle = square::file(to) == 2;
                let rook_from = rook_sqs[us_int][queenside_castle as uint];
                let rook_to = rook_dests[us_int][queenside_castle as uint];

                let on_from = self.piece_on(rook_from);

                clear_bit(self.mut_pieces_of_type(ROOK), rook_from);
                clear_bit(self.mut_pieces_of_color(us), rook_from);
                clear_bit(&mut self.occupied, rook_from);
                set_bit(&mut self.by_piece[0], rook_from);
                self.set_piece_on(rook_from, NP);
                self.hash.clear_piece(ROOK, us, rook_from);

                set_bit(self.mut_pieces_of_type(ROOK), rook_to);
                set_bit(self.mut_pieces_of_color(us), rook_to);
                set_bit(&mut self.occupied, rook_to);
                clear_bit(&mut self.by_piece[0], rook_to);
                self.set_piece_on(rook_to, on_from);
                self.hash.set_piece(ROOK, us, rook_to);
                // TODO should we just construct the rook piece here...
            }
            static castling_rights_masks : [u8, ..2] = [5, 10];
            self.hash.clear_castling(self.castling_rights);
            self.castling_rights &= !castling_rights_masks[us_int];
            self.hash.set_castling(self.castling_rights);
        } else if piece == ROOK {
            static rook_sqs : [[Square, ..2], ..2] =
                [[Square(7), Square(0)], [Square(63), Square(56)]];
            // TODO: remove branch?
            let curr_rook_sqs = rook_sqs[us_int];
            if from == curr_rook_sqs[0] || from == curr_rook_sqs[1] {
                let queenside_castle = from == curr_rook_sqs[1];
                let idx = us_int | (if queenside_castle {2} else {0});
                self.hash.clear_castling(self.castling_rights);
                self.castling_rights &= !(1 << idx);
                self.hash.set_castling(self.castling_rights);
            }
        }
        if capture == ROOK {
            static rook_sqs : [[Square, ..2], ..2] =
                [[Square(7), Square(0)], [Square(63), Square(56)]];
            let curr_rook_sqs = rook_sqs[them_int];
            if to == curr_rook_sqs[0] || to == curr_rook_sqs[1] {
                let queenside_castle = to == curr_rook_sqs[1];
                let idx = them_int | (if queenside_castle {2} else {0});
                self.hash.clear_castling(self.castling_rights);
                self.castling_rights &= !(1 << idx);
                self.hash.set_castling(self.castling_rights);
            }
        }

        self.hash.set_ep(self.ep_square);

        self.to_move = color::flip(self.to_move);
        self.hash.flip_color();

    }

    pub fn unmake_move(&mut self, _move: Move) -> () {
        self.hash.flip_color();
        self.to_move = color::flip(self.to_move);

        self.half_moves = self.half_moves_hist.pop().unwrap();

        self.hash.clear_ep(self.ep_square);
        self.ep_square = self.ep_square_hist.pop().unwrap();
        self.hash.set_ep(self.ep_square);

        self.hash.clear_castling(self.castling_rights);
        self.castling_rights = self.castling_hist.pop().unwrap();
        self.hash.set_castling(self.castling_rights);

        let from = get_from(_move);
        let to = get_to(_move);

        let us = self.to_move;
        let us_int = color::to_int(us);
        let them = color::flip(self.to_move);
        let them_int = color::to_int(them);

        let piece = self.type_of_piece_on(to);
        let capture = self.capture_hist.pop().unwrap();

        // Note that piece is only reliable here if it is a pawn or king.
        // Anything else might have been a promotion. If any logic is added
        // here for other pieces, the promotion logic needs to be pulled
        // above.
        if piece == PAWN {
            if to == self.ep_square {
                let target = Square::new(square::rank(from), square::file(to));
                set_bit(self.mut_pieces_of_color(them), target);
                set_bit(self.mut_pieces_of_type(PAWN), target);
                set_bit(&mut self.occupied, target);
                clear_bit(&mut self.by_piece[0], target);
                self.set_piece_on(target, Piece::new(PAWN, them));
                self.hash.set_piece(PAWN, them, target);
            }
        } else if get_promotion(_move) != NO_PIECE_TYPE {
            // Set explicitly the pawn board. Piece is incorrect here...
            set_bit(self.mut_pieces_of_type(PAWN), to);
            clear_bit(self.mut_pieces_of_type(get_promotion(_move)), to);
            // Set the piece now so when we grab it again after this
            // if/else block it will be correctly a pawn
            self.set_piece_on(to, Piece::new(PAWN, us));
            self.hash.clear_piece(get_promotion(_move), us, to);
            self.hash.set_piece(PAWN, us, to);
        } else if is_castle(_move) {
            static rook_sqs : [[Square, ..2], ..2] =
                [[Square(7), Square(0)], [Square(63), Square(56)]];
            static rook_dests : [[Square, ..2], ..2] =
                [[Square(5), Square(3)], [Square(61), Square(59)]];
            let queenside_castle = square::file(to) == 2;
            let rook_from = rook_sqs[us_int][queenside_castle as uint];
            let rook_to = rook_dests[us_int][queenside_castle as uint];

            let on_to = self.piece_on(rook_to);

            clear_bit(self.mut_pieces_of_type(ROOK), rook_to);
            clear_bit(self.mut_pieces_of_color(us), rook_to);
            clear_bit(&mut self.occupied, rook_to);
            set_bit(&mut self.by_piece[0], rook_to);
            self.set_piece_on(rook_to, NP);
            self.hash.clear_piece(ROOK, us, rook_to);

            set_bit(self.mut_pieces_of_type(ROOK), rook_from);
            set_bit(self.mut_pieces_of_color(us), rook_from);
            set_bit(&mut self.occupied, rook_from);
            clear_bit(&mut self.by_piece[0], rook_from);
            self.set_piece_on(rook_from, on_to);
            self.hash.set_piece(ROOK, us, rook_from);
            // TODO should we just construct the rook piece here...
        }

        // TODO: comment this line
        let piece = self.type_of_piece_on(to);

        let captured_piece = Piece::new(capture, them);
        let moved_piece =  Piece::new(piece, us);
        self.set_piece_on(to, captured_piece);
        self.set_piece_on(from, moved_piece);

        set_bit(&mut self.occupied, from);
        set_bit(self.mut_pieces_of_color(us), from);
        set_bit(self.mut_pieces_of_type(piece), from);
        clear_bit(&mut self.by_piece[0], from);
        self.hash.set_piece(piece, us, from);

        clear_bit(&mut self.occupied, to);
        clear_bit(self.mut_pieces_of_color(us), to);
        clear_bit(self.mut_pieces_of_type(piece), to);
        set_bit(&mut self.by_piece[0], to);
        self.hash.clear_piece(piece, us, to);

        if capture != NO_PIECE_TYPE {
            set_bit(self.mut_pieces_of_type(capture), to);
            set_bit(self.mut_pieces_of_color(them), to);
            set_bit(&mut self.occupied, to);
            clear_bit(&mut self.by_piece[0], to);
            self.hash.set_piece(capture, them, to);
        }

    }

    // Return any attacks to square s by the side not to move
    fn attacks(&self, s: Square) -> BitBoard {
        return self.attacks_for_occ(s, self.occupied);
    }

    // Return attacks to square s by the side not to move with modified occupancy
    // This is used to compute squares the king can't move to
    fn attacks_for_occ(&self, s: Square, occ: BitBoard) -> BitBoard {
        let opp_color = color::flip(self.to_move);
        return (pawn_attacks(s, self.to_move) & self.pieces(opp_color, PAWN))
               | (knight_attacks(s) & self.pieces(opp_color, KNIGHT))
               | (bishop_attacks(occ, s) & (self.pieces(opp_color, BISHOP) | self.pieces(opp_color, QUEEN)))
               | (rook_attacks(occ, s) & (self.pieces(opp_color, ROOK) | self.pieces(opp_color, QUEEN)))
               | (king_attacks(s) & self.pieces(opp_color, KING));
    }

    // Return checkers to the side to move
    pub fn checkers(&self) -> BitBoard {
        let king_sq = bit_scan_forward(self.pieces(self.to_move, KING));
        return self.attacks(king_sq);
    }

    pub fn gen_moves(&self, attacks_only: bool) -> Vec<Move> {
        let mut moves: Vec<Move> = vec![];
        if !attacks_only {
            self.gen_castle_moves(&mut moves);
        }
        self.gen_king_moves(&mut moves, attacks_only);
        self.gen_queen_moves(&mut moves, attacks_only);
        self.gen_rook_moves(&mut moves, attacks_only);
        self.gen_bishop_moves(&mut moves, attacks_only);
        self.gen_knight_moves(&mut moves, attacks_only);
        self.gen_pawn_moves(&mut moves, attacks_only);

        return moves;
    }

    pub fn gen_castle_moves(&self, moves: &mut Vec<Move>) -> () {
        static masks : [BitBoard, ..4] = [BitBoard(0x60), BitBoard(0x6000000000000000), 
                                          BitBoard(0xe), BitBoard(0xe00000000000000)];
        static targets : [Square, ..4] = [Square(6), Square(62), Square(2), Square(58)];
        let king = self.pieces(self.to_move, KING);
        let from = bit_scan_forward(king);
        // O-O
        let idx = color::to_int(self.to_move) | 0;
        if self.castling_rights & (1 << idx) != 0 {
            if masks[idx] & self.occupied == BitBoard(0) {
                moves.push(Move::new_castle(from, targets[idx]));
            }
        }
        // O-O-O
        let idx = color::to_int(self.to_move) | 2;
        if self.castling_rights >> idx & 1 == 1 {
            if masks[idx] & self.occupied == BitBoard(0) {
                moves.push(Move::new_castle(from, targets[idx]));
            }
        }
    }

    fn gen_king_moves(&self, moves: &mut Vec<Move>, attacks_only: bool) -> () {
        let king = self.pieces(self.to_move, KING);
        let king_sq = bit_scan_forward(king);
        let self_pieces = self.pieces_of_color(self.to_move);
        let targets = king_attacks(king_sq) & !self_pieces;
        let mut attack_moves = targets & !self.by_piece[0] ;
        while attack_moves != BitBoard(0) {
            let to = bit_scan_forward(attack_moves);
            moves.insert(0, Move::new(king_sq, to, 1));
            attack_moves = clear_lsb(attack_moves);
        }
        if !attacks_only {
            let mut quiet_moves = targets & self.by_piece[0] ;
            while quiet_moves != BitBoard(0) {
                let to = bit_scan_forward(quiet_moves);
                moves.push(Move::new(king_sq, to, 0));
                quiet_moves = clear_lsb(quiet_moves);
            }
        }
    }

    fn gen_knight_moves(&self, moves: &mut Vec<Move>, attacks_only: bool) -> () {
        let mut knights = self.pieces(self.to_move, KNIGHT);
        let self_pieces = self.pieces_of_color(self.to_move);
        while knights != BitBoard(0) {
            let from = bit_scan_forward(knights);
            let targets: BitBoard = knight_attacks(from) & !self_pieces;
            let mut attack_moves = targets & !self.by_piece[0];
            while attack_moves != BitBoard(0) {
                let to = bit_scan_forward(attack_moves);
                moves.insert(0, Move::new(from, to, 1));
                attack_moves = clear_lsb(attack_moves);
            }
            if !attacks_only { 
                let mut quiet_moves = targets & self.by_piece[0];
                while quiet_moves != BitBoard(0) {
                    let to = bit_scan_forward(quiet_moves);
                    moves.push(Move::new(from, to, 0));
                    quiet_moves = clear_lsb(quiet_moves);
                }
            }
            knights = clear_lsb(knights);
        }
    }

    fn gen_queen_moves(&self, moves: &mut Vec<Move>, attacks_only: bool) -> () {
        let mut queens = self.pieces(self.to_move, QUEEN);
        let self_pieces = self.pieces_of_color(self.to_move);
        while queens != BitBoard(0) {
            let from = bit_scan_forward(queens);
            let targets = (rook_attacks(self.occupied, from)
                              | bishop_attacks(self.occupied, from))
                              & !self_pieces;
            let mut attack_moves = targets & !self.by_piece[0];
            while attack_moves != BitBoard(0) {
                let to = bit_scan_forward(attack_moves);
                moves.insert(0, Move::new(from, to, 1));
                attack_moves = clear_lsb(attack_moves);
            }
            if !attacks_only { 
                let mut quiet_moves = targets & self.by_piece[0];
                while quiet_moves != BitBoard(0) {
                    let to = bit_scan_forward(quiet_moves);
                    moves.push(Move::new(from, to, 0));
                    quiet_moves = clear_lsb(quiet_moves);
                }
            }
            queens = clear_lsb(queens);
        }
    }

    fn gen_rook_moves(&self, moves: &mut Vec<Move>, attacks_only: bool) -> () {
        let mut rooks = self.pieces(self.to_move, ROOK);
        let self_pieces = self.pieces_of_color(self.to_move);
        while rooks != BitBoard(0) {
            let from = bit_scan_forward(rooks);
            let targets = rook_attacks(self.occupied, from) & !self_pieces;
            let mut attack_moves = targets & !self.by_piece[0];
            while attack_moves != BitBoard(0) {
                let to = bit_scan_forward(attack_moves);
                moves.insert(0, Move::new(from, to, 1));
                attack_moves = clear_lsb(attack_moves);
            }
            if !attacks_only { 
                let mut quiet_moves = targets & self.by_piece[0];
                while quiet_moves != BitBoard(0) {
                    let to = bit_scan_forward(quiet_moves);
                    moves.push(Move::new(from, to, 0));
                    quiet_moves = clear_lsb(quiet_moves);
                }
            }
            rooks = clear_lsb(rooks);
        }
    }

    fn gen_bishop_moves(&self, moves: &mut Vec<Move>, attacks_only: bool) -> () {
        let mut bishops = self.pieces(self.to_move, BISHOP);
        let self_pieces = self.pieces_of_color(self.to_move);
        while bishops != BitBoard(0) {
            let from = bit_scan_forward(bishops);
            let targets = bishop_attacks(self.occupied, from) & !self_pieces;
            let mut attack_moves = targets & !self.by_piece[0];
            while attack_moves != BitBoard(0) {
                let to = bit_scan_forward(attack_moves);
                moves.insert(0, Move::new(from, to, 1));
                attack_moves = clear_lsb(attack_moves);
            }
            if !attacks_only { 
                let mut quiet_moves = targets & self.by_piece[0];
                while quiet_moves != BitBoard(0) {
                    let to = bit_scan_forward(quiet_moves);
                    moves.push(Move::new(from, to, 0));
                    quiet_moves = clear_lsb(quiet_moves);
                }
            }
            bishops = clear_lsb(bishops);
        }
    }

    fn gen_pawn_moves(&self, moves: &mut Vec<Move>, attacks_only: bool) -> () {
        let pawns = self.pieces(self.to_move, PAWN);
        static promotion_mask : [BitBoard, ..2] = [bitboard::RANK_8, bitboard::RANK_1];
        if !attacks_only {
            let empty = !self.occupied;

            static single_push_diffs : [uint, ..2] = [8, 64 - 8];
            let diff = single_push_diffs[color::to_int(self.to_move)];
            let single_pushes = 
                circular_left_shift(pawns, diff)
                & empty;
            let non_promotions = single_pushes & !promotion_mask[color::to_int(self.to_move)];
            self.add_pawn_moves(non_promotions, moves, diff);

            let promotions = single_pushes & promotion_mask[color::to_int(self.to_move)];
            self.add_pawn_promotions(promotions, moves, diff, 0);

            let double_push_mask = [bitboard::RANK_3, bitboard::RANK_6];
            let double_pushes = 
                circular_left_shift(single_pushes & double_push_mask[color::to_int(self.to_move)], 
                                    diff)
                & empty;
            self.add_pawn_moves(double_pushes, moves, 2 * diff);
        }

        static attack_diffs : [[uint, ..2], ..2] = [[7, 64-9], [9, 64-7]];
        static file_masks : [BitBoard, ..2] = [bitboard::NOT_FILE_H, bitboard::NOT_FILE_A];
        for i in range(0u,2u) {
            let diff = attack_diffs[i][color::to_int(self.to_move)];
            let targets = circular_left_shift(pawns, diff) & file_masks[i];
            let attack_moves = targets & self.pieces_of_color(color::flip(self.to_move));
            let non_promotions = attack_moves & !promotion_mask[color::to_int(self.to_move)];
            self.add_pawn_attacks(non_promotions, moves, diff);

            if self.ep_square != square::NULL {
                let en_passant = targets & bitboard::single_bit(self.ep_square);
                self.add_pawn_attacks(en_passant, moves, diff);
            }

            let promotions = attack_moves & promotion_mask[color::to_int(self.to_move)];
            self.add_pawn_promotions(promotions, moves, diff, 1);
        }

    }

    // Attacks from a square by pawns of a given color

    pub fn add_pawn_moves(&self, mut targets: BitBoard, moves: &mut Vec<Move>, diff: uint) -> () {
        let t = &mut targets;
        while *t != BitBoard(0) {
            let to = bit_scan_forward(*t);
            // Add 64 because % is remainder, not modulo
            let from = ((to + 64) - diff) % 64;
            moves.push(Move::new(from, to, 0));
            *t = clear_lsb(*t);
        }
    }

    pub fn add_pawn_attacks(&self, mut targets: BitBoard, moves: &mut Vec<Move>, diff: uint) -> () {
        let t = &mut targets;
        while *t != BitBoard(0) {
            let to = bit_scan_forward(*t);
            // Add 64 because % is remainder, not modulo
            let from = ((to + 64) - diff) % 64;
            moves.insert(0, Move::new(from, to, 1));
            *t = clear_lsb(*t);
        }
    }

    pub fn add_pawn_promotions(&self, mut targets: BitBoard, moves: &mut Vec<Move>, diff: uint, attack: uint) -> () {
        let t = &mut targets;
        while *t != BitBoard(0) {
            let to = bit_scan_forward(*t);
            // Add 64 because % is remainder, not modulo
            let from = ((to + 64) - diff) % 64;
            moves.insert(0, Move::new_promotion(from, to, KNIGHT, attack));
            moves.insert(0, Move::new_promotion(from, to, BISHOP, attack));
            moves.insert(0, Move::new_promotion(from, to, ROOK, attack));
            moves.insert(0, Move::new_promotion(from, to, QUEEN, attack));
            *t = clear_lsb(*t);
        }
    }

    // Absolutely pinned pieces
    pub fn pinned_pieces(&self) -> BitBoard {
        let king = self.pieces(self.to_move, KING);
        let king_sq = bit_scan_forward(king);
        let mut pinned = BitBoard(0);
        let mut pinners = x_ray_rook_attacks(self.occupied, self.pieces_of_color(self.to_move), king_sq)
                     & self.pieces_of_color(color::flip(self.to_move))
                     & (self.pieces_of_type(ROOK) | self.pieces_of_type(QUEEN));
        while pinners != BitBoard(0){
            let pinner_sq = bit_scan_forward(pinners);
            pinned = pinned | in_between(king_sq, pinner_sq) & self.pieces_of_color(self.to_move);
            pinners = clear_lsb(pinners);
        }
        let mut pinners = x_ray_bishop_attacks(self.occupied, self.pieces_of_color(self.to_move), king_sq)
                     & self.pieces_of_color(color::flip(self.to_move))
                     & (self.pieces_of_type(BISHOP) | self.pieces_of_type(QUEEN));
        while pinners != BitBoard(0){
            let pinner_sq = bit_scan_forward(pinners);
            pinned = pinned | in_between(king_sq, pinner_sq) & self.pieces_of_color(self.to_move);
            pinners = clear_lsb(pinners);
        }
        return pinned;
    }

    fn rook_mob(occ: BitBoard, Square(i): Square) -> uint {
        let occ = BitBoard(constants::occupancy_mask_rook[i]) & occ;
        let BitBoard(index) = (occ * constants::magic_number_rook[i]) >> constants::magic_number_shifts_rook[i];
        return constants::get_rook_mob()[constants::rook_indexes[i] + index as uint] as uint;
    }

    fn bishop_mob(occ: BitBoard, Square(i): Square) -> uint {
        let occ = BitBoard(constants::occupancy_mask_bishop[i]) & occ;
        let BitBoard(index) = (occ * constants::magic_number_bishop[i]) >> constants::magic_number_shifts_bishop[i];
        return constants::get_bishop_mob()[constants::bishop_indexes[i] + index as uint] as uint;
    }

    pub fn evaluation(&self) -> int {
        use bitboard::popcnt;
        static PAWN_TABLE : [i8, ..64] = [
            0,  0,  0,  0,  0,  0,  0,  0,
            50, 50, 50, 50, 50, 50, 50, 50,
            10, 10, 20, 30, 30, 20, 10, 10,
            5,  5,  10, 27, 27, 10, 5,  5,
            0,  0,  0,  25, 25, 0,  0,  0,
            5, -5, -10, 0,  0, -10,-5,  5,
            5, 10, 10, -25,-25, 10, 10, 5,
            0, 0,  0,  0,  0,  0,  0,  0
        ];
        static KNIGHT_TABLE : [i8, ..64] = [
            -50,-40,-30,-30,-30,-30,-40,-50,
            -40,-20,  0,  0,  0,  0,-20,-40,
            -30,  0, 10, 15, 15, 10, 0, -30,
            -30,  5, 15, 20, 20, 15, 5, -30,
            -30,  0, 15, 20, 20, 15, 0, -30,
            -30,  5, 10, 15, 15, 10, 5, -30,
            -40,-20,  0,  5,  5,  0,-20,-40,
            -50,-40,-20,-30,-30,-20,-40,-50,
        ];
        static BISHOP_TABLE : [i8, ..64] = [
            -20,-10,-10,-10,-10,-10,-10,-20,
            -10,  0,  0,  0,  0,  0,  0,-10,
            -10,  0,  5, 10, 10,  5, 0, -10,
            -10,  5,  5, 10, 10,  5, 5, -10,
            -10,  0, 10, 10, 10, 10, 0, -10,
            -10, 10, 10, 10, 10, 10,10, -10,
            -10,  5,  0,  0,  0,  0,  5,-10,
            -20,-10,-40,-10,-10,-40,-10,-20,
        ];
        static PAWN_WEIGHT : int = 100;
        static KNIGHT_WEIGHT : int = 350;
        static BISHOP_WEIGHT : int = 350;
        static ROOK_WEIGHT : int = 525;
        static QUEEN_WEIGHT : int = 1050;
        static KING_WEIGHT : int = 20000;

        if self.half_moves >= 100 {
            // Check if there are any legal moves, as checkmate takes precedent
            if self.gen_moves(false).len() != 0 {
                return 0;
            }
        }

        let white = self.by_color[0];
        let black = self.by_color[1];
        let pawns = self.pieces_of_type(PAWN);
        let knights = self.pieces_of_type(KNIGHT);
        let bishops = self.pieces_of_type(BISHOP);
        let rooks = self.pieces_of_type(ROOK);
        let queens = self.pieces_of_type(QUEEN);
        let kings = self.pieces_of_type(KING);

        let pawn_diff = popcnt(pawns & white) as int - popcnt(pawns & black) as int;
        let knight_diff = popcnt(knights & white) as int - popcnt(knights & black) as int;
        let bishop_diff = popcnt(bishops & white) as int - popcnt(bishops & black) as int;
        let rook_diff = popcnt(rooks & white) as int - popcnt(rooks & black) as int;
        let queen_diff = popcnt(queens & white) as int - popcnt(queens & black) as int;
        let king_diff = popcnt(kings & white) as int - popcnt(kings & black) as int;

        let mut targets = pawns & white;
        let mut pawn_sqs: int = 0;
        while targets != BitBoard(0) {
            let Square(pawn) = bit_scan_forward(targets);
            pawn_sqs += PAWN_TABLE[63 - pawn] as int;
            targets = clear_lsb(targets);
        }
        let mut targets = pawns & black;
        while targets != BitBoard(0) {
            let Square(pawn) = bit_scan_forward(targets);
            pawn_sqs -= PAWN_TABLE[pawn] as int;
            targets = clear_lsb(targets);
        }
        let mut targets = knights & white;
        let mut knight_sqs: int = 0;
        while targets != BitBoard(0) {
            let Square(night) = bit_scan_forward(targets);
            knight_sqs += KNIGHT_TABLE[63 - night] as int;
            targets = clear_lsb(targets);
        }
        let mut targets = knights & black;
        while targets != BitBoard(0) {
            let Square(night) = bit_scan_forward(targets);
            knight_sqs -= KNIGHT_TABLE[night] as int;
            targets = clear_lsb(targets);
        }
        let mut targets = bishops & white;
        let mut bishop_sqs: int = 0;
        while targets != BitBoard(0) {
            let Square(bishop) = bit_scan_forward(targets);
            bishop_sqs += BISHOP_TABLE[63 - bishop] as int;
            targets = clear_lsb(targets);
        }
        let mut targets = bishops & black;
        while targets != BitBoard(0) {
            let Square(bishop) = bit_scan_forward(targets);
            bishop_sqs -= BISHOP_TABLE[bishop] as int;
            targets = clear_lsb(targets);
        }

        let score = PAWN_WEIGHT * pawn_diff
                    + KNIGHT_WEIGHT * knight_diff
                    + BISHOP_WEIGHT * bishop_diff
                    + ROOK_WEIGHT * rook_diff
                    + QUEEN_WEIGHT * queen_diff
                    + KING_WEIGHT * king_diff
                    + pawn_sqs
                    + knight_sqs
                    + bishop_sqs;
        static to_move_arr : [int, ..2] = [1, -1];
        return score * to_move_arr[color::to_int(self.to_move)];
    }
}

pub fn x_ray_rook_attacks(occ: BitBoard, blockers: BitBoard, sq: Square) -> BitBoard {
    let attacks = rook_attacks(occ, sq);
    return attacks ^ rook_attacks(occ ^ (blockers & attacks), sq);
}

pub fn x_ray_bishop_attacks(occ: BitBoard, blockers: BitBoard, sq: Square) -> BitBoard {
    let attacks = bishop_attacks(occ, sq);
    return attacks ^ bishop_attacks(occ ^ (blockers & attacks), sq);
}

fn rook_attacks(occ: BitBoard, Square(i): Square) -> BitBoard {
    let occ = BitBoard(constants::occupancy_mask_rook[i]) & occ;
    let BitBoard(index) = (occ * constants::magic_number_rook[i]) >> constants::magic_number_shifts_rook[i];
    return constants::get_rook_moves()[constants::rook_indexes[i] + index as uint]
}

fn bishop_attacks(occ: BitBoard, Square(i): Square) -> BitBoard {
    let occ = BitBoard(constants::occupancy_mask_bishop[i]) & occ;
    let BitBoard(index) = (occ * constants::magic_number_bishop[i]) >> constants::magic_number_shifts_bishop[i];
    return constants::get_bishop_moves()[constants::bishop_indexes[i] + index as uint];
}

fn king_attacks(Square(s): Square) -> BitBoard {
    return constants::get_king_moves()[s]
}

fn knight_attacks(Square(s): Square) -> BitBoard {
    return constants::get_knight_moves()[s]
}


fn pawn_attacks(s : Square, c: Color) -> BitBoard {
    let board = bitboard::single_bit(s);
    if c == WHITE {
        return no_we_one(board) | no_ea_one(board);
    } else {
        return so_we_one(board) | so_ea_one(board);
    }
}

pub fn perft(position: &mut Position, depth: int) -> uint {
    let moves = position.gen_moves(false);
    let mut count = 0;
    let pinned = position.pinned_pieces();
    let checkers = position.checkers();
    if depth == 1 {
        for _move in moves.iter() {
            if position.move_is_legal(*_move, pinned, checkers) {
                count += 1;
            }
        }
        return count;
    }
    for _move in moves.iter() {
        if position.move_is_legal(*_move, pinned, checkers) {
            position.make_move(*_move);
            count += perft(position, depth - 1);
            position.unmake_move(*_move);
        }
    }
    return count;
}

pub fn divide(position: &mut Position, depth: int) -> HashMap<Move, uint> {
    let pinned = position.pinned_pieces();
    let checkers = position.checkers();
    let moves = position.gen_moves(false);
    let mut res = HashMap::new ();
    if depth == 1 {
        for _move in moves.iter() {
            if position.move_is_legal(*_move, pinned, checkers) {
                res.insert(*_move, 1);
            }
        }
        return res;
    }
    for _move in moves.iter() {
        if position.move_is_legal(*_move, pinned, checkers) {
            position.make_move(*_move);
            res.insert(*_move, perft(position, depth - 1));
            position.unmake_move(*_move);
        }
    }
    return res;
}

pub fn print_divide(position: &mut Position, depth: int) -> () {
    let res =  divide(position, depth);
    let mut keys: Vec<&Move> = res.keys().collect();
    keys.sort_by(|a, b| a.cmp(b));
    for _move in keys.iter() {
        print!("{} {}\n", _move, res.get(*_move));
    }
}

impl fmt::Show for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::iter::range_step;
        for rank in range_step(7, -1, -1 as int) {
            write!(f, "+---+---+---+---+---+---+---+---+\n");
            for file in range(0u, 8u) {
                let square = Square(file | (rank as uint << 3));
                write!(f, "| {} ", piece::to_char(self.piece_on(square)));
            }
            write!(f, "|\n");
        }
        write!(f, "+---+---+---+---+---+---+---+---+\n")
    }
}

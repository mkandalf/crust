extern crate time;

use _move;
use _move::Move;
use piece_type::PieceType;
use position::Position;
use zobrist::{Table, EXACT_BOUND, ALPHA_BOUND, BETA_BOUND};
use std::i16;

// Aspiration window parameter
static WINDOW : i16 = 50;

pub struct Searcher {
    pub quiescent_node_count : u64,
    pub node_count : u64,
    pub pos: Box<Position>,
    pub table: Box<Table>,
    pub killers: [[Move; 2]; 32],
    pub ancient: u8
}

fn move_delta(_move: Move, pos: &Box<Position>) -> i16 {
    static WEIGHTS : [i16; 7] = [0, 100, 350, 350, 525, 1000, 20000];
    let PieceType(from) = pos.type_of_piece_on(_move::get_from(_move));
    let PieceType(to) = pos.type_of_piece_on(_move::get_to(_move));
    if _move::get_to(_move) == pos.ep_square {
        return 0;
    } else {
        return WEIGHTS[to as usize] - WEIGHTS[from as usize];
    }
}

impl Searcher {
    pub fn new(pos: Position) -> Searcher {
        Searcher {
            quiescent_node_count: 0,
            node_count: 0,
            pos: box pos,
            table: box Table::new(),
            killers: [[_move::NULL; 2]; 32],
            ancient: 0
        }
    }

    fn set_killer(&mut self, _move: Move, ply: u8) -> () {
        if self.killers[ply as usize][0] != _move::NULL {
            self.killers[ply as usize][1] = self.killers[ply as usize][0];
        }
        self.killers[ply as usize][0] = _move;
    }

    pub fn alphabeta(&mut self, alpha: i16, beta: i16, depth: u8, ply: u8) -> i16 {
        self.node_count += 1;

        let (score, best_move): (Option<i16>, Move) = self.table.probe(self.pos.hash, depth, alpha, beta);
        match score {
            Some(s) => return s,
            None => ()
        }
        if depth == 0 { 
            let score = self.quiesce(alpha, beta);
            self.table.record(self.pos.hash, score, _move::NULL, depth, EXACT_BOUND, self.ancient);
            return score;
        }

        let pinned = self.pos.pinned_pieces();
        let checkers = self.pos.checkers();

        let mut alpha = alpha;
        let mut best = _move::NULL;

        let [killer_move_one, killer_move_two] = self.killers[ply as usize];
        let mut moves = self.pos.gen_moves(false);

        for _move in moves.iter_mut() {
            if *_move == best_move {
                _move.set_score(10000);
            } else if *_move == killer_move_one {
                _move.set_score(9999);
            } else if *_move == killer_move_two {
                _move.set_score(9998);
            } else if _move::is_attack(*_move) {
                let sc = move_delta(*_move, &self.pos) + 5;
                _move.set_score(sc);
            }
        }

        moves.sort_by(|a, b| b.get_score().cmp(&a.get_score()));

        for _move in moves.iter() {
            if self.pos.move_is_legal(*_move, pinned, checkers) {
                self.pos.make_move(*_move);
                let score = -self.alphabeta(-beta, -alpha, depth - 1, ply + 1);
                self.pos.unmake_move(*_move);
                if score >= beta {
                    self.set_killer(*_move, ply);
                    self.table.record(self.pos.hash, beta, *_move, depth, BETA_BOUND, self.ancient);
                    return beta;
                }
                if score > alpha {
                    alpha = score;
                    best = *_move;
                }
            }
        }
        self.table.record(self.pos.hash, alpha, best, depth, ALPHA_BOUND, self.ancient);
        return alpha;
    }

    pub fn quiesce(&mut self, alpha: i16, beta: i16) -> i16 {
        self.quiescent_node_count += 1;
        self.node_count += 1;

        let mut alpha = alpha;

        let stand_pat = self.pos.evaluation();
        if stand_pat >= beta {
            return beta;
        }
        if alpha < stand_pat {
            alpha = stand_pat;
        }

        let mut moves = self.pos.gen_moves(true);
        moves.sort_by(|a, b| move_delta(*b, &self.pos).cmp(&move_delta(*a, &self.pos)));
        if moves.len() != 0 {
            let pinned = self.pos.pinned_pieces();
            let checkers = self.pos.checkers();
            for _move in moves.iter() {
                if self.pos.move_is_legal(*_move, pinned, checkers) {
                    self.pos.make_move(*_move);
                    let score = -self.quiesce(-beta, -alpha);
                    self.pos.unmake_move(*_move);
                    if score >= beta {
                        return beta;
                    }
                    if score > alpha {
                        alpha = score;
                    }
                }
            }
        }
        return alpha;
    }

    pub fn root_alpha_beta(&mut self, alpha: i16, beta: i16, depth: u8) -> (Move, i16) {
        self.quiescent_node_count = 0;
        self.node_count = 0;

        let mut alpha: i16 = alpha;

        if depth == 0 { 
            debug_assert!(false, "Root alpha beta with depth 0");
            return (_move::NULL, 0);
        }

        let pinned = self.pos.pinned_pieces();
        let checkers = self.pos.checkers();

        let mut best = _move::NULL;

        let moves = self.pos.gen_moves(false);

        for _move in moves.iter() {
            if self.pos.move_is_legal(*_move, pinned, checkers) {
                self.pos.make_move(*_move);
                let score = -self.alphabeta(-beta, -alpha, depth - 1, 1);
                let eval = self.pos.evaluation();
                self.pos.unmake_move(*_move);
                if score > alpha {
                    best = *_move;
                    alpha = score;
                } else {
                }
            }
        }

        return (best, alpha);
    }

    pub fn search(&mut self, secs: f64, print: bool) -> Move {
        self.ancient = self.ancient ^ 0x1;
        let time = time::precise_time_s();
        let mut i = 1;
        let mut alpha: i16 = i16::MIN + 1;
        let mut beta: i16 = i16::MAX - 1;
        loop {
            let (_move, score) = self.root_alpha_beta(alpha, beta, i);
            let (_move, score) =
                if score <= alpha || score >= beta {
                    alpha = i16::MIN + 1;
                    beta = i16::MAX - 1;
                    self.root_alpha_beta(alpha, beta, i)
                } else {
                    (_move, score)
                };
            alpha = score - WINDOW;
            beta = score + WINDOW;
            if print {
                print!("{}:\t({})\t{:?}\n", i, score as f64 / 100.0, self.extract_pv(_move));
            }
            let elapsed = time::precise_time_s() - time;
            if elapsed > secs {
                if print {
                    print!("Elapsed: {}\n", elapsed);
                }
                return _move;
            }
            i += 1;
        }
    }

    pub fn search_depth(&mut self, depth: u8, print: bool) -> Move {
        self.ancient = self.ancient ^ 0x1;
        let mut i = 1;
        let mut alpha: i16 = i16::MIN + 1;
        let mut beta: i16 = i16::MAX - 1;
        loop {
            let (_move, score) = self.root_alpha_beta(alpha, beta, i);
            let (_move, score) =
                if score <= alpha || score >= beta {
                    alpha = i16::MIN + 1;
                    beta = i16::MAX - 1;
                    self.root_alpha_beta(alpha, beta, i)
                } else {
                    (_move, score)
                };
            alpha = score - WINDOW;
            beta = score + WINDOW;
            if print {
                print!("{}:\t({})\t{:?}\n", i, score as f64 / 100.0, self.extract_pv(_move));
            }
            if i == depth { return _move; }
            i += 1;
        }
    }

    pub fn extract_pv(&mut self, best: Move) -> Vec<Move> {
        if best == _move::NULL {
            return vec![];
        } else {
            let pv = &mut vec![];
            self.rec_extract(pv, best);
            return pv.clone();
        }
    }

    fn rec_extract(&mut self, pv: &mut Vec<Move>, best: Move) -> () {
        pv.push(best);
        self.pos.make_move(best);
        match self.table.best_move(self.pos.hash) {
            None => (),
            Some(m) => self.rec_extract(pv, m)
        };
        self.pos.unmake_move(best);
    }
}

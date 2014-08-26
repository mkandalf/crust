extern crate time;

use move;
use move::Move;
use piece_type::PieceType;
use position::Position;
use zobrist::{Table, EXACT_BOUND, ALPHA_BOUND, BETA_BOUND};

// Aspiration window parameter
static WINDOW : int = 50;

pub struct Searcher {
    pub quiescent_node_count : uint,
    pub node_count : uint,
    pub pos: Position,
    pub table: Box<Table>,
    pub killers: [[Move, ..2], ..32],
    pub ancient: uint
}

fn move_delta(move: Move, pos: &Position) -> int {
    static weights : [int, ..7] = [0, 100, 350, 350, 525, 1000, 20000];
    let PieceType(from) = pos.type_of_piece_on(move::get_from(move));
    let PieceType(to) = pos.type_of_piece_on(move::get_to(move));
    if move::get_to(move) == pos.ep_square {
        return 0;
    } else {
        return weights[to] - weights[from];
    }
}

impl Searcher {
    pub fn new(pos: Position) -> Searcher {
        Searcher {
            quiescent_node_count: 0,
            node_count: 0,
            pos: pos,
            table: box Table::new(),
            killers: [[move::NULL, ..2], ..32],
            ancient: 0
        }
    }

    fn set_killer(&mut self, move: Move, ply: uint) -> () {
        if self.killers[ply][0] != move::NULL {
            self.killers[ply][1] = self.killers[ply][0];
        }
        self.killers[ply][0] = move;
    }

    pub fn alphabeta(&mut self, alpha: int, beta: int, depth: uint, ply: uint) -> int {
        self.node_count += 1;

        let (score, best_move): (Option<int>, Move) = self.table.probe(self.pos.hash, depth, alpha, beta);
        match score {
            Some(s) => return s,
            None => ()
        }
        if depth == 0 { 
            let score = self.quiesce(alpha, beta);
            self.table.record(self.pos.hash, score, move::NULL, depth, EXACT_BOUND, self.ancient);
            return score;
        }


        let pinned = self.pos.pinned_pieces();
        let checkers = self.pos.checkers();

        let mut alpha = alpha;
        let mut best = move::NULL;

        let [killer_move_one, killer_move_two] = self.killers[ply];
        let mut moves = self.pos.gen_moves(false);

        for move in moves.mut_iter() {
            if *move == best_move {
                move.set_score(10000);
            } else if *move == killer_move_one {
                move.set_score(9999);
            } else if *move == killer_move_two {
                move.set_score(9998);
            } else if move::is_attack(*move) {
                let sc = move_delta(*move, &self.pos) + 5;
                move.set_score(sc);
            }
        }

        moves.sort_by(|a, b| b.get_score().cmp(&a.get_score()));

        for move in moves.iter() {
            if self.pos.move_is_legal(*move, pinned, checkers) {
                self.pos.make_move(*move);
                let score = -self.alphabeta(-beta, -alpha, depth - 1, ply + 1);
                self.pos.unmake_move(*move);
                if score >= beta {
                    self.set_killer(*move, ply);
                    self.table.record(self.pos.hash, beta, *move, depth, BETA_BOUND, self.ancient);
                    return beta;
                }
                if score > alpha {
                    alpha = score;
                    best = *move;
                }
            }
        }
        self.table.record(self.pos.hash, alpha, best, depth, ALPHA_BOUND, self.ancient);
        return alpha;
    }

    pub fn quiesce(&mut self, alpha: int, beta: int) -> int {
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
            for move in moves.iter() {
                if self.pos.move_is_legal(*move, pinned, checkers) {
                    self.pos.make_move(*move);
                    let score = -self.quiesce(-beta, -alpha);
                    self.pos.unmake_move(*move);
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

    pub fn root_alpha_beta(&mut self, alpha: int, beta: int, depth: uint) -> (Move, int) {
        self.quiescent_node_count = 0;
        self.node_count = 0;

        let mut alpha: int = alpha;

        if depth == 0 { 
            debug_assert!(false, "Root alpha beta with depth 0");
            return (move::NULL, 0);
        }

        let pinned = self.pos.pinned_pieces();
        let checkers = self.pos.checkers();

        let mut best = move::NULL;

        let moves = self.pos.gen_moves(false);

        for move in moves.iter() {
            if self.pos.move_is_legal(*move, pinned, checkers) {
                self.pos.make_move(*move);
                let score = -self.alphabeta(-beta, -alpha, depth - 1, 1);
                self.pos.unmake_move(*move);
                if score > alpha {
                    best = *move;
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
        let mut alpha: int = -1000000;
        let mut beta: int = 1000000;
        loop {
            let (move, score) = self.root_alpha_beta(alpha, beta, i);
            let (move, score) =
                if score <= alpha || score >= beta {
                    alpha = -1000000;
                    beta = 1000000;
                    self.root_alpha_beta(alpha, beta, i)
                } else {
                    (move, score)
                };
            alpha = score - WINDOW;
            beta = score + WINDOW;
            if print {
                print!("{}:\t({})\t{}\n", i, score as f64 / 100.0, self.extract_pv(move));
            }
            let elapsed = time::precise_time_s() - time;
            if elapsed > secs {
                if print {
                    print!("Elapsed: {}\n", elapsed);
                }
                return move;
            }
            i += 1;
        }
    }

    pub fn search_depth(&mut self, depth: uint, print: bool) -> Move {
        self.ancient = self.ancient ^ 0x1;
        let mut i = 1;
        let mut alpha: int = -1000000;
        let mut beta: int = 1000000;
        loop {
            let (move, score) = self.root_alpha_beta(alpha, beta, i);
            let (move, score) =
                if score <= alpha || score >= beta {
                    alpha = -1000000;
                    beta = 1000000;
                    self.root_alpha_beta(alpha, beta, i)
                } else {
                    (move, score)
                };
            alpha = score - WINDOW;
            beta = score + WINDOW;
            if print {
                print!("{}:\t({})\t{}\n", i, score as f64 / 100.0, self.extract_pv(move));
            }
            if i == depth { return move; }
            i += 1;
        }
    }

    pub fn extract_pv(&mut self, best: Move) -> Vec<Move> {
        if best == move::NULL {
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

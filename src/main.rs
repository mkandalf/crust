#![feature(asm)]

mod bitboard;
mod color;
mod constants;
mod move;
mod piece;
mod piece_type;
mod position;
mod search;
mod square;
mod zobrist;

fn main() {
    use std::collections::HashMap;
    use std::io;

    use constants;
    use move;
    use position::Position;
    use search::Searcher;
    use zobrist;


    // Initialize various constants, including magic bitboards
    constants::init();
    // Initialize zobrist keys
    zobrist::init();

    let board = Position::new();
    let mut reader = io::stdin();
    let mut searcher = Searcher::new(board);

    loop {
        let mut user_move;
        let available_moves = searcher.pos.gen_moves(false);
        println!("Available Moves: {}", available_moves);
        let mut move_map = HashMap::new();
        for move in available_moves.iter() {
            move_map.insert(move::to_int(*move) & 0xfff, move);
        }
        let pinned = searcher.pos.pinned_pieces();
        let checkers = searcher.pos.checkers();
        loop {
            print!("Enter move: ");
            let input = reader.read_line().ok().unwrap();
            match move::from_str(input.as_slice().trim()) {
                Some(m) => { 
                    match move_map.contains_key(&(move::to_int(m) & 0xfff)) {
                        true => {
                            let actual_move = *move_map[(move::to_int(m) & 0xfff)];
                            if searcher.pos.move_is_legal(actual_move, pinned, checkers) {
                                user_move = actual_move;
                                break; 
                            } else {
                                println!("Illegal move")
                            }
                        }
                        false => println!("Illegal move")
                    }
                }
                None => println!("Invalid move format")
            }
        }
        searcher.pos.make_move(user_move);
        let move = searcher.search(0.75, true);
        if move == move::NULL {
            print!("game over");
            break;
        } else {
            searcher.pos.make_move(move);
            println!("Computer move: {}", move);
            println!("all nodes: {}\nquiesce nodes: {}", searcher.node_count, searcher.quiescent_node_count); 
            print!("{}", searcher.pos);
        }
    }
}

#![feature(asm)]
#![feature(box_syntax)]

mod bitboard;
mod color;
mod constants;
mod _move;
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
    use _move;
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
        println!("Available Moves: {:?}", available_moves);
        let mut move_map = HashMap::new();
        for _move in available_moves.iter() {
            move_map.insert(_move::to_int(*_move) & 0xfff, _move);
        }
        let pinned = searcher.pos.pinned_pieces();
        let checkers = searcher.pos.checkers();
        loop {
            print!("Enter move: ");
            let input = reader.read_line().ok().unwrap();
            match _move::from_str(input.as_slice().trim()) {
                Some(m) => { 
                    match move_map.contains_key(&(_move::to_int(m) & 0xfff)) {
                        true => {
                            let actual_move = *move_map[(_move::to_int(m) & 0xfff)];
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
        let _move = searcher.search(0.75, true);
        if _move == _move::NULL {
            print!("game over");
            break;
        } else {
            searcher.pos.make_move(_move);
            println!("Computer move: {:?}", _move);
            println!("all nodes: {}\nquiesce nodes: {}", searcher.node_count, searcher.quiescent_node_count); 
            print!("{:?}", searcher.pos);
        }
    }
}

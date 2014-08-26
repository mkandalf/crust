extern crate test;
extern crate chess;

use test::Bencher;

#[bench]
fn bench_find_move(b: &mut Bencher) {
    use std::io;
    use std::io::{BufferedReader, File};

    use chess::position::Position;
    use chess::search::Searcher;

    use chess::constants;
    use chess::zobrist;

    // Initialize various constants, including magic bitboards
    constants::init();
    // Initialize zobrist keys
    zobrist::init();

    let board = Position::new();
    let mut reader = io::stdin();
    let mut searcher = Searcher::new(board);

    b.iter(|| {
        let path = Path::new("ccr.csv");
        let mut file = BufferedReader::new(File::open(&path));
        for line in file.lines() {
            match line {
                Ok(r) => {
                    let pos = Position::from_fen(r.as_slice());
                    let mut search = Searcher::new(pos);
                    search.search_depth(3, false);
                },
                _ => ()
            }
        }
    });
}

extern crate test;
extern crate crust;

use test::Bencher;

#[bench]
fn bench_find_move(b: &mut Bencher) {
    use std::io;
    use std::io::{BufferedReader, File};

    use crust::position::Position;
    use crust::search::Searcher;

    use crust::constants;
    use crust::zobrist;

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

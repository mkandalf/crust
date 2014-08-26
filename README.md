# Crust

## Overview
Crust is a simple Chess engine written in [Rust][rust-home].

[rust-home]: http://rust-lang.org

## Features
* Bitboard Based Board Representation
* Magic Bitboards for Sliding Piece Move Generation
* Alpha-Beta Search
* Iterative Deepening
* Quiescence Search
* Transposition Table
* Aspiration Windows
* Killer Move Heuristic
* Material and Piece-Square Based Evaluation

## Planned
* Static Exchange Evaluation
* Mobility and Pawn Structure Evaluation Terms
* Negascout/PVS
* Null Move Pruning

## Build
Simply run `cargo run`. To run a simple benchmark, run `cargo bench` (this may take a little while).

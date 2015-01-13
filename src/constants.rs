use std::iter::range_step;

use bitboard::{BitBoard, east_one, west_one, north_one, south_one, single_bit, popcnt};
use square::{Square, file, rank};

static mut king_moves : [BitBoard; 64] = [BitBoard(0); 64];
static mut knight_moves : [BitBoard; 64] = [BitBoard(0); 64];
static mut rook_moves : [BitBoard; 102400] = [BitBoard(0); 102400];
static mut rook_mob : [u8; 102400] = [0; 102400];
static mut bishop_moves : [BitBoard; 5248] = [BitBoard(0); 5248];
static mut bishop_mob : [u8; 5248] = [0; 5248];
static mut between_bb : [[BitBoard; 64]; 64] = [[BitBoard(0); 64]; 64];
pub static MAGIC_NUMBER_SHIFTS_ROOK : [u8; 64] =
    [52,53,53,53,53,53,53,52,53,54,54,54,54,54,54,53,
     53,54,54,54,54,54,54,53,53,54,54,54,54,54,54,53,
     53,54,54,54,54,54,54,53,53,54,54,54,54,54,54,53,
     53,54,54,54,54,54,54,53,52,53,53,53,53,53,53,52];
pub static MAGIC_NUMBER_SHIFTS_BISHOP : [u8; 64] =
    [58,59,59,59,59,59,59,58,59,59,59,59,59,59,59,
     59,59,59,57,57,57,57,59,59,59,59,57,55,55,57,
     59,59,59,59,57,55,55,57,59,59,59,59,57,57,57,
     57,59,59,59,59,59,59,59,59,59,59,58,59,59,59,
     59,59,59,58];
pub static MAGIC_NUMBER_ROOK : [u64; 64] =
    [0xa180022080400230, 0x40100040022000, 0x80088020001002, 0x80080280841000,
     0x4200042010460008, 0x4800a0003040080, 0x400110082041008, 0x8000a041000880,
     0x10138001a080c010, 0x804008200480, 0x10011012000c0, 0x22004128102200,
     0x200081201200c, 0x202a001048460004, 0x81000100420004, 0x4000800380004500,
     0x208002904001, 0x90004040026008, 0x208808010002001, 0x2002020020704940,
     0x8048010008110005, 0x6820808004002200, 0xa80040008023011, 0xb1460000811044,
     0x4204400080008ea0, 0xb002400180200184, 0x2020200080100380, 0x10080080100080,
     0x2204080080800400, 0xa40080360080, 0x2040604002810b1, 0x8c218600004104,
     0x8180004000402000, 0x488c402000401001, 0x4018a00080801004, 0x1230002105001008,
     0x8904800800800400, 0x42000c42003810, 0x8408110400b012, 0x18086182000401,
     0x2240088020c28000, 0x1001201040c004, 0xa02008010420020, 0x10003009010060,
     0x4008008008014, 0x80020004008080, 0x282020001008080, 0x50000181204a0004,
     0x102042111804200, 0x40002010004001c0, 0x19220045508200, 0x20030010060a900,
     0x8018028040080, 0x88240002008080, 0x10301802830400, 0x332a4081140200,
     0x8080010a601241, 0x1008010400021, 0x4082001007241, 0x211009001200509,
     0x8015001002441801, 0x801000804000603, 0xc0900220024a401, 0x1000200608243];
pub static MAGIC_NUMBER_BISHOP : [u64; 64] =
    [0x2910054208004104, 0x2100630a7020180, 0x5822022042000000, 0x2ca804a100200020,
     0x204042200000900, 0x2002121024000002, 0x80404104202000e8, 0x812a020205010840,
     0x8005181184080048, 0x1001c20208010101, 0x1001080204002100, 0x1810080489021800,
     0x62040420010a00, 0x5028043004300020, 0xc0080a4402605002, 0x8a00a0104220200,
     0x940000410821212, 0x1808024a280210, 0x40c0422080a0598, 0x4228020082004050,
     0x200800400e00100, 0x20b001230021040, 0x90a0201900c00, 0x4940120a0a0108,
     0x20208050a42180, 0x1004804b280200, 0x2048020024040010, 0x102c04004010200,
     0x20408204c002010, 0x2411100020080c1, 0x102a008084042100, 0x941030000a09846,
     0x244100800400200, 0x4000901010080696, 0x280404180020, 0x800042008240100,
     0x220008400088020, 0x4020182000904c9, 0x23010400020600, 0x41040020110302,
     0x412101004020818, 0x8022080a09404208, 0x1401210240484800, 0x22244208010080,
     0x1105040104000210, 0x2040088800c40081, 0x8184810252000400, 0x4004610041002200,
     0x40201a444400810, 0x4611010802020008, 0x80000b0401040402, 0x20004821880a00,
     0x8200002022440100, 0x9431801010068, 0x1040c20806108040, 0x804901403022a40,
     0x2400202602104000, 0x208520209440204, 0x40c000022013020, 0x2000104000420600,
     0x400000260142410, 0x800633408100500, 0x2404080a1410, 0x138200122002900];
pub static OCCUPANCY_MASK_ROOK : [u64; 64] =
    [0x101010101017e, 0x202020202027c, 0x404040404047a, 0x8080808080876, 0x1010101010106e,
     0x2020202020205e, 0x4040404040403e, 0x8080808080807e, 0x1010101017e00, 0x2020202027c00,
     0x4040404047a00, 0x8080808087600, 0x10101010106e00, 0x20202020205e00, 0x40404040403e00,
     0x80808080807e00, 0x10101017e0100, 0x20202027c0200, 0x40404047a0400, 0x8080808760800,
     0x101010106e1000, 0x202020205e2000, 0x404040403e4000, 0x808080807e8000,
     0x101017e010100, 0x202027c020200, 0x404047a040400, 0x8080876080800, 0x1010106e101000,
     0x2020205e202000, 0x4040403e404000, 0x8080807e808000, 0x1017e01010100, 0x2027c02020200,
     0x4047a04040400, 0x8087608080800, 0x10106e10101000, 0x20205e20202000, 0x40403e40404000,
     0x80807e80808000, 0x17e0101010100, 0x27c0202020200, 0x47a0404040400, 0x8760808080800,
     0x106e1010101000, 0x205e2020202000, 0x403e4040404000, 0x807e8080808000,
     0x7e010101010100, 0x7c020202020200, 0x7a040404040400, 0x76080808080800,
     0x6e101010101000, 0x5e202020202000, 0x3e404040404000, 0x7e808080808000,
     0x7e01010101010100, 0x7c02020202020200, 0x7a04040404040400, 0x7608080808080800,
     0x6e10101010101000, 0x5e20202020202000, 0x3e40404040404000, 0x7e80808080808000];
pub static OCCUPANCY_MASK_BISHOP : [u64; 64] =
    [0x40201008040200, 0x402010080400, 0x4020100a00, 0x40221400, 0x2442800, 0x204085000,
     0x20408102000, 0x2040810204000, 0x20100804020000, 0x40201008040000, 0x4020100a0000,
     0x4022140000, 0x244280000, 0x20408500000, 0x2040810200000, 0x4081020400000,
     0x10080402000200, 0x20100804000400, 0x4020100a000a00, 0x402214001400, 0x24428002800,
     0x2040850005000, 0x4081020002000, 0x8102040004000, 0x8040200020400, 0x10080400040800,
     0x20100a000a1000, 0x40221400142200, 0x2442800284400, 0x4085000500800, 0x8102000201000,
     0x10204000402000, 0x4020002040800, 0x8040004081000, 0x100a000a102000, 0x22140014224000,
     0x44280028440200, 0x8500050080400, 0x10200020100800, 0x20400040201000, 0x2000204081000,
     0x4000408102000, 0xa000a10204000, 0x14001422400000, 0x28002844020000, 0x50005008040200,
     0x20002010080400, 0x40004020100800, 0x20408102000, 0x40810204000, 0xa1020400000,
     0x142240000000, 0x284402000000, 0x500804020000, 0x201008040200, 0x402010080400,
     0x2040810204000, 0x4081020400000, 0xa102040000000, 0x14224000000000, 0x28440200000000,
     0x50080402000000, 0x20100804020000, 0x40201008040200];
pub static ROOK_INDEXES : [usize; 64] =
    [0, 4096, 6144, 8192, 10240, 12288, 14336, 16384, 20480, 22528, 23552, 24576, 25600, 26624,
    27648, 28672, 30720, 32768, 33792, 34816, 35840, 36864, 37888, 38912, 40960, 43008, 44032,
    45056, 46080, 47104, 48128, 49152, 51200, 53248, 54272, 55296, 56320, 57344, 58368, 59392,
    61440, 63488, 64512, 65536, 66560, 67584, 68608, 69632, 71680, 73728, 74752, 75776, 76800,
    77824, 78848, 79872, 81920, 86016, 88064, 90112, 92160, 94208, 96256, 98304];
pub static BISHOP_INDEXES : [usize; 64] =
    [0, 64, 96, 128, 160, 192, 224, 256, 320, 352, 384, 416, 448, 480, 512, 544, 576, 608, 640,
    768, 896, 1024, 1152, 1184, 1216, 1248, 1280, 1408, 1920, 2432, 2560, 2592, 2624, 2656, 2688,
    2816, 3328, 3840, 3968, 4000, 4032, 4064, 4096, 4224, 4352, 4480, 4608, 4640, 4672, 4704, 4736,
    4768, 4800, 4832, 4864, 4896, 4928, 4992, 5024, 5056, 5088, 5120, 5152, 5184];


pub fn init() -> () {
    init_king_moves();
    init_knight_moves();
    init_rook_moves();
    init_bishop_moves();
    init_between_bb();
}

fn init_king_moves() -> () {
    for i in 0..64 {
        let board = single_bit(Square(i));
        let moves = north_one(board) | south_one(board);
        unsafe {
            king_moves[i as usize] = moves | west_one(moves | board) | east_one(moves | board);
        }
    }
}

fn init_knight_moves() -> () {
    for i in 0..64 {
        let board = single_bit(Square(i));
        let east = east_one(board);
        let west = west_one(board);
        let mut moves = north_one(north_one(east | west));
        moves = moves | south_one(south_one(east | west));
        let east_east = east_one(east);
        let west_west = west_one(west);
        moves = moves | north_one(east_east|west_west);
        moves = moves | south_one(east_east|west_west);
        unsafe {
            knight_moves[i as usize] = moves;
        }
    }
}

#[allow(unsigned_negation)]
fn init_between_bb() -> () {
    static M1 : u64  = -1u64;
    static A2A7 : u64 = 0x0001010101010100u64;
    static B2G7 : u64 = 0x0040201008040200u64;
    static H1B7 : u64 = 0x0002040810204080u64; /* Thanks Dustin, g2b7 did not work for c1-a3 */
    for sq1 in 0..64 {
        for sq2 in 0..64 {
            let btwn = (M1 << sq1) ^ (M1 << sq2);
            let file = ((sq2 & 7) - (sq1 & 7)) as u64;
            let rank = (((sq2 | 7) - sq1) >> 3)  as u64;
            let mut line = ((file & 7) - 1) & A2A7; /* a2a7 if same file */
            line += 2 * (((rank & 7) - 1) >> 58); /* b1g1 if same rank */
            line += (((rank - file) & 15) - 1) & B2G7; /* b2g7 if same diagonal */
            line += (((rank + file) & 15) - 1) & H1B7; /* h1b7 if same antidiag */
            line *= btwn & -btwn; /* mul acts like shift by smaller square */
            unsafe {
                between_bb[sq1][sq2] = BitBoard(line & btwn);
            }
        }
    }
}

pub fn rook_moves_for_occ(sq: Square, b: BitBoard) -> BitBoard {
    let mut ret = BitBoard(0);
    for i in range_step(file(sq) as i8 - 1, -1, -1i8){
        let add = single_bit(Square::new(rank(sq), i as u8));
        ret = ret | add;
        if b & add != BitBoard(0) { 
            break
        }
    }
    for i in range_step(file(sq) + 1, 8, 1){
        let add = single_bit(Square::new(rank(sq), i));
        ret = ret | add;
        if b & add != BitBoard(0) { 
            break
        }
    }
    for i in range_step(rank(sq) + 1, 8, 1){
        let add = single_bit(Square::new(i, file(sq)));
        ret = ret | add;
        if b & add != BitBoard(0) { 
            break
        }
    }
    for i in range_step(rank(sq) as i8 - 1, -1, -1i8){
        let add = single_bit(Square::new(i as u8, file(sq)));
        ret = ret | add;
        if b & add != BitBoard(0) { 
            break
        }
    }
    return ret;
}

fn bishop_moves_for_occ(sq: Square, b: BitBoard) -> BitBoard {
    let mut ret = BitBoard(0);
    let mut f = file(sq);
    let mut r = rank(sq);
    loop {
        f = f + 1;
        r = r + 1;
        if f == 8 || r == 8 { break; } 
        let add = single_bit(Square::new(r, f));
        ret = ret | add;
        if b & add != BitBoard(0) { break; }
    }
    let mut f = file(sq);
    let mut r = rank(sq);
    loop {
        f = f + 1;
        r = r - 1;
        if f == 8 || r == -1 { break; } 
        let add = single_bit(Square::new(r, f));
        ret = ret | add;
        if b & add != BitBoard(0) { break; }
    }
    let mut f = file(sq);
    let mut r = rank(sq);
    loop {
        f = f - 1;
        r = r + 1;
        if f == -1 || r == 8 { break; } 
        let add = single_bit(Square::new(r, f));
        ret = ret | add;
        if b & add != BitBoard(0) { break; }
    }
    let mut f = file(sq);
    let mut r = rank(sq);
    loop {
        f = f - 1;
        r = r - 1;
        if f == -1 || r == -1 { break; } 
        let add = single_bit(Square::new(r, f));
        ret = ret | add;
        if b & add != BitBoard(0) { break; }
    }
    return ret;
}

fn init_rook_moves() -> () {
    for i in 0..64 {
        let d: BitBoard = BitBoard(OCCUPANCY_MASK_ROOK[i]);
        let mut subset: BitBoard = BitBoard(0);
        loop {
            let BitBoard(index) = (subset * MAGIC_NUMBER_ROOK[i]) >> MAGIC_NUMBER_SHIFTS_ROOK[i] as usize;
            unsafe {
                let moves = rook_moves_for_occ(Square(i as u8), subset);
                rook_moves[ROOK_INDEXES[i] + index as usize] = moves;
                rook_mob[ROOK_INDEXES[i] + index as usize] = popcnt(moves) as u8;
            }
            subset = (subset - d) & d;
            if subset == BitBoard(0) { break };
        }
    }
}

fn init_bishop_moves() -> () {
    for i in 0..64 {
        let d: BitBoard = BitBoard(OCCUPANCY_MASK_BISHOP[i]);
        let mut subset: BitBoard = BitBoard(0);
        loop {
            let BitBoard(index) = (subset * MAGIC_NUMBER_BISHOP[i]) >> MAGIC_NUMBER_SHIFTS_BISHOP[i] as usize;
            unsafe {
                let moves = bishop_moves_for_occ(Square(i as u8), subset);
                bishop_moves[BISHOP_INDEXES[i] + index as usize] = moves;
                bishop_mob[BISHOP_INDEXES[i] + index as usize] = popcnt(moves) as u8;
            }
            subset = (subset - d) & d;
            if subset == BitBoard(0) { break };
        }
    }
}

#[inline(always)]
pub fn get_rook_moves() -> &'static[BitBoard; 102400] {
    unsafe { &rook_moves }
}

#[inline(always)]
pub fn get_bishop_moves() -> &'static[BitBoard; 5248] {
    unsafe { &bishop_moves }
}

#[inline(always)]
pub fn get_rook_mob() -> &'static[u8; 102400] {
    unsafe { &rook_mob }
}

#[inline(always)]
pub fn get_bishop_mob() -> &'static[u8; 5248] {
    unsafe { &bishop_mob }
}

#[inline(always)]
pub fn get_king_moves() -> &'static[BitBoard; 64] {
    unsafe { &king_moves }
}

#[inline(always)]
pub fn get_knight_moves() -> &'static[BitBoard; 64] {
    unsafe { &knight_moves }
}

#[inline(always)]
pub fn get_between_bb() -> &'static[[BitBoard; 64]; 64] {
    unsafe { &between_bb }
}

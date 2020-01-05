:- ensure_loaded('puzzleSolver.pl').
:- ensure_loaded('puzzlePrinter.pl').

load_puzzle1([[a, -3, A], [b, -1, B], [c, 2, [[ca, -2, CA],[cb, -1, CB],[cc, 1, CC]]]]).
load_puzzle2([[a, -3, A],[b, -1, [[ba, -1, BA],[bb, 2, BB]]], [c, 2, C], [d, 5, D]]).
load_puzzle3([[a, -3, A],[b, -2, B], [c, 1, [[ca, -1, [[caa, -3, CAA], [cab, 1, CAB]]],[cb, 1, CB]]],[d, 2, D]]).
load_14var_puzzle([[a,-2,[[aa,-2,AA],[ab,-1,AB],[ac,5,AC]]],[b,1,[[ba,-1,[[ba1,-1,[[b1a,-2,B1A],[b1b,-1,B1B],[b1c,1,[[b2a,-1,[[b3a,-2,B3A],[b3b,1,[[b4a,-1,B4A],[b4b,2,B4B]]]]],[b2b,1,B2B],[b2c,2,B2C]]]]],[bab,1,BAB],[bac,2,BAC]]],[bb,1,BB],[bc,3,BC]]]]).
load_18var_puzzle([[a,-2,[[aa,-2,AA],[ab,-1,AB],[ac,1,[[aca,-2,ACA],[acb,1,ACB],[acc,2,ACC]]]]],[b,-1,[[ba,-2,BA],[bb,1,BB],[bc,2,BC],[bd,3,BD],[be,4,BE],[bf,5,BF]]],[c,1,[[ca,-1,CA],[cb,1,CB],[cc,2,CC]]],[d,2,[[da,-1,DA],[db,2,DB]]],[e,3,E],[f,4,F]]).
load_20var_puzzle([[a, -4, [[aa, -3, AA],[ab, -2, AB],[ac, 3, AC]]],[b, -2, [[ba, -2, BA],[bb, -1, BB],[bc, 1,[[bca, -1, [[bcaa, -1, BCAA],[bcab, 2, BCAB],[bcac, 3, BCAC]]],[bcb, 1, BCB],[bcc, 2, BCC]]]]],[c, 1, C],[d, 3, [[da, -1, [[daa, -1, [[daaa, -1, DAAA],[daab, 1, [[daaba, -2, DAABA],[daabb, 1, DAABB]]],[daac, 2, DAAC],[daad, 3, DAAD]]],[dab, 1, DAB],[dac, 2, DAC]]],[db, 1, DB],[dc, 2, DC]]]]).

test5var_1(Res):-
    load_puzzle1(Puzzle),
    solvePuzzle(Puzzle, Res),
    printLoop(Puzzle).

test5var_2(Res):-
    load_puzzle2(Puzzle),
    solvePuzzle(Puzzle, Res),
    printLoop(Puzzle).

test6var(Res):-
    load_puzzle3(Puzzle),
    solvePuzzle(Puzzle, Res),
    printLoop(Puzzle).

test14var(Res):-
    load_14var_puzzle(Puzzle),
    solvePuzzle(Puzzle,Res),
    printLoop(Puzzle).

test18var(Res):-
    load_18var_puzzle(Puzzle),
    solvePuzzle(Puzzle,Res),
    printLoop(Puzzle).

nightmare(Res):-
    load_20var_puzzle(Puzzle),
    solvePuzzle(Puzzle, Res),
    printLoop(Puzzle).

:- use_module(library(fdbg)).
:- ensure_loaded('puzzleSolver.pl').
:- ensure_loaded('puzzleGenerator.pl').
:- ensure_loaded('puzzlePrinter.pl').

%test1(Res):-
%    solvePuzzle([[a, -3, A], [b, -1, B], [c, 2, [[ca, -2, CA],[cb, -1, CB],[cc, 1, CC]]]], Res).

test2(Number):-
    fdbg_on([file('fdbg.log',write)]).
    fdbg_assign_name(Puzzle,puzzle),
    generatePuzzle(Number, Puzzle),
    printPuzzle(Puzzle).

load_puzzle1([[a, -3, A], [b, -1, B], [c, 2, [[ca, -2, CA],[cb, -1, CB],[cc, 1, CC]]]]).
load_puzzle2([[a, -3, A],[b, -1, [[ba, -1, BA],[bb, 2, BB]]], [c, 2, C], [d, 5, D]]).
load_puzzle3([[a, -3, A],[b, -2, B], [c, 1, [[ca, -1, [[caa, -3, CAA], [cab, 1, CAB]]],[cb, 1, CB]]],[d, 2, D]]).
load_20var_puzzle([[a, -4, [[aa, -3, AA],[ab, -2, AB],[ac, 3, AC]]],[b, -2, [[ba, -2, BA],[bb, -1, BB],[bc, 1,[[bca, -1, [[bcaa, -1, BCAA],[bcab, 2, BCAB],[bcac, 3, BCAC]]],[bcb, 1, BCB],[bcc, 2, BCC]]]]],[c, 1, C],[d, 3, [[da, -1, [[daa, -1, [[daaa, -1, DAAA],[daab, 1, [[daaba, -2, DAABA],[daabb, 1, DAABB]]],[daac, 2, DAAC],[daad, 3, DAAD]]],[dab, 1, DAB],[dac, 2, DAC]]],[db, 1, DB],[dc, 2, DC]]]]).

test5var_1(Res):-
    load_puzzle1(Puzzle),
    solvePuzzle(Puzzle, Res),
    %printPuzzle(Puzzle, a),
    %printPuzzle(Puzzle, b),
    printPuzzle(Puzzle).

test5var_2(Res):-
    load_puzzle2(Puzzle),
    solvePuzzle(Puzzle, Res),
    printPuzzle(Puzzle, b).

test6var(Res):-
    load_puzzle3(Puzzle),
    solvePuzzle(Puzzle, Res),
    printPuzzle(Puzzle),
    printPuzzle(Puzzle).

nightmare(Res):-
    load_20var_puzzle(Puzzle),
    solvePuzzle(Puzzle, Res),
    printPuzzle(Puzzle).

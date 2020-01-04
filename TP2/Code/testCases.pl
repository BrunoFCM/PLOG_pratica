:- use_module(library(fdbg)).
:- ensure_loaded('puzzleSolver.pl').
:- ensure_loaded('puzzleGenerator.pl').

test1(Res):-
    solvePuzzle([[a, -3, A], [b, -1, B], [c, 2, [[ca, -2, CA],[cb, -1, CB],[cc, 1, CC]]]], Res).

test2(Number):-
    fdbg_on([file('fdbg.log',write)]).
    fdbg_assign_name(Puzzle,puzzle),
    generatePuzzle(Number, Puzzle),
    printPuzzle(Puzzle).
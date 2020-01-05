:- ensure_loaded('puzzleSolver.pl').
:- ensure_loaded('puzzleGenerator.pl').
:- ensure_loaded('puzzlePrinter.pl').

debugThing(0,Puzzle):-generatePuzzle(2,Puzzle).
debugThing(1,Puzzle, AuxVars):-
    generatePuzzleShape(2, Puzzle, AuxVars),
    labeling([],AuxVars).
debugThing(2,[A,B], SubAuxVars):-
    generateNodes(2,[A,B], [1,1], SubAuxVars),
    labeling([],SubAuxVars).
debugThing(21,[A,B], SubAuxVars):-
    generateNodes(2,[B], [1], SubAuxVars),
    labeling([],SubAuxVars).
debugThing(3,Children, SubAuxVars):-
    generateChildren(3,3,Children, SubAuxVars),
    labeling([],SubAuxVars).
debugThing(4,Children, SubAuxVars):-
    domain([Weights],2,3),
    generateChildren(3,Weights,Children, SubAuxVars),
    labeling([],[Weights|SubAuxVars]).
debugThing(5,[A,B], SubAuxVars):-
    generateNodes(3,[B], [2], SubAuxVars),
    labeling([],SubAuxVars).
debugThing(6,[A,B], SubAuxVars):-
    generateNodes(3,[A], [1], SubAuxVars),
    labeling([],SubAuxVars).
debugThing(7,Puzzle, AuxVars):-
    generatePuzzleShape(3, Puzzle, AuxVars),
    labeling([],AuxVars).
debugThing(8,Puzzle, AuxVars):-
    
    labeling([],AuxVars).

load_puzzle1([[a, -3, A], [b, -1, B], [c, 2, [[ca, -2, CA],[cb, -1, CB],[cc, 1, CC]]]]).
load_puzzle2([[a, -3, A],[b, -1, [[ba, -1, BA],[bb, 2, BB]]], [c, 2, C], [d, 5, D]]).
load_puzzle3([[a, -3, A],[b, -2, B], [c, 1, [[ca, -1, [[caa, -3, CAA], [cab, 1, CAB]]],[cb, 1, CB]]],[d, 2, D]]).
load_20var_puzzle([[a, -4, [[aa, -3, AA],[ab, -2, AB],[ac, 3, AC]]],[b, -2, [[ba, -2, BA],[bb, -1, BB],[bc, 1,[[bca, -1, [[bcaa, -1, BCAA],[bcab, 2, BCAB],[bcac, 3, BCAC]]],[bcb, 1, BCB],[bcc, 2, BCC]]]]],[c, 1, C],[d, 3, [[da, -1, [[daa, -1, [[daaa, -1, DAAA],[daab, 1, [[daaba, -2, DAABA],[daabb, 1, DAABB]]],[daac, 2, DAAC],[daad, 3, DAAD]]],[dab, 1, DAB],[dac, 2, DAC]]],[db, 1, DB],[dc, 2, DC]]]]).

test5var_1(Res):-
    load_puzzle1(Puzzle),
    solvePuzzle(Puzzle, Res).
    %printPuzzle(Puzzle, a),
    %printPuzzle(Puzzle, b),
    %printPuzzle(Puzzle).

test5var_2(Res):-
    load_puzzle2(Puzzle),
    solvePuzzle(Puzzle, Res).
    %printPuzzle(Puzzle).

test6var(Res):-
    load_puzzle3(Puzzle),
    solvePuzzle(Puzzle, Res).
    %printPuzzle(Puzzle).

nightmare(Res):-
    load_20var_puzzle(Puzzle),
    solvePuzzle(Puzzle, Res),
    printLoop(Puzzle).
    %printPuzzle(Puzzle).

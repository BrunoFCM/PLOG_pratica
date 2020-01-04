:- use_module(library(clpfd)).
:- ensure_loaded('puzzleNodes.pl').
:- ensure_loaded('puzzleSolver.pl').

generatePuzzle(WeightsNumber, Puzzle):-
    generatePuzzleShape(WeightsNumber, Puzzle, AuxVars),
    solveGeneratedPuzzle(Puzzle, SolvedVars),
    labeling([], FinalVarList).

generatePuzzleShape(WeightsNumber, Puzzle, [ListLength|AuxVars]):-
    domain([ListLength], 2, WeightsNumber),
    length(Puzzle, ListLength),
    
    length(WeightsList, ListLength),
    SubWeightNumber is WeightsNumber - 1,
    domain(WeightsList, 1, SubWeightNumber),
    sum(WeightsList, #=, WeightsNumber),

    generateNodes(Puzzle, WeightsList, SubAuxList),
    append(AuxVars, WeightsList, SubAuxList).
    
    
generateNodes([Node|Nodes], [WeightsNumber|WeightsNumbers], [Distance,ListLength|AuxVars]):-
    length(Node,3),
    getNodeID(Node,a),
    getNodeDistance(Node,Distance),
    domain([Distance], -100, 100), Distance #\= 0,

    domain([ListLength], 1, WeightsNumber),
    
    getNodeChildren(Node, Children),
    length(Children, ListLength),

    length(WeightsList, ListLength),
    SubWeightNumber is WeightsNumber - 1,
    domain(WeightsList, 1, SubWeightNumber),
    sum(WeightsList, #=, WeightsNumber),

    WeightsNumber #= 1 #<=> IsWeight,
    generateChildren(IsWeight, Children, WeightsList, ChildAuxVars),
    append(SelfAuxVars, WeightsList, ChildAuxVars),

    generateNodes(Nodes, WeightsNumbers, SiblingsAuxList),
    append(AuxVars, SelfAuxVars, SiblingsAuxList).

generateChildren(1, Children, _, []).

generateChildren(0, Children, WeightsList, ChildAuxVars):-
    generateNodes(Children, WeightsList, ChildAuxVars).
    

solveGeneratedPuzzle(Puzzle, FinalVarList):-
    balanceNodes(Puzzle, 0, [], FinalVarList, _),
    length(FinalVarList, VarNum),
    domain(FinalVarList, 1, VarNum),
    all_distinct(FinalVarList).


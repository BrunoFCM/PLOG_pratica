:- use_module(library(clpfd)).
:- ensure_loaded('puzzleNodes.pl').
:- ensure_loaded('puzzleSolver.pl').

generatePuzzle(WeightsNumber, Puzzle):-
    generatePuzzleShape(WeightsNumber, Puzzle, FinalVarList),
    %optimizeNodes(Puzzle),

    solveGeneratedPuzzle(Puzzle, SolvedVars),
    append(SolvedVars, FinalVarList, AuxVars),

    labeling([], AuxVars).

generatePuzzleShape(WeightsNumber, Puzzle, [ListLength|AuxVars]):-
    domain([ListLength], 2, WeightsNumber),
    length(Puzzle, ListLength),
    
    length(WeightsList, ListLength),
    SubWeightNumber is WeightsNumber - 1,
    domain(WeightsList, 1, SubWeightNumber),
    sum(WeightsList, #=, WeightsNumber),

    generateNodes(WeightsNumber,Puzzle, WeightsList, SubAuxList),
    append(AuxVars, WeightsList, SubAuxList).

generateNodes(_,_,[],[]). 

generateNodes(WeightsNumber,[Node|Nodes], [Weights|WeightsList], [Distance|AuxVars]):-
    length(Node,3),
    getNodeID(Node,a),
    getNodeDistance(Node,Distance),
    domain([Distance], -100, 100), Distance #\= 0,

    generateChildren(WeightsNumber, Weights, Children, SubWeightsList, ChildrenGenerateVars),
    generateNodes(WeightsNumber, Children, SubWeightsList, ChildAuxVars),
    getNodeChildren(Node, Children),

    append(ChildAuxVars, ChildrenGenerateVars, ChildVars),
    append(SubWeightsList, ChildVars, SelfAuxVars),
    
    generateNodes(WeightsNumber,Nodes, WeightsList, SiblingsAuxList),
    append(SelfAuxVars, SiblingsAuxList, AuxVars).

generateChildren(_, 1, Weight, [], []).

generateChildren(WeightsNumber, Weights, Children, WeightsList, [ListLength]):-
    domain([ListLength], 2, WeightsNumber), 
    ListLength #=< Weights,

    length(Children, ListLength),
    length(WeightsList, ListLength),

    domain(WeightsList, 1, WeightsNumber),
    sum(WeightsList, #=, Weights).
    

solveGeneratedPuzzle(Puzzle, FinalVarList):-
    balanceNodes(Puzzle, 0, [], FinalVarList, _),
    length(FinalVarList, VarNum),
    domain(FinalVarList, 1, VarNum),
    all_distinct(FinalVarList).

/*
optimizeNodes(Weight):-
    var(Weight).

optimizeNodes(Nodes):-
    nonvar(Nodes),
    length(Nodes, 1).

optimizeNodes([NodeA,NodeB|Nodes]):-    
    getNodeChildren(NodeA,Children),
    optimizeNodes(Children),

    getNodeDistance(NodeA, DistanceA),
    getNodeDistance(NodeB, DistanceB),
    DistanceA #< DistanceB,

    optimizeNodes([NodeB|Nodes]).

*/



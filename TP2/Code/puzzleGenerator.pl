:- use_module(library(clpfd)).
:- ensure_loaded('puzzleNodes.pl').
:- ensure_loaded('puzzleSolver.pl').

generatePuzzle(WeightsNumber, Puzzle):-
    generatePuzzleShape(WeightsNumber, Puzzle, FinalVarList),
    %optimizeNodes(Puzzle),

    solveGeneratedPuzzle(Puzzle, SolvedVars),
    append(SolvedVars, FinalVarList, AuxVars),

    optimizeDistances(Puzzle, Options, OptimizeVars),

    labeling([], AuxVars).

generatePuzzleShape(WeightsNumber, Puzzle, [ListLength|AuxVars]):-
    domain([ListLength], 2, WeightsNumber),
    length(Puzzle, ListLength),
    
    length(WeightsList, ListLength),
    SubWeightNumber is WeightsNumber - 1,
    domain(WeightsList, 1, SubWeightNumber),
    sum(WeightsList, #=, WeightsNumber),

    generateNodes(WeightsNumber,Puzzle, WeightsList, SubAuxList),
    append(SubAuxList, WeightsList, AuxVars).

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

generateChildren(_, 1, _, [], []).

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

%balanceNodes(-Nodelist, -Balance, -Varlist, +FinalVarList, +TotWeight) , call with Balance = 0 and VarList = []
balanceNodes([], Balance, VarList, VarList, 0):-
    Balance #= 0.
balanceNodes([Node|MoreNodes], Balance, VarList, FinalVarList, TotWeight):-
    getNodeWeight(Node, VarList, Weight, Torque, NewVarList),
    AddedBalance #= Balance + Torque,
    TotWeight #= PrevWeight + Weight,
    balanceNodes(MoreNodes, AddedBalance, NewVarList, FinalVarList, PrevWeight).

optimizeDistances([Node|Puzzle], [min(AbsDistance)|Options], [AbsDistance|OptimizeVars]):-
    getNodeDistance(Node, Distance),
    AbsDistance #= abs(Distance),
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



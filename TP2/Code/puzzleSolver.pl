:- use_module(library(clpfd)).
:- ensure_loaded('puzzleNodes.pl').

solvePuzzle(Puzzle, FinalVarList):-
    balanceNodes(Puzzle, 0, [], FinalVarList, _),
    length(FinalVarList, VarNum),
    domain(FinalVarList, 1, VarNum),
    all_distinct(FinalVarList),
    labeling([], FinalVarList).

%balanceNodes(-Nodelist, -Balance, -Varlist, +FinalVarList, +TotWeight) , call with Balance = 0 and VarList = []
balanceNodes([], Balance, VarList, VarList, 0):-
    Balance #= 0.
balanceNodes([Node|MoreNodes], Balance, VarList, FinalVarList, TotWeight):-
    getNodeWeight(Node, VarList, Weight, Torque, NewVarList),
    AddedBalance #= Balance + Torque,
    TotWeight #= PrevWeight + Weight,
    balanceNodes(MoreNodes, AddedBalance, NewVarList, FinalVarList, PrevWeight).

%getNodeWeight(-Node, -VarList, +Weight, Torque, NewVarList)
%case 2: is dead end
getNodeWeight([_, Distance, Weight], VarList, Weight, Torque, [Weight|VarList]):-
    var(Weight),
    Torque #= Distance * Weight.
%case 1: has children nodes
getNodeWeight([_, Distance, Children], VarList, Weight, Torque, NewVarList):-
    balanceNodes(Children, 0, VarList, NewVarList, Weight),
    Torque #= Distance * Weight.
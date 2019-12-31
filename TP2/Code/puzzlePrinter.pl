:-ensure_loaded('puzzleNodes.pl').

fillDistance(0, Char).

fillDistance(Distance, Char):-
    RealDistance is abs(Distance) * 6,
    repeatChar(RealDistance, Char).

%-------------------------------------------------------------------

fillDistanceDelta(0, Char).

fillDistanceDelta(Distance, Char):-
    RealDistance is abs(Distance) * 6 - 1,
    repeatChar(RealDistance, Char).

%-------------------------------------------------------------------

repeatChar(0, _).
%-------------------------------------------------------------------
repeatChar(Distance, Char):-
    write(Char),
    NewDistance is Distance - 1,
    repeatChar(NewDistance, Char).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printPuzzle(Root, SelectedNode):-
    getPathTo(Root,SelectedNode,Path),
    getLeftDistance(Root,Path,Distance),
    printPuzzle(Root, Distance, Path).
%-------------------------------------------------------------------
printPuzzle(Root, Distance, []):-
    printHoldingChar(Distance),
    printFulcrumLine(Root,Distance),
    printIdLine(Root,Distance), nl.
%-------------------------------------------------------------------
printPuzzle(Root, Distance, [NextNodeID|Path]):-
    printHoldingChar(Distance),
    printFulcrumLine(Root,Distance),
    printIdLine(Root,Distance),
    
    getNodeChild(Root, NextNodeID, Node),
    getNodeDistance(Node,NodeDistance),
    NewDistance is Distance - NodeDistance,

    getNodeChildren(Node,Children),

    printPuzzle(Children, NewDistance, Path).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printHoldingChar(Distance):-
    fillDistance(Distance, ' '), 
    write('|'), 
    nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printFulcrumLine(Nodes, Distance):-
    getFulcrumDistance(Nodes, FulcrumDistance),
    DistanceFill is Distance - FulcrumDistance,
    fillDistance(DistanceFill, ' '), 
    printFulcrumLine(Nodes).
%--------------------------------------------------------------------
printFulcrumLine([LeftNode,RightNode|Nodes]):-
    getNodeDistance(LeftNode, LeftDistance),
    getNodeDistance(RightNode, RightDistance),
    LeftDistance < 0,
    RightDistance > 0,

    printFulcrumLine([LeftNode,[foo,0,bar],RightNode|Nodes]).
%--------------------------------------------------------------------
printFulcrumLine([LeftNode,RightNode|Nodes]):-
    getNodeDistance(LeftNode, LeftDistance),
    getNodeDistance(RightNode, RightDistance),
    Delta is RightDistance - LeftDistance,

    write('+'),
    fillDistanceDelta(Delta, '-'),

    printFulcrumLine([RightNode|Nodes]).
%--------------------------------------------------------------------
printFulcrumLine([Node]):-
    write('+'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printIdLine(Nodes, Distance):-
    getFulcrumDistance(Nodes, FulcrumDistance),
    DistanceFill is Distance - FulcrumDistance,
    fillDistance(DistanceFill, ' '), 
    printIdLine(Nodes).
%--------------------------------------------------------------------
printIdLine([LeftNode,RightNode|Nodes]):-
    getNodeDistance(LeftNode, LeftDistance),
    getNodeDistance(RightNode, RightDistance),

    printNodeAttributes(LeftNode, UsedSpace),
    
    Delta is 6 * (RightDistance - LeftDistance) - UsedSpace,
    repeatChar(Delta, ' '),

    printIdLine([RightNode|Nodes]).
%--------------------------------------------------------------------
printIdLine([Node]):-
    printNodeAttributes(Node,_), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printNodeAttributes(Node, UsedSpace):-
    getNodeChildren(Node, NodeWeight),
    number(NodeWeight),
    getNodeID(Node, NodeID),
    write(NodeID), write('='), write(NodeWeight),

    atom_chars(NodeID, CharList), length(CharList, IDlength),
    number_length(NodeWeight, WeightLength),

    UsedSpace is IDlength + WeightLength + 1.
%--------------------------------------------------------------------
printNodeAttributes(Node, UsedSpace):-
    getNodeID(Node, NodeID),
    write(NodeID),

    atom_chars(NodeID, CharList), length(CharList, IDlength),
    
    UsedSpace is IDlength + 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

number_length(Number, Length):-
    Number >= 1,
    NewNumber is Number / 10,

    number_length(NewNumber, Length_i),
    Length is Length_i + 1.
%--------------------------------------------------------------------
number_length(_,0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getLeftDistance([FirstNode|_],[],Distance):-
    getNodeDistance(FirstNode, Distance).
%--------------------------------------------------------------------
getLeftDistance([FirstNode|Nodes],[PathID|Path],LeftDistance):-
    getNodeDistance(FirstNode, DistanceA),

    getNodeChild([FirstNode|Nodes], PathID, FoundNode),
    getNodeChildren(FoundNode, Children),
    getLeftDistance(Children, Path, DistanceB),

    getNodeDistance(FoundNode, AddedDistance),
    RealDistanceB is DistanceB + AddedDistance,

    LeftDistance is min(DistanceA, RealDistanceB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getFulcrumDistance([FirstNode|_], Distance):-
    getNodeDistance(FirstNode, Distance).






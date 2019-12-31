fillDistance(Distance, Char):-
    RealDistance is abs(Distance) * 6,
    repeatChar(RealDistance, Char).

repeatChar(0, _).

repeatChar(Distance, Char):-
    write(Char),
    NewDistance is Distance - 1,
    repeatChar(NewDistance, Char).

getNodeDistance(_-NodeDistance-_, NodeDistance).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print(Root, SelectedNode):-
    getPathTo(Root,SelectedNode,Path),
    getLeftDistance(Root,Path,LeftDistance),
    print(Root, Distance, [_|Path]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print(Root, Distance, [NextNodeID|Path]):-
    printHoldingChar(Distance),
    printFulcrumLine(Root,Distance),
    printIdLine(Root,Distance),
    
    getNode(Root, NextNodeID, Node),
    getNodeDistance(Node,NodeDistance),
    NewDistance is Distance - NodeDistance,

    print(Node, NewDistance, Path).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print(Root, Distance, []):-
    printHoldingChar(Distance),
    printFulcrumLine(Root,Distance),
    printIdLine(Root,Distance), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printHoldingChar(Distance):-
    fillDistance(Distance, ' '), 
    write('|'), 
    nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printFulcrumLine(Nodes, Distance):-
    fillDistance(Distance, ' '), 
    printFulcrumLine(Nodes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printFulcrumLine([LeftNode,RightNode|Nodes]):-
    getNodeDistance(LeftNode, LeftDistance),
    getNodeDistance(RightNode, RightDistance),
    Delta is RightDistance - LeftDistance,

    write('+'),
    fillDistance(Delta, '-'),

    printFulcrumLine([RightNode|Nodes]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printFulcrumLine([Node]):-
    write('+'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printIdLine(Nodes, Distance):-
    fillDistance(Distance, ' '), 
    printIdLine(Nodes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printIdLine([LeftNode,RightNode|Nodes]):-
    getNodeDistance(LeftNode, LeftDistance),
    getNodeDistance(RightNode, RightDistance),

    printNodeAttributes(Node, UsedSpace),
    
    Delta is RightDistance - LeftDistance - UsedSpace,
    fillDistance(Delta, '-'),

    printIdLine([RightNode|Nodes]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printIdLine([Node]):-
    printNodeAttributes(Node,_), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printNodeAttributes(NodeID-_-NodeWeight, UsedSpace):-
    number(NodeWeight),
    write(NodeID), write('='), write(NodeWeight),

    atom_chars(NodeID, CharList), length(CharList, IDlength),
    number_length(NodeWeight, WeightLength),

    UsedSpace is IDlength + WeightLength.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printNodeAttributes(NodeID-_-_, UsedSpace):-
    write(NodeID),

    atom_chars(NodeID, CharList), length(CharList, UsedSpace).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

number_length(Number, Length):-
    Number >= 1,
    NewNumber is Number / 10,

    number_length(NewNumber, Length_i),
    Length is Length_i + 1.

number_length(_,0).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getNode([NodeID-NodeDistance-NodeWeight], NodeID, NodeID-NodeDistance-NodeWeight).

getNode([_|Nodes], NodeID, Node):-
    getNode(Nodes, NodeID, Node).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getPathTo([NodeID-_-_|Nodes],Node,[]).

getPathTo([_-_-Nodes|_],Node,Path):-
    getPathTo(Nodes, Node, Path).

getPathTo([_|Nodes], Node, Path):-
    getPathTo(Nodes, Node, Path).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getLeftDistance(_,[],0):-

getLeftDistance([FirstNode|Nodes],[PathID|Path],LeftDistance):-
    getNodeDistance(FirstNode, DistanceA),

    getNode([FirstNode|Nodes], PathID, _-_-FollowingNodes),
    getLeftDistance(FollowingNodes, Path, DistanceB),

    LeftDistance is min(DistanceA, DistanceB).
    









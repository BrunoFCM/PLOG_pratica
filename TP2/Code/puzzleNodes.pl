getNodeID([NodeID,_,_], NodeID).
%--------------------------------------------------------------------
getNodeDistance([_,NodeDistance,_], NodeDistance).
%--------------------------------------------------------------------
getNodeChildren([_,_,Children], Children).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getPathTo([Node|_],NodeID,[NodeID]):-
    getNodeID(Node, NodeID).
%--------------------------------------------------------------------
getPathTo([Node|_],NodeID,[ParentID|Path]):-
    getNodeChildren(Node, Children),
    getPathTo(Children, NodeID, Path),
    getNodeID(Node,ParentID).
%--------------------------------------------------------------------
getPathTo([_|Nodes], NodeID, Path):-
    getPathTo(Nodes, NodeID, Path).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getNodeChild([Node|_], NodeID, Node):-
    getNodeID(Node,NodeID).
%--------------------------------------------------------------------
getNodeChild([_|Nodes], NodeID, Node):-
    getNodeChild(Nodes, NodeID, Node).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





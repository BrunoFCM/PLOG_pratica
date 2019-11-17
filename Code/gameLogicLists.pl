:-use_module(library(random)).

/*------------------------------------------------------------------------------------------------------*/

makePointList([], _, []).
makePointList(_, [], []).

makePointList([X|XList], Y, List):-
    makePointList(XList, Y, Points),
    append([[X,Y]], Points, List).

makePointList(X, [Y|YList], List):-
    makePointList(X, YList, Points),
    append([[X,Y]], Points, List).

/*------------------------------------------------------------------------------------------------------*/

subtractRandomIndex(List, Value, NewList):-
    length(List, Length),
    random_between(1, Length, Index),
    subtractIndex(List, Index, Value, NewList).

subtractIndex([Value|List], 1, Value, List).

subtractIndex([Element|List], Index, Value, [Element|NewList]):-
    IndexN is Index - 1,
    subtractIndex(List, IndexN, Value, NewList).

/*------------------------------------------------------------------------------------------------------*/

intersectUnit(_,[],[]).

intersectUnit(Element, [Element|_], [Element]).

intersectUnit(ElementA, [_|ListB], List):-
    intersectUnit(ElementA, ListB, List).

intersect([],_,[]).

intersect([ElementA|ListA], ListB, List):-
    intersectUnit(ElementA, ListB, List1),
    intersect(ListA, ListB, List2),
    append(List1, List2, List).

/*------------------------------------------------------------------------------------------------------*/
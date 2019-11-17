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
    random_between(0, Length, Index),
    subtractIndex(List, Index, Value, NewList).

subtractIndex([Value|List], 1, Value, List).

subtractIndex([Element|List], Index, Value, [Element|NewList]):-
    IndexN is Index - 1,
    subtractIndex(List, IndexN, Value, NewList).

/*------------------------------------------------------------------------------------------------------*/
getPlayerInput(1, Xcoord, Ycoord):-
    write('Player 1, please insert valid coordinates for your next piece (X,Y)'),
    get_char(Xcoord), get_char(','), get_char(Ycoord).

getPlayerInput(2, Xcoord, Ycoord):-
    write('Player 2, please insert valid coordinates for your next piece (X,Y)'),
    get_char(Xcoord), get_char(','), get_char(Ycoord).
    
    
    
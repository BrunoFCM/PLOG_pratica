
/* getPlayerInput(+Player, -Xcoord, -Ycoord) */
/* as get_char leaves the end of line character in the buffer, skip_line is used */
getPlayerInput(1, Xcoord, Ycoord):-
    write('Player 1, please insert valid coordinates for your next piece (X,Y)'),
    get_char(Xcoord), get_char(','), get_char(Ycoord), skip_line.


getPlayerInput(2, Xcoord, Ycoord):-
    write('Player 2, please insert valid coordinates for your next piece (X,Y)'),
    get_char(Xcoord), get_char(','), get_char(Ycoord), skip_line.

/* cleans buffer in case of bad inputs not passing the ',' */
getPlayerInput(_, 0, 0):-
    skip_line.
    
    
    
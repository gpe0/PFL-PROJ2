drawHeader :- 
    write('      A       B       C       D       E       F       G       H       I       J     '), nl,
    write('   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  '), nl.

drawFooter :-
    write('   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  '), nl,
    write('      A       B       C       D       E       F       G       H       I       J     '), nl.

/*
Piece:
    0 - No Piece
    1 - Elephant
    2 - Mouse
    3 - Lion

Offset:
    1 - Top
    2 - Mid
    2 - Bot

Colors:
    0 - White
    1 - Black (dots)
*/

%drawPlace(Piece, Offset, Color).

% Empty
drawPlace(0, _, 0) :-
    write('       |').

drawPlace(0, _, 1) :-
    write(' . . . |').

% Elephant
drawPlace(1, 1, _) :- write('()o o()|').

drawPlace(1, _, _) :- write('  ( )  |').

% Mouse
drawPlace(2, 1, _) :- write('| o o ||').

drawPlace(2, 2, _) :- write(' \\   / |').

drawPlace(2, 3, _) :- write(' -\\_/- |').

% Lion
drawPlace(3, 1, _) :- write(' @@@@@ |').

drawPlace(3, 2, _) :- write('@o\\ /o@|').

drawPlace(3, 3, _) :- write(' @@@@@ |').

% drawLineLoop(Index, Line, Offset)
drawLineLoop(_, [], _).
drawLineLoop(Index, [H|T], Offset) :-
    Color is Index mod 2,
    I1 is Index + 1,
    drawPlace(H, Offset, Color),
    drawLineLoop(I1, T, Offset).

% drawLine(LineIndex, Line, Offset)
drawLine(10, Line, 2) :-
    write('10|'),
    drawLineLoop(0, Line, 2),
    write('10'), nl.

drawLine(N, Line, 2) :-
    write(N), write(' |'), 
    N1 is N mod 2,
    drawLineLoop(N1, Line, 2),
    write(N), nl.

drawLine(N, Line, Offset) :-
    write('  |'),
    N1 is N mod 2,
    drawLineLoop(N1, Line, Offset), nl.

% drawRow(Index, Row)
drawRow(Index, Row) :-
    drawLine(Index, Row, 1),
    drawLine(Index, Row, 2),
    drawLine(Index, Row, 3).

% drawRowLoop(Index, Row)
drawRowLoop(1, [H|_]) :-
    drawRow(1, H).
drawRowLoop(Index, [H|T]) :-
    drawRow(Index, H),
    write('   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  '), nl,
    I1 is Index - 1,
    drawRowLoop(I1, T).

% For now, empty hard-coded board.
drawBoard :-
    append([], [
        [0, 0, 0, 0, 1, 1, 0, 0, 0, 0],
        [0, 0, 0, 3, 2, 2, 3, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 3, 2, 2, 3, 0, 0, 0],
        [0, 0, 0, 0, 1, 1, 0, 0, 0, 0]
    ], Board),
    drawHeader,
    drawRowLoop(10, Board), 
    drawFooter.
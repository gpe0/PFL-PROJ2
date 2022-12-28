% Draws the header of the board
drawHeader :- 
    write('      A       B       C       D       E       F       G       H       I       J     '), nl,
    write('   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  '), nl.

% Draws the footer of the board
drawFooter :-
    write('      A       B       C       D       E       F       G       H       I       J     '), nl.

/*
Piece:
    0 - No Piece
    1 - Elephant (Player 1)
    2 - Mouse (Player 1)
    3 - Lion (Player 1)
    4 - Elephant (Player 2)
    5 - Mouse (Player 2)
    6 - Lion (Player 2)
    7 - Target
    8 - Possible Move
    9 - Previous Position

Offset:
    1 - Top
    2 - Mid
    3 - Bot

Colors:
    0 - White
    1 - Black (dots)
*/

/*
    drawPlace(+Piece, +Offset, +Color).

    Draws a line of a square
    (Square has 3 lines)

    +Piece : Piece to draw
    +Offset : Line of the square
    +Color : Color of the square
*/

% Empty
drawPlace(0, _, 0) :- write('       |').
drawPlace(0, _, 1) :- write(' . . . |').

% Target
drawPlace(7, 2, _) :- write(' --|-- |').
drawPlace(7, _, _) :- write('   |   |').

% Elephant
drawPlace(1, 1, _) :- write('()o o()|').
drawPlace(1, 2, _) :- write('  (1)  |').
drawPlace(1, _, _) :- write('  ( )  |').
drawPlace(4, 1, _) :- write('()o o()|').
drawPlace(4, 2, _) :- write('  (2)  |').
drawPlace(4, _, _) :- write('  ( )  |').

% Mouse
drawPlace(2, 1, _) :- write('| o o ||').
drawPlace(2, 2, _) :- write(' \\ 1 / |').
drawPlace(2, 3, _) :- write(' -\\_/- |').
drawPlace(5, 1, _) :- write('| o o ||').
drawPlace(5, 2, _) :- write(' \\ 2 / |').
drawPlace(5, 3, _) :- write(' -\\_/- |').

% Lion
drawPlace(3, 1, _) :- write(' @@@@@ |').
drawPlace(3, 2, _) :- write('@o\\1/o@|').
drawPlace(3, 3, _) :- write(' @@@@@ |').
drawPlace(6, 1, _) :- write(' @@@@@ |').
drawPlace(6, 2, _) :- write('@o\\2/o@|').
drawPlace(6, 3, _) :- write(' @@@@@ |').

% Possible Move
drawPlace(8, _, _) :- write('XXXXXXX|').

% Previous Position
drawPlace(9, _, _) :- write('~~~~~~~|').

/*
    drawLineLoop(+Index, +Pieces, +Offset)

    Loop to draw a line on all squares of the row

    +Index : Column of the square
    +Pieces : Pieces on the row
    +Offset : Line of the squares
*/
drawLineLoop(_, [], _).
drawLineLoop(Index, [H|T], Offset) :-
    Color is Index mod 2,
    I1 is Index + 1,
    drawPlace(H, Offset, Color),
    drawLineLoop(I1, T, Offset).

/*
    drawLine(+N, +Pieces, +Offset)

    High-level call to draw a line of all squares of the row

    +N : Row index
    +Pieces : Pieces of the row
    +Offset : Line of the row
*/
drawLine(10, Pieces, 2) :-
    write('10|'),
    drawLineLoop(0, Pieces, 2),
    write('10'), nl.

drawLine(N, Pieces, 2) :-
    format(' ~d|', [N]),
    N1 is N mod 2,
    drawLineLoop(N1, Pieces, 2),
    write(N), nl.

drawLine(N, Pieces, Offset) :-
    write('  |'),
    N1 is N mod 2,
    drawLineLoop(N1, Pieces, Offset), nl.

/*
    drawRow(+Index, +Row)

    Draws a row of the board

    +Index : Row index
    +Row : Pieces of the row
*/
drawRow(Index, Row) :-
    drawLine(Index, Row, 1),
    drawLine(Index, Row, 2),
    drawLine(Index, Row, 3).

/*
    drawRowLoop(+Index, +Row)

    Draws all the rows of the board

    +Index : Current row index
    +Row : Pieces of the row
*/
drawRowLoop(_, []).
drawRowLoop(Index, [H|T]) :-
    drawRow(Index, H),
    write('   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  '), nl,
    I1 is Index - 1,
    drawRowLoop(I1, T).

/*
    drawBoard(+Board)

    Draws a board

    +Board : Board to draw
*/
drawBoard(Board) :- 
    drawHeader,
    drawRowLoop(10, Board),
    drawFooter,
    !.

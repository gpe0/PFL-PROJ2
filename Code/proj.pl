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

Stage:
    1 - Top
    2 - Mid
    2 - Bot

Default:
    1 - Empty
    2 - With dots
*/

%drawPlace(Piece, Stage, Default).

drawPlace(0, _, 1) :-
    write('       |').

drawPlace(0, _, 2) :-
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

drawLine(10, Pieces, 2, 1) :-
    write('10|'), 
    nth1(1, Pieces, P1), nth1(2, Pieces, P2),
    nth1(3, Pieces, P3), nth1(4, Pieces, P4),
    nth1(5, Pieces, P5),nth1(6, Pieces, P6),
    nth1(7, Pieces, P7), nth1(8, Pieces, P8),
    nth1(9, Pieces, P9), nth1(10, Pieces, P10),
    drawPlace(P1, 2, 1), drawPlace(P2, 2, 2), drawPlace(P3, 2, 1),
    drawPlace(P4, 2, 2), drawPlace(P5, 2, 1), drawPlace(P6, 2, 2),
    drawPlace(P7, 2, 1), drawPlace(P8, 2, 2), drawPlace(P9, 2, 1),
    drawPlace(P10, 2, 2),
    write('10'), nl.

drawLine(N, Pieces, 2, 1) :-
    write(N), write(' |'), 
    nth1(1, Pieces, P1), nth1(2, Pieces, P2),
    nth1(3, Pieces, P3), nth1(4, Pieces, P4),
    nth1(5, Pieces, P5),nth1(6, Pieces, P6),
    nth1(7, Pieces, P7), nth1(8, Pieces, P8),
    nth1(9, Pieces, P9), nth1(10, Pieces, P10),
    drawPlace(P1, 2, 1), drawPlace(P2, 2, 2), drawPlace(P3, 2, 1),
    drawPlace(P4, 2, 2), drawPlace(P5, 2, 1), drawPlace(P6, 2, 2),
    drawPlace(P7, 2, 1), drawPlace(P8, 2, 2), drawPlace(P9, 2, 1),
    drawPlace(P10, 2, 2),
    write(N), nl.

drawLine(10, Pieces, 2, 2) :-
    write('10|'), 
    nth1(1, Pieces, P1), nth1(2, Pieces, P2),
    nth1(3, Pieces, P3), nth1(4, Pieces, P4),
    nth1(5, Pieces, P5),nth1(6, Pieces, P6),
    nth1(7, Pieces, P7), nth1(8, Pieces, P8),
    nth1(9, Pieces, P9), nth1(10, Pieces, P10),
    drawPlace(P1, 2, 2), drawPlace(P2, 2, 1), drawPlace(P3, 2, 2),
    drawPlace(P4, 2, 1), drawPlace(P5, 2, 2), drawPlace(P6, 2, 1),
    drawPlace(P7, 2, 2), drawPlace(P8, 2, 1), drawPlace(P9, 2, 2),
    drawPlace(P10, 2, 1),
    write('10'), nl.

drawLine(N, Pieces, 2, 2) :-
    write(N), write(' |'), 
    nth1(1, Pieces, P1), nth1(2, Pieces, P2),
    nth1(3, Pieces, P3), nth1(4, Pieces, P4),
    nth1(5, Pieces, P5),nth1(6, Pieces, P6),
    nth1(7, Pieces, P7), nth1(8, Pieces, P8),
    nth1(9, Pieces, P9), nth1(10, Pieces, P10),
    drawPlace(P1, 2, 2), drawPlace(P2, 2, 1), drawPlace(P3, 2, 2),
    drawPlace(P4, 2, 1), drawPlace(P5, 2, 2), drawPlace(P6, 2, 1),
    drawPlace(P7, 2, 2), drawPlace(P8, 2, 1), drawPlace(P9, 2, 2),
    drawPlace(P10, 2, 1),
    write(N), nl.

drawLine(_, Pieces, Stage, 1) :-
    write('  |'),
    nth1(1, Pieces, P1), nth1(2, Pieces, P2),
    nth1(3, Pieces, P3), nth1(4, Pieces, P4),
    nth1(5, Pieces, P5),nth1(6, Pieces, P6),
    nth1(7, Pieces, P7), nth1(8, Pieces, P8),
    nth1(9, Pieces, P9), nth1(10, Pieces, P10),
    drawPlace(P1, Stage, 1), drawPlace(P2, Stage, 2), drawPlace(P3, Stage, 1),
    drawPlace(P4, Stage, 2), drawPlace(P5, Stage, 1), drawPlace(P6, Stage, 2),
    drawPlace(P7, Stage, 1), drawPlace(P8, Stage, 2), drawPlace(P9, Stage, 1),
    drawPlace(P10, Stage, 2), nl.

drawLine(_, Pieces, Stage, 2) :-
    write('  |'),
    nth1(1, Pieces, P1), nth1(2, Pieces, P2),
    nth1(3, Pieces, P3), nth1(4, Pieces, P4),
    nth1(5, Pieces, P5),nth1(6, Pieces, P6),
    nth1(7, Pieces, P7), nth1(8, Pieces, P8),
    nth1(9, Pieces, P9), nth1(10, Pieces, P10),
    drawPlace(P1, Stage, 2), drawPlace(P2, Stage, 1), drawPlace(P3, Stage, 2),
    drawPlace(P4, Stage, 1), drawPlace(P5, Stage, 2), drawPlace(P6, Stage, 1),
    drawPlace(P7, Stage, 2), drawPlace(P8, Stage, 1), drawPlace(P9, Stage, 2),
    drawPlace(P10, Stage, 1), nl.

drawNRow(1, Pieces, Default) :-
    nth1(1, Pieces, SetOfPieces),
    drawLine(1, SetOfPieces, 1, Default),
    drawLine(1, SetOfPieces, 2, Default),
    drawLine(1, SetOfPieces, 3, Default).

drawNRow(N, Pieces, Default) :-
    nth1(N, Pieces, SetOfPieces),
    drawLine(N, SetOfPieces, 1, Default),
    drawLine(N, SetOfPieces, 2, Default),
    drawLine(N, SetOfPieces, 3, Default),
    write('   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  '), nl.

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
    ], Pieces),
    use_module(library(lists)),
    drawHeader,
    drawNRow(10, Pieces, 1), drawNRow(9, Pieces, 2), drawNRow(8, Pieces, 1), 
    drawNRow(7, Pieces, 2), drawNRow(6, Pieces, 1), drawNRow(5, Pieces, 2),
    drawNRow(4, Pieces, 1), drawNRow(3, Pieces, 2), drawNRow(2, Pieces, 1), 
    drawNRow(1, Pieces, 2), 
    drawFooter.
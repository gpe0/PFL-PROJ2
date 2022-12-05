drawHeader :- 
    write('      A       B       C       D       E       F       G       H       I       J     '), nl,
    write('   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  '), nl.

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
drawPlace(0, _, 0) :- write('       |').
drawPlace(0, _, 1) :- write(' . . . |').

% Target
drawPlace(7, 2, _) :- write(' --|-- |').
drawPlace(7, _, _) :- write('   |   |').

% Elephant
drawPlace(1, 1, _) :- write('()o o()|').
drawPlace(1, _, _) :- write('  ( )  |').
drawPlace(4, 1, _) :- write('()o o()|').
drawPlace(4, _, _) :- write('  ( )  |').

% Mouse
drawPlace(2, 1, _) :- write('| o o ||').
drawPlace(2, 2, _) :- write(' \\   / |').
drawPlace(2, 3, _) :- write(' -\\_/- |').

drawPlace(5, 1, _) :- write('| o o ||').
drawPlace(5, 2, _) :- write(' \\   / |').
drawPlace(5, 3, _) :- write(' -\\_/- |').

% Lion
drawPlace(3, 1, _) :- write(' @@@@@ |').
drawPlace(3, 2, _) :- write('@o\\ /o@|').
drawPlace(3, 3, _) :- write(' @@@@@ |').

drawPlace(6, 1, _) :- write(' @@@@@ |').
drawPlace(6, 2, _) :- write('@o\\ /o@|').
drawPlace(6, 3, _) :- write(' @@@@@ |').

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
    format(' ~d|', [N]),
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
drawRowLoop(_, []).
drawRowLoop(Index, [H|T]) :-
    drawRow(Index, H),
    write('   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  '), nl,
    I1 is Index - 1,
    drawRowLoop(I1, T).

% drawBoard(Board)
drawBoard(Board) :- drawRowLoop(10, Board).

% For now, empty hard-coded board.
drawBoard :-
    append([], [
        [0, 0, 0, 0, 4, 4, 0, 0, 0, 0],
        [0, 0, 0, 6, 5, 5, 6, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 7, 0, 0, 7, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 7, 0, 0, 7, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 3, 2, 2, 3, 0, 0, 0],
        [0, 0, 0, 0, 1, 1, 0, 0, 0, 0]
    ], Board),
    drawHeader,
    drawBoard(Board), 
    drawFooter.

movePiece(Line1, Column1, Line2, Column2, Board, NewBoard) :-
    (Line1 = 4 ; Line1 = 7),
    (Column1 = 4 ; Column1 = 7),
    findPiece(Line1, Column1, Board, Piece),
    setPiece(Line1, Column1, Board, 7, TempBoard), % clear the previous space
    setPiece(Line2, Column2, TempBoard, Piece, NewBoard).

movePiece(Line1, Column1, Line2, Column2, Board, NewBoard) :-
    findPiece(Line1, Column1, Board, Piece),
    setPiece(Line1, Column1, Board, 0, TempBoard), % clear the previous space
    setPiece(Line2, Column2, TempBoard, Piece, NewBoard).    


findPiece(Line, Column, Board, Piece) :-
    use_module(library(lists)),
    nth1(Line, Board, BoardLine),
    nth1(Column, BoardLine, Piece).

setPiece(Line, Column, Board, Piece, BoardRes) :-
    use_module(library(lists)),
    nth1(Line, Board, BoardLine, RestBoard),
    nth1(Column, BoardLine, _, RestLine),
    nth1(Column, ModifiedLine, Piece, RestLine),
    nth1(Line, BoardRes, ModifiedLine, RestBoard).


    

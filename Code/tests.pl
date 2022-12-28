% =========================================================================
% BOARD
% =========================================================================

test_getPiece :-
    get_initial_board(B),
    getPiece(1, 1, B, 0),
    getPiece(5, 1, B, 4),
    getPiece(-5, -5, B, 99).

test_setPiece :-
    get_initial_board(B),
    getPiece(1, 1, B, 0),
    setPiece(1, 1, B, 5, NB),
    getPiece(1, 1, NB, 5).

% Visualization test (don't fail)
test_vis(X, Y, Piece) :-
    get_initial_board(B),
    visualize_moves(X, Y, Piece, B, 0, _).

% =========================================================================
% AI RANDOM BOT
% =========================================================================

test_validPieces :-
    get_initial_board(Board),
    validPieces(Board, 0, Pieces),
    sort(Pieces, [4-9-3, 5-9-2, 5-10-1, 6-9-2, 6-10-1, 7-9-3]).

test_2 :-
    board_stuck_bug(B),
    drawBoard(B),
    validPieces(B, 1, P),
    write(P).

getRandomPiece(Board, Player, X-Y-Piece) :-
    validPieces(Board, Player, ValidPieces),
    random_member(X-Y-Piece, ValidPieces).

getRandomMove(Board, Player, X-Y-Piece, XF-YF) :-
    getMoves(X, Y, Piece, Board, Player, ValidMoves),
    random_member(XF-YF, ValidMoves).

test_random_bot :-
    get_initial_board(Board),
    getRandomPiece(Board, 1, X-Y-Piece),
    getRandomMove(Board, 1, X-Y-Piece, XF-YF),
    write('Piece: '), write(Piece), nl,
    write('Current Position: '), write(X-Y), nl,
    write('Move Position: '), write(XF-YF).

% =========================================================================
% FEAR MECHANIC
% =========================================================================

test_scare :-
    board_scared_1(B),
    findScaredPieces(B, 0, Pieces),
    write(Pieces).

test_scare_mov :-
    board_scared_2(B),
    visualize_moves(5, 5, 1, B, 0, _).

test_fix_bug :-
    board_bug_scared(B),
    bots_loop(1, B).

test_scare_wikipedia_example :-
    board_scared_3(B),
    visualize_moves(5, 5, 1, B, 0, _).

% =========================================================================
% GAME OVER
% =========================================================================

% gameOver(+GameState, -Winner)
gameOver(Board, Winner) :-
    getTargetPieces(Board, Pieces),
    gameOverWinner(Pieces, Winner).

test_gameOver :-
    board_game_is_over1(B1),
    board_game_is_over2(B2),
    board_game_is_over3(B3),
    board_game_is_over4(B4),
    board_game_not_over1(B5),
    board_game_not_over2(B6),
    gameOver(B1, 1),
    gameOver(B2, 2),
    gameOver(B3, 2),
    gameOver(B4, 2),
    gameOver(B5, 3),
    gameOver(B6, 3).

% =========================================================================
% EVALUATE
% =========================================================================

ev_draw_value(Value) :-
    Value < 9,
    format('  ~d    |', [Value]).
ev_draw_value(Value) :-
    Value < 99,
    format('  ~d   |', [Value]).
ev_draw_value(Value) :-
    format(' ~d |', [Value]).

ev_drawLineLoop(_, [], _).
ev_drawLineLoop(Index, [H|T], 2) :-
    I1 is Index + 1,
    ev_draw_value(H),
    ev_drawLineLoop(I1, T, 2).
ev_drawLineLoop(Index, [_|T], Offset) :-
    I1 is Index + 1,
    write('       |'),
    ev_drawLineLoop(I1, T, Offset).

ev_drawLine(10, Y, 2) :-
    write('10|'),
    ev_drawLineLoop(0, Y, 2),
    write('10'), nl.

ev_drawLine(N, Y, 2) :-
    format(' ~d|', [N]),
    N1 is N mod 2,
    ev_drawLineLoop(N1, Y, 2),
    write(N), nl.

ev_drawLine(N, Y, Offset) :-
    write('  |'),
    N1 is N mod 2,
    ev_drawLineLoop(N1, Y, Offset), nl.

ev_drawRow(Index, Row) :-
    ev_drawLine(Index, Row, 1),
    ev_drawLine(Index, Row, 2),
    ev_drawLine(Index, Row, 3).

ev_drawRowLoop(_, []).
ev_drawRowLoop(Index, [H|T]) :-
    ev_drawRow(Index, H),
    write('   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  '), nl,
    I1 is Index - 1,
    ev_drawRowLoop(I1, T).

ev_get_value(X, Y, Piece, Value) :-
    evaluatePiece(Piece, X, Y, [0, 0, 0, 0], Value).

ev_get_row(11, _, _, []).
ev_get_row(X, Y, Piece, [Value|T]) :-
    ev_get_value(X, Y, Piece, Value),
    X1 is X + 1,
    ev_get_row(X1, Y, Piece, T).

ev_get_rows(11, _, []).
ev_get_rows(Y, Piece, [H|T]) :-
    ev_get_row(1, Y, Piece, H),
    Y1 is Y + 1,
    ev_get_rows(Y1, Piece, T).

ev_get_board(Piece, Res) :-
    ev_get_rows(1, Piece, Res).
    
test_evaluate_elephant :-
    ev_get_board(1, Board),
    drawHeader,
    ev_drawRowLoop(10, Board),
    drawFooter.

test_evaluate_mouse :-
    ev_get_board(2, Board),
    drawHeader,
    ev_drawRowLoop(10, Board),
    drawFooter.

test_evaluate_lion :-
    ev_get_board(3, Board),
    drawHeader,
    ev_drawRowLoop(10, Board),
    drawFooter.
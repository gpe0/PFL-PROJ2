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

% =========================================================================
% GAME OVER
% =========================================================================

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
    
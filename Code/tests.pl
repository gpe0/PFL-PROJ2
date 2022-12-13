% =========================================================================
% BOARD
% =========================================================================

test_vis(X, Y, Piece) :-
    get_initial_board(B),
    visualize_moves(X, Y, Piece, B, 0, _).

% =========================================================================
% AI RANDOM BOT
% =========================================================================

test :-
    get_initial_board(Board),
    getRandomPiece(Board, 1, X-Y-Piece),
    getRandomMove(Board, 1, X-Y-Piece, XF-YF),
    write('Piece: '), write(Piece), nl,
    write('Current Position: '), write(X-Y), nl,
    write('Move Position: '), write(XF-YF).

% =========================================================================
% AI BIG BRAIN BOT
% =========================================================================

test_all :-
    Player = 1,
    get_initial_board(Board),
    valid_pieces(Board, Player, ValidPieces),
    generateBoards(Board, Player, ValidPieces, [], NewBoards),
    evaluateBoards(NewBoards, Player, BoardsEvaluated),
    sort(BoardsEvaluated, SortedBoards),
    nth1(1, SortedBoards, V-_),
    getBestBoards(SortedBoards, V, BestBoards),
    random_member(MoveChosen, BestBoards),
    drawBoard(MoveChosen).

bots_main :-
    get_initial_board(Board),
    drawBoard(Board),
    bots_loop(0, Board).

bots_loop(_, Board) :-
    gameOver(Board, Winner),
    Winner < 3,
    format('WINNER IS ~d', [Winner]).
bots_loop(Player, Board) :-
    findScaredPieces(Board, Player, ScaredPieces),
    ScaredPieces  = [_|_],
    write('Has scared pieces.'), nl, write(ScaredPieces), nl,
    bots_loop_aux(Player, Board, ScaredPieces).
bots_loop(Player, Board) :-
    valid_pieces(Board, Player, Pieces),
    write('Free game.'), nl,
    bots_loop_aux(Player, Board, Pieces).

bots_loop_aux(Player, Board, Pieces) :-
    generateBoards(Board, Player, Pieces, [], NewBoards),
    evaluateBoards(NewBoards, Player, BoardsEvaluated),
    sort(BoardsEvaluated, SortedBoards),
    nth1(1, SortedBoards, V-_),
    getBestBoards(SortedBoards, V, BestBoards),
    random_member(MoveChosen, BestBoards),

    Other is 1 - Player,
    drawBoard(MoveChosen),
    bots_loop(Other, MoveChosen).

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
    
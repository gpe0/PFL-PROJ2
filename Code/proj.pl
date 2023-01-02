:- use_module(library(lists)).
:- use_module(library(random)).

:- ensure_loaded('boards.pl').
:- ensure_loaded('view.pl').
:- ensure_loaded('tests.pl').
:- ensure_loaded('stats.pl').
:- ensure_loaded('AI.pl').

% =========================================================================
% BASE FACTS
% =========================================================================

elephant(1).
elephant(4).
lion(3).
lion(6).
mouse(2).
mouse(5).
target(7).
empty(0).
empty(7).
empty(9).

/*
    playerPiece(+Piece, +Player)

    Checks if a Piece is from a player
*/
playerPiece(1, 0).
playerPiece(2, 0).
playerPiece(3, 0).
playerPiece(4, 1).
playerPiece(5, 1).
playerPiece(6, 1).

% =========================================================================
% BOARD
% =========================================================================

/*
    backtrackGetPiece(+X, +Y, +Board, -P)

    Gets a piece on a position of the board

    +X : X of position
    +Y : Y of position
    +Board : Current Board
    -P : Piece on the position
*/
backtrackGetPiece(X, Y, Board, P) :-
    nth1(Y, Board, Row),
    nth1(X, Row, P).

/*
    getPiece(+X, +Y, +Board, -P)

    Gets a piece on a position of the board
    !! This version prevents backtrack !!

    +X : X of position
    +Y : Y of position
    +Board : Current Board
    -P : Piece on the position
*/
getPiece(X, Y, Board, P) :-
    nth1(Y, Board, Row),
    nth1(X, Row, P), !.
getPiece(_, _, _, 99). % Fail prevention (AI)
 
/*
    setPiece(+X, +Y, +Board, +P, -NewBoard)

    Modifies a piece on the Board

    +X : X of position
    +Y : Y of position
    +Board : Current Board
    +P : Piece to set
    -NewBoard : Board modified
*/
setPiece(X, Y, Board, P, NewBoard) :-
    nth1(Y, Board, Row, RestBoard),
    nth1(X, Row, _, RestY),
    nth1(X, ModifiedY, P, RestY),
    nth1(Y, NewBoard, ModifiedY, RestBoard).

% =========================================================================
% MOVEMENT
% =========================================================================

/*
Warning:
Predicate valid_moves asked on project description not used!
Details in report

flatten([], []).
flatten([XI-YI-Moves|T2], Flat) :-
    flattenAux(XI-YI, Moves, Flat2),
    flatten(T2, Flat3),
    append(Flat2, Flat3, Flat).

flattenAux(_, [], []).
flattenAux(XI-YI, [XF-YF|T], [XI-YI-XF-YF|T2]) :-
    flattenAux(XI-YI, T, T2).

valid_moves(Board, Player, Moves) :-
    findall(X-Y-M, (backtrackGetPiece(X, Y, Board, Piece), playerPiece(Piece, Player), getMoves(X, Y, Piece, Board, Player, M)), MovesAux),
    flatten(MovesAux, Moves).

Test predicate on SICStus:

|: get_initial_board(B), valid_moves(B, 0, Moves).
|: get_initial_board(B), valid_moves(B, 0, Moves), nth1(1, Moves, M), move(B, M, NB), display_game(NB).
*/

/*
    move(+Board, +Move, -NewBoard)

    Performs a piece movement

    +Board : Current Board
    +Move : Move to perform
    -NewBoard : Modified Board
*/
move(Board, X-Y-XF-YF, NewBoard) :-
    getPiece(X, Y, Board, Piece),
    setPiece(X, Y, Board, 9, TempBoard),
    setPiece(XF, YF, TempBoard, Piece, NewBoard).    

/*
    getMoves(+X, +Y, +Piece, +Board, +Player, -Moves)

    Calculates the possible movements of a Piece

    +X : X of position
    +Y : Y of position
    +Piece : Piece in the position
    +Board : Current Board
    +Player : Player of the piece
    -Moves : Moves calculated
*/
getMoves(X, Y, P, Board, Player, Moves) :-
    mouse(P),
    expand_cross(X, Y, Board, Player, P, Moves),
    !.
getMoves(X, Y, P, Board, Player, Moves) :-
    lion(P),
    expand_diagonal(X, Y, Board, Player, P, Moves),
    !.
getMoves(X, Y, P, Board, Player, Moves) :-
    elephant(P),
    expand_cross(X, Y, Board, Player, P, Moves1),
    expand_diagonal(X, Y, Board, Player, P, Moves2),
    append(Moves1, T, Moves), T = Moves2,
    !.

/*
    expand_cross(+X, +Y, +Board, +Player, +P, -Moves)

    Expands the movement of a piece in 
    Vertical and Horizontal direction

    +X : X of position
    +Y : Y of position
    +Board : Current Board
    +P : Piece in the position
    -Moves : Moves calculated on that directions
*/
expand_cross(X, Y, Board, Player, P, Moves) :-
    expand(X, Y, -1, 0, Board, Player, P, Left),
    expand(X, Y, 1, 0, Board, Player, P, Right),
    expand(X, Y, 0, 1, Board, Player, P, Top),
    expand(X, Y, 0, -1, Board, Player, P, Down),
    append(Left, Right, Aux1),
    append(Aux1, Top, Aux2),
    append(Aux2, Down, Moves).

/*
    expand_diagonal(+X, +Y, +Board, +Player, +P, -Moves)

    Expands the movement of a piece in 
    Vertical and Horizontal direction

    +X : X of position
    +Y : Y of position
    +Board : Current Board
    +P : Piece in the position
    -Moves : Moves calculated on that directions
*/
expand_diagonal(X, Y, Board, Player, P, Moves) :-
    expand(X, Y, -1, -1, Board, Player, P, DL),
    expand(X, Y, 1, 1, Board, Player, P, TR),
    expand(X, Y, -1, 1, Board, Player, P, TL),
    expand(X, Y, 1, -1, Board, Player, P, DR),
    append(DL, TR, Aux1),
    append(Aux1, TL, Aux2),
    append(Aux2, DR, Moves).

/*
    expand(+X, +Y, +StepX, +StepY, +Board, +Player, +Piece, -Moves)

    Expands the movement of a piece in a defined direction (StepX, StepY)

    +X : X of position
    +Y : Y of position
    +StepX : X direction
    +StepY : Y direction
    +Board : Current Board
    +Player : Player of the piece
    +Piece : Piece in the position
    -Moves : Moves calculated on that directions
*/
expand(X, Y, StepX, StepY, Board, Player, Piece, Moves) :-
    X1 is X + StepX,
    Y1 is Y + StepY,
    expand_acc(X1, Y1, StepX, StepY, Board, Player, Piece, [], Moves).

/*
    expand_acc(+X, +Y, +StepX, +StepY, +Board, +Player, +Piece, +Acc, -Result)

    Expands the movement of a piece in a defined direction (StepX, StepY)
    Version with accumulator

    +X : X of position
    +Y : Y of position
    +StepX : X direction
    +StepY : Y direction
    +Board : Current Board
    +Player : Player of the piece
    +Piece : Piece in the position
    +Acc : Accumulator of movements
    -Result : Moves calculated
*/
expand_acc(0, _, _, _, _, _, _, Acc, Acc).
expand_acc(Width1, _, _, _, _, _, _, Acc, Acc):- boardWidth(Width), Width1 is Width + 1.
expand_acc(_, 0, _, _, _, _, _, Acc, Acc).
expand_acc(_, Height1, _, _, _, _, _, Acc, Acc):- boardHeight(Height), Height1 is Height + 1.
expand_acc(X, Y, _, _, Board, _, _, Acc, Acc) :-
    getPiece(X, Y, Board, Piece),
    \+empty(Piece).
expand_acc(X, Y, StepX, StepY, Board, Player, Piece, Acc, Result) :-
    isScared(X, Y, Board, Player, Piece),
    X1 is X + StepX,
    Y1 is Y + StepY,
    expand_acc(X1, Y1, StepX, StepY, Board, Player, Piece, Acc, Result).
expand_acc(X, Y, StepX, StepY, Board, Player, Piece, Acc, Result) :-
    A1 = [X-Y|Acc],
    X1 is X + StepX,
    Y1 is Y + StepY,
    expand_acc(X1, Y1, StepX, StepY, Board, Player, Piece, A1, Result).

/*
    visualize_moves(+X, +Y, +Piece, +Board, +Player, -Moves)

    Calculates the moves of a Piece and draws them in the board

    +X : X of position
    +Y : Y of position
    +Piece : Piece in the position
    +Board : Current Board
    +Player : Player of the piece
    -Moves : Moves calculated
*/
visualize_moves(X, Y, Piece, Board, Player, Moves) :-
    getMoves(X, Y, Piece, Board, Player, Moves),
    drawMoves(Board, Moves, NewBoard),
    display_game(NewBoard).

/*
    drawMoves(Board, Moves, NewBoard).  

    Modifies the board to show the possible movements

    +Board : Current Board
    +Moves : Possible Moves
    -NewBoard : Board modified
*/
drawMoves(Board, [], Board).
drawMoves(Board, [X-Y|T], NewBoard) :-
    setPiece(X,Y,Board,8,B1),
    drawMoves(B1,T,NewBoard).

% =========================================================================
% AI RANDOM BOT
% =========================================================================

/*
    validPieces(+Board, +Player, -Pieces)

    Calculates the valid pieces a player can move

    +Board : Current Board
    +Player : Player to play
    -Pieces : Pieces calculated
*/
validPieces(Board, Player, Pieces) :-
    findall(X-Y-P, validPiecesAux(X, Y, Board, Player, P), Pieces).

/*
    validPiecesAux(-X, -Y, +Board, +Player, -P)

    Auxiliar predicate to findall

    -X : X of position
    -Y : Y of position
    +Board : Current Board
    +Player : Player to play
    -P : Valid Piece
*/
validPiecesAux(X, Y, Board, Player, P) :-
    backtrackGetPiece(X, Y, Board, P),
    playerPiece(P, Player),
    getMoves(X, Y, P, Board, Player, [_|_]).

% =========================================================================
% AI GREEDY BOT
% =========================================================================

/*
    generateBoards(+Board, +Player, +ValidPieces, +Acc, -Boards)

    Generate all the possible boards with all possible movements

    +Board : Current Board
    +Player : Player playing
    +ValidPieces : Pieces to play
    +Acc : Acc of boards
    -Boards : Boards calculated
*/
generateBoards(_, _, [], Acc, Acc).
generateBoards(Board, Player, [X-Y-Piece|RestPieces], Acc, Boards) :-
    getMoves(X, Y, Piece, Board, Player, ValidMoves),
    generateBoardsAux(Board, X-Y-Piece, ValidMoves, [], NewBoards),
    append(Acc, NewBoards, Aux),
    generateBoards(Board, Player, RestPieces, Aux, Boards).

/*
    generateBoardsAux(Board, X-Y-Piece, Moves, Acc, Boards)

    Auxiliar predicate to generate all the possible boards 
    with all possible movements **of a certain piece**

    +Board : Current Board
    +X-Y-Piece : Piece to play
    +Moves : Moves remaining to play
    +Acc : Acc of boards
    -Boards : Boards calculated
*/
generateBoardsAux(_, _, [], Acc, Acc).
generateBoardsAux(Board, X-Y-Piece, [XF-YF|RestMoves], Acc, Boards) :-
    move(Board, X-Y-XF-YF, NewBoard),
    generateBoardsAux(Board, X-Y-Piece, RestMoves, [NewBoard | Acc], Boards).

/*
    value_simple(+Board, +Player, -Value).

    Evaluates a board (1st version created - Simple)
    (MAIN EVALUATE ON FILE AI.pl with the predicate name "value")

    +Board : Board to evaluate
    +Player : Player being evaluated
    -Value : Value calculated
*/
value_simple(Board, Player, Value) :-
    % Evaluate objectives
    getTargetPieces(Board, TargetPieces),
    getPlayerPoints(TargetPieces, Player, 0, Points),
    % Evaluate Other Player Pieces
    OtherPlayer is 1 - Player,
    findScaredPieces(Board, OtherPlayer, ScaredOtherPlayer),
    numScaredOnTarget(ScaredOtherPlayer, 0, ScaredOnTarget),
    length(ScaredOtherPlayer, NumScaredOtherPlayer),
    % Formula
    Value is -100 * Points - 50 * ScaredOnTarget - NumScaredOtherPlayer.

/*
    evaluateBoards(+Boards, +Player, -BoardsEvaluated).

    Receives a list of boards and evaluates them

    +Boards : Boards to evaluate
    +Player : Player being evaluated
    -BoardsEvaluated : Boards evaluated
*/
evaluateBoards([], _, []).
evaluateBoards([B|RestBoards], Player, [V-B|RT]) :-
    evaluationType(Player, 0),
    value_simple(B, Player, V),
    evaluateBoards(RestBoards, Player, RT).
evaluateBoards([B|RestBoards], Player, [V-B|RT]) :-
    evaluationType(Player, 1),
    value(B, Player, V),
    evaluateBoards(RestBoards, Player, RT).

/*
    getBestBoards(+Boards, +BestValue, -Result) :-

    Filters all the boards with the same value (best value)
    (Used to bot choose a random best move)

    +Boards : Boards calculated before
    +BestValue : Best value of the boards
    -Result : All boards with same best value
*/
getBestBoards([V-B|RestBoards], V, [B|T]) :-
    getBestBoards(RestBoards, V, T).
getBestBoards(_, _, []).

% =========================================================================
% FEAR MECHANIC
% =========================================================================

% Defines what pieces are scared of other pieces
scared(P1, P2) :- lion(P1), elephant(P2).
scared(P1, P2) :- elephant(P1), mouse(P2).
scared(P1, P2) :- mouse(P1), lion(P2).

/*
    findScaredPieces(+Board, +Player, -Pieces)

    Find the scared pieces, given a player

    +Board : Board Game
    +Player : Player
    -Pieces : Scared Pieces
*/
findScaredPieces(Board, Player, Pieces) :-
    findall(X-Y-P, findScaredPiecesAux(X, Y, Board, Player, P), Pieces).

/*
    findScaredPiecesAux(+X, +Y, +Board, +Player, -Pieces)

    Find the scared pieces, given a player (Aux function)
    
    +X : X Position
    +Y : Y Position
    +Board : Board Game
    +Player : Player
    -Pieces : Scared Pieces
*/
findScaredPiecesAux(X, Y, Board, Player, P) :-
    validPiecesAux(X, Y, Board, Player, P),
    isScared(X, Y, Board, Player, P).

/*
    adjacent(+X, +Y, -X1, -Y1)

    Find the adjacent squares

    +X : X Position
    +Y : Y Position
    -X1 : X Adjacent Position
    -Y1 : Y Adjacent Position
*/
adjacent(X, Y, X1, Y) :- X1 is X + 1.
adjacent(X, Y, X1, Y) :- X1 is X - 1.
adjacent(X, Y, X1, Y1) :- X1 is X + 1, Y1 is Y + 1.
adjacent(X, Y, X1, Y1) :- X1 is X + 1, Y1 is Y - 1.
adjacent(X, Y, X1, Y1) :- X1 is X - 1, Y1 is Y - 1.
adjacent(X, Y, X1, Y1) :- X1 is X - 1, Y1 is Y + 1.
adjacent(X, Y, X, Y1) :- Y1 is Y + 1.
adjacent(X, Y, X, Y1) :- Y1 is Y - 1.

/*
    isScared(+X, +Y, +Board, +Player, +P)

    Checks if a piece is scared

    +X : X Position
    +Y : Y Position
    +Board : Board Game
    +Player : Player
    -P : Piece
*/
isScared(X, Y, Board, Player, P) :-
    OtherPlayer is 1 - Player,
    adjacent(X, Y, X1, Y1),
    getPiece(X1, Y1, Board, NeighPiece),
    playerPiece(NeighPiece, OtherPlayer),
    scared(P, NeighPiece).

% =========================================================================
% GAME OVER
% =========================================================================

/*
    boolToInt(+Function, -Number)

    Converts a boolean to a number

    +Function : Evaluation Function
    -Number : Number (0 - False ; 1 - True)
*/
boolToInt(Pred, 1) :- call(Pred).
boolToInt(_, 0).

/*
    getPlayerPoints(+TargetPieces, +Player, +Accumulator, -Points)

    Gets the number of pieces in target positions

    +TargetPieces : Target Pieces
    +Player : Player
    +Accumulator : Accumulator
    -Points : Number of pieces in target positions
*/
getPlayerPoints([], _, Acc, Acc).
getPlayerPoints([H|T], Player, Acc, Points) :-
    boolToInt(playerPiece(H, Player), Point),
    Acc1 is Acc + Point,
    getPlayerPoints(T, Player, Acc1, Points).

/*
    getTargetPieces(+Board, +TargetPieces)

    Gets the pieces in target positions

    +Board : Board Game
    +TargetPieces : Target Pieces
*/
getTargetPieces(Board, [P1,P2,P3,P4]) :-
    boardWidth(Width),
    boardHeight(Height),
    X1 is div(Width, 2) - 1,
    Y1 is div(Height, 2) - 1,
    X2 is X1 + 3,
    Y2 is Y1 + 3,
    getPiece(X1, Y1, Board, P1),
    getPiece(X1, Y2, Board, P2),
    getPiece(X2, Y1, Board, P3),
    getPiece(X2, Y2, Board, P4).

/*
    game_over(-Winner)

    Finish the game

    -Winner : Winner
*/
game_over(99) :-
    retract(num_turn(100)), !.

game_over(Winner) :-
    retract(num_turn(N)),
    N1 is N + 1,
    asserta(num_turn(N1)),
    board(Board),
    getTargetPieces(Board, Pieces),
    gameOverWinner(Pieces, Winner).

/*
    gameOverWinner(+Pieces, -Winner)

    Gives the game winner

    +Pieces : Target Pieces
    -Winner : Winner
*/
gameOverWinner(Pieces, 1) :-
    getPlayerPoints(Pieces, 0, 0, Points),
    Points > 2,
    !.
gameOverWinner(Pieces, 2) :-
    getPlayerPoints(Pieces, 1, 0, Points),
    Points > 2,
    !.
gameOverWinner(_,3).

% =========================================================================
% INPUT
% =========================================================================

/*
    getBuffer(-Input)

    Fetches every character from the buffer

    +Input : User Input
*/
getBuffer([]) :- peek_code(10), get_code(10), !.
getBuffer([H|T]) :-
    get_code(H),
    getBuffer(T).

/*
    readNumber(+Input, -Number)

    Reads a number (user input)

    +Input : User Input
    -Number : Number
*/
readNumber(L, Res) :- readNumberAux(L, 0, Res).

/*
    readNumberAux(+Input, +Accumulator, -Number)

    Reads a number (Aux function)

    +Input : User Input
    +Accumulator : Accumulator
    -Number : Number
*/
readNumberAux([], Acc, Acc).
readNumberAux([C|T], Acc, Res):- 
    C >= 48,
    C =< 57,
    !,
    Acc1 is 10*Acc + (C - 48),
    readNumberAux(T, Acc1, Res).
readNumberAux(_, Acc, Acc).

/*
    letterToIndex(+Letter, -X)

    Transforms a letter into a number

    +Letter : Letter
    -X : Number
*/
letterToIndex(Letter, X) :-
    Letter > 64,
    Letter < 75,
    X is Letter - 64.

letterToIndex(Letter, X) :-
    Letter > 96,
    Letter < 107,
    X is Letter - 96.

/*
    writeStatus(+Mode)

    Displays a status message

    +Mode : Display message status
*/
writeStatus(0) :- write('[Player 1] Choose the piece to move:').
writeStatus(1) :- write('[Player 2] Choose the piece to move:').
writeStatus(2) :- write('[Player 1] Choose the piece destination:').
writeStatus(3) :- write('[Player 2] Choose the piece destination:').

/*
    getInput(+Mode, -X, -Y)

    Gets the user input

    +Mode : Display message status
    -X : X Position
    -Y : Y Position
*/
getInput(Mode, X, Y) :-
    writeStatus(Mode),
    getBuffer([Letter|Number]),
    readNumber(Number, YInput),
    letterToIndex(Letter, X),
    boardHeight(Height),
    Y is Height + 1 - YInput.

getInput(Mode, X, Y) :-
    write('error: Invalid input, try again!'), nl, nl,
    getInput(Mode, X, Y).

/*
    readPosition(-X, -Y, -Piece, +Board, +Player, +PossiblePieces)

    Gets the selected square position and piece

    -X : X Position
    -Y : Y Position
    -Piece : Piece in the square
    +Board : Board Game
    +Player : Player
    +PossiblePieces : Possible pieces the player can select
*/
readPosition(X, Y, Piece, Board, Player, PossiblePieces) :-
    getInput(Player, X, Y),
    getPiece(X, Y, Board, Piece),
    playerPiece(Piece, Player),
    member(X-Y-Piece, PossiblePieces).

/*
    readDestination(-X, -Y, +Moves, +Player)

    Gets the destination based on the moved piece

    -X : X Position
    -Y : Y Position
    +Moves : Possible moves
    +Player : Player
*/
readDestination(X, Y, Moves, Player) :-
    Mode is Player + 2,
    getInput(Mode, X, Y),
    member(X-Y, Moves).

/*
    parsePlayerType(+Input, -Option)

    Given input, calculates the player type selected by the user

    +Input : User input
    -Option : Option selected
*/
parsePlayerType(Input, Option) :-
    readNumber(Input, Option),
    Option > 0,
    Option < 5.
parsePlayerType(_, _) :-
    write('error: Invalid input, try again!'), nl,
    fail.

/*
    parseEvaluationType(+Input, -Option)

    Given input, calculates the evaluation type selected by the user

    +Input : User input
    -Option : Option selected
*/
parseEvaluationType(Input, Option) :-
    readNumber(Input, Option),
    Option > 0,
    Option < 3.
parseEvaluationType(_, _) :-
    write('error: Invalid input, try again!'), nl,
    fail.

% =========================================================================
% PLAY
% =========================================================================

:- dynamic playerType/2.
:- dynamic board/1.
:- dynamic playerTurn/1.
:- dynamic evaluationType/2.
:- dynamic num_turn/1.
:- dynamic targetPosition/2.

/**
 * Player Type
 * - 0 => Human
 * - 1 => Random Bot
 * - 2 => Greedy Bot
 * - 3 => MinMax Bot
 */

/*
    displayWinnerMessage(+Winner)

   Shows the game winner (or draw)

    +Winner : Winner
*/
displayWinnerMessage(99) :-
    board(FinalBoard),
    display_game(FinalBoard),
    write('============================'), nl,
    write('=          DRAW!           ='), nl,
    write('============================'), nl.

displayWinnerMessage(Winner) :-
    board(FinalBoard),
    display_game(FinalBoard),
    write('============================'), nl,
    format('=        PLAYER ~d          =', [Winner]), nl,
    write('=          WINS!           ='), nl,
    write('============================'), nl.

% Name the pieces
piece_name(P, 'Elephant') :- elephant(P).
piece_name(P, 'Lion') :- lion(P).
piece_name(P, 'Mouse') :- mouse(P).

/*
    log_pieces(+Board, +Pieces)

    Warns the player of possible scared pieces

    +Pieces : List of Pieces (X-Y-Piece)
*/
log_pieces([]).
log_pieces([X-Y-P|T]) :-
    piece_name(P, Name),
    boardHeight(Height),
    Y1 is Height + 1 - Y,
    Col is 64 + X,
    write('Position '),
    put_code(Col),
    format('~d : ~w', [Y1, Name]), nl,
    log_pieces_to_move(T).

/*
    warn_player(+Board, +Pieces)

    Warns the player of possible scared pieces

    +Board : Board Game
    +Pieces : Pieces
*/
warn_player(Player, Pieces) :-
    playerType(Player, 0), % Only if is human
    write('Warning: you have the following pieces scared'), nl,
    log_pieces(Pieces).
warn_player(_, _).

/*
    turn(+Board, +Player)

    Generates the movable pieces and makes a game turn

    +Board : Board Game
    +Player : Player
*/
% Test if player has scared pieces
% If yes, he needs to play them
turn(Board, Player) :-
    findScaredPieces(Board, Player, ScaredPieces),
    ScaredPieces  = [_|_],
    warn_player(Player, ScaredPieces),
    turn_action(Board, Player, ScaredPieces),
    !.
% Otherwise, play a normal turn
turn(Board, Player) :-
    validPieces(Board, Player, Pieces),
    turn_action(Board, Player, Pieces).

/*
    turn_action(+Board, +Player, +Pieces)

    makes a game turn based on the player type

    +Board : Board Game
    +Player : Player
    +Pieces : Pieces able to move
*/
% If Player is Human
turn_action(Board, Player, PiecesToMove) :-
    playerType(Player, 0),
    turn_human(Board, Player, PiecesToMove).

% If Player is Random
turn_action(Board, Player, PiecesToMove) :-
    playerType(Player, 1),
    choose_move(Board, Player, 1, PiecesToMove).

% If Player is Greedy
turn_action(Board, Player, PiecesToMove) :-
    playerType(Player, 2),
    choose_move(Board, Player, 2, PiecesToMove).

% If Player is Greedy (MinMax)
turn_action(Board, Player, PiecesToMove) :-
    playerType(Player, 3),
    choose_move(Board, Player, 3, PiecesToMove).

/*
    turn_human(+Board, +Player, +Pieces)

    Gets the user input to move a piece in the board

    +Board : Board Game
    +Player : Player
    +Pieces : Pieces able to move
*/
% Handle Human Turn
turn_human(Board, Player, PiecesToMove) :-
    readPosition(X, Y, Piece, Board, Player, PiecesToMove),
    visualize_moves(X, Y, Piece, Board, Player, Moves),
    readDestination(XF, YF, Moves, Player),
    move(Board, X-Y-XF-YF, NewBoard),
    !,
    setBoard(NewBoard).

/*
    choose_move(+Board, +Player, +BotType, +Pieces)

    Moves the best piece based on the bot type

    +Board : Board Game
    +Player : Player
    +BotType : Bot type (1 - random ; 2 - greedy ; 3 - minmax)
    +Pieces : Pieces able to move
*/
% Handle Random Turn
choose_move(Board, Player, 1, PiecesToMove) :-
    random_member(X-Y-Piece, PiecesToMove),
    getMoves(X, Y, Piece, Board, Player, ValidMoves),
    random_member(XF-YF, ValidMoves),
    move(Board, X-Y-XF-YF, NewBoard),
    !,
    setBoard(NewBoard).

% Handle Greedy Turn
choose_move(Board, Player, 2, PiecesToMove) :-
    generateBoards(Board, Player, PiecesToMove, [], NewBoards),
    evaluateBoards(NewBoards, Player, BoardsEvaluated),
    sort(BoardsEvaluated, SortedBoards),
    nth1(1, SortedBoards, V-_),
    getBestBoards(SortedBoards, V, BestBoards),
    !,
    random_member(MoveChosen, BestBoards),
    setBoard(MoveChosen).

% Handle MinMax Turn
choose_move(Board, Player, 3, PiecesToMove) :-
    retractall(moveBoard(_, _)),
    asserta(moveBoard(-100000, [])),
    maxValue(Board, Player, 0, -100000, 100000, PiecesToMove, _),
    moveBoard(_, New),
    setBoard(New),
    retractall(moveBoard(_, _)), !.

% Menu header
displayInitalMessage :-
    write('===== Barca Board Game ====='), nl,
    write('          PFL 22/23         '), nl,
    write('============================'), nl, nl.

/*
    displayPlayerTypes(+Player)

    Displays the player types available

    +Player : Player
*/
displayPlayerTypes(Player) :-
    Aux is Player + 1,
    format('       PLAYER ~d TYPE        ', [Aux]), nl,
    write('1. Human'), nl,
    write('2. Random'), nl,
    write('3. Greedy'), nl,
    write('4. MinMax'), nl.

/*
    displayEvaluationTypes(+Player)

    Displays the evaluation types available

    +Player : Player
*/
displayEvaluationTypes(Player) :-
    Aux is Player + 1,
    format('       PLAYER ~d EVALUATION TYPE        ', [Aux]), nl,
    write('1. Simple'), nl,
    write('2. Complex'), nl.

/*
    getPlayerType(+Player)

    Gets the player type

    +Player : Player
*/
getPlayerType(Player) :-
    getBuffer(Input),
    parsePlayerType(Input, Option),
    Option1 is Option - 1,
    retractall(playerType(Player, _)),
    asserta(playerType(Player, Option1)).

/*
    readPlayerType(+Player)

    Error proof loop to get the player type

    +Player : Player
*/
readPlayerType(Player) :-
    repeat,
    displayPlayerTypes(Player),
    getPlayerType(Player).

/*
    getEvaluationType(+Player)

    Gets the evaluation type

    +Player : Player
*/
getEvaluationType(Player) :-
    boardHeight(10),
    boardWidth(10),
    getBuffer(Input),
    parseEvaluationType(Input, Option),
    Option1 is Option - 1,
    retractall(evaluationType(Player, _)),
    asserta(evaluationType(Player, Option1)).

/*
    readEvaluationType(+Player)

    Error proof loop to get the evaluation type

    +Player : Player
*/
readEvaluationType(Player) :-
    boardPreference(2),
    retractall(evaluationType(Player, _)),
    asserta(evaluationType(Player, 0)).

readEvaluationType(Player) :-
    playerType(Player, Type),
    Type > 1,
    repeat,
    displayEvaluationTypes(Player),
    getEvaluationType(Player).

readEvaluationType(_).

% Even number definition
even(N):- mod(N,2) =:= 0.

%Sets up the game board
setupBoard :-
    readBoardPreference,
    setupCustomBoard,
    readTargetPreference,
    setupTargetPositions.

% Error proof loop to get the target preference
readTargetPreference :-
    boardPreference(1),
    retractall(targetDistance(_)),
    asserta(targetDistance(1)).

readTargetPreference :-
    boardPreference(2),
    repeat,
    displayTargetPreferences,
    getTargetPreference.

% Displays the options available to the target preference
displayTargetPreferences :-
    write('       TARGET POSITION PREFERENCE        '), nl,
    write('1. Default'), nl,
    write('2. Custom'), nl.

%Gets the target preference
getTargetPreference :-
    getBuffer(Input),
    parseTargetPreference(Input, Option),
    retractall(targetDistance(_)),
    asserta(targetDistance(Option)).

/*
    parseTargetPreference(+Input, -Option)

    Given input, calculates the target preference selected by the user

    +Input : User input
    -Option : Option selected
*/
parseTargetPreference(Input, Option) :-
    readNumber(Input, Option),
    Option > 0,
    Option < 3.
parseTargetPreference(_, _) :-
    write('error: Invalid input, try again!'), nl,
    fail.

% Sets up the target positions
setupTargetPositions :-
    boardPreference(1),
    fillBoard.

setupTargetPositions :-
    targetDistance(1),
    fillBoard.

setupTargetPositions :-
    getTargetDistanceFromDimensions(Distance),
    readTargetDistance(Distance),
    fillBoard.

% Fills the board game
fillBoard :-
    board(Board),
    boardWidth(Width),
    boardHeight(Height),
    targetDistance(Distance),    
    setTargetPositionLoop(Distance, Height, Width, Board, NewBoard),
    setDefaultPieces(NewBoard, NewerBoard),
    setBoard(NewerBoard).

/*
    setTargetPositionLoop(+Distance, +Height, +Width, +Board, -NewBoard)

    sets all the targets in the board

    +Distance : Distance to target
    +Height : Board's Height
    +Width : Board's Width
    +Board : Board Game
    -NewBoard : Board Game with the new target
*/
setTargetPositionLoop(Distance, Height, Width, Board, NewBoard) :-
    X is div(Width, 2) - Distance,
    Y is div(Height, 2) - Distance,
    retractall(targetPosition(_,_)),
    setTargetPosition(X,Y, Board, TempBoard1),
    X1 is X + 1 + 2 * Distance,
    setTargetPosition(X1,Y, TempBoard1, TempBoard2),
    Y1 is Y + 1 + 2 * Distance,
    setTargetPosition(X,Y1, TempBoard2, TempBoard3),
    setTargetPosition(X1,Y1, TempBoard3, NewBoard).

/*
    setTargetPosition(+X, +Y, +Board, -NewBoard)

    sets the target position in a specific position of the board

    +X : X Position
    +Y : Y Position
    +Board : Board Game
    -NewBoard : Board Game with the new target
*/
setTargetPosition(X, Y, Board, NewBoard) :-
    nth1(Y, Board, Row, RestBoard),
    nth1(X, Row, _, RestRow),
    nth1(X, ModifiedRow, 7, RestRow),
    nth1(Y, NewBoard, ModifiedRow, RestBoard),
    asserta(targetPosition(X, Y)).

/*
    readTargetDistance(-Distance)

    Calculates the distance to target having board dimensions

    -Distance : Distance to target
*/
getTargetDistanceFromDimensions(Distance) :-
    boardHeight(Height),
    boardWidth(Width),
    Width < Height,
    Distance is div(Width, 2) - 3.

getTargetDistanceFromDimensions(Distance) :-
    boardHeight(Height),
    Distance is div(Height, 2) - 3.

/*
    readTargetDistance(+Distance)

    Error proof loop to get the target distance

    +Distance : Distance to target
*/
readTargetDistance(Distance) :-
    repeat,
    displayTargetDistances(Distance),
    getTargetDistance(Distance).

/*
    displayTargetDistances(+Distance)

    Display useful information for the distance target selection

    +Distance : Distance to target
*/
displayTargetDistances(Distance) :-
    write('       TARGET POSITIONS        '), nl,
    write('Please choose the distance from the target positions to the center (diagonally)'), nl,
    write('Note that the distance must be between 1 and '),
    write(Distance), nl.

/*
    getTargetDistance(+Distance)

    Gets the target distance

    +Distance : Distance to target
*/
getTargetDistance(Distance) :-
    getBuffer(Input),
    parseTargetDistance(Input, Option, Distance),
    retractall(targetDistance(_)),
    asserta(targetDistance(Option)).

/*
    parseTargetDistance(+Input, -Option, +Distance)

    Given input and a distance, calculates the target distance selected by the user

    +Input : User input
    -Option : Option selected
    +Distance : Distance to target
*/
parseTargetDistance(Input, Option, Distance) :-
    readNumber(Input, Option),
    Option > 0,
    Option < Distance + 1.

parseTargetDistance(_, _, _) :-
    write('error: Invalid input, try again!'), nl,
    fail.

% Error proof loop to get the board preference
readBoardPreference :-
    repeat,
    displayBoardPreferences,
    getBoardPreference.

% Displays the possible board preferences
displayBoardPreferences :-
    write('       BOARD PREFERENCE        '), nl,
    write('1. Default'), nl,
    write('2. Custom'), nl,
    write('Note that selecting this option will prevent you from using complex evaluation'), nl.

% Gets the board preference (user input)
getBoardPreference :-
    getBuffer(Input),
    parseBoardPreference(Input, Option),
    retractall(boardPreference(_)),
    asserta(boardPreference(Option)).

/*
    parseBoardDimensions(+Input, -Option)

    Given input, calculates the board preference selected by the user

    +Input : User input
    -Option : Option selected
*/
parseBoardPreference(Input, Option) :-
    readNumber(Input, Option),
    Option > 0,
    Option < 3.
parseBoardPreference(_, _) :-
    write('error: Invalid input, try again!'), nl,
    fail.

% Sets up the game board
setupCustomBoard :-
    boardPreference(1),
    retractall(boardHeight(_)),
    asserta(boardHeight(10)),
    retractall(boardWidth(_)),
    asserta(boardWidth(10)),
    setupEmptyBoard.


setupCustomBoard :-
    boardPreference(2),
    retractall(evaluationType(Player, _)),
    asserta(evaluationType(Player, 0)),
    readBoardDimensions,
    setupEmptyBoard.

% Sets up an empty board
setupEmptyBoard :-
    boardWidth(Width),
    boardHeight(Height),
    length(Row, Width),
    maplist(=(0), Row),
    length(Board, Height),
    maplist(=(Row), Board),
    setBoard(Board).

/*
    setDefaultPieces(+Board, -NewBoard)

    sets the pieces in the default positions (10x10 board)

    +Board : Empty Board
    -NewBoard : Game Board
*/
setDefaultPieces(Board, NewBoard) :-
    boardHeight(Height),
    boardWidth(Width),
    X2 is div(Width, 2),
    X1 is X2 - 1,
    X3 is X2 + 1,
    X4 is X3 + 1,
    Y1 is Height - 1,
    setPiece(X2, Height, Board, 4, TempBoard1),
    setPiece(X3, Height, TempBoard1, 4, TempBoard2),
    setPiece(X1, Y1, TempBoard2, 6, TempBoard3),
    setPiece(X2, Y1, TempBoard3, 5, TempBoard4),
    setPiece(X3, Y1, TempBoard4, 5, TempBoard5),
    setPiece(X4, Y1, TempBoard5, 6, TempBoard6),
    setPiece(X2, 1, TempBoard6, 1, TempBoard7),
    setPiece(X3, 1, TempBoard7, 1, TempBoard8),
    setPiece(X1, 2, TempBoard8, 3, TempBoard9),
    setPiece(X2, 2, TempBoard9, 2, TempBoard10),
    setPiece(X3, 2, TempBoard10, 2, TempBoard11),
    setPiece(X4, 2, TempBoard11, 3, NewBoard).

% Gets board dimensions
readBoardDimensions :-
    readBoardWidth,
    readBoardHeight.

% Error proof loop to get the board dimensions form the user
readBoardWidth :-
    repeat,
    displayBoardWidth,
    getBoardWidth.

readBoardHeight :-
    repeat,
    displayBoardHeight,
    getBoardHeight.

% Displays some information to guide the user when choosing board dimensions
displayBoardWidth :-
    write('       BOARD DIMENSIONS        '), nl,
    write('Please choose a width for your board'), nl,
    write('Note that the width must be an even number between 8 and 26'), nl.

displayBoardHeight :-
    write('Please choose a height for your board'), nl,
    write('Note that the height must be an even number between 8 and 26'), nl.

% Gets the board dimensions
getBoardWidth :-
    getBuffer(Input),
    parseBoardDimensions(Input, Option),
    retractall(boardWidth(_)),
    asserta(boardWidth(Option)).

getBoardHeight :-
    getBuffer(Input),
    parseBoardDimensions(Input, Option),
    retractall(boardHeight(_)),
    asserta(boardHeight(Option)).

/*
    parseBoardDimensions(+Input, -Option)

    Given input, calculates the board dimension selected by the user

    +Input : User input
    -Option : Option selected
*/
parseBoardDimensions(Input, Option) :-
    readNumber(Input, Option),
    even(Option),
    Option > 7,
    Option < 27.
parseBoardDimensions(_, _) :-
    write('error: Invalid input, try again!'), nl,
    fail.

% Menu to the the game settings
menu :-
    displayInitalMessage,
    setupBoard,
    readPlayerType(0),
    readEvaluationType(0),
    readPlayerType(1),
    readEvaluationType(1).

% Player changes turns with the other player
switchPlayer :-
    playerTurn(0),
    retractall(playerTurn(_)),
    asserta(playerTurn(1)),
    !. 
switchPlayer :-
    playerTurn(1),
    retractall(playerTurn(_)),
    asserta(playerTurn(0)),
    !.
switchPlayer :-
    asserta(playerTurn(0)).

/*
    setBoard(+Board)

    Dynamically sets the board

    +Board : Board Game
*/
setBoard(Board) :-
    retractall(board(_)),
    asserta(board(Board)).

/*
    removeLastPosition(+Board, -Result)

    Resets the origin position after a move

    +X : X Position
    +Y : Y Position
    +Board : Board Game
    -Result : Board Reseted
*/
removeLastPosition(Board, Res) :-
    backtrackGetPiece(X, Y, Board, 9),
    !,
    resetPosition(X, Y, Board, Res).
removeLastPosition(Board, Board).

/*
    resetPosition(+X, +Y, +Board, -Result)

    Resets the origin position after a move

    +X : X Position
    +Y : Y Position
    +Board : Board Game
    -Result : Board Reseted
*/
resetPosition(X, Y, Board, Res) :-
    targetPosition(X, Y),
    setPiece(X, Y, Board, 7, Res),
    !.
resetPosition(X, Y, Board, Res) :-
    setPiece(X, Y, Board, 0, Res).

% Game loop
startGame :-
    switchPlayer, % Will set player 0 turn
    repeat,
    board(B),
    display_game(B),
    playerTurn(Player),
    removeLastPosition(B, Board),
    turn(Board, Player),
    switchPlayer,
    game_over(Winner),
    isGameOver(Winner),
    displayWinnerMessage(Winner).

isGameOver(1).
isGameOver(2).
isGameOver(99).

% Game start point (displays a menu then starts the game)
play :-
    menu,
    retractall(playerTurn(_)),
    retractall(num_turn(_)),
    asserta(num_turn(0)),
    startGame.
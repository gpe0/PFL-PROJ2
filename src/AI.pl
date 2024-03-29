/*
    Calculate the quadrant of a position
*/

firstQuadrant(X, Y) :- Y < 6, X > 5.
secondQuadrant(X, Y) :- Y < 6, X < 6.
thirdQuadrant(X, Y) :- Y > 5, X < 6.
fourthQuadrant(X, Y) :- Y > 5, X > 5.


/*
    evaluatePiece(+Piece, +X, +Y, +Goals, -Points)

    Calculates the points for a specific piece in a specific position in the board

    +Piece : Given Piece
    +X : X of position
    +Y : Y of position
    +Goals : List of the Pieces in the targets
    -Points : Points evaluated
*/

/*
    If the position is a target the value is determined.
*/
evaluatePiece(Piece, X, Y, _, 10000) :- mouse(Piece), targetPosition(X, Y).
evaluatePiece(Piece, X, Y, _, 10000) :- lion(Piece), targetPosition(X, Y).
evaluatePiece(Piece, X, Y, _, 10000) :- elephant(Piece), targetPosition(X, Y).

/*
    Depending on the quadrant, evaluate the value on the corresponding target
*/
evaluatePiece(Piece, X, Y, [_, Target, _, _], Value) :-
    firstQuadrant(X, Y),
    evaluatePieceAux(Piece, X, Y, Target, Value).
evaluatePiece(Piece, X, Y, [Target, _, _, _], Value) :-
    secondQuadrant(X, Y),
    evaluatePieceAux(Piece, X, Y, Target, Value).
evaluatePiece(Piece, X, Y, [_, _, Target, _], Value) :-
    thirdQuadrant(X, Y),
    evaluatePieceAux(Piece, X, Y, Target, Value).
evaluatePiece(Piece, X, Y, [_, _, _, Target], Value) :-
    fourthQuadrant(X, Y),
    evaluatePieceAux(Piece, X, Y, Target, Value).

/*
    If the target of the quadrant is empty,
    all the positions on the quadrant value double.
*/  
evaluatePieceAux(Piece, X, Y, Target, Value) :-
    empty(Target),
    getPieceValue(Piece, X, Y, 2, Value).
evaluatePieceAux(Piece, X, Y, _, Value) :-
    getPieceValue(Piece, X, Y, 1, Value).

/*
    getPieceValue(+Piece, +X, +Y, +Modifier, -Value)

    Calculates the points for a specific piece in a specific position in the board (Aux Function)

    +Piece : Given Piece
    +X : X of position
    +Y : Y of position
    +Modifier : Multiplier
    -Points : Points evaluated
*/
/*
    Redirects the piece value calculation depending
    on the type, also receives the modifier calculated
    by evaluatePieceAux
*/
getPieceValue(Piece, X, Y, Modifier, Value) :-
    mouse(Piece),
    evaluateMouse(X, Y, Modifier, Value).
getPieceValue(Piece, X, Y, Modifier, Value) :-
    lion(Piece),
    evaluateLion(X, Y, Modifier, Value).
getPieceValue(Piece, X, Y, Modifier, Value) :-
    elephant(Piece),
    evaluateElephant(X, Y, Modifier, Value).
getPieceValue(_, _, _, _, 0).

% =========================================================================
% Evaluate the position of a ELEPHANT
% =========================================================================

/*
    evaluateElephant(+X, +Y, +Modifier, -Value)

    Calculates the points for an elephant piece in a specific position in the board (Aux Function)

    +X : X of position
    +Y : Y of position
    +Modifier : Multiplier
    -Points : Points evaluated
*/
/*
    RED ZONE
    - CENTER OF THE BOARD
*/
evaluateElephant(X, Y, Modifier, Value) :-
    X > 2, X < 9,
    Y > 2, Y < 9,
    Value is Modifier * 20.

/*
    ORANGE ZONE
    - CAN MOVE TO TARGET
*/
evaluateElephant(_, 4, Modifier, Value) :- Value is Modifier * 15.
evaluateElephant(_, 7, Modifier, Value) :- Value is Modifier * 15.
evaluateElephant(X, X, Modifier, Value) :- Value is Modifier * 15.
evaluateElephant(X, Y, Modifier, Value) :- Y is 11 - X, Value is Modifier * 15.

/*
    YELLOW ZONE
    - CAN MOVE TO CENTER
*/
evaluateElephant(4, _, Modifier, Value) :- Value is Modifier * 10.
evaluateElephant(7, _, Modifier, Value) :- Value is Modifier * 10.
evaluateElephant(5, _, Modifier, Value) :- Value is Modifier * 10.
evaluateElephant(6, _, Modifier, Value) :- Value is Modifier * 10.
evaluateElephant(_, 5, Modifier, Value) :- Value is Modifier * 10.
evaluateElephant(_, 6, Modifier, Value) :- Value is Modifier * 10.

/* 
    GREEN ZONE
    - REST OF POSITIONS (BAD ONES)
*/
evaluateElephant(_, _, Modifier, Value) :- Value is Modifier * 5.

% =========================================================================
% Evaluate the position of a MOUSE
% =========================================================================

/*
    evaluateMouse(+X, +Y, +Modifier, -Value)

    Calculates the points for a mouse piece in a specific position in the board (Aux Function)

    +X : X of position
    +Y : Y of position
    +Modifier : Multiplier
    -Points : Points evaluated
*/

/*
    RED ZONE
    - CAN MOVE TO TARGET
*/
evaluateMouse(4, _, Modifier, Value) :- Value is Modifier * 20.
evaluateMouse(7, _, Modifier, Value) :- Value is Modifier * 20.
evaluateMouse(_, 4, Modifier, Value) :- Value is Modifier * 20.
evaluateMouse(_, 7, Modifier, Value) :- Value is Modifier * 20.

/*
    ORANGE ZONE
    - CENTER OF THE BOARD
*/
evaluateMouse(X, Y, Modifier, Value) :-
    X > 4, X < 7,
    Y > 4, Y < 7,
    Value is Modifier * 15.

/*
    YELLOW ZONE
    - CAN MOVE TO CENTER
*/
evaluateMouse(X, _, Modifier, Value) :- 
    X > 4, X < 7,
    Value is Modifier * 10.
evaluateMouse(_, Y, Modifier, Value) :- 
    Y > 4, Y < 7,
    Value is Modifier * 10.

/* 
    GREEN ZONE
    - REST OF POSITIONS (BAD ONES)
*/
evaluateMouse(_, _, Modifier, Value) :- Value is Modifier * 5.

% =========================================================================
% Evaluate the position of a LION
% =========================================================================

/*
    evaluateMouse(+X, +Y, +Modifier, -Value)

    Calculates the points for a lion piece in a specific position in the board (Aux Function)

    +X : X of position
    +Y : Y of position
    +Modifier : Multiplier
    -Points : Points evaluated
*/

/*
    RED ZONE
    - CAN MOVE TO TARGET
*/
evaluateLion(X, X, Modifier, Value) :- Value is Modifier * 20.
evaluateLion(X, Y, Modifier, Value) :- Y is 11 - X, Value is Modifier * 20.
evaluateLion(X, Y, Modifier, Value) :- Y > 2, Y is X - 3, Value is Modifier * 20.
evaluateLion(X, Y, Modifier, Value) :- Y > 2, Y is 8 - X, Value is Modifier * 20.
evaluateLion(X, Y, Modifier, Value) :- Y < 9, Y is X + 3, Value is Modifier * 20.
evaluateLion(X, Y, Modifier, Value) :- Y < 9, Y is 14 - X, Value is Modifier * 20.

/*
    ORANGE ZONE
    - CENTER OF THE BOARD
*/
evaluateLion(X, Y, Modifier, Value) :-
    X > 4, X < 7,
    Y > 4, Y < 7,
    Value is Modifier * 15.

/*
    GREEN ZONE
    - REST OF POSITIONS (BAD ONES)
*/
evaluateLion(_, _, Modifier, Value) :- Value is Modifier * 5.

/*
    numScaredOnTarget(+Pieces, +Accumulator, -Result)

    Calculates the number of scared pieces on the targets

    +Pieces : List of X-Y-Piece of every piece in the targets
    +Accumulator : Accumulator
    -Result : Number of scared pieces
*/
numScaredOnTarget([], Acc, Acc).
numScaredOnTarget([X-Y-_|Rest], Acc, Res) :-
    targetPosition(X, Y),
    Acc1 is Acc + 1,
    numScaredOnTarget(Rest, Acc1, Res).
numScaredOnTarget([_|Rest], Acc, Res) :- numScaredOnTarget(Rest, Acc, Res).

/*
    value(+Board, +Player, -Value)

    Evaluates the points for a specific board (square patterns evaluation)

    +Board : Game board
    +Player : Player to be evaluated
    -Value : Points
*/
value(Board, Player, Value) :-
    getTargetPieces(Board, Goals),
    getBoardPoints(Board, 1, Player, Goals, 0, Points),
    OtherPlayer is 1 - Player,
    findScaredPieces(Board, OtherPlayer, ScaredOtherPlayer),
    numScaredOnTarget(ScaredOtherPlayer, 0, ScaredOnTarget),
    length(ScaredOtherPlayer, NumScaredOtherPlayer),
    % Formula
    Value is -1 * Points - 5000 * ScaredOnTarget - 25 * NumScaredOtherPlayer.

/*
    getBoardPoints(+Row, +Y, +Player, +Accumulator, -Value)

    Gets the points for a specific row in the board

    +Row : Board row
    +Y : Current Y position
    +Player : Player
    +Accumulator : Accumulator
    -Value : Points
*/
getBoardPoints([], _, _, _, Acc, Acc).
getBoardPoints([Row|Rest], Y, Player, Goals, Acc, Points) :-
    getRowPoints(Row, 1, Y, Player, Goals, 0, RowPoints),
    Acc1 is Acc + RowPoints,
    Y1 is Y + 1,
    getBoardPoints(Rest, Y1, Player, Goals, Acc1, Points).

getRowPoints([], _, _, _, _, Acc, Acc).
getRowPoints([Piece|Rest], X, Y, Player, Goals, Acc, Points) :-
    playerPiece(Piece, Player),
    evaluatePiece(Piece, X, Y, Goals, Val),
    Acc1 is Acc + Val,
    X1 is X + 1,
    getRowPoints(Rest, X1, Y, Player, Goals, Acc1, Points).
getRowPoints([_|Rest], X, Y, Player, Goals, Acc, Points) :-
    X1 is X + 1,
    getRowPoints(Rest, X1, Y, Player, Goals, Acc, Points).

/*
    evaluateValue(+Board, +Player, -Value)

    Chooses the evaluation method based on user input

    +Board : Game board
    +Player : Player to be evaluated
    -Value : Points
*/
evaluateValue(Board, Player, Value) :-
    evaluationType(Player, 0),
    value_simple(Board, Player, V),
    Value is V * -1, !.

evaluateValue(Board, Player, Value) :-
    evaluationType(Player, 1),
    value(Board, Player, V),
    Value is V * -1, !.


/*
    getPiecesToPlay(+Board, +Player, -Pieces)

    Calculates the pieces that a player can play

    +Board : Game board
    +Player : Player
    -Pieces : Pieces able to play
*/
% Test if player has scared pieces
% If yes, he needs to play them
getPiecesToPlay(Board, Player, ScaredPieces) :-
    findScaredPieces(Board, Player, ScaredPieces),
    ScaredPieces  = [_|_],
    !.
% Otherwise, play a normal turn
getPiecesToPlay(Board, Player, Pieces) :-
    validPieces(Board, Player, Pieces).

/*
    maxValue(+Board, +Player, +Depth, +A, +B, +Pieces, -Value)

    Calculates the best move based on a board for a specific player, given a specific depth

    +Board : Game board
    +Player : Player to be evaluated
    +Depth : Current depth of the computation
    +A : Alpha Value
    +B : Beta Value
    +Pieces : Pieces that make a move
    -Value : Points
*/
maxValue(Board, Player, Depth, A, B, Pieces, Value) :-
    Depth1 is Depth + 1,
    generateBoards(Board, Player, Pieces, [], NextBoards),
    random_permutation(NextBoards, ShuffledBoards),
    V = -100000,
    maxValueAux(ShuffledBoards, Player, Depth1, A, B, V, Value), !.

/*
    minValue(+Board, +Player, +Depth, +A, +B, +Pieces, -Value)

    Calculates the best move based on a board for a specific player, given a specific depth

    +Board : Game board
    +Player : Player to be evaluated
    +Depth : Current depth of the computation
    +A : Alpha Value
    +B : Beta Value
    +Pieces : Pieces that make a move
    -Value : Points
*/
minValue(Board, Player, Depth, A, B, Value) :-
    Depth1 is Depth + 1,
    getPiecesToPlay(Board, Player, Pieces),
    generateBoards(Board, Player, Pieces, [], NextBoards),
    V = 100000,
    minValueAux(NextBoards, Player, Depth1, A, B, V, Value),
    updateMoveBoard(Value, Board), !.

/*
    maxValueAux(+Board, +Player, +Depth, +A, +B, +CurrentValue, -BestValue)

    Calculates the best move based on a board for a specific player, given a specific depth (Aux function)

    +Board : Game board
    +Player : Player to be evaluated
    +Depth : Current depth of the computation
    +A : Alpha Value
    +B : Beta Value
    +CurrentValue : Best value obtained so far
    -BestValue : Best possible Points
*/
maxValueAux([], _, _, _, _, Value, Value):- !.

maxValueAux([Board|Rest], Player, 3, A, B, CurrentValue, NewValue) :-
    evaluateValue(Board, Player, V1),
    max(CurrentValue, V1, BestValue),
    V1 < B,
    max(A, V1, NewA),
    maxValueAux(Rest, Player, 3, NewA, B, BestValue, NewValue), !.

maxValueAux([Board|_], Player, Depth, _, _, CurrentValue, CurrentValue) :-
    Depth \= 3,
    getTargetPieces(Board, Pieces),
    gameOverWinner(Pieces, Winner),
    W is Player + 1,
    Winner = W,
    forceUpdateBoard(CurrentValue, Board), !.


maxValueAux([Board|Rest], Player, Depth, A, B, CurrentValue, NewValue) :-
    Depth \= 3,
    OtherPlayer is 1 - Player,
    minValue(Board, OtherPlayer, Depth, A, B, V1),
    max(CurrentValue, V1, BestValue),
    V1 < B,
    max(A, V1, NewA),
    maxValueAux(Rest, Player, Depth, NewA, B, BestValue, NewValue), !.

maxValueAux([Board|_], Player, Depth, A, B,  CurrentValue, BestValue):-
    Depth \= 3,
    OtherPlayer is 1 - Player,
    minValue(Board, OtherPlayer, Depth, A, B, V1),
    max(CurrentValue, V1, BestValue),
    V1 >= B, !.

maxValueAux([Board|_], Player, 3, _, B,  CurrentValue, BestValue):-
    evaluateValue(Board, Player, V1),
    max(CurrentValue, V1, BestValue),
    V1 >= B, !.

/*
    minValueAux(+Board, +Player, +Depth, +A, +B, +CurrentValue, -BestValue)

    Calculates the best move based on a board for a specific player, given a specific depth (Aux function)

    +Board : Game board
    +Player : Player to be evaluated
    +Depth : Current depth of the computation
    +A : Alpha Value
    +B : Beta Value
    +CurrentValue : Best value obtained so far
    -BestValue : Best possible Points
*/
minValueAux([], _, _, _, _, Value, Value):- !.

minValueAux([Board|Rest], Player, Depth, A, B, CurrentValue, NewValue) :-
    OtherPlayer is 1 - Player,
    getPiecesToPlay(Board, OtherPlayer, Pieces),
    maxValue(Board, OtherPlayer, Depth, A, B, Pieces, V1),
    min(CurrentValue, V1, BestValue),
    V1 > A,
    min(B, V1, NewB),
    minValueAux(Rest, Player, Depth, A, NewB, BestValue, NewValue), !.

minValueAux([Board|_], Player, Depth, A, B, CurrentValue, BestValue):-     
    OtherPlayer is 1 - Player,
    getPiecesToPlay(Board, OtherPlayer, Pieces),
    maxValue(Board, OtherPlayer, Depth, A, B, Pieces, V1),
    min(CurrentValue, V1, BestValue),
    V1 =< A, !.


/*
    forceUpdateBoard(+NewValue, +Board)

    Forces a board update

    +NewValue : Board value
    +Board : Game board
*/
forceUpdateBoard(V1, Board) :-
    retractall(moveBoard(_, _)),
    asserta(moveBoard(V1, Board)), !.

/*
    updateMoveBoard(+NewValue, +Board)

    Tries to update a board

    +NewValue : Board value
    +Board : Game board
*/
updateMoveBoard(V1, Board) :-
    moveBoard(V2, _),
    V2 < V1,
    retractall(moveBoard(_, _)),
    asserta(moveBoard(V1, Board)), !.

updateMoveBoard(_, _).

/*
    max(+Value1, +Value2, -Value3)

    Finds the max number between two numbers

    +Value1 : Value 1
    +Value2 : Value 2
    -Value3 : Max Value
*/
max(A, B, A) :- A >= B, !.
max(A, B, B) :- B > A, !.

/*
    min(+Value1, +Value2, -Value3)

    Finds the min number between two numbers

    +Value1 : Value 1
    +Value2 : Value 2
    -Value3 : Min Value
*/
min(A, B, A) :- A =< B, !.
min(A, B, B) :- B < A, !.
/*
    Calculate the quadrant of a position
*/
firstQuadrant(X, Y) :- Y < 6, X > 5.
secondQuadrant(X, Y) :- Y < 6, X < 6.
thirdQuadrant(X, Y) :- Y > 5, X < 6.
fourthQuadrant(X, Y) :- Y > 5, X > 5.

% evaluatePiece(+Piece, +X, +Y, +Goals, -Points)

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

% evaluateElephant(X, Y, Modifier, Value)

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

evaluateBigBrain(Board, Player, Value) :-
    getTargetPieces(Board, Goals),
    getBoardPoints(Board, 1, Player, Goals, Points),
    OtherPlayer is 1 - Player,
    findScaredPieces(Board, OtherPlayer, ScaredOtherPlayer),
    length(ScaredOtherPlayer, NumScaredOtherPlayer),
    % Formula
    Value is -1 * Points - 100 * NumScaredOtherPlayer.

getBoardPoints([], _, _, _, 0).
getBoardPoints([Xs|Rest], X, Player, Goals, Points) :-
    getRowPoints(Xs, X, 1, Player, Goals, XPoints),
    X1 is X + 1,
    getBoardPoints(Rest, X1, Player, Goals, RestPoints),
    Points is XPoints + RestPoints.

getRowPoints([], _, _, _, _, 0).
getRowPoints([Piece|Rest], X, Y, 0, Goals, Points) :-
    Y1 is Y + 1,
    Piece < 4,
    evaluatePiece(Piece, X, Y, Goals, Val),
    getRowPoints(Rest, X, Y1, 0, Goals, RestPoints),
    Points is Val + RestPoints.
getRowPoints([Piece|Rest], X, Y, 1, Goals, Points) :-
    Y1 is Y + 1,
    Piece > 3,
    evaluatePiece(Piece, X, Y, Goals, Val),
    getRowPoints(Rest, X, Y1, 1, Goals, RestPoints),
    Points is Val + RestPoints.

getRowPoints([_|Rest], X, Y, Player, Goals, Points) :-
    Y1 is Y + 1,
    getRowPoints(Rest, X, Y1, Player, Goals, Points).

% =========================================================================
% CHANGE MAIN LOOP TO NOT DRAW BOARD
% =========================================================================

statsGame(Winner) :-
    retractall(playerTurn(_)),
    retractall(num_turn(_)),
    asserta(num_turn(0)),
    initial_state(InitialBoard),
    setBoard(InitialBoard),
    switchPlayer,
    !,
    repeat,
    board(B),
    playerTurn(Player),
    removeLastPosition(B, NB),
    turn(NB, Player),
    switchPlayer,
    game_over(Winner),
    isGameOver(Winner),
    !.

% =========================================================================
% LOOP GAMES
% =========================================================================

/*
    loopGame(+N, -Wins1, -Wins2)

    Runs N games and unifies Wins1 and Wins2
    with the corresponding player wins

    +N : Number of games to run
    -Wins1 : Wins of Player 1
    -Wins2 : Wins of Player 2
*/
loopGame(N, Wins1, Wins2) :-
    loopGameAux(N, 0, 0, Wins1, Wins2).

/*
    loopGameAux(+N, +Acc1, +Acc2, -Res1, -Res2)

    Auxliar predicate of loopGame with accumulators

    +N : Games left to run
    +Acc1 : Accumulator of Player 1 Wins
    +Acc2 : Accumulator of Player 2 Wins
    -Res1 : Wins of Player 1
    -Res2 : Wins of Player 2
*/
loopGameAux(0, Acc1, Acc2, Acc1, Acc2).
loopGameAux(N, Acc1, Acc2, Res1, Res2) :-
    % write('playing game..'), nl,
    statsGame(Winner),
    handleWinner(Winner, Acc1, Acc2, Aux1, Aux2),
    N1 is N - 1,
    loopGameAux(N1, Aux1, Aux2, Res1, Res2).

/*
    handleWinner(+Winner, +Acc1, +Acc2, -Res1, -Res2)

    Handles the end of one game

    +Winner : Winner of the game
    +Acc1 : Actual wins of player 1
    +Acc2 : Actual wins of player 2
    -Res1 : New wins of player 1
    -Res2 : New wins of player 2
*/
handleWinner(1, Acc1, Acc2, Res1, Acc2) :- Res1 is Acc1 + 1.
handleWinner(2, Acc1, Acc2, Acc1, Res2) :- Res2 is Acc2 + 1.
handleWinner(99, Acc1, Acc2, Acc1, Acc2). % Draw

% =========================================================================
% PRINT STATS
% =========================================================================

% Calculates stats of the game
getStats :-
    retractall(boardPreference(_)),
    asserta(boardPreference(1)),
    setupCustomBoard,

    write('============================'), nl,
    write('=          STATS           ='), nl,
    write('============================'), nl,

    N = 10,

    retractall(evaluationType(_, _)),
    asserta(evaluationType(0, 1)),
    asserta(evaluationType(1, 1)),

    retractall(playerType(_,_)),
    asserta(playerType(0, 2)), asserta(playerType(1, 1)),
    loopGame(N, P1, P2),
    D1 is N - P1 - P2,

    write('Greedy Bot vs Random Bot'), nl,
    write('Number of games: '), write(N), nl,
    write('Greedy Bot wins: '), write(P1), nl,
    write('Random Bot wins: '), write(P2), nl,
    write('Draws: '), write(D1), nl, nl,

    % ===============================================

    retractall(evaluationType(_, _)),
    asserta(evaluationType(0, 0)),
    asserta(evaluationType(1, 1)),

    retractall(playerType(_,_)),
    asserta(playerType(0, 2)), asserta(playerType(1, 2)),
    loopGame(N, P3, P4),
    D2 is N - P3 - P4,

    write('Greedy Bot Evaluation Simple vs Greedy Bot Evaluation Complex'), nl,
    write('Number of games: '), write(N), nl,
    write('Greedy Bot 1 wins: '), write(P3), nl,
    write('Greedy Bot 2 wins: '), write(P4), nl,
    write('Draws: '), write(D2), nl, nl,

    % ===============================================

    retractall(evaluationType(_, _)),
    asserta(evaluationType(0, 0)),
    asserta(evaluationType(1, 0)),

    loopGame(N, P5, P6),
    D3 is N - P5 - P6,

    write('Greedy Bot vs Greedy Bot'), nl,
    write('-> Both using evaluation simple'), nl,
    write('Number of games: '), write(N), nl,
    write('Greedy Bot 1 wins: '), write(P5), nl,
    write('Greedy Bot 2 wins: '), write(P6), nl,
    write('Draws: '), write(D3), nl, nl,

    % ===============================================

    retractall(evaluationType(_, _)),
    asserta(evaluationType(0, 1)),
    asserta(evaluationType(1, 1)),

    loopGame(N, P7, P8),
    D4 is N - P7 - P8,

    write('Greedy Bot vs Greedy Bot'), nl,
    write('-> Both using evaluation complex'), nl,
    write('Number of games: '), write(N), nl,
    write('Greedy Bot 1 wins: '), write(P7), nl,
    write('Greedy Bot 2 wins: '), write(P8), nl,
    write('Draws: '), write(D4), nl, nl.
% =========================================================================
% CHANGE MAIN LOOP TO NOT DRAW BOARD
% =========================================================================

statsGame(Winner) :-
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
    gameOver(Winner),
    isGameOver(Winner),
    !.

% =========================================================================
% LOOP GAMES
% =========================================================================

loopGame(N, Wins1, Wins2) :-
    loopGameAux(N, 0, 0, Wins1, Wins2).

loopGameAux(0, Acc1, Acc2, Acc1, Acc2).
loopGameAux(N, Acc1, Acc2, Res1, Res2) :-
    retractall(playerTurn(_)),
    retractall(num_turn(_)),
    asserta(num_turn(0)),
    write('playing game..'), nl,
    statsGame(Winner),
    handleWinner(Winner, Acc1, Acc2, Aux1, Aux2),
    N1 is N - 1,
    loopGameAux(N1, Aux1, Aux2, Res1, Res2).

handleWinner(1, Acc1, Acc2, Res1, Acc2) :- Res1 is Acc1 + 1.
handleWinner(2, Acc1, Acc2, Acc1, Res2) :- Res2 is Acc2 + 1.
handleWinner(99, Acc1, Acc2, Acc1, Acc2). % Draw

% =========================================================================
% PRINT STATS
% =========================================================================

getStats :-
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

    write('Greedy Bot vs Random Bot'), nl,
    write('Number of games: '), write(N), nl,
    write('Greedy Bot wins: '), write(P1), nl,
    write('Random Bot wins: '), write(P2), nl,

    % ===============================================

    retractall(evaluationType(_, _)),
    asserta(evaluationType(0, 0)),
    asserta(evaluationType(1, 1)),

    retractall(playerType(_,_)),
    asserta(playerType(0, 2)), asserta(playerType(1, 2)),
    loopGame(N, P3, P4),

    write('Greedy Bot Evaluation Simple vs Greedy Bot Evaluation Complex'), nl,
    write('Number of games: '), write(N), nl,
    write('Greedy Bot 1 wins: '), write(P3), nl,
    write('Greedy Bot 2 wins: '), write(P4), nl,

    % ===============================================

    retractall(evaluationType(_, _)),
    asserta(evaluationType(0, 0)),
    asserta(evaluationType(1, 0)),

    loopGame(N, P5, P6),

    write('Greedy Bot vs Greedy Bot'), nl,
    write('-> Both using evaluation simple'), nl,
    write('Number of games: '), write(N), nl,
    write('Greedy Bot 1 wins: '), write(P5), nl,
    write('Greedy Bot 2 wins: '), write(P6), nl,

    % ===============================================

    retractall(evaluationType(_, _)),
    asserta(evaluationType(0, 1)),
    asserta(evaluationType(1, 1)),

    loopGame(N, P7, P8),

    write('Greedy Bot vs Greedy Bot'), nl,
    write('-> Both using evaluation complex'), nl,
    write('Number of games: '), write(N), nl,
    write('Greedy Bot 1 wins: '), write(P7), nl,
    write('Greedy Bot 2 wins: '), write(P8), nl.    
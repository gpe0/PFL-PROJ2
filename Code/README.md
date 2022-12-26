# PFL 22/23 - Trabalho Prático 2

## Identificação do trabalho

### Jogo: Barca

### Grupo: (Barca_2)

- 202005358 - Alexandre Ferreira Nunes
- 202004907 - Gonçalo da Costa Sequeira Pinto
- 202005285 - Guilherme António Cerqueira Magalhães

## Instalação e Execução

## Descrição do jogo

## Lógica do jogo

### Representação interna do estado do jogo

O estado do jogo é representado `dinamicamente` com os seguintes 3 predicados:

```prolog
playerType/2
board/1
playerTurn/1
```

- `playerType(+Player, -Type)`:

Indica o tipo do jogador.

Os jogadores são representados por:

0. Player 1
1. Player 2

Os tipos podem ser:

0. Human
1. Random Bot
2. Greedy Bot
3. MinMax Bot

Exemplos de uso:

```
playerType(0, 0) % O player 1 é human?
playerType(1, 2) % O player 2 é greedy?
```

Este predicado é utilizado para possibilitar uma interface de turnos generalizada e consistente.

---

- `board(-Board)`:

Unifica a variável Board com o tabuleiro atual do jogo.

O tabuleiro é representado como uma lista de listas.

Exemplo de um tabuleiro:

```prolog
[
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
].
```

Os possíveis valores em cada átomo do tabuleiro são:

0. No Piece
1. Elephant (Player 1)
2. Mouse (Player 1)
3. Lion (Player 1)
4. Elephant (Player 2)
5. Mouse (Player 2)
6. Lion (Player 2)
7. Target
8. Possible Move
9. Previous Position

---

- `playerTurn(+Player)`:

Verifica se é a vez de o Player (0/1) jogar.

### Visualização do estado de jogo

O código relacionado com a visualização do estado do jogo está no ficheiro `view.pl`.

O predicado principal é:

```prolog
drawBoard(Board) :- 
    drawHeader,
    drawRowLoop(10, Board),
    drawFooter,
    !.
```

Sendo intuitivo o funcionamento high-level.

Cada posição do tabuleiro é um quadrado 3x3, portanto o desenho de uma linha do tabuleiro corresponde a 3 linhas de output, sendo cada peça dividade em 3 partes.

Ou seja, o desenho de uma linha do tabuleiro vai ser:

```prolog
drawRow(Index, Row) :-
    drawLine(Index, Row, 1),
    drawLine(Index, Row, 2),
    drawLine(Index, Row, 3).
```

A tradução dos valores do tabuleiro para as peças é realizada no seguinte predicado:

```prolog
drawPlace(Piece, Offset, Color).
```

Sendo `Piece` o valor do tabuleiro; `Offset` a parte atual da peça; `Color` cor da posição

Exemplo - Desenho do elefante:

```prolog
drawPlace(1, 1, _) :- write('()o o()|').
drawPlace(1, 2, _) :- write('  (1)  |').
drawPlace(1, _, _) :- write('  ( )  |').
drawPlace(4, 1, _) :- write('()o o()|').
drawPlace(4, 2, _) :- write('  (2)  |').
drawPlace(4, _, _) :- write('  ( )  |').
```

As peças não deferem consoante a cor mas este parâmetro permite desenhar lugares vazios de uma forma mais clara e bonita.

A cor é calculada com o Index da coluna:

```prolog
Color is Index mod 2
```

Tabuleiro inicial:

```
      A       B       C       D       E       F       G       H       I       J     
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  |       | . . . |       | . . . |()o o()|()o o()|       | . . . |       | . . . |
10|       | . . . |       | . . . |  (2)  |  (2)  |       | . . . |       | . . . |10
  |       | . . . |       | . . . |  ( )  |  ( )  |       | . . . |       | . . . |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  | . . . |       | . . . | @@@@@ || o o ||| o o || @@@@@ |       | . . . |       |
 9| . . . |       | . . . |@o\2/o@| \ 2 / | \ 2 / |@o\2/o@|       | . . . |       |9
  | . . . |       | . . . | @@@@@ | -\_/- | -\_/- | @@@@@ |       | . . . |       |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  |       | . . . |       | . . . |       | . . . |       | . . . |       | . . . |
 8|       | . . . |       | . . . |       | . . . |       | . . . |       | . . . |8
  |       | . . . |       | . . . |       | . . . |       | . . . |       | . . . |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  | . . . |       | . . . |   |   | . . . |       |   |   |       | . . . |       |
 7| . . . |       | . . . | --|-- | . . . |       | --|-- |       | . . . |       |7
  | . . . |       | . . . |   |   | . . . |       |   |   |       | . . . |       |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  |       | . . . |       | . . . |       | . . . |       | . . . |       | . . . |
 6|       | . . . |       | . . . |       | . . . |       | . . . |       | . . . |6
  |       | . . . |       | . . . |       | . . . |       | . . . |       | . . . |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  | . . . |       | . . . |       | . . . |       | . . . |       | . . . |       |
 5| . . . |       | . . . |       | . . . |       | . . . |       | . . . |       |5
  | . . . |       | . . . |       | . . . |       | . . . |       | . . . |       |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  |       | . . . |       |   |   |       | . . . |   |   | . . . |       | . . . |
 4|       | . . . |       | --|-- |       | . . . | --|-- | . . . |       | . . . |4
  |       | . . . |       |   |   |       | . . . |   |   | . . . |       | . . . |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  | . . . |       | . . . |       | . . . |       | . . . |       | . . . |       |
 3| . . . |       | . . . |       | . . . |       | . . . |       | . . . |       |3
  | . . . |       | . . . |       | . . . |       | . . . |       | . . . |       |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  |       | . . . |       | @@@@@ || o o ||| o o || @@@@@ | . . . |       | . . . |
 2|       | . . . |       |@o\1/o@| \ 1 / | \ 1 / |@o\1/o@| . . . |       | . . . |2
  |       | . . . |       | @@@@@ | -\_/- | -\_/- | @@@@@ | . . . |       | . . . |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  | . . . |       | . . . |       |()o o()|()o o()| . . . |       | . . . |       |
 1| . . . |       | . . . |       |  (1)  |  (1)  | . . . |       | . . . |       |1
  | . . . |       | . . . |       |  ( )  |  ( )  | . . . |       | . . . |       |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
      A       B       C       D       E       F       G       H       I       J     
```

```
A seguinte posição corresponde a uma casa objetivo.

 ------- 
|   |   |
| --|-- |
|   |   |
 ------- 
```

Tabuleiro durante o jogo:

```
      A       B       C       D       E       F       G       H       I       J     
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  |       | . . . |       | . . . |()o o()|()o o()|       | . . . |       | . . . |
10|       | . . . |       | . . . |  (2)  |  (2)  |       | . . . |       | . . . |10
  |       | . . . |       | . . . |  ( )  |  ( )  |       | . . . |       | . . . |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  | . . . |       | . . . |       | . . . |       | @@@@@ |       | . . . |       |
 9| . . . |       | . . . |       | . . . |       |@o\2/o@|       | . . . |       |9
  | . . . |       | . . . |       | . . . |       | @@@@@ |       | . . . |       |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  |       | . . . |       | . . . || o o ||~~~~~~~|       | . . . | @@@@@ | . . . |
 8|       | . . . |       | . . . | \ 2 / |~~~~~~~|       | . . . |@o\2/o@| . . . |8
  |       | . . . |       | . . . | -\_/- |~~~~~~~|       | . . . | @@@@@ | . . . |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  | . . . |       | . . . |()o o()| . . . |       | @@@@@ |       | . . . |       |
 7| . . . |       | . . . |  (1)  | . . . |       |@o\1/o@|       | . . . |       |7
  | . . . |       | . . . |  ( )  | . . . |       | @@@@@ |       | . . . |       |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  |       | . . . |       | . . . |       | . . . |       | . . . |       | . . . |
 6|       | . . . |       | . . . |       | . . . |       | . . . |       | . . . |6
  |       | . . . |       | . . . |       | . . . |       | . . . |       | . . . |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  | . . . |       | . . . |       | . . . |       | . . . |       | . . . |       |
 5| . . . |       | . . . |       | . . . |       | . . . |       | . . . |       |5
  | . . . |       | . . . |       | . . . |       | . . . |       | . . . |       |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  |       | . . . |       || o o ||       | . . . |   |   | . . . |       | . . . |
 4|       | . . . |       | \ 2 / |       | . . . | --|-- | . . . |       | . . . |4
  |       | . . . |       | -\_/- |       | . . . |   |   | . . . |       | . . . |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  | . . . |       | . . . |       | . . . |       | . . . |       | . . . |       |
 3| . . . |       | . . . |       | . . . |       | . . . |       | . . . |       |3
  | . . . |       | . . . |       | . . . |       | . . . |       | . . . |       |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  || o o || . . . |       | . . . |       || o o ||       | . . . |       | . . . |
 2| \ 1 / | . . . |       | . . . |       | \ 1 / |       | . . . |       | . . . |2
  | -\_/- | . . . |       | . . . |       | -\_/- |       | . . . |       | . . . |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  | . . . |       |()o o()|       | . . . |       | . . . | @@@@@ | . . . |       |
 1| . . . |       |  (1)  |       | . . . |       | . . . |@o\1/o@| . . . |       |1
  | . . . |       |  ( )  |       | . . . |       | . . . | @@@@@ | . . . |       |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
      A       B       C       D       E       F       G       H       I       J     
```

```
A seguinte posição corresponde à posição anterior da peça que se moveu mais recentemente.

Torna mais claro qual a peça que se mexeu no turno.

 ------- 
|~~~~~~~|
|~~~~~~~|
|~~~~~~~|
 ------- 
```

Também implementamos uma feature de permite o jogador observar os possíveis movimentos que pode realizar depois de selecionar uma peça. Permite que o utilizador observe movimentos inválidos que possam não ser obvios.

Exemplo complexo de movimentos do elefante na posição `E6`:

```
      A       B       C       D       E       F       G       H       I       J     
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  |XXXXXXX| . . . |       | . . . |XXXXXXX|()o o()|       | . . . |XXXXXXX| . . . |
10|XXXXXXX| . . . |       | . . . |XXXXXXX|  (1)  |       | . . . |XXXXXXX| . . . |10
  |XXXXXXX| . . . |       | . . . |XXXXXXX|  ( )  |       | . . . |XXXXXXX| . . . |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  | . . . |XXXXXXX| . . . | @@@@@ |XXXXXXX|       | . . . |XXXXXXX| . . . |       |
 9| . . . |XXXXXXX| . . . |@o\1/o@|XXXXXXX|       | . . . |XXXXXXX| . . . |       |9
  | . . . |XXXXXXX| . . . | @@@@@ |XXXXXXX|       | . . . |XXXXXXX| . . . |       |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  |       | . . . |XXXXXXX| . . . |XXXXXXX|| o o ||       | . . . |       | . . . |
 8|       | . . . |XXXXXXX| . . . |XXXXXXX| \ 1 / |       | . . . |       | . . . |8
  |       | . . . |XXXXXXX| . . . |XXXXXXX| -\_/- |       | . . . |       | . . . |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  | . . . |       | . . . |   |   | . . . |       || o o ||       | . . . |       |
 7| . . . |       | . . . | --|-- | . . . |       | \ 2 / |       | . . . |       |7
  | . . . |       | . . . |   |   | . . . |       | -\_/- |       | . . . |       |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  |       | . . . |       || o o ||()o o()| . . . |       | . . . |XXXXXXX|XXXXXXX|
 6|       | . . . |       | \ 2 / |  (1)  | . . . |       | . . . |XXXXXXX|XXXXXXX|6
  |       | . . . |       | -\_/- |  ( )  | . . . |       | . . . |XXXXXXX|XXXXXXX|
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  | . . . |       | . . . |       | . . . |XXXXXXX| . . . |       | . . . |       |
 5| . . . |       | . . . |       | . . . |XXXXXXX| . . . |       | . . . |       |5
  | . . . |       | . . . |       | . . . |XXXXXXX| . . . |       | . . . |       |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  |       |()o o()|XXXXXXX| @@@@@ |XXXXXXX| . . . || o o || . . . |       | . . . |
 4|       |  (2)  |XXXXXXX|@o\1/o@|XXXXXXX| . . . | \ 1 / | . . . |       | . . . |4
  |       |  ( )  |XXXXXXX| @@@@@ |XXXXXXX| . . . | -\_/- | . . . |       | . . . |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  | . . . |XXXXXXX| . . . |       | @@@@@ |       | . . . |       | . . . |       |
 3| . . . |XXXXXXX| . . . |       |@o\2/o@|       | . . . |       | . . . |       |3
  | . . . |XXXXXXX| . . . |       | @@@@@ |       | . . . |       | . . . |       |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  |XXXXXXX| . . . |       | . . . |       | . . . | @@@@@ | . . . |       | . . . |
 2|XXXXXXX| . . . |       | . . . |       | . . . |@o\2/o@| . . . |       | . . . |2
  |XXXXXXX| . . . |       | . . . |       | . . . | @@@@@ | . . . |       | . . . |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
  | . . . |       | . . . |       | . . . |()o o()| . . . |       | . . . |       |
 1| . . . |       | . . . |       | . . . |  (2)  | . . . |       | . . . |       |1
  | . . . |       | . . . |       | . . . |  ( )  | . . . |       | . . . |       |
   ------- ------- ------- ------- ------- ------- ------- ------- ------- -------  
      A       B       C       D       E       F       G       H       I       J     
```

```
A seguinte posição corresponde a uma jogada VÁLIDA.

 ------- 
|XXXXXXX|
|XXXXXXX|
|XXXXXXX|
 ------- 
```

### Execução de Jogadas

### Lista de Jogadas Válidas

### Final do Jogo

### Avaliação do Tabuleiro

A avaliação do tabuleiro tem como principal fator o número de pontos que o Player tem.

Com pontos quer-se dizer com número de casas objetivo dominadas.

Além dos pontos é avaliada a diferença do número de peças assutadas dos dois jogadores.

Esta verificação é um bocado custosa mas permite um grande melhoramento no comportamento do bot greedy e minmax.

Sem ela, o bot não tentaria retirar as peças do outro jogador das casas objetivo.

```prolog
evaluate(Board, Player, Value) :-
    % Evaluate objectives
    getTargetPieces(Board, TargetPieces),
    getPlayerPoints(TargetPieces, Player, 0, Points),
    % Evaluate Pieces
    OtherPlayer is 1 - Player,
    findScaredPieces(Board, OtherPlayer, ScaredOtherPlayer),
    findScaredPieces(Board, Player, ScaredPlayer),
    length(ScaredPlayer, NumScaredPlayer),
    length(ScaredOtherPlayer, NumScaredOtherPlayer),
    % Formula
    Value is -100 * Points + NumScaredPlayer - NumScaredOtherPlayer.
```

### Jogada do Computador

...

...

...

...


```prolog
% Handle Random Turn
turn_random(Board, Player, PiecesToMove) :-
    random_member(X-Y-Piece, PiecesToMove),
    getMoves(X, Y, Piece, Board, Player, ValidMoves),
    random_member(XF-YF, ValidMoves),
    movePiece(X, Y, XF, YF, Board, NewBoard),
    !,
    setBoard(NewBoard).

% Handle Greedy Turn
turn_greedy(Board, Player, Pieces) :-
    generateBoards(Board, Player, Pieces, [], NewBoards),
    evaluateBoards(NewBoards, Player, BoardsEvaluated),
    sort(BoardsEvaluated, SortedBoards),
    nth1(1, SortedBoards, V-_),
    getBestBoards(SortedBoards, V, BestBoards),
    !,
    random_member(MoveChosen, BestBoards),
    setBoard(MoveChosen).

% Handle MinMax Turn
turn_greedy_minmax(Board, Player, Pieces) :-
    generateBoards(Board, Player, Pieces, [], Lv1),
    generateLevel2(Lv1, Player, Lv2),
    evaluateLevel2(Lv2, Player, BestBoard, 99, Res),
    !,
    setBoard(Res).
```

## Conclusões

## Bibliografia

https://boardgamegeek.com/boardgame/69347/barca

https://en.wikipedia.org/wiki/Barca_(board_game)
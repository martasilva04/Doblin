%testar

main:-
    Board = [
        [' ', 1  ,  2 ,  3 ,  4 ,  5 ,  6 ], % Cabeçalho (colunas)
        ['E', 'x', 'o', 'x', 'x', ' ', 'x'], % Linha 1
        ['C', 'o', 'o', 'o', 'x', 'o', 'x'], % Linha 2
        ['F', ' ', 'o', 'x', 'x', 'x', 'o'], % Linha 3
        ['A', 'o', 'o', 'x', 'o', 'x', 'x'], % Linha 4
        ['B', 'x', 'o', 'x', 'x', 'o', 'x'], % Linha 5
        ['D', 'x', 'o', 'x', 'x', 'o', 'x']  % Linha 6
    ],
    %valid_moves(Board, EmptyCells).

    valid_moves(Board, ValidMoves),
    write(ValidMoves),nl,
    exclude(adjacent_or_diagonal(game_state(Board, Board, player2, 'x', _D1, _D2)), ValidMoves, GreedyMoves),
    write('Greedy: '), nl, write(GreedyMoves),nl.
    
    %calcular_pontos(Board, x, ScoreX).


% initial_state/2
% Gera dois tabuleiros (inicial e embaralhado) completamente preenchidos.
initial_state_preenchido(GameConfig, game_state(BoardInicial, BoardEmbaralhado, player1)) :-
    member(board_size(Option), GameConfig),
    get_board_size(Option, Rows, Cols),
    
    % Gera o tabuleiro inicial preenchido
    create_filled_board(Rows, Cols, BoardInicial),
    
    % Gera o tabuleiro embaralhado preenchido
    create_filled_board(Rows, Cols, TempBoard),
    shuffle_board(TempBoard, BoardEmbaralhado).

% Cria um tabuleiro preenchido alternadamente com 'x' e 'o'.
create_filled_board(Rows, Cols, Board) :-
    num_list(1, Rows, RowNumbers),
    alphabet_list(Rows, Letras),
    Board = [[' ' | RowNumbers] | Lines],
    create_filled_rows(Letras, Cols, Lines).

% Cria as linhas preenchidas com símbolos alternados.
create_filled_rows([], _, []).
create_filled_rows([Letter | RestLetters], Cols, [[Letter | Row] | RestRows]) :-
    fill_row(Cols, Row, x),  % Começa preenchendo com 'x'.
    create_filled_rows(RestLetters, Cols, RestRows).

% Preenche uma linha alternando os símbolos.
fill_row(0, [], _).
fill_row(N, [Symbol | Rest], Symbol) :-
    N > 0,
    NextN is N - 1,
    alternate_symbol(Symbol, NextSymbol),  % Alterna entre 'x' e 'o'.
    fill_row(NextN, Rest, NextSymbol).

% Alterna entre os símbolos 'x' e 'o'.
alternate_symbol(x, o).
alternate_symbol(o, x).

% Embaralha um tabuleiro preenchido.
shuffle_board(Board, ShuffledBoard) :-
    % Remove o cabeçalho e embaralha as linhas
    Board = [Header | Rows],
    random_permutation(Rows, ShuffledRows),
    % Adiciona o cabeçalho de volta
    ShuffledBoard = [Header | ShuffledRows].


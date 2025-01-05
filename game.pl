:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(system)).

% Main predicate of the game
play :-
    write('=== Welcome to the Game ==='), nl,
    game_menu.

% Displays the game menu and sets up options
game_menu :-
    write('1. Play (H/H)'), nl,
    write('2. Play (H/PC)'), nl,
    write('3. Play (PC/H)'), nl,
    write('4. Play (PC/PC)'), nl,
    write('Choose an option: '),
    get_valid_option(Type, [1,2,3,4]),
    handle_option(Type, Difficulty1, Difficulty2),
    write('Choose the board size: '), nl,
    write('1. 6x6'), nl,
    write('2. 8x8'), nl,
    get_valid_option(Size, [1,2]), 
    setup_game(Size, Difficulty1, Difficulty2).

handle_option(1, 0, 0). % H/H
handle_option(2, 0, Difficulty2) :- choose_difficulty(player2, Difficulty2). % H/PC
handle_option(3, Difficulty1, 0) :- choose_difficulty(player1, Difficulty1). % PC/H
handle_option(4, Difficulty1, Difficulty2) :- % PC/PC
    choose_difficulty(player1, Difficulty1),
    choose_difficulty(player2, Difficulty2).

% Sets up the game according to player types
setup_game(Size, Difficulty1, Difficulty2) :- 
    write('Setting up the game...'), nl,
    GameConfig = [board_size(Size), Difficulty1, Difficulty2],
    initial_state(GameConfig, InitialGameState),
    game_loop(InitialGameState).

get_valid_option(Number, Valid) :-
    get_char(Option),
    get_char(Pending),            % Reads the next character in the buffer
    (   Pending \= '\n'           % Checks if the input contains more than one character
    ->  write('Invalid Option! Input must be a single character.'), nl,
        clear_input_buffer,       % Clears the buffer to avoid issues
        get_valid_option(Number, Valid)
    ;   char_code(Option, Code),
        NumberTemp is Code - 48,  % Converts the character to a number
        (   member(NumberTemp, Valid)  % Checks if the option is valid
        ->  Number = NumberTemp
        ;   length(Valid, Size),
            format('Invalid Option! Choose Number (1-~w): ', Size), nl,
            get_valid_option(Number, Valid)
        )
    ).

% Clears the remaining input buffer
clear_input_buffer :-
    get_char(Char),
    (Char = '\n' -> true ; clear_input_buffer).

choose_difficulty(Bot, Difficulty) :-
    format('Please select ~a status:\n', [Bot]),
    write('1 - Random\n'),
    write('2 - Greedy\n'),
    get_valid_option(Option, [1,2]), !,
    Difficulty = Option.

game_loop(GameState):-
    clear_console,
    display_game(GameState),
    game_over(GameState, Winner),
    print_winner(Winner), !.
game_loop(GameState):-
    choose_move(GameState, Coordinates),
    move(GameState, Coordinates, NewGameState),
    game_loop(NewGameState).

choose_move(game_state(B1, B2, P, S, D1, D2), Move):-
    write('Your turn: '), write(P), nl,
    format('Place a ~w', S), nl,
    (P = player1 -> Difficulty = D1; Difficulty = D2),
    (   Difficulty = 0 -> % Human player
        write(P), nl,
        read_input(Move, game_state(B1, B2, P, S, D1, D2))
    ;   Difficulty = 1 -> % Random bot
        write('Bot evaluating options...'),
        sleep(3),
        valid_moves(B1, ValidMoves),
        random_member(Move, ValidMoves)
    ;   Difficulty = 2 -> % Greedy bot
        write('Bot evaluating options...'),
        sleep(3),
        greedy_move(game_state(B1, B2, P, S, D1, D2), Move)
    ).

% greedy bot

adjacent_or_diagonal(game_state(B1, B2, player2, S, _D1, _D2), Move) :-
    move(game_state(B1, B2, player2, S, _D1, _D2), Move, game_state(NewB1, NewB2, NextPlayer, NextSymbol, _D1, _D2)), % Simulate the move.
    adjacent_or_diagonal_check(NewB2, Move, S).

adjacent_or_diagonal(game_state(B1, B2, player1, S, _D1, _D2), Move) :-
    move(game_state(B1, B2, player1, S, _D1, _D2), Move, game_state(NewB1, NewB2, NextPlayer, NextSymbol, _D1, _D2)), % Simulate the move.
    adjacent_or_diagonal_check(NewB1, Move, S).

% Check if the move places a symbol adjacent or diagonally near an existing symbol.
adjacent_or_diagonal_check(Board, Move, S) :-
    % Extract the coordinates of the move.
    coordenadas_para_indices_segundo(Move, Board, X, Y),
    % Check for adjacency or diagonal proximity.
    (
        adjacent(X, Y, X1, Y1),
        symbol_at(Board, X1, Y1, S)
    ).

% Define adjacency and diagonal positions.
adjacent(X, Y, X1, Y1) :-
    member(DX, [-1, 0, 1]), % Difference for X.
    member(DY, [-1, 0, 1]), % Difference for Y.
    (DX \= 0; DY \= 0),     % Exclude (0, 0), i.e., the same position.
    X1 is X + DX,           % Calculate X1.
    Y1 is Y + DY.

symbol_at(Board, X, Y, Symbol) :-
    nth0(X, Board, Row), 
    nth0(Y, Row, Symbol).

greedy_move(game_state(B1, B2, player2, S, _D1, _D2), Move) :-
    valid_moves(B2, ValidMoves),
    exclude(adjacent_or_diagonal(game_state(B1, B2, player2, S, _D1, _D2)), ValidMoves, GreedyMoves),
    get_next_symbol(S, NewSymbol),
    % If there are 3 of the same symbol, place the other symbol to block.
    findall(
        Value-M,
        (
            member(M, ValidMoves),
            move(game_state(B1, B2, player2, NewSymbol, _D1, _D2), M, NextGameState),
            value(NextGameState, player2, Value)
        ),
        ScoreMoves1
    ),
    board_score(B2, CurrentScore2),
    board_score(B1, CurrentScore1),
    (CurrentScore1 = 0 -> NewScore1 is 0.5; NewScore1 = CurrentScore1),
    (CurrentScore2 = 0 -> NewScore2 is 0.5; NewScore2 = CurrentScore2),
    Ratio is NewScore2/NewScore1,
    max_member(Score1-Move1, ScoreMoves1),
    findall(
        Value-M,
        (
            member(M, ValidMoves),
            move(game_state(B1, B2, player2, S, _D1, _D2), M, NextGameState),
            value(NextGameState, player2, Value)
        ),
        ScoreMoves2
    ),
    min_member(MinScore-Move2, ScoreMoves2),
    (  
        Score1 > Ratio -> Move = Move1  
    ;   
        (   
            MinScore < Ratio -> Move = Move2  
        ;
            (   % Otherwise, choose the move from GreedyMoves that gives the most points to the opponent
                GreedyMoves \= [] ->  
                findall(  
                    ValueOpponent-M,  
                    (  
                        member(M, GreedyMoves),  
                        move(game_state(B1, B2, player2, S, _D1, _D2), M, NextGameState),  
                        value(NextGameState, player2, ValueOpponent) 
                    ),  
                    Score1Moves2  
                ),  
                min_member(_-Move, Score1Moves2) , 
                write('Move: '), write(Move), nl
            ;   % Otherwise, use logic to minimize the opponent's points.  
                findall(  
                    MinValue-M,  
                    (  
                        member(M, ValidMoves),  
                        move(game_state(B1, B2, player2, S, _D1, _D2), M, NextGameState),  
                        value(NextGameState, player2, MinValue)
                    ),  
                    ScoreMoves2  
                ),  
                min_member(_-Move, ScoreMoves2)  
            )  
        )
    ). 

greedy_move(game_state(B1, B2, player1, S, _D1, _D2), Move) :-
    valid_moves(B1, ValidMoves),
    exclude(adjacent_or_diagonal(game_state(B1, B2, player1, S, _D1, _D2)), ValidMoves, GreedyMoves),
    get_next_symbol(S, NewSymbol),
    % If there are 3 of the same symbol, place the other symbol to block.
    findall(
        Value-M,
        (
            member(M, ValidMoves),
            move(game_state(B1, B2, player1, NewSymbol, _D1, _D2), M, NextGameState),
            value(NextGameState, player1, Value)
        ),
        ScoreMoves1
    ),

    board_score(B1, CurrentScore1),
    board_score(B2, CurrentScore2),
    (CurrentScore1 = 0 -> NewScore1 is 0.5; NewScore1 = CurrentScore1),
    (CurrentScore2 = 0 -> NewScore2 is 0.5; NewScore2 = CurrentScore2),
    Ratio is NewScore1/NewScore2,
    max_member(Score1-Move1, ScoreMoves1),
    findall(
        Value-M,
        (
            member(M, ValidMoves),
            move(game_state(B1, B2, player1, S, _D1, _D2), M, NextGameState),
            value(NextGameState, player1, Value)
        ),
        ScoreMoves2
    ),
    min_member(MinScore-Move2, ScoreMoves2),
    (  
        Score1 > Ratio -> Move = Move1  
    ;   
        (
            MinScore < Ratio -> Move = Move2  
        ;
            (   % Otherwise, choose the move from GreedyMoves that gives the most points to the opponent
                GreedyMoves \= [] ->  
                findall(  
                    ValueOpponent-M,  
                    (  
                        member(M, GreedyMoves),  
                        move(game_state(B1, B2, player1, S, _D1, _D2), M, NextGameState),  
                        value(NextGameState, player1, ValueOpponent)  
                    ),  
                    Score1Moves2  
                ),  
                min_member(_-Move, Score1Moves2)  
            ;   % Otherwise, use logic to minimize the opponent's points.  
                findall(  
                    Value-M,  
                    (  
                        member(M, ValidMoves),  
                        move(game_state(B1, B2, player1, S, _D1, _D2), M, NewGameState),  
                        value(NewGameState, player1, Value)
                    ),  
                    ScoreMoves11  
                ),  
                min_member(_-Move, ScoreMoves11)  
            ) 
        ) 
    ).  

% The game state is better the fewer points the player has and the more the opponent has, the game state is more positive the lower the value.
value(game_state(B1, B2, P, S, _D1, _D2), player1, Value) :-
    board_score(B1, ScoreB1),
    board_score(B2, ScoreB2),
    (ScoreB1 = 0 -> NewScore1 is 0.5; NewScore1 = ScoreB1),
    (ScoreB2 = 0 -> NewScore2 is 0.5; NewScore2 = ScoreB2),
    Value is NewScore1/NewScore2.

value(game_state(B1, B2, P, S, _D1, _D2), player2, Value) :-
    board_score(B1, ScoreB1),
    board_score(B2, ScoreB2),
    (ScoreB1 = 0 -> NewScore1 is 0.5; NewScore1 = ScoreB1),
    (ScoreB2 = 0 -> NewScore2 is 0.5; NewScore2 = ScoreB2),
    Value is NewScore2/NewScore1.


% Predicado initial_state/4
% Configura o estado inicial do jogo e gera dois tabuleiros (normal e embaralhado)
initial_state(GameConfig, game_state(Board1, Board2, player1, x, Difficulty1, Difficulty2)) :-
    member(board_size(Option), GameConfig),
    nth1(2, GameConfig, Difficulty1),
    nth1(3, GameConfig, Difficulty2),
    get_board_size(Option, Rows, Cols),
    
    % Gera o tabuleiro inicial
    create_shuffle_board(Rows, Cols, Board1),
    
    % Gera o tabuleiro embaralhado
    create_shuffle_board(Rows, Cols, Board2).


board_score(Board, Score):-
    calculate_points(Board, x, ScoreX),
    calculate_points(Board, o, ScoreO),
    Score is ScoreX+ScoreO.

game_over(game_state(T1, T2, _P, _S, _D1, _D2), Winner):-
    board_completed(T1),
    calculate_points(T1, x, ScoreX1),
    calculate_points(T1, o, ScoreO1),
    ScoreB1 is ScoreX1+ScoreO1,

    calculate_points(T2, x, ScoreX2),
    calculate_points(T2, o, ScoreO2),
    ScoreB2 is ScoreX2+ScoreO2,
    print_results(ScoreB1, ScoreB2),

    (   
        ScoreB1 < ScoreB2 ->
        Winner = player1
    ;   
        ScoreB1 > ScoreB2 ->
        Winner = player2
    ;   
        Winner = draw
    ).

print_results(ScoreB1, ScoreB2):-
    write('End of game! Calculating points...'), nl,
    write('Score Board1 (Player1): '), write(ScoreB1), nl,
    write('Score Board2 (Player2): '), write(ScoreB2), nl.

print_winner(player1):-
    write('Player 1 won!').

print_winner(player2):-
    write('Player 2 won!').

print_winner(draw):-
    write('Draw').

row_completed([]).
row_completed([H|T]) :-
    H \= ' ',
    row_completed(T).

all_rows_completed([]).
all_rows_completed([H|T]):-
    row_completed(H),
    all_rows_completed(T).    

board_completed([[' '|FirstRow]|Rest]):-
    row_completed(FirstRow),
    all_rows_completed(Rest).





% Determina o número de linhas e colunas com base na opção escolhida
get_board_size(1, 6, 6). % Opção 1 -> Tabuleiro 6x6
get_board_size(2, 8, 8). % Opção 2 -> Tabuleiro 8x8

% Cria linhas com letras na borda esquerda e células vazias.
create_lettered_rows([], _, []). % Caso base: sem letras, sem linhas.
create_lettered_rows([Letter | RestLetters], Cols, [[Letter | EmptyLine] | RestLines]) :-
    create_empty_row(Cols, EmptyLine), % Cria uma linha vazia para cada letra.
    create_lettered_rows(RestLetters, Cols, RestLines).

% Cria uma linha vazia
create_empty_row(0, []) :- !.
create_empty_row(Cols, [' '|Rest]) :-
    Cols > 0,
    NextCols is Cols - 1,
    create_empty_row(NextCols, Rest).

% Gera o tabuleiro embaralhado de acordo com o tamanho (6x6 ou 8x8)
create_shuffle_board(Rows, Cols, Board) :-
    % Cria listas de números e letras conforme o tamanho do tabuleiro
    num_list(1, Cols, Columns),
    alphabet_list(Rows, Letters),
    
    % Embaralha as colunas e as letras
    random_permutation(Columns, ShuffleColumns),
    random_permutation(Letters, ShuffleLetters),
    
    % Gera a linha do cabeçalho (números embaralhados)
    Board = [[' ' | ShuffleColumns] | ShuffleLines],
    
    % Gera as linhas restantes com as letras embaralhadas
    create_lettered_rows(ShuffleLetters, Cols, ShuffleLines).




% display_game(+GameState)
% Exibe o estado atual do jogo com base no GameState atualizado.
display_game(game_state(Board1, Board2, _CurrentPlayer, _Symbol, _D1, _D2)) :-
    write('Board Player1:'), nl,
    display_board(Board1), nl,
    
    write('Board Player2:'), nl,
    display_board(Board2), nl.


% Exibe o tabuleiro
display_board([]).
display_board([Row|Rows]) :-
    write(Row), nl,
    display_board(Rows).


% move(+GameState, +Move, -NewGameState)
% Atualiza o estado do jogo com base no movimento do jogador.
move(game_state(Board1, Board2, CurrentPlayer, Symbol, _D1, _D2), Move, game_state(NewBoard1, NewBoard2, NextPlayer, NewSymbol, _D1, _D2)) :-
    coordenadas_para_indices_segundo(Move, Board1, LineIndex1, ColumnIndex1),
    update_board(Board1, LineIndex1, ColumnIndex1, Symbol, NewBoard1),
    coordenadas_para_indices_segundo(Move, Board2, LineIndex2, ColumnIndex2),
    update_board(Board2, LineIndex2, ColumnIndex2, Symbol, NewBoard2),
    get_next_symbol(Symbol,NewSymbol),
    next_player(CurrentPlayer, Symbol, NextPlayer).


% Função para extrair as letras das linhas
extract_letters(Board, Letters) :-
    maplist(nth1(1), Board, Letters).  % Pega a primeira letra de cada linha

% Função para gerar os números das colunas
generate_columns([H|Board], H).

% coordenadas_para_indices_segundo(+Coordenadas, +BoardEmbaralhado, -Linha, -Coluna)
% Converte as coordenadas (ex.: "A1") para os índices correspondentes no tabuleiro embaralhado.
coordenadas_para_indices_segundo([Letra, Numero], BoardEmbaralhado, Linha, Coluna) :-
    extract_letters(BoardEmbaralhado, LetrasEmbaralhadas),
    generate_columns(BoardEmbaralhado, ColunasEmbaralhadas),
    nth0(Linha, LetrasEmbaralhadas, Letra),  % Encontra o índice da letra embaralhada
    nth0(Coluna, ColunasEmbaralhadas, Numero).

empty_cell(Board, RowIndex, ColumnIndex) :-
    nth0(RowIndex, Board, CurrentRow),
    nth0(ColumnIndex, CurrentRow, Value),
    Value = ' '.  % The cell is empty if it contains a blank space


% atualizar_tabuleiro(+Board, +Linha, +Coluna, +Simbolo, -NewBoard)
% Atualiza uma célula específica em um tabuleiro.
update_board(Board, Row, Column, Symbol, NewBoard) :-
    nth0(Row, Board, RowBoard, OutBoard),
    nth0(Column, RowBoard, _, RowOut),
    nth0(Column, UpdatedRow, Symbol, RowOut),
    nth0(Row, NewBoard, UpdatedRow, OutBoard).


% simbolo_jogador(+Player, -Simbolo)
% Associa cada jogador ao seu respectivo símbolo.
get_next_symbol(x, o).
get_next_symbol(o, x).

% proximo_jogador(+CurrentPlayer, -NextPlayer)
% Alterna o jogador atual.
next_player(player1, x, player1).
next_player(player1, o, player2).
next_player(player2, x, player2).
next_player(player2, o, player1).




% Predicado valid_move/2
% Verifica se o movimento é válido
valid_move(Board, move(Row, Col)) :-
    within_bounds(Board, Row, Col), % Verifica se a posição está dentro dos limites
    get_cell(Board, Row, Col, empty). % Verifica se a célula está vazia.

% Predicado within_bounds/3
% Verifica se uma posição está dentro dos limites do tabuleiro
within_bounds(Board, Row, Col) :-
    length(Board, NumRows),          % Número de linhas do tabuleiro
    nth1(1, Board, FirstRow),        % Obtém a primeira linha
    length(FirstRow, NumCols),       % Número de colunas do tabuleiro
    Row > 0, Row =< NumRows,         % Verifica se a linha está dentro dos limites
    Col > 0, Col =< NumCols.         % Verifica se a coluna está dentro dos limites


read_input([Letter, Number], game_state(B1, _B2, _P, _S, _D1, _D2)) :-
    length(B1, Size),
    Size1 is Size - 1,
    alphabet_list(Size1, AlphaList),
    AlphaList = [FirstLetter|_],
    last(_, LastLetter, AlphaList),
    num_list(1, Size1, NumList),
    NumList = [FirstNumber|_],
    last(_, LastNumber, NumList),
    repeat, 
    get_valid_letter(AlphaList, FirstLetter, LastLetter, Letter),
    format('Choose number (~w-~w): ', [FirstNumber, LastNumber]),
    get_valid_option(Number, NumList),
    coordenadas_para_indices_segundo([Letter, Number], B1, LinhaIndex, ColunaIndex),
    (   empty_cell(B1, LinhaIndex, ColunaIndex) 
    ->  ! 
    ;   write('Invalid! This cell is ocupied.'), nl,
        fail  
    ).

    

get_valid_letter(AlphaList, FirstLetter, LastLetter, Letter) :-
    format('Choose letter (~w-~w): ', [FirstLetter, LastLetter]),
    get_char(LetraTemp),           
    get_char(Pending),              
    (   Pending \= '\n'            
    ->  write('Invalid input! Input must be a single letter.'), nl,
        clear_input_buffer,        
        get_valid_letter(AlphaList, FirstLetter, LastLetter, Letter)
    ;   (   member(LetraTemp, AlphaList)  
        ->  Letter = LetraTemp            
        ;   write('Letra inválida! A letra deve ser válida.'), nl,
            get_valid_letter(AlphaList, FirstLetter, LastLetter, Letter)
        )
    ).


% Gera uma lista de números de 1 até N
num_list(M, M, [M]).  % Caso base: quando M é igual a N
num_list(M, N, [M|Rest]) :-
    M < N,
    M1 is M + 1,
    num_list(M1, N, Rest).


% calcular pontos 

% Calcula os pontos de um tabuleiro
calculate_points(Board, Symbol, Points) :-
    remove_header(Board, NewBoard),
    % Calculate points for horizontal lines
    findall(1, complete_row(NewBoard, Symbol), RowPoints),
    % Calculate points for vertical columns
    findall(1, complete_column(NewBoard, Symbol), ColumnPoints),
    % Calculate points for 2x2 squares
    findall(1, complete_square(NewBoard, Symbol), SquarePoints),
    % Calculate points for diagonals
    findall(1, complete_diagonal(NewBoard, Symbol), DiagonalPoints),
    % Sum all points
    length(RowPoints, RowScore),
    length(ColumnPoints, ColumnScore),
    length(SquarePoints, SquareScore),
    length(DiagonalPoints, DiagonalScore),
    Points is RowScore + ColumnScore + SquareScore + DiagonalScore.

% Check if a row is complete
complete_row(Board, Symbol) :-
    member(Row, Board),
    sublist([Symbol, Symbol, Symbol, Symbol], Row).

% Check if a column is complete
complete_column(Board, Symbol) :-
    transpose(Board, TransposedBoard),
    complete_row(TransposedBoard, Symbol).

% Check if a 2x2 square is complete
complete_square(Board, Symbol) :-
    length(Board, Size),
    between(1, Size, Row), 
    between(1, Size, Column), 
    nth1(Row, Board, Row1),
    nth1(Column, Row1, Symbol),
    Row2Index is Row + 1,
    nth1(Row2Index, Board, Row2),
    Column2Index is Column + 1,
    nth1(Column, Row2, Symbol),
    nth1(Column2Index, Row1, Symbol),
    nth1(Column2Index, Row2, Symbol).

% Check if a diagonal of 4 symbols is complete
complete_diagonal(Board, Symbol) :-
    % Normal diagonals (top-left to bottom-right)
    normal_diagonal(Board, Symbol);
    % Inverted diagonals (top-right to bottom-left)
    inverted_diagonal(Board, Symbol).

% Check if a normal diagonal (left to right) is complete
normal_diagonal(Board, Symbol) :-
    length(Board, Size), 
    Max is Size - 3, 
    between(1, Max, Row), 
    between(1, Max, Column), 
    nth1(Row, Board, Row1),
    nth1(Column, Row1, Symbol),
    Row2 is Row + 1,
    Column2 is Column + 1,
    nth1(Row2, Board, Row2_),
    nth1(Column2, Row2_, Symbol),
    Row3 is Row + 2,
    Column3 is Column + 2,
    nth1(Row3, Board, Row3_),
    nth1(Column3, Row3_, Symbol),
    Row4 is Row + 3,
    Column4 is Column + 3,
    nth1(Row4, Board, Row4_),
    nth1(Column4, Row4_, Symbol).

% Check if an inverted diagonal (right to left) is complete
inverted_diagonal(Board, Symbol) :-
    length(Board, Size), 
    Max is Size - 3,
    between(1, Max, Row), % Inverted diagonals start from 1 to 3
    between(4, Size, Column), % Inverted diagonals start from 4 to 6
    nth1(Row, Board, Row1),
    nth1(Column, Row1, Symbol),
    Row2 is Row + 1,
    Column2 is Column - 1,
    nth1(Row2, Board, Row2_),
    nth1(Column2, Row2_, Symbol),
    Row3 is Row + 2,
    Column3 is Column - 2,
    nth1(Row3, Board, Row3_),
    nth1(Column3, Row3_, Symbol),
    Row4 is Row + 3,
    Column4 is Column - 3,
    nth1(Row4, Board, Row4_),
    nth1(Column4, Row4_, Symbol).

% Transpõe uma matriz (tabuleiro)
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    lists_firsts_rests(Rest, Fs, Oss).

% Define se uma lista é sublista de outra
sublist(Sub, List) :-
    append(_, Rest, List),
    append(Sub, _, Rest).

% Gera números de Min até Max
between(Min, Max, Min) :- Min =< Max.
between(Min, Max, N) :-
    Min < Max,
    Next is Min + 1,
    between(Next, Max, N).

alphabet_list(N, List) :-
    N > 0,
    findall(Char, (between(1, N, X), nth1(X, ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'], Char)), List).


% valid_moves(+Board, -EmptyCells)
% Encontra as coordenadas de todas as células vazias no tabuleiro (de acordo com letras e números no tabuleiro)
valid_moves(Board, ListOfMoves) :-
    % Identificar as letras e números do tabuleiro
    nth0(0, Board, HeaderRow), % Primeira linha contém os números das colunas
    exclude(=([]), HeaderRow, Numbers), % Remove elementos vazios da linha de cabeçalho
    tail(Board, Rows), % Remove a linha de cabeçalho do resto do tabuleiro
    findall([Letra, Numero],
        (   nth0(RowIndex, Rows, Row),   % Iterar sobre as linhas do tabuleiro
            nth0(ColIndex, Row, Cell),  % Iterar sobre as colunas
            Cell = ' ',                 % Verificar se a célula está vazia
            nth0(RowIndex, Rows, [Letra|_]), % Obter a letra da linha atual
            nth0(ColIndex, HeaderRow, Numero) % Obter o número da coluna
        ),
        ListOfMoves).

% tail(+List, -Tail)
% Retorna o resto da lista, ignorando o primeiro elemento
tail([_|T], T).


% remove o cabeçalho do tabuleiro
remove_header(Board, NewBoard):-
    Board=[_|BoardAux],
    transpose(BoardAux,TransposeBoard),
    TransposeBoard=[_|NewBoard].




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
create_filled_board(Rows, Cols, Tabuleiro) :-
    num_list(1, Rows, RowNumbers),
    alphabet_list(Rows, Letras),
    Tabuleiro = [[' ' | RowNumbers] | Linhas],
    create_filled_rows(Letras, Cols, Linhas).

% Cria as linhas preenchidas com símbolos alternados.
create_filled_rows([], _, []).
create_filled_rows([Letra | RestLetras], Cols, [[Letra | Row] | RestRows]) :-
    fill_row(Cols, Row, x),  % Começa preenchendo com 'x'.
    create_filled_rows(RestLetras, Cols, RestRows).

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

% clear_console/0
% Clears console
clear_console:- 
    write('\33\[2J').
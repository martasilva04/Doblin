:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(system)).

% Predicado principal do jogo
play :-
    write('=== Bem-vindo ao Jogo ==='), nl,
    game_menu.

% Exibe o menu do jogo e configura as opções
game_menu :-
    write('1. Jogar (H/H)'), nl,
    write('2. Jogar (H/PC)'), nl,
    write('3. Jogar (PC/H)'), nl,
    write('4. Jogar (PC/PC)'), nl,
    write('Escolha uma opcao: '),
    get_valid_option(Type, [1,2,3,4]),
    handle_option(Type, Difficulty1, Difficulty2),
    write('Escolha o tamanho do tabuleiro: '), nl,
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

% Configura o jogo de acordo com os tipos dos jogadores
setup_game(Size, Difficulty1, Difficulty2) :- 
    write('Configurando o jogo...'), nl,
    GameConfig = [board_size(Size), Difficulty1, Difficulty2],
    initial_state(GameConfig, InitialGameState),
    game_loop(InitialGameState).

get_valid_option(Number, Valid) :-
    get_char(Option),
    get_char(Pending),            % Lê o próximo caractere no buffer
    (   Pending \= '\n'           % Verifica se a entrada contém mais de um caractere
    ->  write('Invalid Option! Input must be a single character.'), nl,
        clear_input_buffer,       % Limpa o buffer para evitar problemas
        get_valid_option(Number, Valid)
    ;   char_code(Option, Code),
        NumberTemp is Code - 48,  % Converte o caractere para número
        (   member(NumberTemp, Valid)  % Verifica se a opção é válida
        ->  Number = NumberTemp
        ;   length(Valid, Size),
            format('Invalid Option! Choose Number (1-~w): ', Size), nl,
            get_valid_option(Number, Valid)
        )
    ).

% Limpa o restante do buffer de entrada
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
    choose_move(GameState, Coordenadas),
    move(GameState, Coordenadas, NewGameState),
    game_loop(NewGameState).

choose_move(game_state(B1, B2, P, S, D1, D2), Move):-
    write('Your turn: '), write(P), nl,
    format('Place a ~w', S), nl,
    (P = player1 -> Difficulty = D1; Difficulty = D2),
    (   Difficulty = 0 -> % Jogador humano
        write(P), nl,
        read_input(Move, game_state(B1, B2, P, S, D1, D2))
    ;   Difficulty = 1 -> % Bot aleatório
        write('Bot evaluating options...'),
        sleep(3),
        valid_moves(B1, ValidMoves),
        random_member(Move, ValidMoves)
    ;   Difficulty = 2 -> % Bot Greedy
        write('Bot evaluating options...'),
        sleep(3),
        greedy_move(game_state(B1, B2, P, S, D1, D2), Move)
    ).

%greedy bot 

adjacent_or_diagonal(game_state(B1, B2, player2, S, _D1, _D2), Move) :-
    move(game_state(B1, B2, player2, S), Move, game_state(NewB1, NewB2, NextPlayer, NextSymbol, _D1, _D2)), % Simulate the move.
    adjacent_or_diagonal_check(NewB2, Move, S).

adjacent_or_diagonal(game_state(B1, B2, player1, S, _D1, _D2), Move) :-
    move(game_state(B1, B2, player1, S), Move, game_state(NewB1, NewB2, NextPlayer, NextSymbol, _D1, _D2)), % Simulate the move.
    adjacent_or_diagonal_check(NewB2, Move, S).


% Check if the move places a symbol adjacent or diagonally near an existing symbol.
adjacent_or_diagonal_check(Board, Move, S) :-
    % Extract the coordinates of the move.
    coordenadas_para_indices_segundo(Move, Board, X, Y),
    % Check for adjacency or diagonal proximity.
    (
        adjacent(X, Y, X1, Y1),
        %write('X: '),write(X1),nl,write('Y: '),write(Y1),nl,

        symbol_at(Board, X1, Y1, S)
    ).


% Define adjacency and diagonal positions.
adjacent(X, Y, X1, Y1) :-
    member(DX, [-1, 0, 1]), % Diferença para X.
    member(DY, [-1, 0, 1]), % Diferença para Y.
    (DX \= 0; DY \= 0),     % Exclui (0, 0), ou seja, a mesma posição.
    X1 is X + DX,           % Calcula X1.
    Y1 is Y + DY.

symbol_at(Board, X, Y, Symbol) :-
    nth0(X, Board, Row), 
    %write(Row),nl,     % Get the Y-th row (1-based indexing).
    nth0(Y, Row, Symbol).
    %,write(S), nl,
    %S=Symbol.


greedy_move(game_state(B1, B2, player2, S, _D1, _D2), Move):-
    valid_moves(B2, ValidMoves),
    exclude(adjacent_or_diagonal(game_state(B1, B2, player2, S, _D1, _D2)), ValidMoves, GreedyMoves),
    get_next_symbol(S, NewSymbol),
    %se tiver 3 de um simbolo mete o outro simbolo para 'tapar'
    findall(
        Value-M,
        (
            member(M, ValidMoves),
            move(game_state(B1, B2, player2, NewSymbol, _D1, _D2), M, game_state(NewB1, NewB2, NextPlayer, NextSymbol, _D1, _D2)),
            value(NextGameState, player2, Value)
        ),
        ScoreMoves1
    ),

    board_score(B2, CurrentScore),
    max_member(Score1-Move1, ScoreMoves1),
    (  
        Score1 > CurrentScore -> Move = Move1  
    ;   
        (   % Caso contrário, escolhe de GreedyMoves o que da mais pontos ao adversário
            GreedyMoves \= [] ->  
            findall(  
                ValueOpponent-M,  
                (  
                    member(M, GreedyMoves),  
                    move(game_state(B1, B2, player2, S, _D1, _D2), M, game_state(NewB1, NewB2, NextPlayer, NextSymbol, _D1, _D2)),  
                    value(NextGameState, player1, ValueOpponent) 
                ),  
                Score1Moves2  
            ),  
            min_member(_-Move, Score1Moves2)  
        ;   % Caso contrário, utiliza a lógica para menos pontos possiveis.  
            findall(  
                MinValue-M,  
                (  
                    member(M, ValidMoves),  
                    move(game_state(B1, B2, player2, S, _D1, _D2), M, game_state(NewB1, NewB2, NextPlayer, NextSymbol, _D1, _D2)),  
                    value(NextGameState, player1, MinValue)
                ),  
                ScoreMoves2  
            ),  
            min_member(_-Move, ScoreMoves2)  
        )  
    ).  


greedy_move(game_state(B1, B2, player1, S, _D1, _D2), Move):-
    valid_moves(B1, ValidMoves),
    exclude(adjacent_or_diagonal(game_state(B1, B2, player1, S, _D1, _D2)), ValidMoves, GreedyMoves),
    get_next_symbol(S, NewSymbol),
    %se tiver 3 de um simbolo mete o outro simbolo para 'tapar'
    findall(
        Value-M,
        (
            member(M, ValidMoves),
            move(game_state(B1, B2, player1, NewSymbol, _D1, _D2), M, NextGameState),
            value(NextGameState, player1, Value)
        ),
        ScoreMoves1
    ),

    board_score(B1, CurrentScore),
    max_member(Score1-Move1, ScoreMoves1),
    write(Score1),nl,
    (  
        Score1 > CurrentScore -> Move = Move1  
    ;   
        (   % Caso contrário, escolhe de GreedyMoves o que da mais pontos ao adversário
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
        ;   % Caso contrário, utiliza a lógica para menos pontos possiveis.  
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
    ).  

%o game state é tanto melhor quantos menos pontos tiver o player e mais o adversario, o estado do jogo é mais positivo quanto menor for o value
value(game_state(B1, B2, P, S, _D1, _D2), player1, Value):-
    board_score(B1, ScoreB1),
    board_score(B2, ScoreB2),
    (
        ScoreB2 = 0 -> Value is ScoreB1 / 0.5
        ;
        Value is ScoreB1 / ScoreB2
    ).

value(game_state(B1, B2, P, S, _D1, _D2), player2, Value):-
    board_score(B1, ScoreB1),
    board_score(B2, ScoreB2),
    (
        ScoreB1 = 0 -> Value is ScoreB2 / 0.5
        ;
        Value is ScoreB2 / ScoreB1
    ).

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
    calcular_pontos(Board, x, ScoreX),
    calcular_pontos(Board, o, ScoreO),
    Score is ScoreX+ScoreO.

game_over(game_state(T1, T2, _P, _S, _D1, _D2), Winner):-
    board_completed(T1),
    calcular_pontos(T1, x, ScoreX1),
    calcular_pontos(T1, o, ScoreO1),
    ScoreB1 is ScoreX1+ScoreO1,

    calcular_pontos(T2, x, ScoreX2),
    calcular_pontos(T2, o, ScoreO2),
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
create_lettered_rows([Letra | RestoLetras], Cols, [[Letra | LinhaVazia] | RestoLinhas]) :-
    create_empty_row(Cols, LinhaVazia), % Cria uma linha vazia para cada letra.
    create_lettered_rows(RestoLetras, Cols, RestoLinhas).

% Cria uma linha vazia
create_empty_row(0, []) :- !.
create_empty_row(Cols, [' '|Rest]) :-
    Cols > 0,
    NextCols is Cols - 1,
    create_empty_row(NextCols, Rest).

% Gera o tabuleiro embaralhado de acordo com o tamanho (6x6 ou 8x8)
create_shuffle_board(Rows, Cols, Board) :-
    % Cria listas de números e letras conforme o tamanho do tabuleiro
    num_list(1, Cols, Colunas),
    alphabet_list(Rows, Letras),
    
    % Embaralha as colunas e as letras
    random_permutation(Colunas, ColunasEmbaralhadas),
    random_permutation(Letras, LetrasEmbaralhadas),
    
    % Gera a linha do cabeçalho (números embaralhados)
    Board = [[' ' | ColunasEmbaralhadas] | LinhasEmbaralhadas],
    
    % Gera as linhas restantes com as letras embaralhadas
    create_lettered_rows(LetrasEmbaralhadas, Cols, LinhasEmbaralhadas).




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
move(game_state(BoardInicial, BoardEmbaralhado, CurrentPlayer, Symbol, _D1, _D2), Move, game_state(NewBoardInicial, NewBoardEmbaralhado, NextPlayer,NewSymbol, _D1, _D2)) :-
    coordenadas_para_indices_segundo(Move, BoardInicial, LinhaIndex1, ColunaIndex1),
    atualizar_tabuleiro(BoardInicial, LinhaIndex1, ColunaIndex1, Symbol, NewBoardInicial),
    coordenadas_para_indices_segundo(Move, BoardEmbaralhado, LinhaIndex2, ColunaIndex2),
    atualizar_tabuleiro(BoardEmbaralhado, LinhaIndex2, ColunaIndex2, Symbol, NewBoardEmbaralhado),
    get_next_symbol(Symbol,NewSymbol),
    next_player(CurrentPlayer, Symbol, NextPlayer).

% coordenadas_para_indices(+Coordenadas, -Linha, -Coluna)
% Converte as coordenadas fornecidas pelo jogador em índices para o primeiro tabuleiro
coordenadas_para_indices([Letra, Numero], LinhaIndex, ColunaIndex) :-
    char_code(Letra, LetraCode),
    LinhaIndex is LetraCode - 65+1,  % A=65 na tabela ASCII, portanto subtraímos 65
    ColunaIndex is Numero.    % O número já está no formato 1-6, mas precisamos de 0-5

% Função para extrair as letras das linhas
extrair_letras(Tabuleiro, Letras) :-
    maplist(nth1(1), Tabuleiro, Letras).  % Pega a primeira letra de cada linha

% Função para gerar os números das colunas
gerar_colunas([H|Tabuleiro], H).

% coordenadas_para_indices_segundo(+Coordenadas, +BoardEmbaralhado, -Linha, -Coluna)
% Converte as coordenadas (ex.: "A1") para os índices correspondentes no tabuleiro embaralhado.
coordenadas_para_indices_segundo([Letra, Numero], BoardEmbaralhado, Linha, Coluna) :-
    extrair_letras(BoardEmbaralhado, LetrasEmbaralhadas),
    gerar_colunas(BoardEmbaralhado, ColunasEmbaralhadas),
    nth0(Linha, LetrasEmbaralhadas, Letra),  % Encontra o índice da letra embaralhada
    nth0(Coluna, ColunasEmbaralhadas, Numero).

celula_vazia(Tabuleiro, LinhaIndex, ColunaIndex) :-
    nth0(LinhaIndex, Tabuleiro, LinhaAtual),
    nth0(ColunaIndex, LinhaAtual, Valor),
    Valor = ' '.  % A célula está vazia se contém um espaço em branco

% atualizar_tabuleiro(+Board, +Linha, +Coluna, +Simbolo, -NewBoard)
% Atualiza uma célula específica em um tabuleiro.
atualizar_tabuleiro(Board, Linha, Coluna, Simbolo, NewBoard) :-
    nth0(Linha, Board, LinhaBoard, OutBoard),
    nth0(Coluna, LinhaBoard, _, LinhaOut),
    nth0(Coluna, LinhaUpdated, Simbolo, LinhaOut),
    nth0(Linha, NewBoard, LinhaUpdated, OutBoard).


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


read_input([Letra, Numero], game_state(B1, _B2, _P, _S, _D1, _D2)) :-
    length(B1, Size),
    Size1 is Size - 1,
    alphabet_list(Size1, AlphaList),
    AlphaList = [FirstLetter|_],
    last(_, LastLetter, AlphaList),
    num_list(1, Size1, NumList),
    NumList = [FirstNumber|_],
    last(_, LastNumber, NumList),
    repeat, 
    get_valid_letter(AlphaList, FirstLetter, LastLetter, Letra),
    format('Choose number (~w-~w): ', [FirstNumber, LastNumber]),
    get_valid_option(Numero, NumList),
    coordenadas_para_indices_segundo([Letra, Numero], B1, LinhaIndex, ColunaIndex),
    (   celula_vazia(B1, LinhaIndex, ColunaIndex) 
    ->  ! 
    ;   write('Invalid! This cell is ocupied.'), nl,
        fail  
    ).

    

get_valid_letter(AlphaList, FirstLetter, LastLetter, Letra) :-
    format('Choose letter (~w-~w): ', [FirstLetter, LastLetter]),
    get_char(LetraTemp),           
    get_char(Pending),              
    (   Pending \= '\n'            
    ->  write('Invalid input! Input must be a single letter.'), nl,
        clear_input_buffer,        
        get_valid_letter(AlphaList, FirstLetter, LastLetter, Letra)
    ;   (   member(LetraTemp, AlphaList)  
        ->  Letra = LetraTemp            
        ;   write('Letra inválida! A letra deve ser válida.'), nl,
            get_valid_letter(AlphaList, FirstLetter, LastLetter, Letra)
        )
    ).


get_valid_number(NumList, FirstNumber, LastNumber, Numero) :-
    format('Choose number (~w-~w): ', [FirstNumber, LastNumber]),
    get_char(NumChar),            
    get_char(_),                  
    char_code(NumChar, NumCode),
    NumeroTemp is NumCode - 48,       
    (   member(NumeroTemp, NumList)  
    ->  Numero = NumeroTemp                    
    ;   write('Número inválido! O número deve ser válido.'), nl,
        get_valid_number(NumList, FirstNumber, LastNumber, Numero) 
    ).

% Gera uma lista de números de 1 até N
num_list(M, M, [M]).  % Caso base: quando M é igual a N
num_list(M, N, [M|Rest]) :-
    M < N,
    M1 is M + 1,
    num_list(M1, N, Rest).


% calcular pontos 

% Calcula os pontos de um tabuleiro
calcular_pontos(Tabuleiro, Simbolo, Pontos) :-
    remove_header(Tabuleiro, NewBoard),
    % Calcula pontos de linhas horizontais
    findall(1, linha_completa(NewBoard, Simbolo), LinhasPontos),
    % Calcula pontos de colunas verticais
    findall(1, coluna_completa(NewBoard, Simbolo), ColunasPontos),
    % Calcula pontos de quadrados 2x2
    findall(1, quadrado_completo(NewBoard, Simbolo), QuadradosPontos),
    % Calcula pontos de diagonais
    findall(1, diagonal_completa(NewBoard, Simbolo), DiagonaisPontos),
    % Soma todos os pontos
    length(LinhasPontos, PontosLinhas),
    length(ColunasPontos, PontosColunas),
    length(QuadradosPontos, PontosQuadrados),
    length(DiagonaisPontos, PontosDiagonais),
    Pontos is PontosLinhas + PontosColunas + PontosQuadrados + PontosDiagonais.

% Verifica se uma linha está completa
linha_completa(Tabuleiro, Simbolo) :-
    member(Linha, Tabuleiro),
    sublist([Simbolo, Simbolo, Simbolo, Simbolo], Linha).
    

% Verifica se uma coluna está completa
coluna_completa(Tabuleiro, Simbolo) :-
    transpose(Tabuleiro, TabuleiroTransposto),
    linha_completa(TabuleiroTransposto, Simbolo).

% Verifica se um quadrado 2x2 está completo
quadrado_completo(Tabuleiro, Simbolo) :-
    length(Tabuleiro,Size),
    between(1, Size, Linha), 
    between(1, Size, Coluna), 
    nth1(Linha, Tabuleiro, Linha1),
    nth1(Coluna, Linha1, Simbolo),
    Linha2Index is Linha + 1,
    nth1(Linha2Index, Tabuleiro, Linha2),
    Coluna2Index is Coluna + 1,
    nth1(Coluna, Linha2, Simbolo),
    nth1(Coluna2Index, Linha1, Simbolo),
    nth1(Coluna2Index, Linha2, Simbolo).

% Verifica se uma diagonal de 4 símbolos está completa
diagonal_completa(Tabuleiro, Simbolo) :-
    % Diagonais normais (de cima para baixo, da esquerda para a direita)
    diagonal_normal(Tabuleiro, Simbolo);
    % Diagonais invertidas (de cima para baixo, da direita para a esquerda)
    diagonal_invertida(Tabuleiro, Simbolo).

% Verifica se uma diagonal normal (esquerda para direita) está completa
diagonal_normal(Tabuleiro, Simbolo) :-
    length(Tabuleiro, Size), 
    Max is Size - 3, 
    between(1, Max, Linha), 
    between(1, Max, Coluna), 
    nth1(Linha, Tabuleiro, Linha1),
    nth1(Coluna, Linha1, Simbolo),
    Linha2 is Linha + 1,
    Coluna2 is Coluna + 1,
    nth1(Linha2, Tabuleiro, Linha2_),
    nth1(Coluna2, Linha2_, Simbolo),
    Linha3 is Linha + 2,
    Coluna3 is Coluna + 2,
    nth1(Linha3, Tabuleiro, Linha3_),
    nth1(Coluna3, Linha3_, Simbolo),
    Linha4 is Linha + 3,
    Coluna4 is Coluna + 3,
    nth1(Linha4, Tabuleiro, Linha4_),
    nth1(Coluna4, Linha4_, Simbolo).

% Verifica se uma diagonal invertida (direita para esquerda) está completa
diagonal_invertida(Tabuleiro, Simbolo) :-
    length(Tabuleiro, Size), 
    Max is Size - 3,
    between(1, Max, Linha), % As diagonais invertidas começam de 1 a 3
    between(4, Size, Coluna), % As diagonais invertidas começam de 4 a 6
    nth1(Linha, Tabuleiro, Linha1),
    nth1(Coluna, Linha1, Simbolo),
    Linha2 is Linha + 1,
    Coluna2 is Coluna - 1,
    nth1(Linha2, Tabuleiro, Linha2_),
    nth1(Coluna2, Linha2_, Simbolo),
    Linha3 is Linha + 2,
    Coluna3 is Coluna - 2,
    nth1(Linha3, Tabuleiro, Linha3_),
    nth1(Coluna3, Linha3_, Simbolo),
    Linha4 is Linha + 3,
    Coluna4 is Coluna - 3,
    nth1(Linha4, Tabuleiro, Linha4_),
    nth1(Coluna4, Linha4_, Simbolo).

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
        ['E', 'x', 'o', 'x', 'x', 'o', 'x'], % Linha 1
        ['C', 'o', 'o', 'o', 'x', 'o', 'x'], % Linha 2
        ['F', ' ', 'o', 'x', 'x', 'x', 'o'], % Linha 3
        ['A', 'o', 'o', 'x', 'o', 'x', 'x'], % Linha 4
        ['B', 'x', 'o', 'x', 'x', 'o', 'x'], % Linha 5
        ['D', 'x', 'o', 'x', 'x', 'o', 'x']  % Linha 6
    ],
    %valid_moves(Board, EmptyCells).

    valid_moves(Board, ValidMoves),
    write(ValidMoves),nl,
    exclude(adjacent_or_diagonal(game_state(Board, Board, player2, 'x')), ValidMoves, GreedyMoves),
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
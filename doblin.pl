:- use_module(library(random)).
:- use_module(library(lists)).

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
    get_valid_option(Number, [1,2,3,4]),
    handle_option(Number).

% Trata a escolha do menu
handle_option(1) :- setup_game(human, human).
handle_option(2) :- setup_game(human, computer).
handle_option(3) :- setup_game(computer, human).
handle_option(4) :- setup_game(computer, computer).
handle_option(_) :-
    write('Opcao invalida. Tente novamente.'), nl,
    game_menu.

% Configura o jogo de acordo com os tipos dos jogadores
setup_game(Player1Type, Player2Type) :-
    write('Escolha o tamanho do tabuleiro: '), nl,
    write('1. 6x6'), nl,
    write('2. 8x8'), nl,
    get_valid_option(Number, [1,2]), 
    write('Configurando o jogo...'), nl,
    GameConfig = [player1(Player1Type), player2(Player2Type), board_size(Number)],
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



game_loop(GameState):-
    display_game(GameState),
    game_over(GameState), !.
game_loop(GameState):-
    read_input(Coordenadas,GameState),
    move(GameState, Coordenadas, NewGameState),
    game_loop(NewGameState).

game_over(game_state(T1, T2, P)):-
    board_completed(T1),
    write('Fim de jogo! Calculando pontos...'), nl,
    calcular_pontos(T1, x, ScoreX1),
    calcular_pontos(T1, o, ScoreO1),
    ScoreB1 is ScoreX1+ScoreO1,
    write('Score Board1 (Player1): '), write(ScoreB1), nl,

    calcular_pontos(T2, x, ScoreX2),
    calcular_pontos(T2, o, ScoreO2),
    ScoreB2 is ScoreX2+ScoreO2,
    write('Score Board2 (Player2): '), write(ScoreB2), nl,

    (   
        ScoreB1 < ScoreB2 ->
        write('Player 1 (X) won!')
    ;   
        ScoreB1 > ScoreB2 ->
        write('Player 2 (O) won!')
    ;   
        write('Draw')
    ).

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

% Predicado initial_state/2
% Configura o estado inicial do jogo e gera dois tabuleiros (normal e embaralhado)
initial_state(GameConfig, game_state(Board1, Board2, player1)) :-
    member(board_size(Option), GameConfig),
    get_board_size(Option, Rows, Cols),
    
    % Gera o tabuleiro inicial
    create_shuffle_board(Rows, Cols, Board1),
    
    % Gera o tabuleiro embaralhado
    create_shuffle_board(Rows, Cols, Board2).





% Determina o número de linhas e colunas com base na opção escolhida
get_board_size(1, 6, 6). % Opção 1 -> Tabuleiro 6x6
get_board_size(2, 8, 8). % Opção 2 -> Tabuleiro 8x8

% Cria um tabuleiro vazio (lista de listas) com o tamanho especificado, incluindo cabeçalho e letras.
/*
create_empty_board(Rows, Cols, Tabuleiro) :-
    % Gera os números do cabeçalho
    numlist(1, Cols, Colunas),
    % Gera as letras para as linhas
    alphabet_list(Rows, Letras),
    % Adiciona o cabeçalho no topo
    Tabuleiro = [[' ' | Colunas] | Linhas],
    % Cria as linhas com letras e células vazias
    create_lettered_rows(Letras, Cols, Linhas). 
*/

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
display_game(game_state(Board1, Board2, CurrentPlayer)) :-
    write('Board 1:'), nl,
    display_board(Board1), nl,
    
    write('Board 2:'), nl,
    display_board(Board2), nl,
    
    % Exibe o jogador atual
    write('Your turn: '), write(CurrentPlayer), nl.


% Exibe o tabuleiro
display_board([]).
display_board([Row|Rows]) :-
    write(Row), nl,
    display_board(Rows).


% move(+GameState, +Move, -NewGameState)
% Atualiza o estado do jogo com base no movimento do jogador.
move(game_state(BoardInicial, BoardEmbaralhado, CurrentPlayer), Move, game_state(NewBoardInicial, NewBoardEmbaralhado, NextPlayer)) :-
    % Converte o movimento (letra e número) para índices.
    coordenadas_para_indices(Move, LinhaIndex, ColunaIndex),
    
    % Verifica se a célula no tabuleiro inicial está vazia.
    celula_vazia(BoardInicial, LinhaIndex, ColunaIndex),
    % Determina o símbolo do jogador atual.
    simbolo_jogador(CurrentPlayer, Simbolo),
    % Atualiza o tabuleiro inicial.
    atualizar_tabuleiro(BoardInicial, LinhaIndex, ColunaIndex, Simbolo, NewBoardInicial),
    % Converte as coordenadas para o tabuleiro embaralhado.
    coordenadas_para_indices_segundo(Move, BoardEmbaralhado, LinhaIndex2, ColunaIndex2),
    % Atualiza o tabuleiro embaralhado.
    atualizar_tabuleiro(BoardEmbaralhado, LinhaIndex2, ColunaIndex2, Simbolo, NewBoardEmbaralhado),
    % Alterna para o próximo jogador.
    proximo_jogador(CurrentPlayer, NextPlayer).

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
simbolo_jogador(player1, x).
simbolo_jogador(player2, o).

% proximo_jogador(+CurrentPlayer, -NextPlayer)
% Alterna o jogador atual.
proximo_jogador(player1, player2).
proximo_jogador(player2, player1).




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



read_input([Letra, Numero], game_state(B1, B2, P)) :-
    length(B1, Size),
    Size1 is Size - 1,
    alphabet_list(Size1, AlphaList),
    AlphaList = [FirstLetter|_],
    last(_, LastLetter, AlphaList),
    get_valid_letter(AlphaList, FirstLetter, LastLetter, Letra), 
    num_list(1, Size1, NumList),
    NumList = [FirstNumber|_],
    last(_, LastNumber, NumList),
    format('Choose number (~w-~w): ', [FirstNumber, LastNumber]),
    get_valid_option(Numero, NumList). 

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
    % Calcula pontos de linhas horizontais
    findall(1, linha_completa(Tabuleiro, Simbolo), LinhasPontos),
    % Calcula pontos de colunas verticais
    findall(1, coluna_completa(Tabuleiro, Simbolo), ColunasPontos),
    % Calcula pontos de quadrados 2x2
    findall(1, quadrado_completo(Tabuleiro, Simbolo), QuadradosPontos),
    % Calcula pontos de diagonais
    findall(1, diagonal_completa(Tabuleiro, Simbolo), DiagonaisPontos),
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
    between(1, 5, Linha), % Verifica as posições das linhas (1 a 5)
    between(1, 5, Coluna), % Verifica as posições das colunas (1 a 5)
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
    between(1, 3, Linha), % As diagonais normais começam de 1 a 3
    between(1, 3, Coluna), % As diagonais normais começam de 1 a 3
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
    between(1, 3, Linha), % As diagonais invertidas começam de 1 a 3
    between(4, 6, Coluna), % As diagonais invertidas começam de 4 a 6
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


alphabet_list(N, List) :-
    N > 0,
    findall(Char, (between(1, N, X), nth1(X, ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'], Char)), List).
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
    get_char(Option),  % Usa get_char para ler a opção como caractere
    get_char(_),
    char_code(Option, Code),  % Converte o caractere para código ASCII
    Number is Code - 48,  % Converte o código ASCII para o número correspondente
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
    get_char(Option),
    get_char(_),
    char_code(Option, Code),
    Number is Code-48,
    write('Configurando o jogo...'), nl,
    GameConfig = [player1(Player1Type), player2(Player2Type), board_size(Number)],
    initial_state(GameConfig, InitialGameState),
    display_game(InitialGameState),
    write('Jogo configurado. Pronto para iniciar!'), nl,
    ler_entrada(Coordenadas),
    write('pesca1'),
    move(InitialGameState, Coordenadas, NewGameState),
    write('pesca2'),
    display_game(NewGameState).


% Predicado initial_state/2
% Configura o estado inicial do jogo e gera dois tabuleiros (normal e embaralhado)
initial_state(GameConfig, game_state(BoardInicial, BoardEmbaralhado, player1)) :-
    member(board_size(Option), GameConfig),
    get_board_size(Option, Rows, Cols),
    
    % Gera o tabuleiro inicial
    create_empty_board(Rows, Cols, BoardInicial),
    
    % Gera o tabuleiro embaralhado
    tabuleiro_embaralhado(Rows, Cols, BoardEmbaralhado).





% Determina o número de linhas e colunas com base na opção escolhida
get_board_size(1, 6, 6). % Opção 1 -> Tabuleiro 6x6
get_board_size(2, 8, 8). % Opção 2 -> Tabuleiro 8x8
get_board_size(_, 3, 3). % Opção padrão -> Tabuleiro 3x3 (fallback)

% Cria um tabuleiro vazio (lista de listas) com o tamanho especificado, incluindo cabeçalho e letras.
create_empty_board(Rows, Cols, Tabuleiro) :-
    % Gera os números do cabeçalho
    numlist(1, Cols, Colunas),
    % Gera as letras para as linhas
    alphabet_list(Rows, Letras),
    % Adiciona o cabeçalho no topo
    Tabuleiro = [[' ' | Colunas] | Linhas],
    % Cria as linhas com letras e células vazias
    create_lettered_rows(Letras, Cols, Linhas).

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
tabuleiro_embaralhado(Rows, Cols, TabuleiroEmbaralhado) :-
    % Cria listas de números e letras conforme o tamanho do tabuleiro
    numlist(1, Cols, Colunas),
    alphabet_list(Rows, Letras),
    
    % Embaralha as colunas e as letras
    random_permutation(Colunas, ColunasEmbaralhadas),
    random_permutation(Letras, LetrasEmbaralhadas),
    
    % Gera a linha do cabeçalho (números embaralhados)
    TabuleiroEmbaralhado = [[' ' | ColunasEmbaralhadas] | LinhasEmbaralhadas],
    
    % Gera as linhas restantes com as letras embaralhadas
    gerar_linhas_embaralhadas(LetrasEmbaralhadas, Cols, LinhasEmbaralhadas).

% Cria as linhas embaralhadas
gerar_linhas_embaralhadas([], _, []). % Caso base: sem letras, sem linhas.
gerar_linhas_embaralhadas([Letra|RestoLetras], Cols, [[Letra | LinhaVazia] | RestoLinhas]) :-
    create_empty_row(Cols, LinhaVazia), % Cria uma linha vazia com o número correto de colunas.
    gerar_linhas_embaralhadas(RestoLetras, Cols, RestoLinhas).



% Cria a lista de letras do tabuleiro
alphabet_list(6, List) :- List = ['A', 'B', 'C', 'D', 'E', 'F'].
alphabet_list(8, List) :- List = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'].

% display_game(+GameState)
% Exibe o estado atual do jogo com base no GameState atualizado.
display_game(game_state(BoardInicial, BoardEmbaralhado, CurrentPlayer)) :-
    write('Estado atual do jogo:'), nl,
    
    % Exibe o tabuleiro inicial
    write('Tabuleiro Inicial:'), nl,
    display_board(BoardInicial), nl,
    
    % Exibe o tabuleiro embaralhado
    write('Tabuleiro Embaralhado:'), nl,
    display_board(BoardEmbaralhado), nl,
    
    % Exibe o jogador atual
    write('Jogador atual: '), write(CurrentPlayer), nl.


% Exibe o tabuleiro
display_board([]).
display_board([Row|Rows]) :-
    write(Row), nl,
    display_board(Rows).




% move(+GameState, +Move, -NewGameState)
% Atualiza o estado do jogo com base no movimento do jogador.
move(game_state(BoardInicial, BoardEmbaralhado, CurrentPlayer), Move, game_state(NewBoardInicial, NewBoardEmbaralhado, NextPlayer)) :-
    % Converte o movimento (letra e número) para índices.
    write(Move),nl,
    coordenadas_para_indices(Move, LinhaIndex, ColunaIndex),
    write('cheguei'),nl,
    
    % Verifica se a célula no tabuleiro inicial está vazia.
    celula_vazia(BoardInicial, LinhaIndex, ColunaIndex),
    write('pesca4'),
    % Determina o símbolo do jogador atual.
    simbolo_jogador(CurrentPlayer, Simbolo),
    write('pesca5'),
    % Atualiza o tabuleiro inicial.
    atualizar_tabuleiro(BoardInicial, LinhaIndex, ColunaIndex, Simbolo, NewBoardInicial),
    write('pesca6'),
    % Converte as coordenadas para o tabuleiro embaralhado.
    coordenadas_para_indices_segundo(Move, BoardEmbaralhado, LinhaIndex2, ColunaIndex2),
    write('pesca7'),
    % Atualiza o tabuleiro embaralhado.
    atualizar_tabuleiro(BoardEmbaralhado, LinhaIndex2, ColunaIndex2, Simbolo, NewBoardEmbaralhado),
    write('pesca8'),
    % Alterna para o próximo jogador.
    write('pesca9'),
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
    write(LetrasEmbaralhadas),
    gerar_colunas(BoardEmbaralhado, ColunasEmbaralhadas),
    write(ColunasEmbaralhadas),
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



ler_entrada([Letra, Numero]) :-
    write('Digite a letra (A-F): '),
    get_char(Letra),       % Lê um caractere para a linha
    get_char(_),           % Descarta o '\n' pendente no buffer
    (   member(Letra, ['A', 'B', 'C', 'D', 'E', 'F'])  % Verifica se a letra está dentro de A-F
    ->  true
    ;   write('Letra inválida! A letra deve ser entre A e F. Tente novamente.'), nl,
        ler_entrada([Letra, Numero])  % Se a letra for inválida, pede a entrada novamente
    ),
    
    write('Digite o número (1-6): '),
    get_char(NumChar),     % Lê o número como caractere
    get_char(_),           % Descarta o '\n' pendente no buffer
    char_code(NumChar, NumCode),
    Numero is NumCode - 48,  % Converte o caractere do número para inteiro

    (   Numero >= 1, Numero =< 6  % Verifica se o número está dentro do intervalo 1-6
    ->  true, write('deu')
    ;   write('Número inválido! O número deve ser entre 1 e 6. Tente novamente.'), nl,
        ler_entrada([Letra, Numero])  % Se o número for inválido, pede a entrada novamente
    ).

% Gera uma lista de números de 1 até N
numlist(M, M, [M]).  % Caso base: quando M é igual a N
numlist(M, N, [M|Rest]) :-
    M < N,
    M1 is M + 1,
    numlist(M1, N, Rest).

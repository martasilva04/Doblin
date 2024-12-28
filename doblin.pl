:- use_module(library(random)).
:- use_module(library(lists)).

% Gera o tabuleiro 6x6 inicial com letras e números na ordem correta
tabuleiro_inicial(Tabuleiro) :-
    Tabuleiro = [
        [' ', 1, 2, 3, 4, 5, 6],  % Cabeçalho com os números
        ['A', ' ', ' ', ' ', ' ', ' ', ' '],  % Linha A
        ['B', ' ', ' ', ' ', ' ', ' ', ' '],  % Linha B
        ['C', ' ', ' ', ' ', ' ', ' ', ' '],  % Linha C
        ['D', ' ', ' ', ' ', ' ', ' ', ' '],  % Linha D
        ['E', ' ', ' ', ' ', ' ', ' ', ' '],  % Linha E
        ['F', ' ', ' ', ' ', ' ', ' ', ' ']   % Linha F
    ].

% Embaralha tanto as colunas quanto as letras das linhas
tabuleiro_embaralhado(TabuleiroEmbaralhado, LetrasEmbaralhadas, ColunasEmbaralhadas) :-
    random_permutation([1, 2, 3, 4, 5, 6], ColunasEmbaralhadas),  % Embaralha os números das colunas
    random_permutation(['A', 'B', 'C', 'D', 'E', 'F'], LetrasEmbaralhadas),  % Embaralha as letras das linhas
    TabuleiroEmbaralhado = [
        [' ', C1, C2, C3, C4, C5, C6],  % Cabeçalho com as colunas embaralhadas
        [L1, ' ', ' ', ' ', ' ', ' ', ' '],  % Linha A (com letra embaralhada)
        [L2, ' ', ' ', ' ', ' ', ' ', ' '],  % Linha B (com letra embaralhada)
        [L3, ' ', ' ', ' ', ' ', ' ', ' '],  % Linha C (com letra embaralhada)
        [L4, ' ', ' ', ' ', ' ', ' ', ' '],  % Linha D (com letra embaralhada)
        [L5, ' ', ' ', ' ', ' ', ' ', ' '],  % Linha E (com letra embaralhada)
        [L6, ' ', ' ', ' ', ' ', ' ', ' ']   % Linha F (com letra embaralhada)
    ],
    % Atribui as letras e números embaralhados
    ColunasEmbaralhadas = [C1, C2, C3, C4, C5, C6],
    LetrasEmbaralhadas = [L1, L2, L3, L4, L5, L6].

% Atualiza uma célula do tabuleiro com o símbolo do jogador
atualizar_tabuleiro(Tabuleiro, Linha, Coluna, Simbolo, NovoTabuleiro) :-
    nth0(Linha, Tabuleiro, LinhaAtual),  % Encontra a linha no tabuleiro
    nth0(Coluna, LinhaAtual, _, RestoLinha),  % Remove o valor antigo da coluna
    nth0(Coluna, NovaLinha, Simbolo, RestoLinha),  % Insere o símbolo na posição
    nth0(Linha, Tabuleiro, _, RestoTabuleiro),  % Remove a linha antiga do tabuleiro
    nth0(Linha, NovoTabuleiro, NovaLinha, RestoTabuleiro).  % Insere a nova linha no tabuleiro

% Lê as coordenadas do usuário
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
    ->  true
    ;   write('Número inválido! O número deve ser entre 1 e 6. Tente novamente.'), nl,
        ler_entrada([Letra, Numero])  % Se o número for inválido, pede a entrada novamente
    ).

% Verifica se a célula está vazia
celula_vazia(Tabuleiro, LinhaIndex, ColunaIndex) :-
    nth0(LinhaIndex, Tabuleiro, LinhaAtual),
    nth0(ColunaIndex, LinhaAtual, Valor),
    Valor = ' '.  % A célula está vazia se contém um espaço em branco

% Converte as coordenadas fornecidas pelo jogador em índices para o primeiro tabuleiro
coordenadas_para_indices([Letra, Numero], LinhaIndex, ColunaIndex) :-
    char_code(Letra, LetraCode),
    LinhaIndex is LetraCode - 65+1,  % A=65 na tabela ASCII, portanto subtraímos 65
    ColunaIndex is Numero.    % O número já está no formato 1-6, mas precisamos de 0-5

% Converte coordenadas em índices para o segundo tabuleiro
coordenadas_para_indices_segundo([Letra, Numero], LetrasEmbaralhadas, ColunasEmbaralhadas, LinhaIndex, ColunaIndex) :-
    nth1(LinhaIndex, LetrasEmbaralhadas, Letra),  % Encontra o índice da letra embaralhada
    nth1(ColunaIndex, ColunasEmbaralhadas, Numero).  % Encontra o índice do número embaralhado

% Imprime um tabuleiro no formato legível
imprimir_tabuleiro([]).
imprimir_tabuleiro([Linha | Resto]) :-
    write(Linha), nl,
    imprimir_tabuleiro(Resto).

tabuleiro_cheio(Tabuleiro) :-
    \+ (member(Linha, Tabuleiro), member(' ', Linha)).

jogar(Tabuleiro1, Tabuleiro2, LetrasEmbaralhadas, ColunasEmbaralhadas, Jogador) :-
    (   tabuleiro_cheio(Tabuleiro1)  % Verifica se o tabuleiro está cheio
    ->  clear_console,
        write('Tabuleiro Final (Inicial):'), nl,
        imprimir_tabuleiro(Tabuleiro1),
        nl,
        write('Tabuleiro Final (Embaralhado):'), nl,
        imprimir_tabuleiro(Tabuleiro2),
        nl,
        write('Fim de jogo!'), nl
    ;   clear_console,
        write('Tabuleiro Atual (Inicial):'), nl,
        imprimir_tabuleiro(Tabuleiro1),
        nl,
        write('Tabuleiro Atual (Embaralhado):'), nl,
        imprimir_tabuleiro(Tabuleiro2),
        nl,
        (   Jogador = 1 -> write('Jogador 1 (X), sua vez:'), nl, Simbolo = 'X'
        ;   Jogador = 2 -> write('Jogador 2 (O), sua vez:'), nl, Simbolo = 'O'
        ),
        ler_entrada(Coordenadas),
        coordenadas_para_indices(Coordenadas, LinhaIndex, ColunaIndex),
        (   celula_vazia(Tabuleiro1, LinhaIndex, ColunaIndex)  % Verifica se a célula está vazia
        ->  atualizar_tabuleiro(Tabuleiro1, LinhaIndex, ColunaIndex, Simbolo, NovoTabuleiro1),
            coordenadas_para_indices_segundo(Coordenadas, LetrasEmbaralhadas, ColunasEmbaralhadas, LinhaIndex2, ColunaIndex2),
            atualizar_tabuleiro(Tabuleiro2, LinhaIndex2, ColunaIndex2, Simbolo, NovoTabuleiro2),
            NovoJogador is (Jogador mod 2) + 1,  % Alterna o jogador
            jogar(NovoTabuleiro1, NovoTabuleiro2, LetrasEmbaralhadas, ColunasEmbaralhadas, NovoJogador)
        ;   write('Célula já ocupada! Tente novamente.'), nl,
            jogar(Tabuleiro1, Tabuleiro2, LetrasEmbaralhadas, ColunasEmbaralhadas, Jogador)
        )
    ).

% Inicia o jogo
jogar :-
    tabuleiro_inicial(Tabuleiro1),
    tabuleiro_embaralhado(Tabuleiro2, LetrasEmbaralhadas, ColunasEmbaralhadas),
    jogar(Tabuleiro1, Tabuleiro2, LetrasEmbaralhadas, ColunasEmbaralhadas, 1).

% clear_console/0
% Clears console
clear_console:- 
    write('\33\[2J').
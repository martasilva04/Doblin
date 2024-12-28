:- use_module(library(random)).
:- use_module(library(lists)).

% Gera o tabuleiro 6x6 inicial com letras e números na ordem correta
tabuleiro_inicial(Tabuleiro) :-
    Tabuleiro = [
        ['.', 1, 2, 3, 4, 5, 6],  % Cabeçalho com os números
        ['A', 'O', 'O', 'O', 'O', 'X', 'X'],  % Linha A
        ['B', 'O', 'O', 'X', 'X', 'X', 'O'],  % Linha B
        ['C', 'X', 'X', 'X', 'O', 'O', 'O'],  % Linha C
        ['D', 'O', 'X', 'X', 'X', 'X', 'X'],  % Linha D
        ['E', 'O', 'O', 'X', 'O', 'X', 'X'],  % Linha E
        ['F', 'O', 'X', 'X', 'X', 'X', 'O']   % Linha F
    ].

% Embaralha tanto as colunas quanto as letras das linhas
tabuleiro_embaralhado(TabuleiroEmbaralhado, LetrasEmbaralhadas, ColunasEmbaralhadas) :-
    random_permutation([1, 2, 3, 4, 5, 6], ColunasEmbaralhadas),  % Embaralha os números das colunas
    random_permutation(['A', 'B', 'C', 'D', 'E', 'F'], LetrasEmbaralhadas),  % Embaralha as letras das linhas
    TabuleiroEmbaralhado = [
        ['.', C1, C2, C3, C4, C5, C6],  % Cabeçalho com as colunas embaralhadas
        [L1, 'O', 'X', 'X', 'O', 'X', 'O'],  % Linha A (com letras embaralhadas)
        [L2, 'X', 'O', 'O', 'X', 'O', 'X'],  % Linha B (com letras embaralhadas)
        [L3, 'X', 'X', 'O', 'O', 'X', 'O'],  % Linha C (com letras embaralhadas)
        [L4, 'O', 'X', 'X', 'X', 'O', 'O'],  % Linha D (com letras embaralhadas)
        [L5, 'X', 'X', 'O', 'O', 'X', 'X'],  % Linha E (com letras embaralhadas)
        [L6, 'O', 'O', 'X', 'X', 'X', 'O']   % Linha F (com letras embaralhadas)
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
        finalizar_jogo(Tabuleiro1, Tabuleiro2)
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

% Finaliza o jogo e calcula a pontuação
finalizar_jogo(Tabuleiro1, Tabuleiro2) :-
    write('Fim de jogo! Calculando pontos...'), nl,
    calcular_pontos(Tabuleiro1, 'X', PontosJogador1),
    calcular_pontos(Tabuleiro1, 'O', PontosJogador2),
    write('Pontuação no Tabuleiro Inicial:'), nl,
    write('Jogador 1 (X): '), write(PontosJogador1), nl,
    write('Jogador 2 (O): '), write(PontosJogador2), nl,

    calcular_pontos(Tabuleiro2, 'X', PontosJogador1Tab2),
    calcular_pontos(Tabuleiro2, 'O', PontosJogador2Tab2),
    write('Pontuação no Tabuleiro Embaralhado:'), nl,
    write('Jogador 1 (X): '), write(PontosJogador1Tab2), nl,
    write('Jogador 2 (O): '), write(PontosJogador2Tab2), nl,

    TotalJogador1 is PontosJogador1 + PontosJogador1Tab2,
    TotalJogador2 is PontosJogador2 + PontosJogador2Tab2,
    write('Pontuação Final:'), nl,
    write('Jogador 1 (X): '), write(TotalJogador1), nl,
    write('Jogador 2 (O): '), write(TotalJogador2), nl,
    (   
        TotalJogador1 < TotalJogador2 ->
        write('Jogador 1 (X) venceu!')
    ;   
        TotalJogador1 > TotalJogador2 ->
        write('Jogador 2 (O) venceu!')
    ;   
        write('Empate')
    ).

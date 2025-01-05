:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(system)).

% play/0
% Predicado principal do jogo
play :-
    write('=== Welcome to Doblin ==='), nl,
    game_menu.

% game_menu/0
% Exibe o menu do jogo e configura as opções.
game_menu :-
    write('1. Play (Human/Human)'), nl,
    write('2. Play (Human/PC)'), nl,
    write('3. Play (PC/Human)'), nl,
    write('4. Play (PC/PC)'), nl,
    write('Choose an option: '),
    get_valid_option(Type, [1,2,3,4]),
    handle_option(Type, Difficulty1, Difficulty2),
    write('Choose the board size: '), nl,
    write('1. 6x6'), nl,
    write('2. 8x8'), nl,
    get_valid_option(Size, [1,2]), 
    setup_game(Size, Difficulty1, Difficulty2).

% handle_option(+Type, -Difficulty1, -Difficulty2)
% O predicado `handle_option/3` lida com as opções de configuração do jogo, baseando-se na escolha do jogador feita no menu principal.
% Ele define as dificuldades dos jogadores de acordo com o tipo de partida selecionado.
handle_option(1, 0, 0). % H/H
handle_option(2, 0, Difficulty2) :- choose_difficulty(player2, Difficulty2). % H/PC
handle_option(3, Difficulty1, 0) :- choose_difficulty(player1, Difficulty1). % PC/H
handle_option(4, Difficulty1, Difficulty2) :- % PC/PC
    choose_difficulty(player1, Difficulty1),
    choose_difficulty(player2, Difficulty2).

% setup_game(+Size, +Difficulty1, +Difficulty2)
% Configura o jogo de acordo com os tipos de jogadores.
setup_game(Size, Difficulty1, Difficulty2) :- 
    write('Setting up the game...'), nl,
    GameConfig = [board_size(Size), Difficulty1, Difficulty2],
    initial_state(GameConfig, InitialGameState),
    game_loop(InitialGameState).

% get_valid_option(+Number, +Valid)
% Esta função solicita ao usuário que insira uma opção válida (um único caractere correspondente a um número).
% A função verifica se a entrada é válida e se o número está na lista de opções permitidas.
% Se a entrada for inválida (mais de um caractere ou um número fora das opções válidas), 
% a função exibe uma mensagem de erro e solicita uma nova entrada até que uma opção válida seja fornecida.
get_valid_option(Number, Valid) :-
    get_char(Option),
    get_char(Pending),            
    (   Pending \= '\n'           
    ->  write('Invalid Option! Input must be a single character.'), nl,
        clear_input_buffer,       
        get_valid_option(Number, Valid)
    ;   char_code(Option, Code),
        NumberTemp is Code - 48,
        (   member(NumberTemp, Valid) 
        ->  Number = NumberTemp
        ;   length(Valid, Size),
            format('Invalid Option! Choose Number (1-~w): ', Size), nl,
            get_valid_option(Number, Valid)
        )
    ).

% clear_input_buffer/0
% Limpa o restante do buffer de entrada
clear_input_buffer :-
    get_char(Char),
    (Char = '\n' -> true ; clear_input_buffer).

% choose_difficulty(+Bot, -Difficulty)
% Solicita ao usuário para escolher o nível de dificuldade do bot.
% A opção válida é atribuída à variável Difficulty.
choose_difficulty(Bot, Difficulty) :-
    format('Please select ~a status:\n', [Bot]),
    write('1 - Random\n'),
    write('2 - Greedy\n'),
    get_valid_option(Option, [1,2]), !,
    Difficulty = Option.

% game_loop(+GameState)
% Esta função representa o loop principal do jogo.
% Ela controla o fluxo do jogo, exibindo o estado atual, verificando se o jogo acabou e processando os movimentos.
game_loop(GameState):-
    clear_console,
    display_game(GameState),
    game_over(GameState, Winner),
    print_winner(Winner), !.
game_loop(GameState):-
    choose_move(GameState, Coordinates),
    move(GameState, Coordinates, NewGameState),
    game_loop(NewGameState).

% choose_move(+GameState, -Move)
% Determina o próximo movimento com base no nível de dificuldade do jogador.
% - Para jogadores humanos, o movimento é obtido a partir das coordenadas de entrada.
% - Para bots com dificuldade "random", o movimento é escolhido aleatoriamente entre os movimentos válidos.
% - Para bots com dificuldade "greedy", o movimento é decidido com base na função greedy_move/2 que tem em conta várias condições:
%   1. Analisa todos os movimentos possíveis usando o símbolo oposto. Se algum desses movimentos evitar que o bot ganhe mais pontos, ele é escolhido para se proteger.
%   2. Caso contrário, verifica os movimentos com o símbolo atual e escolhe aquele que atribua mais pontos ao adversário.
%   3. Se nenhum destes movimentos estratégicos for possível, seleciona um movimento que não seja adjacente ao símbolo atual, reduzindo a chance de ganho de pontos.
%   4. Se ainda assim não houver solução ideal, escolhe o movimento da lista de movimentos válidos que minimize os pontos do bot.
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

% adjacent_or_diagonal(+GameState, +Move)
% Verifica se o movimento resulta em uma célula adjacente ou diagonal ocupada pelo mesmo símbolo.
% A função simula o movimento no estado atual do jogo, gerando um novo estado,
% e então utiliza adjacent_or_diagonal_check/3 para validar se o símbolo atual (S) ocupa
% uma posição adjacente ou diagonal à célula alvo no tabuleiro atualizado (NewB2).
adjacent_or_diagonal(game_state(B1, B2, player2, S, _D1, _D2), Move) :-
    move(game_state(B1, B2, player2, S, _D1, _D2), Move, game_state(NewB1, NewB2, NextPlayer, NextSymbol, _D1, _D2)), % Simulate the move.
    adjacent_or_diagonal_check(NewB2, Move, S).

adjacent_or_diagonal(game_state(B1, B2, player1, S, _D1, _D2), Move) :-
    move(game_state(B1, B2, player1, S, _D1, _D2), Move, game_state(NewB1, NewB2, NextPlayer, NextSymbol, _D1, _D2)), % Simulate the move.
    adjacent_or_diagonal_check(NewB1, Move, S).

% adjacent_or_diagonal_check(+Board, +Move, +S)
% Verifica se o movimento coloca um símbolo adjacente ou diagonalmente próximo a um símbolo existente.
adjacent_or_diagonal_check(Board, Move, S) :-
    % Extrai as coordenadas do movimento.
    coordinates_to_indices(Move, Board, X, Y),
    % Verifica a proximidade adjacente ou diagonal.
    (
        adjacent(X, Y, X1, Y1), % Verifica células adjacentes ou diagonais.
        symbol_at(Board, X1, Y1, S) % Confirma se o símbolo S está presente na posição verificada.
    ).

% adjacent(+X, +Y, -X1, -Y1)
% Define a relação de posições adjacentes, incluindo as diagonais.
adjacent(X, Y, X1, Y1) :- 
    member(DX, [-1, 0, 1]), 
    member(DY, [-1, 0, 1]), 
    (DX \= 0; DY \= 0), 
    X1 is X + DX, 
    Y1 is Y + DY.

% symbol_at(+Board, +X, +Y, -Symbol)
% Verifica o símbolo em uma posição específica do tabuleiro.
symbol_at(Board, X, Y, Symbol) :- 
    nth0(X, Board, Row), 
    nth0(Y, Row, Symbol).


%greedy_move(+game_state(B1, B2, player2, S, _D1, _D2), -Move)
greedy_move(game_state(B1, B2, player2, S, _D1, _D2), Move) :-
    valid_moves(B2, ValidMoves),
    exclude(adjacent_or_diagonal(game_state(B1, B2, player2, S, _D1, _D2)), ValidMoves, GreedyMoves),
    get_next_symbol(S, NewSymbol),
    % Se houver 3 símbolos iguais, coloca o outro símbolo para bloquear.
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
    % Escolhe o movimento que atribui mais pontos ao adversário e menos a ele.
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
        % verifica se alguma jogada atribui mais pontos ao adversário dos que tem atualmente.
        Score1 > Ratio -> Move = Move1  
    ;   
        (   
            MinScore < Ratio -> Move = Move2  
        ;
            (   % Caso contrário, escolha a jogada de GreedyMoves que dê menos pontos para o bot.
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
            ;   % Caso contrário, use a lógica para minimizar os pontos do bot.
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
    % Se houver 3 símbolos iguais, coloca o outro símbolo para bloquear.
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
    % Escolhe o movimento que atribui mais pontos ao adversário e menos a ele.
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
            (   % Caso contrário, escolha a jogada de GreedyMoves que dê menos pontos para o bot.
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
            ;   % Caso contrário, use a lógica para minimizar os pontos do bot.
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

% value(+GameState, +Player, -Value)
% O estado do jogo é melhor quanto menos pontos o jogador tem e mais pontos o oponente tem. 
% O estado do jogo é mais positivo para o jogador atual quanto menor o valor dos pontos do jogador atual a dividir pelos pontos do adversário.
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

% initial_state(+GameConfig, -GameState)
% O predicado `initial_state/2` recebe as configurações iniciais do jogo e retorna o estado inicial do jogo (GameState), que contém os dois tabuleiros vazios, além de outras informações de configuração.
% - Ele começa extraindo as configurações de tamanho do tabuleiro e as dificuldades dos jogadores a partir de `GameConfig` (usando `member/2` e `nth1/3` para acessar os elementos da lista).
% - O predicado `get_board_size/3` é usado para determinar o número de linhas (`Rows`) e colunas (`Cols`) do tabuleiro com base na opção de tamanho escolhida.
% - O tabuleiro para o `player1` (Board1) e para o `player2` (Board2) é gerado pela função `create_shuffle_board/3`, que cria tabuleiros vazios com as dimensões determinadas (Rows, Cols).
% - Finalmente, o predicado retorna o estado do jogo (`game_state`) com os dois tabuleiros, o jogador inicial (player1) e seu símbolo (x), além das dificuldades configuradas para ambos os jogadores.
initial_state(GameConfig, game_state(Board1, Board2, player1, x, Difficulty1, Difficulty2)) :-
    member(board_size(Option), GameConfig),
    nth1(2, GameConfig, Difficulty1),
    nth1(3, GameConfig, Difficulty2),
    get_board_size(Option, Rows, Cols),
    
    % Gera o tabuleiro do player1
    create_shuffle_board(Rows, Cols, Board1),
    
    % Gera o tabuleiro do player2
    create_shuffle_board(Rows, Cols, Board2).


% board_score(+Board, -Score)
% Esta função calcula a pontuação total de um tabuleiro, somando as pontuações dos jogadores X e O.
% O valor total de pontos é atribuído à variável Score.
board_score(Board, Score):-
    calculate_points(Board, x, ScoreX),
    calculate_points(Board, o, ScoreO),
    Score is ScoreX+ScoreO.

% game_over(+game_state(T1, T2, _P, _S, _D1, _D2), Winner)
% Esta função verifica se o jogo acabou, verificando se os tabuleiros estão totalmente preenchidos e determina o vencedor com base nas pontuações dos jogadores.
% Ela verifica se o tabuleiro está completo, calcula as pontuações de cada jogador e exibe o resultado final.
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

% print_results(+ScoreB1, +ScoreB2)
% Função que imprime a pontuação dos dois jogadores
print_results(ScoreB1, ScoreB2):-
    write('End of game! Calculating points...'), nl,
    write('Score Board1 (Player1): '), write(ScoreB1), nl,
    write('Score Board2 (Player2): '), write(ScoreB2), nl.

% print_winner(+Winner)
% exibe uma mensagem informando o resultado do jogo, dependendo de quem foi o vencedor ou se houve empate.
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

% all_rows_completed(+Rows)
% Verifica se todas as linhas em uma lista de linhas estão completas.
all_rows_completed([]).
all_rows_completed([H|T]):-
    row_completed(H),
    all_rows_completed(T).   
 
% board_completed(+Board)
% Verifica se o tabuleiro está completo.
board_completed([[' '|FirstRow]|Rest]):-
    row_completed(FirstRow),
    all_rows_completed(Rest).

% Determina o número de linhas e colunas com base na opção escolhida
get_board_size(1, 6, 6). % Opção 1 -> Tabuleiro 6x6
get_board_size(2, 8, 8). % Opção 2 -> Tabuleiro 8x8

% create_lettered_rows(+Letters, +Cols, -Rows)
% Cria linhas com letras na borda esquerda e células vazias.
create_lettered_rows([], _, []).
create_lettered_rows([Letter | RestLetters], Cols, [[Letter | EmptyLine] | RestLines]) :-
    create_empty_row(Cols, EmptyLine),
    create_lettered_rows(RestLetters, Cols, RestLines).


% create_empty_row(+Cols, -Row)
% Cria uma linha vazia
create_empty_row(0, []) :- !.
create_empty_row(Cols, [' '|Rest]) :-
    Cols > 0,
    NextCols is Cols - 1,
    create_empty_row(NextCols, Rest).

% create_shuffle_board(+Rows, +Cols, -Board)
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
% Exibe o estado atual do jogo, mostrando os tabuleiros de ambos os jogadores.
% - A entrada (`GameState`) é o estado atual do jogo, que contém os tabuleiros dos dois jogadores, o jogador atual e o símbolo, e a dificuldade associada a cada jogador.
% O predicado funciona da seguinte maneira:
%   - Primeiro, ele exibe o tabuleiro do Player 1 (`Board1`), chamando o predicado `display_board/1` para mostrar o tabuleiro.
%   - Em seguida, exibe o tabuleiro do Player 2 (`Board2`),  também utilizando o predicado `display_board/1` para mostrar o tabuleiro.
% O resultado é uma exibição visual do estado atual do jogo, permitindo que os jogadores vejam os tabuleiros de ambos os lados.
display_game(game_state(Board1, Board2, _CurrentPlayer, _Symbol, _D1, _D2)) :-
    write('Board Player1:'), nl,
    display_board(Board1), nl,
    
    write('Board Player2:'), nl,
    display_board(Board2), nl.

% display_board(+Board)
% Exibe o tabuleiro
display_board([]).
display_board([Row|Rows]) :-
    write(Row), nl,
    display_board(Rows).


% move(+GameState, +Move, -NewGameState)
% Atualiza o estado do jogo após o movimento de um jogador.
% - O predicado converte as coordenadas do movimento em índices para os tabuleiros de ambos os jogadores.
% - Atualiza os tabuleiros de Player 1 e Player 2 com o símbolo do jogador.
% - Alterna para o próximo jogador, atualizando o símbolo e determinando quem será o próximo a jogar.
% O resultado é o novo estado do jogo, com os tabuleiros atualizados e o próximo jogador definido.
move(game_state(Board1, Board2, CurrentPlayer, Symbol, _D1, _D2), Move, game_state(NewBoard1, NewBoard2, NextPlayer, NewSymbol, _D1, _D2)) :-
    coordinates_to_indices(Move, Board1, LineIndex1, ColumnIndex1),
    update_board(Board1, LineIndex1, ColumnIndex1, Symbol, NewBoard1),
    coordinates_to_indices(Move, Board2, LineIndex2, ColumnIndex2),
    update_board(Board2, LineIndex2, ColumnIndex2, Symbol, NewBoard2),
    get_next_symbol(Symbol,NewSymbol),
    next_player(CurrentPlayer, Symbol, NextPlayer).


% extract_letters(+Board, -Letters)
% Extrai as letras de cada linha do tabuleiro.
extract_letters(Board, Letters) :-
    maplist(nth1(1), Board, Letters).  % Pega a primeira letra de cada linha

% generate_columns(+Board, -Column)
% Função para gerar os números das colunas
generate_columns([H|Board], H).

% coordinates_to_indices(+Coordenate, +Board, -Row, -Column)
% Converte as coordenadas (ex.: "A1") para os índices correspondentes no tabuleiro embaralhado.
coordinates_to_indices([Letter, Number], Board, Row, Column) :-
    extract_letters(Board, ShuffledLetters),
    generate_columns(Board, ShuffledColumns),
    nth0(Row, ShuffledLetters, Letter),  % Encontra o índice da letra embaralhada
    nth0(Column, ShuffledColumns, Number).

% empty_cell(+Board, +RowIndex, +ColumnIndex)
% Verifica se a célula especificada no tabuleiro está vazia.
empty_cell(Board, RowIndex, ColumnIndex) :-
    nth0(RowIndex, Board, CurrentRow),
    nth0(ColumnIndex, CurrentRow, Value),
    Value = ' '.  % The cell is empty if it contains a blank space

% update_board(+Board, +Row, +Column, +Symbol, -NewBoard)
% Atualiza uma célula específica em um tabuleiro.
update_board(Board, Row, Column, Symbol, NewBoard) :-
    nth0(Row, Board, RowBoard, OutBoard),
    nth0(Column, RowBoard, _, RowOut),
    nth0(Column, UpdatedRow, Symbol, RowOut),
    nth0(Row, NewBoard, UpdatedRow, OutBoard).

% Associa cada jogador ao seu respectivo símbolo.
get_next_symbol(x, o).
get_next_symbol(o, x).

% Alterna entre os jogadores, dependendo do jogador atual e do símbolo.
next_player(player1, x, player1).
next_player(player1, o, player2).
next_player(player2, x, player2).
next_player(player2, o, player1).


% read_input(-Move, +GameState)
% Solicita ao jogador que insira uma célula válida para o movimento.
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
    coordinates_to_indices([Letter, Number], B1, LineIndex, ColumnIndex),
    (   empty_cell(B1, LineIndex, ColumnIndex) 
    ->  ! 
    ;   write('Invalid! This cell is ocupied.'), nl,
        fail  
    ).

    
% get_valid_letter(+AlphaList, +FirstLetter, +LastLetter, -Letter)
% Solicita ao jogador que insira uma letra válida dentro do intervalo de letras disponíveis.
get_valid_letter(AlphaList, FirstLetter, LastLetter, Letter) :-
    format('Choose letter (~w-~w): ', [FirstLetter, LastLetter]),
    get_char(LetterTemp),           
    get_char(Pending),              
    (   Pending \= '\n'            
    ->  write('Invalid input! Input must be a single letter.'), nl,
        clear_input_buffer,        
        get_valid_letter(AlphaList, FirstLetter, LastLetter, Letter)
    ;   (   member(LetterTemp, AlphaList)  
        ->  Letter = LetterTemp            
        ;   write('Invalid letter! The letter must be valid.'), nl,
            get_valid_letter(AlphaList, FirstLetter, LastLetter, Letter)
        )
    ).


% Gera uma lista de números de 1 até N
num_list(M, M, [M]).
num_list(M, N, [M|Rest]) :-
    M < N,
    M1 is M + 1,
    num_list(M1, N, Rest).


% calculate_points(+Board, +Symbol, -Points)
% Calcula os pontos de um tabuleiro
calculate_points(Board, Symbol, Points) :-
    remove_header(Board, NewBoard),
    % Calcular pontos horizontais
    findall(1, complete_row(NewBoard, Symbol), RowPoints),
    % Calculate pontos verticais
    findall(1, complete_column(NewBoard, Symbol), ColumnPoints),
    % Calcular pontos quadrados
    findall(1, complete_square(NewBoard, Symbol), SquarePoints),
    % Calcular pontos diagonais
    findall(1, complete_diagonal(NewBoard, Symbol), DiagonalPoints),
    % Somar todos os pontos
    length(RowPoints, RowScore),
    length(ColumnPoints, ColumnScore),
    length(SquarePoints, SquareScore),
    length(DiagonalPoints, DiagonalScore),
    Points is RowScore + ColumnScore + SquareScore + DiagonalScore.

% complete_row(+Board, +Symbol)
% Verifica linhas com 4 símbolos
complete_row(Board, Symbol) :-
    member(Row, Board),
    sublist([Symbol, Symbol, Symbol, Symbol], Row).

% complete_column(+Board, +Symbol)
% Verifica colunas com 4 símbolos
complete_column(Board, Symbol) :-
    transpose(Board, TransposedBoard),
    complete_row(TransposedBoard, Symbol).

% complete_square(+Board, +Symbol)
% Verifica se há quadrados
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

% complete_diagonal(+Board, +Symbol)
% Verifica diagonal de 4 símbolos
complete_diagonal(Board, Symbol) :-
    normal_diagonal(Board, Symbol);
    inverted_diagonal(Board, Symbol).

% normal_diagonal(+Board, +Symbol)
% Verifica se diagonal normal contém 4 símbolos
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

% inverted_diagonal(+Board, +Symbol)
% Verifica se diagonal invertida contém 4 símbolos
inverted_diagonal(Board, Symbol) :-
    length(Board, Size), 
    Max is Size - 3,
    between(1, Max, Row), 
    between(4, Size, Column), 
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

% transpose(+Matrix, -TransposedMatrix)
% Transpõe uma matriz (tabuleiro)
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).

% lists_firsts_rests(+ListOfLists, -Firsts, -Rest)
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    lists_firsts_rests(Rest, Fs, Oss).

% sublist(+Sub, +List)
% Define se uma lista é sublista de outra
sublist(Sub, List) :-
    append(_, Rest, List),
    append(Sub, _, Rest).

% between(+Min, +Max, -N)
% Gera números de Min até Max
between(Min, Max, Min) :- Min =< Max.
between(Min, Max, N) :-
    Min < Max,
    Next is Min + 1,
    between(Next, Max, N).

alphabet_list(N, List) :-
    N > 0,
    findall(Char, (between(1, N, X), nth1(X, ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'], Char)), List).


% valid_moves(+Board, -ListOfMoves)
% A função `valid_moves/2` recebe o estado atual do tabuleiro (`Board`) e retorna uma lista de todas as células vazias no tabuleiro. 
% Ela percorre o tabuleiro, identificando as células representadas pelo caractere `' '` (espaço), que indicam que a célula está disponível para um movimento.
% Para cada célula vazia, a função gera uma coordenada no formato de uma lista contendo a letra da linha (como 'A', 'B', 'C', etc.) e o número da coluna (como 1, 2, 3, etc.).
% A função usa a primeira linha do tabuleiro para identificar os números das colunas e as linhas subsequentes para identificar as letras das linhas.
% O resultado é uma lista de coordenadas representando todas as células vazias onde um jogador pode fazer um movimento.
valid_moves(Board, ListOfMoves) :-
    % Identificar as letras e números do tabuleiro
    nth0(0, Board, HeaderRow), % Primeira linha contém os números das colunas
    exclude(=([]), HeaderRow, Numbers), % Remove elementos vazios da linha de cabeçalho
    tail(Board, Rows), % Remove a linha de cabeçalho do resto do tabuleiro
    findall([Letter, Number],
        (   nth0(RowIndex, Rows, Row),   % Iterar sobre as linhas do tabuleiro
            nth0(ColIndex, Row, Cell),  % Iterar sobre as colunas
            Cell = ' ',                 % Verificar se a célula está vazia
            nth0(RowIndex, Rows, [Letter|_]), % Obter a letra da linha atual
            nth0(ColIndex, HeaderRow, Number) % Obter o número da coluna
        ),
        ListOfMoves).

% tail(+List, -Tail)
% Retorna o resto da lista, ignorando o primeiro elemento
tail([_|T], T).

% remove_header(+Board, -NewBoard)
% remove o cabeçalho do tabuleiro
remove_header(Board, NewBoard):-
    Board=[_|BoardAux],
    transpose(BoardAux,TransposeBoard),
    TransposeBoard=[_|NewBoard].

% clear_console/0
% limpa a consola
clear_console:- 
    write('\33\[2J').
# PFL - Projeto2
## Identificação do tópico e grupo
**Jogo** : Doblin
**Group T11_Doblin_7:**
- Marta Silva: up202208258
- Tomás Vinhas: up202208437

  **Distribution of work**
- Marta Silva:
- Tomás Vinhas:

## Instalação e Execução
Para instalar o jogo Doblin, comece fazendo o download e descompactando os arquivos do arquivo PFL_TP2_T05_Doblin_7.zip. Posteriormente, acesse o diretório src e abra o arquivo game.pl. Você pode fazer isso através da linha de comando ou utilizando a interface gráfica do Sicstus Prolog 4.7.1. O jogo está disponível tanto para Windows quanto para Linux. Para iniciar o jogo, execute o predicado play/0.

## Descrição do jogo

## Considerações para extensões do jogo

## Lógica do jogo
**Representação do Movimento** : Para representar um movimento do jogo, é necessário ter informações sobre os dois tabuleiros, o movimento a executar, o jogador atual e o símbolo que será jogado. As coordenadas de um movimento são representadas por uma lista [Letter, Number], onde “Letter” identifica a linha do tabuleiro e “Number” a coluna. Essas coordenadas são convertidas em índices correspondentes nos dois tabuleiros, que possuem letras e números embaralhados. Essa conversão é realizada por uma função ???? que utiliza nth0/3 para localizar a posição da letra e do número em listas auxiliares derivadas de cada um dos tabuleiros, determinando assim o índice real do movimento correspondente em cada tabuleiro. Posteriormente, o predicado atualizar_tabuleiro/5 modifica a célula correspondente nos dois tabuleiros com o símbolo do jogador atual.
Assim, a função “move” recebe o estado atual do jogo, que inclui informações sobre os dois tabuleiros, o jogador atual e o símbolo que será jogado, além do movimento a ser executado. Ela retorna um novo estado do jogo após a execução do movimento.
Para isso, a função realiza a conversão das coordenadas, traduzindo-as em índices para localizar a célula correspondente nos tabuleiros. Em seguida, as células correspondentes nos dois tabuleiros são atualizadas com o símbolo do jogador atual. Por fim, o estado do jogo é atualizado, alterando o símbolo e o jogador para o próximo turno. O novo estado do jogo é então retornado na variável NewGameState.

**Interação com o usuário**:  O sistema de menu do jogo permite que o usuário escolha entre diferentes modos de jogo (Human/Human, Human/PC, PC/Human, PC/PC) e defina o tamanho do tabuleiro. Se o modo de jogo envolver o PC, é também solicitada a escolha do nível de dificuldade ("random" ou "greedy"). As opções são apresentadas com números correspondentes, e o jogador deve selecionar o número desejado. A interação é cuidadosamente controlada, garantindo entradas válidas através da função get_valid_option/2, que verifica se o número inserido está dentro das opções válidas. Caso a entrada seja inválida (como múltiplos caracteres ou um número fora do intervalo), o sistema limpa o buffer e solicita novamente.
Além disso, a função read_input/2 lê as coordenadas do movimento do jogador, validando-as. A função get_valid_letter/4 garante que a letra da coordenada (linha) esteja dentro do intervalo válido, de acordo com o tamanho do tabuleiro, enquanto a função get_valid_option/2 valida o número (coluna). Após as coordenadas serem validadas, elas são convertidas em índices, e a função empty_cell verifica se a célula no tabuleiro está vazia antes de realizar a jogada.

## Conclusões:
O programa atingiu seus principais objetivos, implementando um jogo funcional com modos de jogo variados, tamanhos de tabuleiro configuráveis e níveis de dificuldade. No entanto, apresenta limitações que podem ser aprimoradas. As estratégias do computador, limitadas a "random" e "greedy", oferecem pouco desafio para jogadores experientes. Além disso, a interface baseada em texto, simples e em preto e branco, carece de apelo visual e intuitividade.
Desenvolver uma interface gráfica (GUI) melhoraria a experiência do usuário, permitindo representar o tabuleiro com elementos visuais e cores, além de fornecer feedback claro para ações do jogo. Também seria importante implementar estratégias de IA mais avançadas, como para oferecer um jogo mais estratégico e desafiador, com vários níveis de dificuldade eu vão de encontro ao perfil de cada jogador. Essas melhorias tornariam o jogo mais moderno, atrativo e acessível.


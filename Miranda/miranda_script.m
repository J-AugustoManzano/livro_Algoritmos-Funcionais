
||                          ////
||                         (O O)
||           +-------oOO----(_)--------------+
||           |                               |
||           |   Linguagem: Miranda (1985)   | 
||           |                               |
||           +---------------------oOO-------+
||                        I__I__I
||                         II II
||                        ooO Ooo
||
|| PROBLEMAS CONHECIDOS
|| ====================
||
||  As funcoes "divisor" e "checa_primo" encontram-se 
||  adaptadas. Detalhes estao indicados nos codigos 
||  dessas funcoes.
||
||  Miranda somente opera com funcoes nomeadas, segun-  
||  do Glynn & Pope (1999, p. 8). Isto posto significa 
||  que a linguagen nao utiliza funcoes anonimas 
||  (http://www.berniepope.id.au/assets/files/mira2has
||  k.pdf). Por esta os exemplos do livro que utilizam 
||  funcoes anonimas (lambda) sao aqui adaptados no  
||  sentido de atender ao maximno a mesma ocorrencia.                        
||
|| * ================================================ *
|| *                                                  * 
|| *           Arquivo BONUS (complementar)           * 
|| *   Codigos exemplos: exercicios de aprendizagem   * 
|| *  ----------------------------------------------  *
|| *                                                  *
|| * Parte integrante do livro: Algoritmos Funcionais *
|| * Copyright: Jose Augusto N. G. Manzano - 2a. ed.  *
|| *                                                  *
|| * A permissao de uso deste material esta vinculado *
|| * a aquisicao do livro correspondente.             *
|| *                                                  *
|| * Scripts dos codigos para a linguagem Miranda     *
|| * usados como ilustracao complementar para a fixa- *
|| * cao dos conceitos logicos abordados no livro.    *
|| * Nao objetiva-se "ensinar" o uso da linguagem F#. *
|| * Isso devera ser realizado a partir de outras     *
|| * fontes de aprendizagem.                          *
|| *                                                  *
|| * Os exemplos deste arquivo sao adaptacoes escri-  *
|| * tas o mais próximo possível dos algoritmos tra-  *
|| * balhados no texto do livro. Detalhes operacio-   *
|| * nais, particulares e tipicos da linguagem foram  *
|| * propositalmente omitidos.                        *
|| *                                                  *
|| * As funcoes estao otimizadas para serem operacio- *
|| * nalizadas apenas com valores numericos.          *
|| *                                                  *
|| * Devido a caracteristicas particulares da lingua- *
|| * gem as funcoes "max" e "min" estao nomeadas como *
|| * "x_max" e "x_min" (uso adaptado).                *
|| *                                                  *
|| * Valores numericos negativos devem ser informados *
|| * obrigatoriamente entre parenteses.               *
|| *                                                  *
|| *                                                  *
|| * OBSERVACAO                                       *
|| * ==========                                       *
|| *                                                  *
|| * Para uso deste documento parte-se do pressuposto *
|| * de que:                                          *
|| *                                                  *
|| * - voce possui os conhecimentos informaticos ne-  *
|| *   cessarios de instalacao, configuracao da lin-  *
|| *   guagem demonstrada e que seu sistema esta      *
|| *   pronto para uso a partir da linha de comando   *
|| *   do modo "Terminal";                            *
|| *                                                  *
|| * - que voce possui o conhecimento de uso do modo  *
|| *   de edicao da linguagem com o comando "\e" para *
|| *   a edicao dos scripts de programas, bem como o  *
|| *   uso dos atalhos de gravacao e saida do modo de *
|| *   edicao para volta ao ambiente ambiente intera- *
|| *   tivo da linguagem.                             *
|| *                                                  *
|| * ================================================ *

|| =============================================================================================
|| >>>>                                                                                     <<<<
|| >>>> Funcoes de suporte as operacoes que nao foram possiveis de implantacao de acordo    <<<<
|| >>>> com o que esta proposto no livro, devido a caracteristicas de comportamento inter-  <<<<
|| >>>> no da linguagem Miranda e outros fatores desconhecidos.                             <<<<
|| >>>>                                                                                     <<<<
|| >>>> Funcoes de apoio basico ao uso das funcoes "dobra_d" e "dobra_e":                   <<<<
|| >>>>                                                                                     <<<<
|| >>>> * smult2 (x, y) similar a acao: \(x, y) -> 2.0 * x + y;                             <<<<
|| >>>> * Funcao: subt (x, y)   similar a acao: \(x, y) -> x - y ou (-).                    <<<<
|| >>>>                                                                                     <<<<
|| =============================================================================================

   smult2 :: (num, num) -> num
   smult2 (x, y) = 2 * x + y

   subt :: (num, num) -> num
   subt (x, y) = x - y

|| =============================================================================================

soma :: (num, num) -> num
soma (valor1, valor2) = valor1 + valor2

x_pi :: num 
x_pi = 3.14159

x_e :: num
x_e = 2.71828

quadrado :: num -> num
quadrado x = x ^ 2

soma2 :: (num, num) -> num
soma2 (x, y) = x + y

quadsoma :: (num, num) -> num 
quadsoma (x, y) = quadrado (soma2 (x, y))

peso :: num
peso = 99999999.49

lognat :: num -> num
lognat x = peso * (x ^ (1 / peso) - 1)

logbas :: (num, num) -> num
logbas (x, b) = lognat x / lognat b

adicao :: (num, num) -> num
adicao (0, y) = y
adicao (x, 0) = x
adicao (x, y) = x + y

par :: num -> bool
par n = True, if ((entier n div 1) mod 2 = 0) 
      = False, otherwise

impar :: num -> bool
impar n = True, if ((entier n div 1) mod 2 ~= 0) 
        = False, otherwise

impar2 :: num -> bool
impar2 n = ~(par n)

x_max :: (num, num) -> num
x_max (x, y) = x, if (x > y)
             = y, otherwise

x_min :: (num, num) -> num
x_min (x, y) = x, if (x < y)
             = y, otherwise

negativo :: num -> num
negativo n = n, if (n < 0)
           = 0 - n, otherwise

valorx :: num -> num
valorx 0 = 0
valorx 1 = 2
valorx n = n * 5, if (n > 1 & n < 9)
         = n / 5, otherwise

potencia :: (num, num) -> num
potencia (x, 0) = 1
potencia (x, 1) = x
potencia (x, n) = x * potencia (x, n - 1)

fib :: num -> num
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = fib ((entier n div 1) - 1) + fib ((entier n div 1) - 2)

fibbase :: (num, num, num) -> num
fibbase (0, anterior, atual) = anterior
fibbase (1, anterior, atual) = atual
fibbase (2, anterior, atual) = atual + anterior
fibbase (n, anterior, atual) = fibbase ((entier n div 1) - 1, atual, anterior + atual)

fib2 :: num -> num
fib2 n = fibbase (n, 0, 1)

mdc :: (num, num) -> num
mdc (0, n) = n
mdc (m, n) = mdc ((entier n div 1) mod (entier m div 1), m)

y = 1

cabeca :: [num] -> num
cabeca [] = error "lista vazia"
cabeca (x : xs) = x

cauda :: [num] -> [num]
cauda [] = error "lista vazia"
cauda (x : xs) = xs

ultimo :: [num] -> num
ultimo [] = error "lista vazia"
ultimo [x] = x
ultimo (x : xs) = ultimo xs

arranjo :: [num] -> [num]
arranjo [] = error "lista vazia"
arranjo [x] = []
arranjo (x : xs) = x : arranjo xs

somar :: [num] -> num
somar [] = 0
somar (x : xs) = x + somar xs

faixa :: (num, num, num) -> [num]
faixa (i, f, p) = [], if (i > f)
                = i : faixa (i + p, f, p), otherwise

|| oposto :: [num] -> [num]
|| oposto [] = []
|| oposto (x : xs) = oposto xs ++ [x]

oposto :: [num] -> [num]
oposto [] = []
oposto xs = ultimo xs : oposto (arranjo xs)

complista :: ([num], num -> bool) -> [num]
complista ([], qualificador) = []
complista (x : conjunto, qualificador) = (x : complista (conjunto, qualificador)), if (qualificador(x))
                                       = complista (conjunto, qualificador), otherwise

listamul :: (num, [num]) -> [num]
listamul (n, []) = []
listamul (n, x : xs) = (n * x) : listamul (n, xs)

listapot :: (num, [num]) -> [num]
listapot (n, []) = []
listapot (n, x : xs) = (x ^ n) : listapot (n, xs)

multiplo :: (num, num) -> bool
multiplo (n, m) = True, if ((entier n div 1) mod (entier m div 1) = 0)
                = False, otherwise

listamult :: ([num], num) -> [num]
listamult ([], n) = []
listamult (x : xs, n) = x : listamult (xs, (entier n div 1)), if ((entier n div 1) mod x = 0)
                      = listamult (xs, (entier n div 1)), otherwise

|| Funcao: divisor(numero)
||
|| Problema no uso da aridade de argumentos na funcao interna "multiplo". Para usar "multiplo"
|| e necessário em Miranda passar explicitamente os dois argumentos da funcao. 
|| Funciona bem nas linguagens Haskell, F#, Ocaml e ML que aceitam a passagem de argumentos
|| implicitos.
||
|| Versao Haskell ...............: divisor n = complista (faixa 1 n 1) (multiplo n) 
||
|| Versao Miranda (poderia ser) .: divisor n = complista (faixa (1, (entier n div 1), 1), multiplo (n))  
||
|| Solucao alternativa:

divisor :: num -> [num]
divisor n = listamult (faixa (1, n, 1), n)

tamanho :: [num] -> num
tamanho [] = 0
tamanho (x : xs) = 1 + tamanho xs

|| Funcao: checa_primo(numero)
||
|| Problema identico a definição da funcao "divisor". 
||
|| Versao Haskell ...............: checa_primo 1 = False
||                                 checa_primo 2 = True
||                                 checa_primo n = 
||                                   if (tamanho (complista (faixa 2 (n - 1) 1) (multiplo n)) > 0) 
||                                   then False 
||                                   else True  
||
|| Versao Miranda (poderia ser) .: checa_primo 1 = False
||                                 checa_primo 2 = True
||                                 checa_primo n = False, if (tamanho (complista (faixa (2, (entier n div 1) - 1, 1)), (multiplo (n))) > 0) 
||                                               = True, otherwise
||
|| Solucao alternativa:

checa_primo :: num -> bool
checa_primo 1 = False
checa_primo 2 = True
checa_primo n = False, if (tamanho (listamult (faixa (2, n - 1, 1), n)) > 0)
              = True, otherwise

lprimos :: num -> [num]
lprimos n = complista (faixa (1, n, 1), checa_primo)

membro :: (num, [num]) -> bool
membro (a, []) = False
membro (a, x : xs) = True, if (a = x)
                   = membro (a, xs), otherwise 

juncao :: ([num], [num]) -> [num]
juncao ([], []) = []
juncao (a, []) = a
juncao ([], b) = b
juncao (a : ax, b) = a : juncao (ax, b)

unico :: [num] -> [num]
unico [] = []
unico (x : xs) = unico xs, if (membro(x, xs))
               = x : unico xs, otherwise  

insira :: (num, [num]) -> [num]
insira (n, []) = [n]
insira (n, x : xs) = n : x : xs, if (n <= x) 
                   = x : insira(n, xs), otherwise

classifica :: [num] -> [num]
classifica [] = []
classifica (x : xs) = insira (x, classifica xs)

uniao :: ([num], [num]) -> [num]
uniao (a, b) = classifica (unico (juncao (a, b)))

interseccao :: ([num], [num]) -> [num]
interseccao (a, []) = []
interseccao ([], b) = []
interseccao (a, x : b) = x : interseccao (a, b), if (membro(x, a)) 
                       = interseccao (a, b), otherwise

diferenca :: ([num], [num]) -> [num]
diferenca (a, []) = a
diferenca ([], b) = []
diferenca (a : ax, b) = diferenca (ax, b), if (membro(a, b))  
                      = a : diferenca (ax, b), otherwise

sub_lista :: ([num], [num]) -> bool
sub_lista ([], y) = True
sub_lista (x : xs, y) = sub_lista (xs, y), if (membro (x, y))
                      = False, otherwise

igualdade :: ([num], [num]) -> bool
igualdade (a, b) = sub_lista(a, b) & sub_lista(b, a)

pega_pos :: (num, [num]) -> num
pega_pos (n, []) = error "elemento nao existe na lista"
pega_pos (n, x : xs) = tamanho xs, if (n = x)
                     = pega_pos (n, xs), otherwise

busca :: (num, [num]) -> num
busca (n, []) = error "lista invalida"
busca (n, x : xs) = pega_pos (n, oposto (x : xs))

mostra :: (num, [num]) -> num
mostra (n, []) = error "indice fora da faixa"
mostra (0, x : xs) = x
mostra (n, x : xs) = mostra (n - 1, xs)

lista_max :: [num] -> num
lista_max [] = error "lista vazia"
lista_max [a] = a
lista_max (x : xs) = x, if (x > lista_max xs) 
                   = lista_max xs, otherwise

lista_min :: [num] -> num
lista_min [] = error "lista vazia"
lista_min [a] = a
lista_min (x : xs) = x, if (x < lista_min xs) 
                   = lista_min xs, otherwise

replicar :: (num, num) -> [num] 
replicar (quantidade, valor) = [], if (quantidade = 0)
                             = valor : replicar (quantidade - 1, valor), otherwise

comeco :: (num, [num]) -> [num]
comeco (n, []) = []
comeco (n, x : xs) = x : comeco (n - 1, xs), if n > 0
                   = [], otherwise

final :: (num, [num]) -> [num]
final (n, []) = []
final (n, x : xs) = final (n - 1, xs), if (n - 1 > 0)
                  = xs, otherwise

separar :: [num] -> ([num], [num])
separar [] = ([], [])
separar xs = (comeco (tamanho xs div 2 + 1, xs), final (tamanho xs div 2 + 1, xs)), if (tamanho xs mod 2 ~= 0)
           = (comeco (tamanho xs div 2, xs), final (tamanho xs div 2, xs)), otherwise 

fatiar :: (num, num, [num]) -> [num]
fatiar (i, f, x) = final (i, comeco (f, x))

mapa :: ([num], num -> num) -> [num]
mapa ([], funcao) = []
mapa (x : xs, funcao) = funcao x : mapa (xs, funcao)

filtro :: (num -> bool, [num]) -> [num]
filtro (funcao, []) = []
filtro (funcao, x : xs) = x : filtro (funcao, xs), if (funcao x)
                        = filtro (funcao, xs), otherwise

reducao :: ([num], (num, num) -> num, num) -> num
reducao ([], funcao, n) = n
reducao (x : xs, funcao, n) = funcao (x, reducao (xs, funcao, n))

dobra_d :: ((num, num) -> num, num, [num]) -> num
dobra_d (f, n, []) = n
dobra_d (f, n, x : xs) = f (x, dobra_d (f, n, xs))

dobra_e :: ((num, num) -> num, num, [num]) -> num
dobra_e (f, n, []) = n
dobra_e (f, n, x : xs) = dobra_e (f, f (n, x), xs)

compacta :: ([num], [num]) -> [(num, num)]
compacta ([], b) = []
compacta (a, []) = []
compacta (x : a, y : b) = (x, y) : compacta (a, b)

dcp_base :: ([num], [num], [(num, num)]) -> ([num], [num]) 
dcp_base (xs, ys, []) = (oposto xs, oposto ys)
dcp_base (xs, ys, (x, y) : zs) = dcp_base (x : xs, y : ys, zs)

descompacta :: [(num, num)] -> ([num], [num])
descompacta [] = ([], [])
descompacta xs = dcp_base([], [], xs)

||
|| ============================================
|| Exemplos de uso das funcionalidades Miranda:
|| ============================================
|| 
|| >>> Capítulo 2
|| 
||     Ação .......: soma(2, 3)
||     Resultado ..: 5
|| 
||     Ação .......: x_pi
||     Resultado ..: 3.14159
|| 
||     Ação .......: x_e
||     Resultado ..: 2.71828
|| 
||     Ação .......: quadsoma(2, 3)
||     Resultado ..: 25
|| 
||     Ação .......: lognat 2
||     Resultado ..: 0.693147180559
|| 
||     Ação .......: logbas(2, 10)
||     Resultado ..: 0.301029993332
|| 
|| >>> Capítulo 3
|| 
||     Ação .......: adicao(0, 0)
||     Resultado ..: 0
|| 
||     Ação .......: adicao(0, 1)
||     Resultado ..: 1 
|| 
||     Ação .......: adicao(2, 0)
||     Resultado ..: 2
|| 
||     Ação .......: adicao(2, 1)
||     Resultado ..: 3 
|| 
||     Ação .......: par 2
||     Resultado ..: True
|| 
||     Ação .......: par 3
||     Resultado ..: False
|| 
||     Ação .......: impar 2
||     Resultado ..: False
|| 
||     Ação .......: impar 3
||     Resultado ..: True
|| 
||     Ação .......: impar2 8
||     Resultado ..: False
|| 
||     Ação .......: impar2 5
||     Resultado ..: True
|| 
||     Ação .......: x_max(2, 3)
||     Resultado ..: 3
|| 
||     Ação .......: x_max(5, 5)
||     Resultado ..: 5
|| 
||     Ação .......: x_max(9, 3)
||     Resultado ..: 9
|| 
||     Ação .......: valorx 2
||     Resultado ..: 10
|| 
||     Ação .......: valorx 9
||     Resultado ..: 1.8
|| 
||     Ação .......: valorx 1
||     Resultado ..: 2
|| 
||     Ação .......: valorx 10
||     Resultado ..: 2
|| 
||     Ação .......: potencia(5, 3)
||     Resultado ..: 125
|| 
||     Ação .......: fib 5
||     Resultado ..: 5
|| 
||     Ação .......: fib 7
||     Resultado ..: 13
|| 
||     Ação .......: fib 30
||     Resultado ..: 832040
|| 
||     Ação .......: fib2 7
||     Resultado ..: 13
|| 
||     Ação .......: fib2 30
||     Resultado ..: 832040
|| 
||     Ação .......: mdc(1024, 12)
||     Resultado ..: 4
||
||     Ação .......: y
||     Resultado ..: 1 (desde que a variavel "y" seja definida no editor como: y = 1)
||
||     Ação .......: (\x -> x + y) 9 - Ou algo semelhante mas, nao implementado:
||                                     Miranda nao opera funcao lambda, apenas funcao nomeada.
||                                     Alternativa para esta acao pode ser a funcao "soma", como: soma(9, y)
||     Resultado ..: 10 
||
|| >>> Capítulo 4
|| 
||     Ação .......: cabeca [1, 2, 3, 4, 5]
||     Resultado ..: 1
|| 
||     Ação .......: cabeca []
||     Resultado ..: program error: lista vazia
|| 
||     Ação .......: cabeca [5, 3, 1]
||     Resultado ..: 5
|| 
||     Ação .......: cauda [1, 2, 3, 4, 5]
||     Resultado ..: [2,3,4,5]
|| 
||     Ação .......: cauda []
||     Resultado ..: program error: lista vazia
|| 
||     Ação .......: cauda [5, 3, 1]
||     Resultado ..: [3,1]
||
||     Ação .......: ultimo [1, 2, 3, 4, 5]
||     Resultado ..: 5
|| 
||     Ação .......: ultimo []
||     Resultado ..: program error: lista vazia
|| 
||     Ação .......: ultimo [5, 3, 1]
||     Resultado ..: 1
|| 
||     Ação .......: arranjo [1, 2, 3, 4, 5]
||     Resultado ..: [1,2,3,4]
|| 
||     Ação .......: arranjo []
||     Resultado ..: program error: lista vazia
|| 
||     Ação .......: arranjo [5, 3, 1]
||     Resultado ..: [5,3]
|| 
||     Ação .......: somar [1, 2, 3, 4, 5]
||     Resultado ..: 15
|| 
||     Ação .......: somar [5, 3, 1]
||     Resultado ..: 9
|| 
||     Ação .......: faixa(3, 6, 1)
||     Resultado ..: [3,4,5,6]
|| 
||     Ação .......: faixa(1, 5, 1)
||     Resultado ..: [1,2,3,4,5]
|| 
||     Ação .......: faixa(1, 9, 2)
||     Resultado ..: [1,3,5,7,9]
|| 
||     Ação .......: oposto(faixa(3, 6, 1))
||     Resultado ..: [6,5,4,3]
|| 
||     Ação .......: oposto [1, 2, 3, 4, 5]
||     Resultado ..: [5,4,3,2,1]
|| 
||     Ação .......: complista(faixa(1, 20, 1), par)
||     Resultado ..: [2,4,6,8,10,12,14,16,18,20] 
|| 
||     Ação .......: complista([1, 2, 3, 4, 5], par)
||     Resultado ..: [2,4] 
|| 
||     Ação .......: listamul(2, [2, 3, 4])
||     Resultado ..: [4,6,8] 
|| 
||     Ação .......: listamul(2, faixa(1, 5, 1))
||     Resultado ..: [2,4,6,8,10]
|| 
||     Ação .......: listapot(2, [2, 3, 4])
||     Resultado ..: [4,9,16] 
|| 
||     Ação .......: listapot(2, faixa(1, 5, 1))
||     Resultado ..: [1,4,9,16,25]
|| 
||     Ação .......: multiplo(15, 3)
||     Resultado ..: True
|| 
||     Ação .......: multiplo(15, 4)
||     Resultado ..: False
|| 
||     Ação .......: multiplo(15, 5)
||     Resultado ..: True
|| 
||     Ação .......: listamult([1, 2, 3, 4, 5, 6], 3)
||     Resultado ..: [1,3]
|| 
||     Ação .......: listamult(faixa(1, 10, 1), 10) 
||     Resultado ..: [1,2,5,10]
|| 
||     Ação .......: divisor(10)
||     Resultado ..: [1,2,5,10]
|| 
||     Ação .......: divisor(30)
||     Resultado ..: [1,2,3,5,6,10,15,30]
|| 
||     Ação .......: checa_primo(1)
||     Resultado ..: False
|| 
||     Ação .......: checa_primo(2)
||     Resultado ..: True
|| 
||     Ação .......: checa_primo(3)
||     Resultado ..: True
|| 
||     Ação .......: checa_primo(4)
||     Resultado ..: False
|| 
||     Ação .......: checa_primo(5)
||     Resultado ..: True
|| 
||     Ação .......: lprimos(30)
||     Resultado ..: [2,3,5,7,11,13,17,19,23,29]
|| 
||     Ação .......: membro(2, [1,2,3])
||     Resultado ..: True
|| 
||     Ação .......: membro(6, [1,2,3])
||     Resultado ..: False
|| 
||     Ação .......: juncao([1, 2, 3], [4, 5, 6])
||     Resultado ..: [1,2,3,4,5,6]
|| 
||     Ação .......: juncao([1, 2, 3], [1, 2, 4])
||     Resultado ..: [1,2,3,1,2,4]
|| 
||     Ação .......: juncao([], [4, 5, 6])
||     Resultado ..: [4,5,6]
|| 
||     Ação .......: juncao([1, 2, 3], [])
||     Resultado ..: [1,2,3]
|| 
||     Ação .......: juncao([], [])
||     Resultado ..: []
|| 
||     Ação .......: unico([1, 1, 1, 1, 1, 2, 2, 2, 2, 3])
||     Resultado ..: [1,2,3]
|| 
||     Ação .......: insira(9, [7, 8, 6, 4, 5, 3])
||     Resultado ..: [7,8,6,4,5,3,9]
|| 
||     Ação .......: insira(2, [7, 8, 6, 4, 5, 3])
||     Resultado ..: [2,7,8,6,4,5,3]
|| 
||     Ação .......: classifica([9, 8, 7, 6, 5, 0, 4, 2, 1, 3])
||     Resultado ..: [0,1,2,3,4,5,6,7,8,9]
|| 
||     Ação .......: uniao([1, 2, 3], [4, 5, 6])
||     Resultado ..: [1,2,3,4,5,6]
|| 
||     Ação .......: uniao([1, 2, 3], [1, 2, 4])
||     Resultado ..: [1,2,3,4]
|| 
||     Ação .......: uniao([], [4, 5, 6])
||     Resultado ..: [4,5,6]
|| 
||     Ação .......: uniao([1, 2, 3], [])
||     Resultado ..: [1,2,3]
|| 
||     Ação .......: uniao([], [])
||     Resultado ..: []
|| 
||     Ação .......: interseccao([1, 2, 3, 4], [3, 4, 5, 6])
||     Resultado ..: [3,4]
|| 
||     Ação .......: diferenca([1, 2, 3, 4, 5], [1, 2, 6, 7])
||     Resultado ..: [3,4,5]
|| 
||     Ação .......: sub_lista([1, 2, 3], [1, 2, 3, 4, 5])
||     Resultado ..: True
|| 
||     Ação .......: sub_lista([1, 2, 7], [1, 2, 3, 4, 5])
||     Resultado ..: False
|| 
||     Ação .......: sub_lista([], [])
||     Resultado ..: True
|| 
||     Ação .......: sub_lista([], [1, 2, 3])
||     Resultado ..: True
|| 
||     Ação .......: sub_lista([1, 2, 3], [])
||     Resultado ..: False
|| 
||     Ação .......: igualdade([1, 2, 3], [3, 2, 1])
||     Resultado ..: True
|| 
||     Ação .......: igualdade([1, 2, 3], [3, 2, 1, 0])
||     Resultado ..: False
|| 
||     Ação .......: pega_pos(4, [1, 2, 3, 4, 5])
||     Resultado ..: 1
|| 
||     Ação .......: pega_pos(negativo(2), [1, 2, 3, 4, 5])
||     Resultado ..: program error: elemento nao existe na lista
||
||     Ação .......: busca(4, [1, 2, 3, 4, 5])
||     Resultado ..: 3
|| 
||     Ação .......: busca(6, [1, 2, 3, 4, 5])
||     Resultado ..: busca(6, [1, 2, 3, 4, 5])
|| 
||     Ação .......: mostra(2, [1, 2, 3, 4, 5])
||     Resultado ..: 3
|| 
||     Ação .......: mostra(7, [1, 2, 3, 4, 5])
||     Resultado ..: program error: indice fora da faixa
|| 
||     Ação .......: lista_max([3, 2, 1, 5, 4])
||     Resultado ..: 5
|| 
||     Ação .......: lista_min([3, 2, 1, 5, 4])
||     Resultado ..: 1
|| 
||     Ação .......: replicar(5, 99)
||     Resultado ..: [99,99,99,99,99]
|| 
||     Ação .......: comeco(3, [5, 4, 3, 2, 1])
||     Resultado ..: [5,4,3]
|| 
||     Ação .......: comeco(7, [5, 4, 3, 2, 1])
||     Resultado ..: [5,4,3,2,1]
|| 
||     Ação .......: comeco(0, [5, 4, 3, 2, 1])
||     Resultado ..: []
|| 
||     Ação .......: final(0, [5, 4, 3, 2, 1])
||     Resultado ..: [4,3,2,1]
|| 
||     Ação .......: final(100, [5, 4, 3, 2, 1])
||     Resultado ..: []
|| 
||     Ação .......: final(3, [5, 4, 3, 2, 1])
||     Resultado ..: [2,1]
|| 
||     Ação .......: separar([1, 2, 3, 4, 5])
||     Resultado ..: ([1,2,3],[4,5])
|| 
||     Ação .......: separar([1, 2, 3, 4, 5, 6])
||     Resultado ..: ([1,2,3],[4,5,6])
|| 
||     Ação .......: separar([1])
||     Resultado ..: ([1],[])
|| 
||     Ação .......: separar([])
||     Resultado ..: ([],[])
|| 
||     Ação .......: fatiar(3, 6, [1, 2, 3, 4, 5, 6, 7, 8, 9, 0])
||     Resultado ..: [4,5,6]
|| 
||     Ação .......: mapa([1, 2, 3, 4, 5], (* 3)) 
||     Resultado ..: [3,6,9,12,15]  
||
||     Ação .......: filtro(impar, [1, 2, 3, 4])
||     Resultado ..: [1,3] 
|| 
||     Ação .......: reducao([1, 2, 3, 4], soma, 0)
||     Resultado ..: 10
||
||     Ação .......: (para um eventual) dobra_d(\(x, y) -> 2 * x + y, 5, [1, 2, 3])
||                   (faca) dobra_d(smult2, 5, [1, 2, 3]) 
||     Resultado ..: 17
|| 
||     Ação .......: (para um eventual) dobra_d((-), 7, [4, 7, 3, 5])
||                   (faca) dobra_d(subt, 7, [4, 7, 3, 5])
||     Resultado ..: 2
||
||     Ação .......: (para um eventual) dobra_e(\(x, y) -> 2 * x + y, 5, [1, 2, 3])
||                   (faca) dobra_e(smult2, 5, [1, 2, 3])
||     Resultado ..: 51
|| 
||     Ação .......: (para um eventual) dobra_e((-), 7, [4, 7, 3, 5])
||                   (faca) dobra_e(subt, 7, [4, 7, 3, 5])
||     Resultado ..: -12
|| 
||     Ação .......: compacta([1, 2, 3], [4, 5, 6])
||     Resultado ..: [(1,4),(2,5),(3,6)]
|| 
||     Ação .......: compacta([1, 2, 3, 4], [5, 6])
||     Resultado ..: [(1,5),(2,6)]
|| 
||     Ação .......: compacta([1, 2], [3, 4, 5, 6])
||     Resultado ..: [(1,3),(2,4)]
|| 
||     Ação .......: dcp_base([], [], [(8, 9)])
||     Resultado ..: ([8],[9])
|| 
||     Ação .......: dcp_base([], [], [(8, 9), (1, 2)])
||     Resultado ..: ([8,1],[9,2])
|| 
||     Ação .......: descompacta([(1, 2), (3, 4)])
||     Resultado ..: ([1,3],[2,4])
|| 
||     Ação .......: descompacta([(1, 2), (3, 4), (5, 6), (7, 8)])
||     Resultado ..: ([1,3,5,7],[2,4,6,8])
||
||     ////   
||    (o o)     
|| ooO_(_)_Ooo_________________________________________________________________
|| _____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|____
|| __|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_
|| _____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|____
|| ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
|| ::::'###::::'##::::'##::'######:::'##::::'##::'######::'########::'#######::
|| :::'## ##::: ##:::: ##:'##... ##:: ##:::: ##:'##... ##:... ##..::'##.... ##:
|| ::'##:. ##:: ##:::: ##: ##:::..::: ##:::: ##: ##:::..::::: ##:::: ##:::: ##:
|| :'##:::. ##: ##:::: ##: ##::'####: ##:::: ##:. ######::::: ##:::: ##:::: ##:
|| : #########: ##:::: ##: ##::: ##:: ##:::: ##::..... ##:::: ##:::: ##:::: ##:
|| : ##.... ##: ##:::: ##: ##::: ##:: ##:::: ##:'##::: ##:::: ##:::: ##:::: ##:
|| : ##:::: ##:. #######::. ######:::. #######::. ######::::: ##::::. #######::
|| :..:::::..:::.......::::......:::::.......::::......::::::..::::::.......:::
|| :'##::::'##::::'###::::'##::: ##::'########::::'###::::'##::: ##::'#######::
|| : ###::'###:::'## ##::: ###:: ##::..... ##::::'## ##::: ###:: ##:'##.... ##:
|| : ####'####::'##:. ##:: ####: ##:::::: ##::::'##:. ##:: ####: ##: ##:::: ##:
|| : ## ### ##:'##:::. ##: ## ## ##::::: ##::::'##:::. ##: ## ## ##: ##:::: ##:
|| : ##. #: ##: #########: ##. ####:::: ##::::: #########: ##. ####: ##:::: ##:
|| : ##:.:: ##: ##.... ##: ##:. ###::: ##:::::: ##.... ##: ##:. ###: ##:::: ##:
|| : ##:::: ##: ##:::: ##: ##::. ##:: ########: ##:::: ##: ##::. ##:. #######::
|| :..:::::..::..:::::..::..::::..::........::..:::::..::..::::..:::.......::::

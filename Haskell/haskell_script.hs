--                          ////
--                         (O O)
--           +-------oOO----(_)--------------+
--           |                               |
--           |   Linguagem: Haskell (1990)   | 
--           |                               |
--           +---------------------oOO-------+
--                        I__I__I
--                         II II
--                        ooO Ooo
--
-- * ================================================ *
-- *                                                  *
-- *                  Arquivo BONUS                   *
-- *   Codigos exemplos: exercicios de aprendizagem   *
-- *   --------------------------------------------   *
-- *                                                  *
-- * Parte integrante do livro: Algoritmos Funcionais *
-- * Copyright: Jose Augusto N. G. Manzano - 2a. ed.  *
-- *                                                  *
-- * A permissao de uso deste material esta vinculado *
-- * a aquisicao do livro correspondente.             *
-- *                                                  *
-- * Scripts dos codigos para a linguagem Haskell     *
-- * usados como ilustracao para a fixacao dos con-   *
-- * ceitos logicos apresentados no livro.            *
-- *                                                  *
-- * As funcoes estao otimizadas para serem operacio- *
-- * nalizadas apenas com valores numericos.          *
-- *                                                  *
-- * ================================================ *

:{ 

soma :: (Num a) => a -> a -> a
soma valor1 valor2 = valor1 + valor2

x_pi :: (Floating a) => a
x_pi = 3.14159

x_e :: (Floating a) => a
x_e = 2.71828  

quadrado :: (Num a) => a -> a
quadrado x = x ^ 2

soma2 :: (Num a) => a -> a -> a
soma2 x y = x + y

quadsoma :: (Num a) => a -> a -> a
quadsoma x y = quadrado (soma2 x y)

peso :: (Floating a) => a
peso = 99999999.49

lognat :: (Floating a) => a -> a  
lognat x = peso * (x ** (1 / peso) - 1)

logbas :: (Floating a) => a -> a -> a    
logbas x b = lognat (x) / lognat (b)

adicao :: (Eq a, Num a) => a -> a -> a
adicao 0 y = y
adicao x 0 = x
adicao x y = x + y

par :: (Integral a) => a -> Bool
par n = if mod n 2 == 0 then True else False  

impar :: (Integral a) => a -> Bool
impar n = if mod n 2 /= 0 then True else False  

impar2 :: (Integral a) => a -> Bool
impar2 n = not (par n)  

max :: (Ord a, Num a) => a -> a -> a
max x y = if x > y then x else y  

negativo :: (Eq a, Ord a, Num a) => a-> a
negativo n = if n < 0 then n else 0 - n

valorx :: (Eq a, Ord a, RealFrac a, Num a) => a -> a
valorx 0 = 0
valorx 1 = 2
valorx n = if n > 1 && n < 9 then n * 5 else n / 5  

potencia :: (Eq a, Ord a, Num a) => a -> a -> a
potencia x 0 = 1
potencia x 1 = x
potencia x n = x * potencia x (n - 1)  

fib :: (Integral a) => a -> a
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

fibbase :: (Integral a) => a -> a -> a -> a
fibbase 0 anterior atual = anterior
fibbase 1 anterior atual = atual
fibbase 2 anterior atual = atual + anterior
fibbase n anterior atual = fibbase (n - 1) atual (anterior + atual)

fib2 :: (Integral a) => a -> a
fib2 n = fibbase n 0 1  

mdc :: (Integral a) => a -> a -> a
mdc 0 n = n
mdc m n = mdc (mod n m) m  

cabeca :: (Num a) => [a] -> a
cabeca [] = error "lista vazia"
cabeca (x : xs) = x

cauda :: (Num a) => [a] -> [a]
cauda [] = error "lista vazia"
cauda (x : xs) = xs

ultimo :: (Num a) => [a] -> a
ultimo [] = error "lista vazia"
ultimo [x] = x
ultimo (x : xs) = ultimo xs

arranjo :: (Num a) => [a] -> [a]
arranjo [] = error "lista vazia"
arranjo [x] = []
arranjo (x : xs) = x : arranjo xs

somar :: (Num a) => [a] -> a
somar [] = 0
somar (x : xs) = x + somar xs

faixa :: (Ord a, Num a) => a -> a -> a -> [a]
faixa i f p = if i > f
              then []
              else i : faixa (i + p) f p

-- oposto :: (Num a) => [a] -> [a]
-- oposto [] = []
-- oposto (x : xs) = oposto xs ++ [x]

oposto :: (Num a) => [a] -> [a]
oposto [] = []
oposto xs = ultimo xs : oposto (arranjo xs)

complista :: (Num a) => [a] -> (a -> Bool) -> [a]
complista [] qualificador = []
complista (x : conjunto) qualificador =
  if qualificador x
  then x : complista conjunto qualificador
  else complista conjunto qualificador

listamul :: (Num a) => a -> [a] -> [a]
listamul _ [] = []
listamul n (x : xs) = (n * x) : listamul n xs

listapot :: (Floating a) => a -> [a] -> [a]
listapot _ [] = []
listapot n (x : xs) = (x ** n) : listapot n xs

multiplo :: Int -> Int -> Bool
multiplo n m = 
  if mod n m == 0 then True else False  

divisor :: Int -> [Int]
divisor n = complista (faixa 1 n 1) (multiplo n) 

listamult :: [Int] -> Int -> [Int]
listamult [] n = []
listamult (x : xs) n = if mod n x == 0 
                       then x : listamult xs n
                       else listamult xs n

-- divisor :: Int -> [Int]
-- divisor n = listamult (faixa 1 n 1) n

tamanho :: (Num a) => [a] -> Int
tamanho [] = 0
tamanho (x : xs) = 1 + tamanho xs

checa_primo :: Int -> Bool
checa_primo 1 = False
checa_primo 2 = True
checa_primo n = 
  if (tamanho (complista (faixa 2 (n - 1) 1) (multiplo n)) > 0) 
  then False 
  else True

-- checa_primo :: Int -> Bool
-- checa_primo 1 = False
-- checa_primo 2 = True
-- checa_primo n = 
--   if tamanho (listamult (faixa 2 (n - 1) 1) n) > 0 
--   then False 
--   else True

lprimos :: Int -> [Int]
lprimos n = complista (faixa 1 n 1) (checa_primo)

membro :: (Eq a, Num a) => a -> [a] -> Bool
membro _ [] = False
membro a (x : xs) = if a == x then True else membro a xs

juncao :: (Ord a, Num a) => [a] -> [a] -> [a]
juncao [] [] = []
juncao a [] = a
juncao [] b = b
juncao (a : ax) b = a : (juncao ax b)

unico :: (Eq a, Num a) => [a] -> [a]
unico [] = []
unico (x : xs) = if membro x xs 
                 then unico xs
                 else x : unico xs

insira :: (Ord a, Num a) => a -> [a] -> [a]
insira n [] = [n]
insira n (x : xs) = if n <= x 
                    then n : x : xs 
                    else x : insira n xs

classifica :: (Ord a, Num a) => [a] -> [a]
classifica [] = []
classifica (x : xs) = insira x (classifica xs)

uniao :: (Eq a, Ord a, Num a) => [a] -> [a] -> [a]
uniao a b = classifica (unico (juncao a b))

interseccao :: (Eq a, Num a) => [a] -> [a] -> [a]
interseccao a [] = []
interseccao [] b = []
interseccao a (x : b) = if (membro x a) 
                        then x : interseccao a b 
                        else interseccao a b

diferenca :: (Eq a, Num a) => [a] -> [a] -> [a]
diferenca a [] = a
diferenca [] b = []
diferenca (a : ax) b = if (membro a b) 
                       then diferenca ax b
                       else a : diferenca ax b

sub_lista :: (Eq a, Num a) => [a] -> [a] -> Bool
sub_lista [] y = True
sub_lista (x : xs) y = if membro x y
                       then sub_lista xs y
                       else False

igualdade :: (Eq a, Ord a, Num a) => [a] -> [a] -> Bool
igualdade a b = sub_lista a b && sub_lista b a

pega_pos :: (Eq a, Num a) => a -> [a] -> Int
pega_pos _ [] = error "elemento nao existe na lista"
pega_pos n (x : xs) = if n == x then tamanho xs else pega_pos n xs

busca :: (Eq a, Num a) => a -> [a] -> Int
busca _ [] = error "lista invalida"
busca n (x : xs) = pega_pos n (oposto (x : xs))

mostra :: (Eq a, Num a) => Int -> [a] -> a
mostra _ [] = error "indice fora da faixa"
mostra 0 (x : xs) = x
mostra n (x : xs) = mostra (n - 1) xs

lista_max :: (Ord a, Num a) => [a] -> a
lista_max [] = error "lista vazia"
lista_max [a] = a
lista_max (x : xs) = if x > lista_max xs 
                     then x 
                     else lista_max xs

lista_min :: (Ord a, Num a) => [a] -> a
lista_min [] = error "lista vazia"
lista_min [a] = a
lista_min (x : xs) = if x < lista_min xs
                     then x 
                     else lista_min xs

replicar :: (Eq a, Num a) => a -> a -> [a] 
replicar quantidade valor = 
  if quantidade == 0
  then []
  else valor : replicar (quantidade - 1) valor

comeco :: (Ord a, Num a) => Int -> [a] -> [a]
comeco _ [] = []
comeco n (x : xs) = if n > 0
                    then x : comeco (n - 1) xs
                    else []

final :: (Ord a, Num a) => Int -> [a] -> [a]
final _ [] = []
final n (x : xs) = if n - 1 > 0
                   then final (n - 1) xs
                   else xs

separar :: (Ord a, Num a) => [a] -> ([a], [a])
separar [] = ([], [])
separar xs = 
  if mod (tamanho xs) 2 /= 0
  then (comeco (div (tamanho xs) 2 + 1) xs, final (div (tamanho xs) 2 + 1) xs)
  else (comeco (div (tamanho xs) 2) xs, final (div (tamanho xs) 2) xs)

fatiar :: (Ord a, Num a) => Int -> Int -> [a] -> [a]
fatiar i f x = final i (comeco f x)

mapa :: (Num a) => [a] -> (a -> a) -> [a]
mapa [] funcao = []
mapa (x : xs) funcao = funcao x : mapa xs funcao

filtro :: (Ord a, Num a) => (a -> Bool) -> [a] -> [a]
filtro funcao [] = []
filtro funcao (x : xs) = if funcao x
                         then x : (filtro funcao xs)
                         else filtro funcao xs

reducao :: (Num a) => [a] ->  (a -> a -> a) -> a -> a
reducao [] funcao n = n
reducao (x : xs) funcao n = funcao x (reducao xs funcao n)

dobra_d :: (Num a) => (a -> a -> a) -> a -> [a] -> a
dobra_d f n [] = n
dobra_d f n (x : xs) = f x (dobra_d f n xs)

dobra_e :: (Num a) => (a -> a -> a) -> a -> [a] -> a
dobra_e f n [] = n
dobra_e f n (x : xs) = dobra_e f (f n x) xs

compacta :: (Num a) => [a] -> [a] -> [(a, a)]
compacta [] b = []
compacta a [] = []
compacta (x : a) (y : b) = (x, y) : compacta a b

dcp_base :: (Num a) => [a] -> [a] -> [(a, a)] -> ([a], [a]) 
dcp_base xs ys [] = (oposto xs, oposto ys)
dcp_base xs ys ((x, y) : zs) = dcp_base (x : xs) (y : ys) zs

descompacta :: (Num a) => [(a, a)] -> ([a], [a])
descompacta [] = ([], [])
descompacta xs = dcp_base [] [] xs

-- *** Funcoes bonus ***

desconst :: (Num a) => [a] -> (a, [a]) -- tupla de cabeca e cauda
desconst [] = error "lista vazia"
desconst (x : xs) = (x, xs)

dobra_d1 :: (Num a) => (a -> a -> a) -> [a] -> a
dobra_d1 _ [x] = x
dobra_d1 f (x : xs) = f x (dobra_d1 f xs)

dobra_e1 :: (Num a) => (a -> a -> a) -> [a] -> a
dobra_e1  f (x : xs) = dobra_e f x xs

isort :: (Num a, Ord a) => [a] -> [a] -- insertion sort
isort [] = []
isort (x : xs) = insira x (isort xs)

min :: (Ord a, Num a) => a -> a -> a
min x y = if x < y then x else y

pares :: (Num a) => [a] -> [(a, a)]
pares xs = compacta xs (cauda xs)

qsort :: (Ord a, Num a) => [a] -> [a] -- quick sort
qsort [] = []
qsort (a : b) = qsort (complista b (<= a))
                ++ [a] ++
                qsort (complista b (> a))

rotac_d :: (Num a) => [a] -> [a]
rotac_d [] = []
rotac_d xs = ultimo xs : arranjo xs

rotac_e :: (Num a) => [a] -> [a]
rotac_e [] = []
rotac_e (x : xs) = xs ++ [x]

vazia :: [a] -> Bool
vazia [] = True
vazia (_ : _) = False

:}

--
--       ////   
--      (o o)     
-- __ooO_(_)_Ooo_________________________________________________________________________
-- |_____|______|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|
-- ___|_____|______|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|___
-- |_____|______|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|
-- ___| :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: |___
-- |___ ::::'###::::'##::::'##::'######:::'##::::'##::'######::'########::'#######:: ___|
-- ___| :::'## ##::: ##:::: ##:'##... ##:: ##:::: ##:'##... ##:... ##..::'##.... ##: |___
-- |___ ::'##:. ##:: ##:::: ##: ##:::..::: ##:::: ##: ##:::..::::: ##:::: ##:::: ##: ___|
-- ___| :'##:::. ##: ##:::: ##: ##::'####: ##:::: ##:. ######::::: ##:::: ##:::: ##: |___
-- |___ : #########: ##:::: ##: ##::: ##:: ##:::: ##::..... ##:::: ##:::: ##:::: ##: ___|
-- ___| : ##.... ##: ##:::: ##: ##::: ##:: ##:::: ##:'##::: ##:::: ##:::: ##:::: ##: |___
-- |___ : ##:::: ##:. #######::. ######:::. #######::. ######::::: ##::::. #######:: ___|
-- ___| :..:::::..:::.......::::......:::::.......::::......::::::..::::::.......::: |___
-- |___ :'##::::'##::::'###::::'##::: ##::'########::::'###::::'##::: ##::'#######:: ___|
-- ___| : ###::'###:::'## ##::: ###:: ##::..... ##::::'## ##::: ###:: ##:'##.... ##: |___
-- |___ : ####'####::'##:. ##:: ####: ##:::::: ##::::'##:. ##:: ####: ##: ##:::: ##: ___|
-- ___| : ## ### ##:'##:::. ##: ## ## ##::::: ##::::'##:::. ##: ## ## ##: ##:::: ##: |___
-- |___ : ##. #: ##: #########: ##. ####:::: ##::::: #########: ##. ####: ##:::: ##: ___|
-- ___| : ##:.:: ##: ##.... ##: ##:. ###::: ##:::::: ##.... ##: ##:. ###: ##:::: ##: |___
-- |___ : ##:::: ##: ##:::: ##: ##::. ##:: ########: ##:::: ##: ##::. ##:. #######:: ___|
-- ___| :..:::::..::..:::::..::..::::..::........::..:::::..::..::::..:::.......:::: |___
-- |_____|______|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|
-- ___|_____|______|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|___
-- |_____|______|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|

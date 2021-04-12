(*

                          ////
                         (O O)
           +-------oOO----(_)--------------+
           |                               |
           |    Linguagem: OCaml (1996)    | 
           |                               |
           +---------------------oOO-------+
                        I__I__I
                         II II
                        ooO Ooo

 * ================================================ *
 *                                                  *
 *           Arquivo BONUS (complementar)           * 
 *   Codigos exemplos: exercicios de aprendizagem   * 
 *   --------------------------------------------   *
 *                                                  *
 * Parte integrante do livro: Algoritmos Funcionais *
 * Copyright: Jose Augusto N. G. Manzano - 2a. ed.  *
 *                                                  *
 * A permissao de uso deste material esta vinculado *
 * a aquisicao do livro correspondente.             *
 *                                                  *
 * Scripts dos codigos para a linguagem OCaml usa-  *
 * dos como ilustracao complementar para a fixacao  *
 * dos conceitos logicos abordados no livro. Nao    *
 * objetiva-se "ensinar" o uso da linguagem OCaml.  *
 * Isso devera ser realizado a partir de outras     *
 * fontes de aprendizagem.                          *
 *                                                  *
 * Os exemplos deste arquivo sao adaptacoes escri-  *
 * tas o mais próximo possível dos algoritmos tra-  *
 * balhados no texto do livro. Detalhes operacio-   *
 * nais, particulares e tipicos da linguagem foram  *
 * propositalmente omitidos.                        *
 *                                                  *
 * As funcoes estao otimizadas para serem operacio- *
 * nalizadas apenas com valores numericos.          *
 *                                                  *
 * Valores numericos negativos devem ser informados *
 * obrigatoriamente entre parenteses.               *
 *                                                  *
 * Valores reais (ponto flutuante) devem ser usados *
 * com a definicao obrigatoria do ponto decimal.    *
 *                                                  *
 * Por questoes de praticidade os valores das fun-  *
 * coes deste conjunto de scripts devem ser opera-  *
 * cionalizados com numeros reais (ponto flutuan-   *
 * te). Nenhum tratamento para uso de valores nume- *
 * mericos inteiros foi previsto neste conjunto de  *
 * funcoes.                                         *
 *                                                  *
 * Os scripts deste arquivo podem ser executados no *
 * ambiente interativo: https://try.ocamlpro.com/.  *
 *                                                  *
 * OBSERVACAO                                       *
 * ==========                                       *
 *                                                  *
 * Para uso deste documento parte-se do pressuposto *
 * de que:                                          *
 *                                                  *
 * - voce possui os conhecimentos informaticos ne-  *
 *   cessarios de instalacao, configuracao da lin-  *
 *   guagem demonstrada e que seu sistema esta      *
 *   pronto para uso a partir da linha de comando   *
 *   do modo "Terminal".                            *
 *                                                  *
 * ================================================ *

 *);;

let soma (num1 : float) num2 = 
  match num1, num2 with
  | (valor1, valor2) -> valor1 +. valor2;;

let x_pi = 3.14159;;

let x_e = 2.71828;;

let quadrado (num : float) =
  match num with
  | x -> x ** 2.0;;

let soma2 (num1 : float) num2 = 
  match num1, num2 with
  | (x, y) -> x +. y;;

let quadsoma (num1 : float) num2 = 
  match num1, num2 with
  | (x, y) -> quadrado (soma2 x y);;

let peso = 99999999.49;;

let lognat (num : float) =
  match num with
  | x -> peso *. (x ** (1.0 /. peso) -. 1.0);;

let logbas (num : float) base =
  match num, base with
  | (x, b) -> lognat x /. lognat b;;

let adicao (num1 : float) num2 = 
  match num1, num2 with
  | 0., y -> y 
  | x, 0. -> x
  | x, y  -> x +. y;;

let par (num : float) =
  match num with
  | n -> if (int_of_float(floor n) mod 2 = 0) then true else false;;

let impar (num : float) =
  match num with
  | n -> if (int_of_float(floor n) mod 2 != 0) then true else false;;

let impar2 (num : float) =
  match num with
  | n -> not (par n);;

let max (num1 : float) num2 = 
  match num1, num2 with
  | x, y -> if (x > y) then x else y;;

let min (num1 : float) num2 = 
  match num1, num2 with
  | x, y -> if (x < y) then x else y;;

let negativo (num : float) =
  match num with
  | n -> if (n < 0.) then n else 0. -. n;;

let valorx (num : float) = 
  match num with
  | 0. -> 0. 
  | 1. -> 2.  
  | n -> if (n > 1.) && (n < 9.) then n *. 5. else n /. 5.;;

let rec potencia (num1 : float) num2 = 
  match num1, num2 with
  | x, 0. -> 1. 
  | x, 1. -> x  
  | x, n -> x *. potencia x (n -. 1.);;

let rec fib (num : float) = 
  match num with
  | 0. -> 0.
  | 1. -> 1.
  | 2. -> 1.
  | n -> fib (floor n -. 1.) +. fib (floor n -. 2.);;

let rec fibbase (num1 : float) (num2 : float) (num3 : float) = 
  match num1, num2, num3 with
  | 0., anterior, atual -> anterior
  | 1., anterior, atual -> atual 
  | 2., anterior, atual -> atual +. anterior 
  | n, anterior, atual -> fibbase (n -. 1.) atual (anterior +. atual);;

let fib2 (num : float) = 
  match num with
  | n -> fibbase (floor n) 0. 1.;;

let rec mdc (num1 : float) num2 =
  match num1, num2 with
  | 0., n -> n
  | m, n -> mdc (float_of_int(int_of_float(n) mod int_of_float(m))) m;;

let cabeca (lista : float list) = 
  match lista with
  | [] -> failwith "lista vazia"
  | x :: xs -> x;;

let cauda (lista : float list) = 
  match lista with
  | [] -> failwith "lista vazia"
  | x :: xs -> xs;;

let rec ultimo (lista : float list) = 
  match lista with
  | [] -> failwith "lista vazia"
  | [x] -> x
  | x :: xs -> ultimo xs;;

let rec arranjo (lista : float list) = 
  match lista with
  | [] -> failwith "lista vazia"
  | [x] -> []
  | x :: xs -> (x :: arranjo xs);;

let rec somar (lista : float list) = 
  match lista with
  | [] -> 0.
  | x :: xs -> x +. somar xs;;

let rec faixa (num1 : float) num2 num3 = 
  match num1, num2, num3 with
  | i, f, p -> if (i > f)
               then []
               else i :: faixa (i +. p) f p;;

(*
let rec oposto (lista : float list) = 
  match lista with
  | ([] : float list) -> []
  | x :: xs -> oposto xs @ [x];;
*);;

let rec oposto (lista : float list) = 
  match lista with
  | ([] : float list) -> []
  | xs -> ultimo xs :: oposto (arranjo xs);;

let rec complista lista func = 
  match lista, func with
  | ([] : float list), qualificador -> []
  | x :: conjunto, qualificador ->
      if (qualificador x)
      then x :: complista conjunto qualificador
      else complista conjunto qualificador;;

let rec listamul (num : float) lista = 
  match num, lista with
  | _, [] -> []
  | n, x :: xs -> n *. x :: listamul n xs;;

let rec listapot (num : float) lista = 
  match num, lista with
  | _, [] -> []
  | n, x :: xs -> (potencia x n) :: (listapot n xs);;

let multiplo (num1 : float) (num2 : float) =
  match num1, num2 with
  | n, m ->
      if (int_of_float(n) mod int_of_float(m) = 0) then true else false;;

let rec listamult lista (num : float) =
  match lista, num with
  | ([] : float list), _ -> []
  | x :: xs, n -> if (int_of_float(n) mod int_of_float(x) = 0)
                  then x :: listamult xs n
                  else listamult xs n;;

let divisor (num : float) =
  match num with
  | n -> complista (faixa 1. n 1.) (multiplo n);;

let rec tamanho (lista : float list) = 
  match lista with
  | [] -> 0.
  | x :: xs -> 1. +. tamanho (xs);;

let checa_primo (num : float) =
  match num with
  | 1. -> false
  | 2. -> true
  | n ->
      if (tamanho (complista (faixa 2. (n -. 1.) 1.) (multiplo n)) > 0.)
      then false
      else true;;
      
let lprimos (num : float) =
  match num with
  | n -> complista (faixa 1. n 1.) (checa_primo);;

let rec membro (num : float) lista =
  match num, lista with
  | _, [] -> false
  | a, x :: xs -> if (a = x) then true else membro a xs;;

let rec juncao (lista1 : float list) lista2 =
  match lista1, lista2 with
  | [], [] -> []
  | a, [] -> a
  | [], b -> b
  | a :: ax, b -> a :: (juncao ax b);;

let rec unico (lista : float list) =
  match lista with
  | [] -> []
  | x :: xs -> if (membro x xs)
               then unico xs
               else x :: (unico xs);;

let rec insira (num : float) lista =
  match num, lista with
  | n, [] -> [n]
  | n, x :: xs -> if (n <= x)
                  then n :: x :: xs
                  else x :: insira n xs;;

let rec classifica (lista : float list) = 
  match lista with
  | [] -> []
  | x :: xs -> insira x (classifica xs);;

let uniao (lista1 : float list) lista2 =
  match lista1, lista2 with
  | a, b -> classifica (unico (juncao a b));;

let rec interseccao (lista1 : float list) lista2 =
  match lista1, lista2 with
  | a, [] -> []
  | [], b -> []
  | a, b :: bx -> if (membro b a)
                  then b :: (interseccao a bx) 
                  else interseccao a bx;;

let rec diferenca (lista1 : float list) lista2 =
  match lista1, lista2 with
  | a, [] -> []
  | [], b -> []
  | a :: ax, b -> if (membro a b)
                  then diferenca ax b 
                  else a :: (diferenca ax b);;

let rec sub_lista (lista1 : float list) lista2 =
  match lista1, lista2 with
  | [], [] -> true
  | [], _ -> true
  | _, [] -> false 
  | a :: ax, b -> if (membro a b)
                  then sub_lista ax b
                  else false;;

let igualdade (lista1 : float list) lista2 =
  match lista1, lista2 with
  | a, b -> sub_lista a b && sub_lista b a;;

let rec pega_pos (num : float) lista =
  match num, lista with
  | _, [] -> failwith "elemento nao existe na lista"
  | n, x :: xs -> if (n = x) then tamanho xs else pega_pos n xs;;

let busca (num : float) lista =
  match num, lista with
  | _, [] -> failwith "lista invalida"
  | n, x :: xs -> pega_pos n (oposto (x :: xs));;

let rec mostra (num : float) (lista : float list) = 
  match num, lista with
  | _, [] -> failwith "indice fora da faixa"
  | 0., x :: xs -> x
  | n, x :: xs -> mostra (n -. 1.) xs;;

let rec lista_max (lista : float list) =
  match lista with
  | ([] : float list) -> failwith "lista vazia"
  | [a] -> a
  | x :: xs -> if (x > lista_max xs)
               then x 
               else lista_max xs;;

let rec lista_min (lista : float list) =
  match lista with
  | ([] : float list) -> failwith "lista vazia"
  | [a] -> a
  | x :: xs -> if (x < lista_min xs)
               then x 
               else lista_min xs;;

let rec replicar (num1 : float) num2 =
  match num1, num2 with
  | quantidade, valor -> 
      if (quantidade = 0.) 
      then ([] : float list)
      else valor :: replicar (quantidade -. 1.) valor;;

let rec comeco (num : float) lista =
  match num, lista with
  | _, ([] : float list) -> []
  | n, x :: xs -> if (n > 0.)
                  then x :: comeco (n -. 1.) xs
                  else [];;

let rec final (num : float) lista =
  match num, lista with
  | _, ([] : float list) -> []
  | n, x :: xs -> if (n -. 1. > 0.)
                  then final (n -. 1.) xs
                  else xs;;

let separar lista = 
  match lista with
  | [] -> ([], [])
  | xs ->
      if (int_of_float(tamanho xs) mod 2 != 0)
      then comeco (float_of_int(int_of_float(tamanho xs) / 2) +. 1.) xs, final (float_of_int(int_of_float(tamanho xs) / 2) +. 1.) xs
      else comeco (float_of_int(int_of_float(tamanho xs) / 2)) xs, final (float_of_int(int_of_float(tamanho xs) / 2)) xs;;

let fatiar (num1 : float) num2 lista =
  match num1, num2, lista with
  | i, f, x -> final i (comeco f x);;

let rec mapa (lista : float list) func = 
  match lista, func with
  | [], funcao -> []
  | x :: xs, funcao -> funcao x :: mapa xs funcao;;

let rec filtro func (lista : float list) =
  match func, lista with
  | funcao, [] -> ([] : float list)
  | funcao, x :: xs -> if (funcao x)
                       then x :: (filtro funcao xs)
                       else filtro funcao xs;;

let rec reducao lista func (num : float) = 
  match lista, func, num with
  | ([] : float list), funcao, n -> n
  | x :: xs, funcao, n -> funcao x (reducao xs funcao n);;

let rec dobra_d func (num : float) lista =
  match func, num, lista with
  | f, n, ([] : float list) -> n
  | f, n, x :: xs -> f x (dobra_d f n xs);;

let rec dobra_e func (num : float) lista =
  match func, num, lista with
  | f, n, ([] : float list) -> n
  | f, n, x :: xs -> dobra_e f (f n x) xs;;

let rec compacta (lista1 : float list) lista2 = 
  match lista1, lista2 with
  | ([] : float list), b -> []
  | a, ([] : float list) -> []
  | x :: a, y :: b -> (x, y) :: compacta a  b;;

let rec dcp_base (lista1 : float list) lista2 lista3 =
  match lista1, lista2, lista3 with
  | xs, ys, [] -> (oposto xs, oposto ys)
  | xs, ys, (x, y) :: zs -> dcp_base (x :: xs) (y :: ys) zs;; 

let descompacta (lista : (float * float) list) =
  match lista with
  | ([] : (float * float) list) -> ([], [])
  | xs -> dcp_base [] [] xs;;

(*

==========================================
Exemplos de uso das funcionalidades OCaml:
==========================================

>>> Capítulo 2

    Ação .......: soma 2. 3.;;
    Resultado ..: - : float = 5.

    Ação .......: x_pi;;
    Resultado ..: - : float = 3.14159

    Ação .......: x_e;;
    Resultado ..: - : float = 2.71828

    Ação .......: quadsoma 2. 3.;;
    Resultado ..: - : float = 25.

    Ação .......: lognat 2.;;
    Resultado ..: - : 0.69314718055924973

    Ação .......: logbas 2. 10.;;
    Resultado ..: - : 0.3010299933321578

>>> Capítulo 3

    Ação .......: adicao 0. 0.;;
    Resultado ..: - : float = 0. 

    Ação .......: adicao 0. 1.;;
    Resultado ..: - : float = 1. 

    Ação .......: adicao 2. 0.;;
    Resultado ..: - : float = 2. 

    Ação .......: adicao 2. 1.;;
    Resultado ..: - : float = 3. 

    Ação .......: par 2.;;
    Resultado ..: - : bool = true

    Ação .......: par 3.;;
    Resultado ..: - : bool = false

    Ação .......: impar 2.;;
    Resultado ..: - : bool = false

    Ação .......: impar 3.;;
    Resultado ..: - : bool = true

    Ação .......: impar2 8.;;
    Resultado ..: - : bool = false
 
    Ação .......: impar2 5.;;
    Resultado ..: - : bool = true

    Ação .......: max 2. 3.;;
    Resultado ..: - : float = 3.

    Ação .......: max 5. 5.;;
    Resultado ..: - : float = 5.

    Ação .......: max 9. 3.;;
    Resultado ..: - : float = 9.

    Ação .......: valorx 2.;;
    Resultado ..: - : float = 10.

    Ação .......: valorx 9.;;
    Resultado ..: - : float = 1.8

    Ação .......: valorx 1.;;
    Resultado ..: - : float = 2.

    Ação .......: valorx 10.;;
    Resultado ..: - : float = 2.

    Ação .......: potencia 5. 3.;;
    Resultado ..: - : float = 125.

    Ação .......: fib 5.;;
    Resultado ..: - : float = 5.

    Ação .......: fib 7.;;
    Resultado ..: - : float = 13.

    Ação .......: fib 30.;;
    Resultado ..: - : float = 832040.

    Ação .......: fib2 7.;;
    Resultado ..: - : float = 13.

    Ação .......: fib2 30.;;
    Resultado ..: - : float = 832040.

    Ação .......: mdc 1024. 12.;;
    Resultado ..: - : float = 4.

    Ação .......: let y = 1.;;
    Resultado ..: val y : float = 1.

    Ação .......: (fun x -> x +. y) 9.;;
    Resultado ..: - : float = 10.

>>> Capítulo 4

    Ação .......: cabeca [1.; 2.; 3.; 4.; 5.];;
    Resultado ..: - : float = 1.

    Ação .......: cabeca [];;
    Resultado ..: Exception: Failure "lista vazia".

    Ação .......: cabeca [5.; 3.; 1.];;
    Resultado ..: - : float = 5.

    Ação .......: cauda [1.; 2.; 3.; 4.; 5.];;
    Resultado ..: - : float list = [2.; 3.; 4.; 5.]

    Ação .......: cauda [];;
    Resultado ..: Exception: Failure "lista vazia".

    Ação .......: cauda [5.; 3.; 1.];;
    Resultado ..: - : float list = [3.; 1.]

    Ação .......: ultimo [1.; 2.; 3.; 4.; 5.];;
    Resultado ..: - : float = 5.

    Ação .......: ultimo [];;
    Resultado ..: Exception: Failure "lista vazia".

    Ação .......: ultimo [5.; 3.; 1.];;
    Resultado ..: - : float = 1.

    Ação .......: arranjo [1.; 2.; 3.; 4.; 5.];;
    Resultado ..: - : float list = [1.; 2.; 3.; 4.]

    Ação .......: arranjo [];;
    Resultado ..: Exception: Failure "lista vazia".

    Ação .......: arranjo [5.; 3.; 1.];;
    Resultado ..: - : float list = [5.; 3.]

    Ação .......: somar [1.; 2.; 3.; 4.; 5.];;
    Resultado ..: - : float = 15.

    Ação .......: somar [5.; 3.; 1.];;
    Resultado ..: - : float = 9.

    Ação .......: faixa 3. 6. 1.;;
    Resultado ..: - : float list = [3.; 4.; 5.; 6.]

    Ação .......: faixa 1. 5. 1.;;
    Resultado ..: - : float list = [1.; 2.; 3.; 4.; 5.]

    Ação .......: faixa 1. 9. 2.;;
    Resultado ..: - : float list = [1.; 3.; 5.; 7.; 9.]

    Ação .......: oposto (faixa 3. 6. 1.);;
    Resultado ..: - : float list = [6.; 5.; 4.; 3.]

    Ação .......: oposto [1.; 2.; 3.; 4.; 5.];;
    Resultado ..: - : float list = [5.; 4.; 3.; 2.; 1.]

    Ação .......: complista (faixa 1. 20. 1.) (fun x -> par(x));;
    Resultado ..: - : float list = [2.; 4.; 6.; 8.; 10.; 12.; 14.; 16.; 18.; 20.] 

    Ação .......: complista [1.; 2.; 3.; 4.] (fun x -> par(x));;
    Resultado ..: - : float list = [2.; 4.] 

    Ação .......: listamul 2. [2.; 3.; 4.];;
    Resultado ..: - : float list = [4.; 6.; 8.] 

    Ação .......: listamul 2. (faixa 1. 5. 1.);;
    Resultado ..: - : float list = [2.; 4.; 6.; 8.; 10.]

    Ação .......: listapot 2. [2.; 3.; 4.];;
    Resultado ..: - : float list = [4.; 9.; 16.] 

    Ação .......: listapot 2. (faixa 1. 5. 1.);;
    Resultado ..: - : float list = [1.; 4.; 9.; 16.; 25.]

    Ação .......: multiplo 15. 3.;;
    Resultado ..: - : bool = true

    Ação .......: multiplo 15. 4.;;
    Resultado ..: - : bool = false

    Ação .......: multiplo 15. 5.;;
    Resultado ..: - : bool = true

    Ação .......: divisor 10.;;
    Resultado ..: - : float list = [1.; 2.; 5.; 10.]

    Ação .......: listamult [1.; 2.; 3.; 4.; 5.; 6.] 3.;;
    Resultado ..: - : float list = [1.; 3.]
 
    Ação .......: listamult (faixa 1. 10. 1.) 10.;;
    Resultado ..: - : float list = [1.; 2.; 5.; 10.]

    Ação .......: checa_primo 1.;;
    Resultado ..: - : bool = false

    Ação .......: checa_primo 2.;;
    Resultado ..: - : bool = true

    Ação .......: checa_primo 3.;;
    Resultado ..: - : bool = true

    Ação .......: checa_primo 4.;;
    Resultado ..: - : bool = false

    Ação .......: checa_primo 5.;;
    Resultado ..: - : bool = true

    Ação .......: lprimos 30.;;
    Resultado ..: - : float list = [2.; 3.; 5.; 7.; 11.; 13.; 17.; 19.; 23.; 29.]

    Ação .......: membro 2. [1.; 2.; 3.];;
    Resultado ..: - : bool = true

    Ação .......: membro 6. [1.; 2.; 3.];;
    Resultado ..: - : bool = false

    Ação .......: juncao [1.; 2.; 3.] [4.; 5.; 6.];;
    Resultado ..: - : float list = [1.; 2.; 3.; 4.; 5.; 6.]

    Ação .......: juncao [1.; 2.; 3.] [1.; 2.; 4.];;
    Resultado ..: - : float list = [1.; 2.; 3.; 1.; 2.; 4.]

    Ação .......: juncao [] [4.; 5.; 6.];;
    Resultado ..: - : float list = [4.; 5.; 6.]

    Ação .......: juncao [1.; 2.; 3.] [];;
    Resultado ..: - : float list = [1.; 2.; 3.]

    Ação .......: juncao [] [];;
    Resultado ..: - : 'a list = []

    Ação .......: unico [1.; 1.; 1.; 1.; 1.; 2.; 2.; 2.; 2.; 3.];;
    Resultado ..: - : float list = [1.; 2.; 3.]

    Ação .......: insira 9. [7.; 8.; 6.; 4.; 5.; 3.];;
    Resultado ..: - : float list = [7.; 8.; 6.; 4.; 5.; 3.; 9.]

    Ação .......: insira 2. [7.; 8.; 6.; 4.; 5.; 3.];;
    Resultado ..: - : float list = [2.; 7.; 8.; 6.; 4.; 5.; 3.]

    Ação .......: classifica [9.; 8.; 7.; 6.; 5.; 0.; 4.; 2.; 1.; 3.];;
    Resultado ..: - : float list = [0.; 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.]

    Ação .......: uniao [1.; 2.; 3.] [4.; 5.; 6.];;
    Resultado ..: - : float list = [1.; 2.; 3.; 4.; 5.; 6.]

    Ação .......: uniao [1.; 2.; 3.] [1.; 2.; 4.];;
    Resultado ..: - : float list = [1.; 2.; 3.; 4.]

    Ação .......: uniao [] [4.; 5.; 6.];;
    Resultado ..: - : float list = [4.; 5.; 6.]

    Ação .......: uniao [1.; 2.; 3.] [];;
    Resultado ..: - : float list = [1.; 2.; 3.]

    Ação .......: uniao [] [];;
    Resultado ..: - : 'a list = []

    Ação .......: interseccao [1.; 2.; 3.; 4.] [3.; 4.; 5.; 6.];;
    Resultado ..: - : float list = [3.; 4.]

    Ação .......: diferenca [1.; 2.; 3.; 4.; 5.] [1.; 2.; 6.; 7.];;
    Resultado ..: - : float list = [3.; 4.; 5.]

    Ação .......: sub_lista [1.; 2.; 3.] [1.; 2.; 3.; 4.; 5.];;
    Resultado ..: - : bool = true

    Ação .......: sub_lista [1.; 2.; 7.] [1.; 2.; 3.; 4.; 5.];;
    Resultado ..: - : bool = false

    Ação .......: sub_lista [] [];;
    Resultado ..: - : bool = true

    Ação .......: sub_lista [] [1.; 2.; 3.];;
    Resultado ..: - : bool = true

    Ação .......: sub_lista [1.; 2.; 3.] [];;
    Resultado ..: - : bool = false

    Ação .......: igualdade [1.; 2.; 3.] [3.; 2.; 1.];;
    Resultado ..: - : bool = true

    Ação .......: igualdade [1.; 2.; 3.] [3.; 2.; 1.; 0.];;
    Resultado ..: - : bool = false

    Ação .......: pega_pos 4. [1.; 2.; 3.; 4.; 5.];;
    Resultado ..: - : float = 1.

    Ação .......: pega_pos (negativo 2.) [1.; 2.; 3.; 4.; 5.];;
    Resultado ..: Exception: Failure "elemento nao existe na lista".

    Ação .......: busca 4. [1.; 2.; 3.; 4.; 5.];;
    Resultado ..: - : float = 3.

    Ação .......: busca 6. [1.; 2.; 3.; 4.; 5.];;
    Resultado ..: Exception: Failure "elemento nao existe na lista".

    Ação .......: mostra 2. [1.; 2.; 3.; 4.; 5.];;
    Resultado ..: - : float = 3.

    Ação .......: mostra 7. [1.; 2.; 3.; 4.; 5.];;
    Resultado ..: Exception: Failure "indice fora da faixa".

    Ação .......: lista_max [3.; 2.; 1.; 5.; 4.];;
    Resultado ..: - : float = 5.

    Ação .......: lista_min [3.; 2.; 1.; 5.; 4.];;
    Resultado ..: - : float = 1.

    Ação .......: replicar 5. 99.;;
    Resultado ..: - : float list = [99.; 99.; 99.; 99.; 99.]

    Ação .......: comeco 3. [5.; 4.; 3.; 2.; 1.];;
    Resultado ..: - : float list = [5.; 4.; 3.]

    Ação .......: comeco 7. [5.; 4.; 3.; 2.; 1.];;
    Resultado ..: - : float list = [5.; 4.; 3.; 2.; 1.]

    Ação .......: comeco 0. [5.; 4.; 3.; 2.; 1.];;
    Resultado ..: - : float list = []

    Ação .......: final 0. [5.; 4.; 3.; 2.; 1.];;
    Resultado ..: - : float list = [4.; 3.; 2.; 1.]

    Ação .......: final 100. [5.; 4.; 3.; 2.; 1.];;
    Resultado ..: - : float list = []

    Ação .......: final 3. [5.; 4.; 3.; 2.; 1.];;
    Resultado ..: - : float list = [2.; 1.]

    Ação .......: separar [1.; 2.; 3.; 4.; 5.];;
    Resultado ..: - : float list * float list = ([1.; 2.; 3.], [4.; 5.])

    Ação .......: separar [1.; 2.; 3.; 4.; 5.; 6.];;
    Resultado ..: - : float list * float list = ([1.; 2.; 3.], [4.; 5.; 6.])

    Ação .......: separar [1.];;
    Resultado ..: - : float list * float list = ([1.], [])

    Ação .......: separar [];;
    Resultado ..: - : 'a list * 'a list = ([], [])

    Ação .......: fatiar 3. 6. [1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.; 0.];;
    Resultado ..: - : float list = [4.; 5.; 6.]

    Ação .......: mapa [1.; 2.; 3.; 4.; 5.] (fun x -> x *. 3.);;
    Resultado ..: - : float list = [3.; 6.; 9.; 12.; 15.] 

    Ação .......: filtro (fun x -> impar(x)) [1.; 2.; 3.; 4.];;
    Resultado ..: - : float list = [1.; 3.] 

    Ação .......: reducao [1.; 2.; 3.; 4.] soma 0.;;
    Resultado ..: - : float = 10.

    Ação .......: dobra_d (fun x y -> 2. *. x +. y) 5. [1.; 2.; 3.];;
    Resultado ..: - : float = 17.

    Ação .......: dobra_d (-.) 7. [4.; 7.; 3.; 5.];;
    Resultado ..: - : float = 2.

    Ação .......: dobra_e (fun x y -> 2. *. x +. y) 5. [1.; 2.; 3.];;
    Resultado ..: - : float = 51.

    Ação .......: dobra_e (-.) 7. [4.; 7.; 3.; 5.];;
    Resultado ..: - : float = -12.

    Ação .......: compacta [1.; 2.; 3.] [4.; 5.; 6.];;
    Resultado ..: - : (float * float) list = [(1., 4.); (2., 5.); (3., 6.)]

    Ação .......: compacta [1.; 2.; 3.; 4.] [5.; 6.];;
    Resultado ..: - : (float * float) list = [(1., 5.); (2., 6.)]

    Ação .......: compacta [1.; 2.] [3.; 4.; 5.; 6.];;
    Resultado ..: - : (float * float) list = [(1., 3.); (2., 4.)]

    Ação .......: dcp_base [] [] [(8., 9.)];;
    Resultado ..: - : float list * float list = ([8.], [9.])

    Ação .......: dcp_base [] [] [(8., 9.); (1., 2.)];;
    Resultado ..: - : float list * float list = ([8.; 1.], [9.; 2.])

    Ação .......: descompacta [(1., 2.); (3., 4.)];;
    Resultado ..: - : float list * float list = ([1.; 3.], [2.; 4.])

    Ação .......: descompacta [(1., 2.); (3., 4.); (5., 6.); (7., 8.)];;
    Resultado ..: - : float list * float list = ([1.; 3.; 5.; 7.], [2.; 4.; 6.; 8.])

       ////   
      (o o)     
 __ooO_(_)_Ooo_________________________________________________________________________
 |_____|______|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|
 ___|_____|______|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|___
 |_____|______|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|
 ___| :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: |___
 |___ ::::'###::::'##::::'##::'######:::'##::::'##::'######::'########::'#######:: ___|
 ___| :::'## ##::: ##:::: ##:'##... ##:: ##:::: ##:'##... ##:... ##..::'##.... ##: |___
 |___ ::'##:. ##:: ##:::: ##: ##:::..::: ##:::: ##: ##:::..::::: ##:::: ##:::: ##: ___|
 ___| :'##:::. ##: ##:::: ##: ##::'####: ##:::: ##:. ######::::: ##:::: ##:::: ##: |___
 |___ : #########: ##:::: ##: ##::: ##:: ##:::: ##::..... ##:::: ##:::: ##:::: ##: ___|
 ___| : ##.... ##: ##:::: ##: ##::: ##:: ##:::: ##:'##::: ##:::: ##:::: ##:::: ##: |___
 |___ : ##:::: ##:. #######::. ######:::. #######::. ######::::: ##::::. #######:: ___|
 ___| :..:::::..:::.......::::......:::::.......::::......::::::..::::::.......::: |___
 |___ :'##::::'##::::'###::::'##::: ##::'########::::'###::::'##::: ##::'#######:: ___|
 ___| : ###::'###:::'## ##::: ###:: ##::..... ##::::'## ##::: ###:: ##:'##.... ##: |___
 |___ : ####'####::'##:. ##:: ####: ##:::::: ##::::'##:. ##:: ####: ##: ##:::: ##: ___|
 ___| : ## ### ##:'##:::. ##: ## ## ##::::: ##::::'##:::. ##: ## ## ##: ##:::: ##: |___
 |___ : ##. #: ##: #########: ##. ####:::: ##::::: #########: ##. ####: ##:::: ##: ___|
 ___| : ##:.:: ##: ##.... ##: ##:. ###::: ##:::::: ##.... ##: ##:. ###: ##:::: ##: |___
 |___ : ##:::: ##: ##:::: ##: ##::. ##:: ########: ##:::: ##: ##::. ##:. #######:: ___|
 ___| :..:::::..::..:::::..::..::::..::........::..:::::..::..::::..:::.......:::: |___
 |_____|______|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|
 ___|_____|______|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|___
 |_____|______|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|

*);;
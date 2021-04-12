(*

                          ////
                         (O O)
           +-------oOO----(_)--------------+
           |                               |
           |    Linguagem: SML/NJ (1983)   | 
           |                               |
           +---------------------oOO-------+
                        I__I__I
                         II II
                        ooO Ooo

 * ================================================ *
 *                                                  * 
 *           Arquivo BONUS (complementar)           * 
 *   Codigos exemplos: exercicios de aprendizagem   * 
 *  ----------------------------------------------  * 
 *                                                  * 
 * Parte integrante do livro: Algoritmos Funcionais * 
 * Copyright: Jose Augusto N. G. Manzano - 2a. ed.  *
 *                                                  *
 * A permissao de uso deste material esta vinculado *
 * a aquisicao do livro correspondente.             *
 *                                                  *
 * Scripts dos codigos para a linguagem SML/NJ usa- *
 * dos como ilustracao complementar para a fixacao  *
 * dos conceitos logicos abordados no livro. Nao    *
 * objetiva-se "ensinar" o uso da linguagem         *
 * Standard ML of New Jersey. Isso devera ser rea-  *
 * lizado a partir de outras fontes de aprendiza-   *
 * gem.                                             *
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
 * O uso de valores negativos em SML/NJ devem ser   *
 * realizados com o uso do símbolo "~" e nao do     *
 * simbolo "-".                                     *
 *                                                  *
 * Valores reais (ponto flutuante) devem ser usados *
 * com a definicao obrigatoria do ponto decimal se- * 
 * do de pelo menos um digito decimal, mesmo que    *
 * seja o algarismo zero.                           *
 *                                                  *
 * Por questoes de praticidade os valores das fun-  *
 * coes deste conjunto de scripts devem ser opera-  *
 * cionalizados com numeros reais (ponto flutuan-   *
 * te). Nenhum tratamento para uso de valores nume- *
 * mericos inteiros foi previsto neste conjunto de  *
 * funcoes.                                         *
 *                                                  *
 * Devido a caracteristicas particulares da lingua- *
 * gem as funcoes "Dobra_d.de" e "Dobra_e.de" sao   *
 * usadas para certos exemplos de forma diferen-    *
 * ciada ao uso previsto no livro (uso adaptado).   *
 *                                                  *
 * A linguagem ML (SML/NJ) nao opera comparacoes de *
 * igualdade para valores reais por questoes de se- *
 * guranca operacional. Desta forma, para realizar  *
 * tal acao recomenda-se o uso do algoritmo de tes- *
 * epsilon representado na funcao "quase_igual" que *
 * nao faz parte do livro "Algoritmos Funcionais"   *
 * por retratar um assunto especifico da linguagem  *
 * ML.                                              *
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

 *)

(* =========================================================================================== *)
(* >>>> Funcao de suporte as operacoes de igualdade com valores reais - Acao complementar <<<< *)
(* >>>> adaptado de: https://floating-point-gui.de/errors/NearlyEqualsTest.java.          <<<< *)
(* =========================================================================================== *)

fun quase_igual (a : real, b : real) =
  let 
    val absA = Real.abs a
    val absB = Real.abs b
  in 
    Real.== (a, b) orelse
    ( 
      if (Real.== (a, 0.0) orelse Real.== (b, 0.0) orelse (Real.abs(a - b) < Real.minNormalPos))
      then Real.abs(a - b) < (0.00001 * Real.minNormalPos)
      else Real.abs(a - b) / Real.min((absA + absB), Real.maxFinite) < 0.00001
    )
end;

(* =========================================================================================== *)

fun soma (valor1 : real, valor2 : real) =
  valor1 + valor2;

val x_pi = 3.14159;

val x_e = 2.71828;

fun quadrado (x : real) =
  Math.pow(x, 2.0);

fun soma2 (x : real, y : real) =
  x + y;

fun quadsoma (x : real, y : real) = 
  quadrado(soma2(x, y));

val peso = 99999999.49;

fun lognat (x : real) =
  peso * (Math.pow(x, (1.0 / peso)) - 1.0);

fun logbas (x : real, b : real) =
  lognat (x) / lognat(b);

fun adicao (x : real, y : real) = 
  if (quase_igual(x, 0.0)) then y else
  if (quase_igual(y, 0.0)) then x else
  x + y;

fun par (n : real) =
  if (trunc(n) mod 2 = 0) then true else false;

fun impar (n : real) =
  if (trunc(n) mod 2 <> 0) then true else false;

fun impar2 (n : real) =
  not(par(n));

fun max (x : real, y : real) =
  if (x > y) then x else y;

fun min (x : real, y : real) =
  if (x < y) then x else y;

fun negativo (n : real) =
  if (n < 0.0) then n else 0.0 - n;

fun valorx (n : real) = 
  if (quase_igual(n, 0.0)) then 0.0 else
  if (quase_igual(n, 1.0)) then 2.0 else
  if (n > 1.0 andalso n < 9.0) then n * 5.0 else n / 5.0;

fun potencia (x : real, n : real) =
  if (quase_igual(n, 0.0)) then 1.0 else
  if (quase_igual(n, 1.0)) then x else
  x * potencia (x, n - 1.0);

fun fib (n : real) =
  if (trunc(n) = 0) then 0.0 else
  if (trunc(n) = 1) then 1.0 else
  if (trunc(n) = 2) then 1.0 else
  fib (n - 1.0) + fib (n - 2.0);

fun fibbase(n : real, anterior : real, atual : real) =
  if (trunc(n) = 0) then anterior else
  if (trunc(n) = 1) then atual else
  if (trunc(n) = 2) then atual + anterior else
  fibbase (n - 1.0, atual, anterior + atual);

fun fib2 (n : real) =
  fibbase (n, 0.0, 1.0);

fun mdc (m : real, n : real) = 
  if (trunc(m) = 0) then n else
  mdc (real (trunc(n) mod trunc(m)), m);

fun cabeca [] = raise Fail("lista vazia")
  | cabeca (x :: xs) = x; 

fun cauda [] = raise Fail("lista vazia")
  | cauda (x :: xs) = xs;

fun ultimo [] = raise Fail("lista vazia")
  | ultimo [x] = x
  | ultimo (x :: xs) = ultimo(xs);

fun arranjo [] = raise Fail("lista vazia")
  | arranjo [x] = []
  | arranjo (x :: xs) = x :: arranjo(xs);

fun somar [] = 0.0
  | somar (x :: xs) = x + somar(xs);

fun faixa (i : real, f : real, p : real) = 
  if (i > f)
  then []
  else i :: faixa(i + p, f, p);

(*
fun oposto [] = []
  | oposto (x :: xs) = oposto(xs) @ [x]; 
*)

fun oposto [] = []
  | oposto xs = ultimo(xs) :: oposto(arranjo(xs));

fun complista ([], qualificador) = []
  | complista ((x :: conjunto), qualificador) = 
      if (qualificador(x))
      then (x :: complista(conjunto, qualificador))
      else complista(conjunto, qualificador);

fun listamul (_ : real, []) = []
  | listamul (n, x :: xs) = n * x :: listamul(n, xs);

fun listapot (_ : real, []) = []
  | listapot (n, x :: xs) = potencia(x, n) :: listapot(n, xs);

fun multiplo (n : real, m : real) =
  if (trunc(n) mod trunc(m) = 0) then true else false;

fun divisor (n : real) =
  complista(faixa(1.0, n, 1.0), (fn d => multiplo(n, d)));

fun listamult ([], _ : real) = []
  | listamult (x :: xs, n) = if (trunc(n) mod trunc(x) = 0)
                             then x :: listamult (xs, n)
                             else listamult (xs, n);

fun tamanho [] = 0.0
  | tamanho (x :: xs) = 1.0 + tamanho(xs);

fun checa_primo (n : real) = 
  if (trunc(n) = 1) then false else
  if (trunc(n) = 2) then true else
    if (tamanho(complista(faixa(2.0, n - 1.0, 1.0), (fn d => multiplo(n, d)))) > 0.0)
    then false
    else true;

fun lprimos (n : real) =
  complista(faixa(2.0, n, 1.0), (fn x => checa_primo(x)));

fun membro (_, []) = false
  | membro (a, x :: xs) = if (quase_igual(a, x)) then true else membro(a, xs);

fun juncao ([], []) : real list = []
  | juncao (a, []) = a
  | juncao ([], b) = b
  | juncao (a :: ax, b) = a :: juncao(ax, b);

fun unico [] = []
  | unico (x :: xs) = if (membro(x, xs))
                      then unico(xs)
                      else x :: (unico(xs));

fun insira (n : real, []) = [n]
  | insira (n, x :: xs) = if (n <= x)
                          then n :: x :: xs
                          else x :: insira(n, xs);

fun classifica [] = []
  | classifica (x :: xs) = insira(x, classifica(xs));

fun uniao (a, b) = 
  classifica(unico(juncao(a, b)));

fun interseccao (a : real list, []) = []
  | interseccao ([], b) = []
  | interseccao (a, b :: bx) = if (membro(b, a))
                               then b :: interseccao(a, bx) 
                               else interseccao(a, bx);

fun diferenca (a : real list, []) = []
  | diferenca ([], b) = []
  | diferenca (a :: ax, b) = if (membro(a, b))
                             then diferenca(ax, b) 
                             else a :: diferenca(ax, b);

fun sub_lista ([], []) = true
  | sub_lista ([], _) = true
  | sub_lista (_, []) = false
  | sub_lista (a :: ax, b) = if (membro(a, b))
                             then sub_lista(ax, b)
                             else false;

fun igualdade (a, b) =
  sub_lista(a, b) andalso sub_lista(b, a);

fun pega_pos (_, []) = raise Fail("elemento nao existe na lista")
  | pega_pos (n, x :: xs) = if (quase_igual(n, x)) then tamanho(xs) else pega_pos(n, xs);

fun busca (_, []) = raise Fail("lista invalida")
  | busca (n, x :: xs) = pega_pos(n, oposto(x :: xs));

fun mostra (_, []) = raise Fail("indice fora da faixa")
  | mostra (0, x :: xs) = x
  | mostra (n, x :: xs) = mostra(n - 1, xs);

fun lista_max ([] : real list) = raise Fail("lista vazia")
  | lista_max ([a]) = a
  | lista_max (x :: xs) = if (x > lista_max(xs))
                          then x 
                          else lista_max(xs);

fun lista_min ([] : real list) = raise Fail("lista vazia")
  | lista_min ([a]) = a
  | lista_min (x :: xs) = if (x < lista_min(xs))
                          then x 
                          else lista_min(xs);

fun replicar (quantidade : real, valor : real) = 
  if (quase_igual(quantidade, 0.0)) 
  then []
  else valor :: replicar(quantidade - 1.0, valor);

fun comeco (_, []) = []
  | comeco (n, x :: xs) = if (n > 0.0)
                          then x :: comeco(n - 1.0, xs)
                          else [];

fun final (_, []) = []
  | final (n, x :: xs) = if (n - 1.0 > 0.0)
                         then final(n - 1.0, xs)
                         else xs;

fun separar [] = ([], [])
  | separar xs = 
      if   ((trunc(tamanho(xs)) mod 2) <> 0)
      then (comeco(real(trunc(tamanho(xs)) div 2 + 1), xs), final(real(trunc(tamanho(xs)) div 2 + 1), xs))
      else (comeco(real(trunc(tamanho(xs)) div 2), xs), final(real(trunc(tamanho(xs)) div 2), xs));

fun fatiar (i, f, x) =
  final(i, comeco(f, x));

fun mapa ([], funcao) = []
  | mapa (x :: xs, funcao) = funcao(x) :: mapa(xs, funcao);

fun filtro (funcao, []) = []
  | filtro (funcao, x :: xs) = if (funcao(x))
                               then x :: filtro(funcao, xs)
                               else filtro(funcao, xs);

fun reducao ([], funcao, n) = n
  | reducao (x :: xs, funcao, n) = funcao(x, reducao(xs, funcao, n));

fun dobra_d (f, n, []) = n
  | dobra_d (f, n, x :: xs) = f(x, dobra_d(f, n, xs));

fun dobra_e (f, n, []) = n   
  | dobra_e (f, n, x :: xs) = dobra_e(f, f(n, x), xs);                         
  
fun compacta ([], b) = []
  | compacta (a, []) = []
  | compacta (x :: a, y :: b) = (x, y) :: compacta(a,  b);

fun dcp_base (xs, ys, []) = (oposto(xs), oposto(ys))
  | dcp_base (xs, ys, (x, y) :: zs) = dcp_base(x :: xs, y :: ys, zs);

fun descompacta [] = ([], [])
  | descompacta xs = dcp_base([], [], xs);

(*

===========================================
Exemplos de uso das funcionalidades SML/NJ:
===========================================

>>> Capítulo 2

    Ação .......: soma(2.0, 3.0);
    Resultado ..: val it = 5.0 : real

    Ação .......: x_pi;
    Resultado ..: val it = 3.14159 : real

    Ação .......: x_e;
    Resultado ..: val it = 2.71828 : real

    Ação .......: quadsoma(2.0, 3.0);
    Resultado ..: val it = 25.0 : real

    Ação .......: lognat(2.0);
    Resultado ..: val it = 0.693147180559 : real

    Ação .......: logbas(2.0, 10.0);
    Resultado ..: val it = 0.301029993332 : real

>>> Capítulo 3

    Ação .......: adicao(0.0, 0.0);
    Resultado ..: val it = 0.0 : real 

    Ação .......: adicao(0.0, 1.0);
    Resultado ..: val it = 1.0 : real 

    Ação .......: adicao(2.0, 0.0);
    Resultado ..: val it = 2.0 : real

    Ação .......: adicao(2.0, 1.0);
    Resultado ..: val it = 3.0 : real 

    Ação .......: par(2.0);
    Resultado ..: val it = true : bool

    Ação .......: par(3.0);
    Resultado ..: val it = false : bool

    Ação .......: impar(2.0);
    Resultado ..: val it = false : bool

    Ação .......: impar(3.0);
    Resultado ..: val it = true : bool

    Ação .......: impar2(8.0);
    Resultado ..: val it = false : bool

    Ação .......: impar2(5.0);
    Resultado ..: val it = true : bool

    Ação .......: max(2.0,3.0);
    Resultado ..: val it = 3.0 : real

    Ação .......: max(5.0,5.0);
    Resultado ..: val it = 5.0 : real

    Ação .......: max(9.0,3.0);
    Resultado ..: val it = 9.0 : real

    Ação .......: valorx(2.0);
    Resultado ..: val it = 10.0 : real

    Ação .......: valorx(9.0);
    Resultado ..: val it = 1.8 : real

    Ação .......: valorx(1.0);
    Resultado ..: val it = 2.0 : real

    Ação .......: valorx(10.0);
    Resultado ..: val it = 2.0 : real

    Ação .......: potencia(5.0, 3.0)
    Resultado ..: val it = 125.0 : real

    Ação .......: fib(5.0);
    Resultado ..: val it = 5.0 : real

    Ação .......: fib(7.0);
    Resultado ..: val it = 13.0 : real

    Ação .......: fib(30.0);
    Resultado ..: val it = 832040.0 : real

    Ação .......: fib2(7.0);
    Resultado ..: val it = 13.0 : real

    Ação .......: fib2(30.0);
    Resultado ..: val it = 832040.0 : real

    Ação .......: mdc(1024.0, 12.0);
    Resultado ..: val it = 4.0 : real

    Ação .......: val y = 1.0;
    Resultado ..: val y = 1.0 : real

    Ação .......: (fn x : real => x + y) 9.0;
    Resultado ..: val it = 10.0 : real

>>> Capítulo 4
 
     Ação .......: cabeca([1.0, 2.0, 3.0, 4.0, 5.0]);
     Resultado ..: val it = 1.0 : real
 
     Ação .......: cabeca([]);
     Resultado ..: uncaught exception Fail [Fail: lista vazia]
 
     Ação .......: cabeca([5.0, 3.0, 1.0]);
     Resultado ..: val it = 5.0 : real
 
     Ação .......: cauda([1.0, 2.0, 3.0, 4.0, 5.0]);
     Resultado ..: val it = [2.0,3.0,4.0,5.0] : real list
 
     Ação .......: cauda([]);
     Resultado ..: uncaught exception Fail [Fail: lista vazia]
 
     Ação .......: cauda([5.0, 3.0, 1.0]);
     Resultado ..: val it = [3.0,1.0] : real list
 
     Ação .......: ultimo([1.0, 2.0, 3.0, 4.0, 5.0]);
     Resultado ..: val it = 5.0 : real
 
     Ação .......: ultimo([]);
     Resultado ..: uncaught exception Fail [Fail: lista vazia]
 
     Ação .......: ultimo([5.0, 3.0, 1.0]);
     Resultado ..: val it = 1.0 : real
 
     Ação .......: arranjo([1.0, 2.0, 3.0, 4.0, 5.0]);
     Resultado ..: val it = [1.0,2.0,3.0,4.0] : real list
 
     Ação .......: arranjo([]);
     Resultado ..: uncaught exception Fail [Fail: lista vazia]
 
     Ação .......: arranjo([5.0, 3.0, 1.0]);
     Resultado ..: val it = [5.0,3.0] : real list
 
     Ação .......: somar([1.0, 2.0, 3.0, 4.0, 5.0]);
     Resultado ..: val it = 15.0 : real
 
     Ação .......: somar([5.0, 3.0, 1.0]);
     Resultado ..: val it = 9.0 : real
 
     Ação .......: faixa(3.0, 6.0, 1.0);
     Resultado ..: val it = [3.0,4.0,5.0,6.0] : real list
 
     Ação .......: faixa(1.0, 5.0, 1.0);
     Resultado ..: val it = [1.0,2.0,3.0,4.0,5.0] : real list
 
     Ação .......: faixa(1.0, 9.0, 2.0);
     Resultado ..: val it = [1.0,3.0,5.0,7.0,9.0] : real list
 
     Ação .......: oposto(faixa(3.0, 6.0, 1.0));
     Resultado ..: val it = [6.0,5.0,4.0,3.0] : real list
 
     Ação .......: oposto([1.0, 2.0, 3.0, 4.0, 5.0]);
     Resultado ..: val it = [5.0,4.0,3.0,2.0,1.0] : real list
 
     Ação .......: complista(faixa(1.0, 20.0, 1.0), fn x => par(x));
     Resultado ..: val it = [2.0,4.0,6.0,8.0,10.0,12.0,14.0,16.0,18.0,20.0] : real list 
 
     Ação .......: complista([1.0, 2.0, 3.0, 4.0, 5.0], fn x => par(x));
     Resultado ..: val it = [2.0,4.0] : real list 
 
     Ação .......: listamul(2.0, [2.0, 3.0, 4,0]);
     Resultado ..: val it = [4.0,6.0,8.0] : real list 
 
     Ação .......: listamul(2.0, faixa(1.0, 5.0, 1.0));
     Resultado ..: val it = [2.0,4.0,6.0,8.0,10.0] : real list
 
     Ação .......: listapot(2.0, [2.0, 3.0, 4.0]);
     Resultado ..: val it = [4.0,9.0,16.0] : real list 
 
     Ação .......: listapot(2.0, faixa(1.0, 5.0, 1.0));
     Resultado ..: val it = [1.0,4.0,9.0,16.0,25.0] : real list

     Ação .......: multiplo(15.0, 3.0);
     Resultado ..: val it = true : bool
 
     Ação .......: multiplo(15.0, 4.0);
     Resultado ..: val it = false : bool
 
     Ação .......: multiplo(15.0, 5.0);
     Resultado ..: val it = true : bool
 
     Ação .......: divisor(10.0);
     Resultado ..: val it = [1.0,2.0,5.0,10.0] : real list

     Ação .......: listamult([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], 3.0);
     Resultado ..: val it = [1.0,3.0] : real list
 
     Ação .......: listamult(faixa(1.0, 10.0, 1.0), 10.0); 
     Resultado ..: val it = [1.0,2.0,5.0,10.0] : real list
 
     Ação .......: checa_primo(1.0);
     Resultado ..: val it = false : bool
 
     Ação .......: checa_primo(2.0);
     Resultado ..: val it = true : bool
 
     Ação .......: checa_primo(3.0);
     Resultado ..: val it = true : bool
 
     Ação .......: checa_primo(4.0);
     Resultado ..: val it = false : bool
 
     Ação .......: checa_primo(5.0);
     Resultado ..: val it = true : bool
 
     Ação .......: lprimos(30.0);
     Resultado ..: val it = [2.0,3.0,5.0,7.0,11.0,13.0,17.0,19.0,23.0,29.0] : real list

     Ação .......: membro(2.0, [1.0, 2.0, 3.0]);
     Resultado ..: val it = true : bool
 
     Ação .......: membro(6.0, [1.0, 2.0, 3.0]);
     Resultado ..: val it = false : bool
 
     Ação .......: juncao([1.0, 2.0, 3.0], [4.0, 5.0, 6.0]);
     Resultado ..: val it = [1.0,2.0,3.0,4.0,5.0,6.0] : real list
 
     Ação .......: juncao([1.0, 2.0, 3.0], [1.0, 2.0, 4.0]);
     Resultado ..: val it = [1.0,2.0,3.0,1.0,2.0,4.0] : real list
 
     Ação .......: juncao([], [4.0, 5.0, 6.0]);
     Resultado ..: val it = [4.0,5.0,6.0] : real list
 
     Ação .......: juncao([1.0, 2.0, 3.0], []);
     Resultado ..: val it = [1.0,2.0,3.0] : real list
 
     Ação .......: juncao([], []);
     Resultado ..: val it = [] : real list
 
     Ação .......: unico([1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 2.0, 3.0]);
     Resultado ..: val it = [1.0,2.0,3.0] : real list
 
     Ação .......: insira(9.0, [7.0, 8.0, 6.0, 4.0, 5.0, 3.0]);
     Resultado ..: val it = [7.0,8.0,6.0,4.0,5.0,3.0,9.0] : real list
 
     Ação .......: insira(2.0, [7.0, 8.0, 6.0, 4.0, 5.0, 3.0]);
     Resultado ..: val it = [2.0,7.0,8.0,6.0,4.0,5.0,3.0] : real list
 
     Ação .......: classifica([9.0, 8.0, 7.0, 6.0, 5.0, 0.0, 4.0, 2.0, 1.0, 3.0]);
     Resultado ..: val it = [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0] : real list
 
     Ação .......: uniao([1.0, 2.0, 3.0], [4.0, 5.0, 6.0]);
     Resultado ..: val it = [1.0,2.0,3.0,4.0,5.0,6.0] : real list
 
     Ação .......: uniao([1.0, 2.0, 3.0], [1.0, 2.0, 4.0]);
     Resultado ..: val it = [1.0,2.0,3.0,4.0] : real list
 
     Ação .......: uniao([], [4.0, 5.0, 6.0]);
     Resultado ..: val it = [4.0,5.0,6.0] : real list
 
     Ação .......: uniao([1.0, 2.0, 3.0], []);
     Resultado ..: val it = [1.0,2.0,3.0] : real list
 
     Ação .......: uniao([], []);
     Resultado ..: val it = [] : real list
 
     Ação .......: interseccao([1.0, 2.0, 3.0, 4.0], [3.0, 4.0, 5.0, 6.0]);
     Resultado ..: val it = [3.0,4.0] : real list
 
     Ação .......: diferenca([1.0, 2.0, 3.0, 4.0, 5.0], [1.0, 2.0, 6.0, 7.0]);
     Resultado ..: val it = [3.0,4.0,5.0] : real list
 
     Ação .......: sub_lista([1.0, 2.0, 3.0], [1.0, 2.0, 3.0, 4.0, 5.0]);
     Resultado ..: val it = true : bool
 
     Ação .......: sub_lista([1.0, 2.0, 7.0], [1.0, 2.0, 3.0, 4.0, 5.0]);
     Resultado ..: val it = false : bool
 
     Ação .......: sub_lista([], []);
     Resultado ..: val it = true : bool
 
     Ação .......: sub_lista([], [1.0, 2.0, 3.0]);
     Resultado ..: val it = true : bool
 
     Ação .......: sub_lista([1.0, 2.0, 3.0], []);
     Resultado ..: val it = false : bool
 
     Ação .......: igualdade([1.0, 2.0, 3.0], [3.0, 2.0, 1.0]);
     Resultado ..: val it = true : bool
 
     Ação .......: igualdade([1.0, 2.0, 3.0], [3.0, 2.0, 1.0, 0.0]);
     Resultado ..: val it = false : bool
 
     Ação .......: pega_pos(4.0, [1.0, 2.0, 3.0, 4.0, 5.0]);
     Resultado ..: val it = 1.0 : real
 
     Ação .......: pega_pos(negativo(2.0), [1.0, 2.0, 3.0, 4.0, 5.0]);
     Resultado ..: uncaught exception Fail [Fail: elemento nao existe na lista]
 
     Ação .......: busca(4.0, [1.0, 2.0, 3.0, 4.0, 5.0]);
     Resultado ..: val it = 3.0 : real
 
     Ação .......: busca(6.0, [1.0, 2.0, 3.0, 4.0, 5.0]);
     Resultado ..: uncaught exception Fail [Fail: elemento nao existe na lista]
 
     Ação .......: mostra(2, [1.0, 2.0, 3.0, 4.0, 5.0]);
     Resultado ..: val it = 3.0 : real
 
     Ação .......: mostra(7, [1.0, 2.0, 3.0, 4.0, 5.0]);
     Resultado ..: uncaught exception Fail [Fail: indice fora da faixa]
 
     Ação .......: lista_max([3.0, 2.0, 1.0, 5.0, 4.0]);
     Resultado ..: val it = 5.0 : real
 
     Ação .......: lista_min([3.0, 2.0, 1.0, 5.0, 4.0]);
     Resultado ..: val it = 1.0 : real
 
     Ação .......: replicar(5.0, 99.0);
     Resultado ..: val it = [99.0,99.0,99.0,99.0,99.0] : real list
 
     Ação .......: comeco(3.0, [5.0, 4.0, 3.0, 2.0, 1.0]);
     Resultado ..: val it = [5.0,4.0,3.0] : real list
 
     Ação .......: comeco(7.0, [5.0, 4.0, 3.0, 2.0, 1.0]);
     Resultado ..: val it = [5.0,4.0,3.0,2.0,1.0] : real list
 
     Ação .......: comeco(0.0, [5.0, 4.0, 3.0, 2.0, 1.0]);
     Resultado ..: val it = [] : real list
 
     Ação .......: final(0.0, [5.0, 4.0, 3.0, 2.0, 1.0]);
     Resultado ..: val it = [4.0,3.0,2.0,1.0] : real list
 
     Ação .......: final(100.0, [5.0, 4.0, 3.0, 2.0, 1.0]);
     Resultado ..: val it = [] : real list
 
     Ação .......: final(3.0, [5.0, 4.0, 3.0, 2.0, 1.0]);
     Resultado ..: val it = [2.0,1.0] : real list
 
     Ação .......: separar([1.0, 2.0, 3.0, 4.0, 5.0]);
     Resultado ..: val it = ([1.0,2.0,3.0],[4.0,5.0]) : real list * real list
 
     Ação .......: separar([1.0, 2.0, 3.0, 4.0, 5.0, 6.0]);
     Resultado ..: val it = ([1.0,2.0,3.0],[4.0,5.0,6.0]) : real list * real list
 
     Ação .......: separar([1.0]);
     Resultado ..: val it = ([1.0],[]) : real list * real list
 
     Ação .......: separar([]);
     Resultado ..: val it = ([],[]) : ?.X1 list * ?.X1 list
 
     Ação .......: fatiar(3.0, 6.0, [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 0.0]);
     Resultado ..: val it = [4.0,5.0,6.0] : real list
 
     Ação .......: mapa([1.0, 2.0, 3.0, 4.0, 5.0], fn x => x * 3.0);
     Resultado ..: val it = [3.0,6.0,9.0,12.0,15.0] : real list 
 
     Ação .......: filtro(fn x => impar(x), [1.0, 2.0, 3.0, 4.0]);
     Resultado ..: val it = [1.0,3.0] : real list 
 
     Ação .......: reducao([1.0, 2.0, 3.0, 4.0], soma, 0.0);
     Resultado ..: val it = 10.0 : real
 
     Ação .......: dobra_d(fn (x, y) => 2.0 * x + y, 5.0, [1.0, 2.0, 3.0]);
     Resultado ..: val it = 17.0 : real
 
     Ação .......: (para) dobra_d((-), 7.0, [4.0, 7.0, 3.0, 5.0]);
                   (faca) dobra_d(fn (x, y) => x - y, 7.0, [4.0, 7.0, 3.0, 5.0]);
     Resultado ..: val it = 2.0 : real
 
     Ação .......: dobra_e(fn (x, y) => 2.0 * x + y, 5.0, [1.0, 2.0, 3.0]);
     Resultado ..: val it = 51.0 : real
 
     Ação .......: (para) dobra_e((-), 7.0, [4.0, 7.0, 3.0, 5.0]);
                   (faca) dobra_e(fn (x, y) => x - y, 7.0, [4.0, 7.0, 3.0, 5.0]);
     Resultado ..: val it = ~12.0 : real
 
     Ação .......: compacta([1.0, 2.0, 3.0], [4.0, 5.0, 6.0]);
     Resultado ..: val it = [(1.0,4.0),(2.0,5.0),(3.0,6.0)] : (real * real) list
 
     Ação .......: compacta([1.0, 2.0, 3.0, 4.0], [5.0, 6.0]);
     Resultado ..: val it = [(1.0,5.0),(2.0,6.0)] : (real * real) list
 
     Ação .......: compacta([1.0, 2.0], [3.0, 4.0, 5.0, 6.0]);
     Resultado ..: val it = [(1.0,3.0),(2.0,4.0)] : (real * real) list
 
     Ação .......: dcp_base([], [], [(8.0, 9.0)]);
     Resultado ..: val it = ([8.0],[9.0]) : real list * real list
 
     Ação .......: dcp_base([], [], [(8.0, 9.0), (1.0, 2.0)]);
     Resultado ..: val it = ([8.0,1.0],[9.0,2.0]) : real list * real list
 
     Ação .......: descompacta([(1.0, 2.0), (3.0, 4.0)]);
     Resultado ..: val it = ([1.0,3.0],[2.0,4.0]) : real list * real list
 
     Ação .......: descompacta([(1.0, 2.0), (3.0, 4.0), (5.0, 6.0), (7.0, 8.0)]);
     Resultado ..: val it = ([1.0,3.0,5.0,7.0],[2.0,4.0,6.0,8.0]) : real list * real list

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

*)


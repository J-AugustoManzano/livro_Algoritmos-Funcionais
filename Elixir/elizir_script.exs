#                          ////
#                         (O O)
#           +-------oOO----(_)--------------+
#           |                               |
#           |    Linguagem: Elixir (2011)   | 
#           |                               |
#           +---------------------oOO-------+
#                        I__I__I
#                         II II
#                        ooO Ooo
#
# * ================================================ *
# *                                                  * 
# *           Arquivo BONUS (complementar)           * 
# *   Codigos exemplos: exercicios de aprendizagem   * 
# *  ----------------------------------------------  * 
# *                                                  * 
# * Parte integrante do livro: Algoritmos Funcionais * 
# * Copyright: Jose Augusto N. G. Manzano - 2a. ed.  *
# *                                                  *
# * A permissao de uso deste material esta vinculado *
# * a aquisicao do livro correspondente.             *
# *                                                  *
# * Scripts dos codigos para a linguagem Elixir usa- *
# * dos como ilustracao complementar para a fixacao  *
# * dos conceitos logicos abordados no livro. Nao    *
# * objetiva-se "ensinar" o uso da linguagem Elixir. *
# * Isso devera ser realizado a partir de outras     *
# * fontes de aprendizagem.                          *
# *                                                  *
# * As funcoes estao otimizadas para serem operacio- *
# * nalizadas apenas com valores numericos.          *
# *                                                  *
# * Os exemplos deste arquivo sao adaptacoes escri-  *
# * tas o mais próximo possível dos algoritmos tra-  *
# * balhados no texto do livro. Detalhes operacio-   *
# * nais, particulares e tipicos da linguagem foram  *
# * propositalmente omitidos.                        *
# *                                                  *
# * Para um funcionamento mais adquado recomenda-se  *
# * configurar o ambiente "IEx" com "charlists:"     *
# * ajusatdo para ":as_lists".                       *
# *                                                  *
# * Devido a caracteristicas particulares da lingua- *
# * gem as funcoes "Dobra_d.de" e "Dobra_e.de" sao   *
# * usadas para certos exemplos de forma diferen-    *
# * ciada ao uso previsto no livro (uso adaptado).   *
# *                                                  *
# * Todas as funcoes deste arquivo podem ser manti-  *
# * das em um unico modulo. No entanto, optou-se por *
# * faze-las de forma separada para deixar o conteu- *
# * do arquivo mais proximo aos codigos de scripts   *
# * usados e definidos no livro.                     *
# *                                                  *
# *                                                  *
# * OBSERVACAO                                       *
# * ==========                                       *
# *                                                  *
# * Para uso deste documento parte-se do pressuposto *
# * de que:                                          *
# *                                                  *
# * - voce possui os conhecimentos informaticos ne-  *
# *   cessarios de instalacao, configuracao da lin-  *
# *   guagem demonstrada e que seu sistema esta      *
# *   pronto para uso a partir da linha de comando   *
# *   do modo "Terminal".                            *
# *                                                  *
# * ================================================ *

IEx.configure(inspect: [charlists: :as_lists])

defmodule Soma do
  def de(valor1, valor2), do: valor1 + valor2
end

defmodule Const do
  def x_pi, do: 3.14159
  def x_e,  do: 2.71828
  def peso, do: 99999999.49
end

defmodule Quadrado do
  def de(x), do: :math.pow(x, 2)
end

defmodule Soma2 do
  def de(x, y), do: x + y
end

defmodule Quadsoma do
  def de(x, y), do: Quadrado.de Soma2.de x, y
end

defmodule Lognat do
  def de(x), do: Const.peso * (:math.pow(x, 1 / Const.peso) - 1)
end

defmodule Logbas do
  def de(x, b), do: (Lognat.de x) / (Lognat.de b)
end

defmodule Adicao do
  def de(0, y), do: y
  def de(x, 0), do: x
  def de(x, y), do: x + y
end

defmodule Par do
  def de(n), do: if (Integer.mod(n, 2) == 0), do: true, else: false 
end

defmodule Impar do
  def de(n), do: if (Integer.mod(n, 2) != 0), do: true, else: false 
end

defmodule Impar2 do
  def de(n), do: ! (Par.de n) 
end

defmodule Max do
  def de(x, y), do: if (x > y), do: x, else: y
end

defmodule Min do
  def de(x, y), do: if (x < y), do: x, else: y
end

defmodule Negativo do
  def de(n), do: if (n < 0), do: n, else: 0 - n
end

defmodule Valorx do
  def de(0), do: 0
  def de(1), do: 2
  def de(n), do: if (n > 1 and n < 9), do: n * 5, else: n / 5
end

defmodule Potencia do
  def de(x, 0), do: 1
  def de(x, 1), do: x
  def de(x, n), do: x * de(x, n - 1)
end

defmodule Fib do
  def de(0), do: 0
  def de(1), do: 1
  def de(2), do: 1
  def de(n), do: de(n - 1) + de(n - 2)
end

defmodule Fibbase do
  def de(0, anterior, atual), do: anterior
  def de(1, anterior, atual), do: atual
  def de(2, anterior, atual), do: atual + anterior
  def de(n, anterior, atual), do: de(n - 1, atual, anterior + atual)
end

defmodule Fib2 do
  def de(n), do: Fibbase.de(n, 0, 1)
end

defmodule Mdc do
  def de(0, n), do: n
  def de(m, n), do: de(Integer.mod(n, m), m) 
end

defmodule Cabeca do
  def de([]), do: raise ArgumentError, message: "lista vazia"
  def de([x | xs]), do: x
end

defmodule Cauda do
  def de([]), do: raise ArgumentError, message: "lista vazia"
  def de([x | xs]), do: xs
end

defmodule Ultimo do
  def de([]), do: raise ArgumentError, message: "lista vazia"
  def de([x]), do: x
  def de([x | xs]), do: de(xs)
end

defmodule Arranjo do
  def de([]), do: raise ArgumentError, message: "lista vazia"
  def de([x]), do: []
  def de([x | xs]), do: [x | de(xs)]
end

defmodule Somar do
  def de([]), do: 0
  def de([x | xs]), do: x + de(xs)
end

defmodule Faixa do
  def de(i, f, p), do: if (i > f), 
                       do: [], 
                       else: [i | de(i + p, f, p)]
end

# defmodule Oposto do
#  def de([]), do: []
#  def de([x | xs]), do: de(xs) ++ [x]
# end

defmodule Oposto do
  def de([]), do: []
  def de(xs), do: [Ultimo.de(xs) | de(Arranjo.de(xs))]
end

defmodule Complista do
  def de([], qualificador), do: []
  def de([x | conjunto], qualificador), do: 
    if (qualificador.(x)), 
    do: [x | de(conjunto, qualificador)], 
    else: de(conjunto, qualificador)
end

defmodule Listamul do
  def de(_, []), do: []
  def de(n, [x | xs]), do: [n * x | de(n, xs)]
end

defmodule Listapot do
  def de(_, []), do: []
  def de(n, [x | xs]), do: [Potencia.de(x, n) | de(n, xs)]
end

defmodule Multiplo do
  def de(n, m), do: 
    if (Integer.mod(n, m) == 0), do: true, else: false
end

defmodule Listamult do
  def de([], _), do: []
  def de([x | xs], n), do: if (Integer.mod(n, x) == 0),
                           do: [x | de(xs, n)],
                           else: de(xs, n)
end

defmodule Divisor do
  def de(n), do: Complista.de (Faixa.de 1, n, 1), fn (d) -> Multiplo.de(n, d) end
end

defmodule Tamanho do
  def de([]), do: 0
  def de([x | xs]), do: 1 + de(xs)
end

defmodule Checa_primo do
  def de(1), do: false
  def de(2), do: true
  def de(n), do:
    if (Tamanho.de(Complista.de (Faixa.de 2, n - 1, 1), fn (d) -> Multiplo.de(n, d) end) > 0), 
    do: false, 
    else: true
end

defmodule Lprimos do
  def de(n), do: Complista.de (Faixa.de 1, n, 1), fn (x) -> Checa_primo.de(x) end
end

defmodule Membro do
  def de(_, []), do: false
  def de(a, [x | xs]), do: if (a == x), do: true, else: de(a, xs)
end

defmodule Juncao do
  def de([], []), do: []
  def de(a, []), do: a
  def de([], b), do: b
  def de([a | ax], b), do: [a | de(ax, b)]
end

defmodule Unico do
  def de([]), do: []
  def de([x | xs]), do: if (Membro.de x, xs), 
                        do: de(xs),
                        else: [x | de(xs)] 
end 

defmodule Insira do
  def de(n, []), do: [n]
  def de(n, [x | xs]), do: if (n <= x),
                           do: [n | [x | xs]],
                           else: [x | de(n, xs)]
end

defmodule Classifica do
  def de([]), do: []
  def de([x | xs]), do: Insira.de x, de(xs)
end

defmodule Uniao do
  def de(a, b), do: Classifica.de(Unico.de(Juncao.de a, b))
end

defmodule Interseccao do
  def de(a, []), do: []
  def de([], b), do: []
  def de(a, [b | bx]), do: if (Membro.de b, a),
                           do: [b | de(a, bx)],
                           else: de(a, bx)
end

defmodule Diferenca do
  def de(a, []), do: []
  def de([], b), do: []
  def de([a | ax], b), do: if (Membro.de a, b),
                           do: de(ax, b),
                           else: [a | de(ax, b)]
end


defmodule Sub_lista do
  def de([], y), do: true
  def de([x | xs], y), do: if (Membro.de x, y),
                           do: de(xs, y),
                           else: false
end

defmodule Igualdade do
  def de(a, b), do: Sub_lista.de(a, b) and Sub_lista.de(b, a)
end

defmodule Pega_pos do
  def de(_, []), do: raise ArgumentError, message: "elemento nao existe na lista"
  def de(n, [x | xs]), do: if (n == x), do: Tamanho.de(xs), else: de(n, xs)
end

defmodule Busca do
  def de(_, []), do: raise ArgumentError, message: "lista invalida" 
  def de(n, [x | xs]), do: Pega_pos.de(n, (Oposto.de([x | xs])))
end

defmodule Mostra do
  def de(_, []), do: raise ArgumentError, message: "indice fora da faixa"
  def de(0, [x | xs]), do: x
  def de(n, [x | xs]), do: de(n - 1, xs)
end

defmodule Lista_max do
  def de([]), do: raise ArgumentError, message: "lista vazia"
  def de([a]), do: a
  def de([x | xs]), do: if (x > de(xs)),
                        do: x, 
                        else: de(xs)
end

defmodule Lista_min do
  def de([]), do: raise ArgumentError, message: "lista vazia"
  def de([a]), do: a
  def de([x | xs]), do: if (x < de(xs)),
                        do: x, 
                        else: de(xs)
end

defmodule Replicar do
  def de(quantidade, valor), do: 
    if (quantidade == 0), 
    do: [],
    else: [valor | (de(quantidade - 1, valor))]
end

defmodule Comeco do
  def de(_, []), do: []
  def de(n, [x | xs]), do: if (n > 0),
                           do: [x | de(n - 1, xs)],
                           else: []
end

defmodule Final do
  def de(_, []), do: []
  def de(n, [x | xs]), do: if (n - 1 > 0),
                           do: de(n - 1, xs),
                           else: xs
end

defmodule Separar do
  def de([]), do: {[], []}
  def de(xs), do: 
    if (Integer.mod(Tamanho.de(xs), 2) != 0),
    do: {(Comeco.de (div(Tamanho.de(xs), 2) + 1), xs), (Final.de (div(Tamanho.de(xs), 2) + 1), xs)},
    else: {(Comeco.de (div(Tamanho.de(xs), 2)), xs), (Final.de (div(Tamanho.de(xs), 2)), xs)}
end

defmodule Fatiar do
  def de(i, f, x), do: Final.de i, (Comeco.de f, x)
end

defmodule Mapa do
  def de([], funcao), do: []
  def de([x | xs], funcao), do: [funcao.(x) | de(xs, funcao)]
end

defmodule Filtro do
  def de(funcao, []), do: []
  def de(funcao, [x | xs]), do: if (funcao.(x)),
                                do: [x | de(funcao, xs)],
                                else: de(funcao, xs)
end

defmodule Reducao do
  def de([], funcao, n), do: n
  def de([x | xs], funcao, n), do: funcao.(x, de(xs, funcao, n))
end

defmodule Dobra_d do
  def de(f, n, []), do: n
  def de(f, n, [x | xs]), do: f.(x, de(f, n, xs))
end

defmodule Dobra_e do
  def de(f, n, []), do: n
  def de(f, n, [x | xs]), do: de(f, f.(n, x), xs)
end

defmodule Compacta do                             
  def de([], b), do: []
  def de(a, []), do: []
  def de([x | a], [y | b]), do: [{x, y} | de(a, b)]
end

defmodule Dcp_base do
  def de(xs, ys, []), do: {Oposto.de(xs), Oposto.de(ys)}
  def de(xs, ys, [{x, y} | zs]), do: de([x | xs], [y | ys], zs)
end

defmodule Descompacta do
  def de([]), do: ({[], []})
  def de(xs), do: Dcp_base.de([], [], xs)
end

# ===========================================
# Exemplos de uso das funcionalidades Elixir:
# ===========================================
# 
# >>> Capítulo 2
# 
#     Ação .......: Soma.de 2, 3
#     Resultado ..: 5
# 
#     Ação .......: Const.x_pi
#     Resultado ..: 3.14159
# 
#     Ação .......: Const.x_e
#     Resultado ..: 2.71828
# 
#     Ação .......: Quadsoma.de 2, 3
#     Resultado ..: 25.0
# 
#     Ação .......: Lognat.de 2
#     Resultado ..: 0.6931471805592497
# 
#     Ação .......: Logbas.de 2, 10
#     Resultado ..: 0.3010299933321578
# 
# >>> Capítulo 3
# 
#     Ação .......: Adicao.de 0, 0
#     Resultado ..: 0 
# 
#     Ação .......: Adicao.de 0, 1
#     Resultado ..: 1 
# 
#     Ação .......: Adicao.de 2, 0
#     Resultado ..: 2
# 
#     Ação .......: Adicao.de 2, 1
#     Resultado ..: 3 
# 
#     Ação .......: Par.de 2
#     Resultado ..: true
# 
#     Ação .......: Par.de 3
#     Resultado ..: false
# 
#     Ação .......: Impar.de 2
#     Resultado ..: false
# 
#     Ação .......: Impar.de 3
#     Resultado ..: true
# 
#     Ação .......: Impar2.de 8
#     Resultado ..: false
# 
#     Ação .......: Impar2.de 5
#     Resultado ..: true
# 
#     Ação .......: Max.de 2, 3
#     Resultado ..: 3
# 
#     Ação .......: Max.de 5, 5
#     Resultado ..: 5
# 
#     Ação .......: Max.de 9, 3
#     Resultado ..: 9
# 
#     Ação .......: Valorx.de 2
#     Resultado ..: 10
# 
#     Ação .......: Valorx.de 9
#     Resultado ..: 1.8
# 
#     Ação .......: Valorx.de 1
#     Resultado ..: 2
# 
#     Ação .......: Valorx.de 10
#     Resultado ..: 2.0
# 
#     Ação .......: Potencia.de 5, 3
#     Resultado ..: 125
# 
#     Ação .......: Fib.de 5
#     Resultado ..: 5
# 
#     Ação .......: Fib.de 7
#     Resultado ..: 13
# 
#     Ação .......: Fib.de 30
#     Resultado ..: 832040
# 
#     Ação .......: Fib2.de 7
#     Resultado ..: 13
# 
#     Ação .......: Fib2.de 30
#     Resultado ..: 832040
# 
#     Ação .......: Mdc.de 1024, 12
#     Resultado ..: 4
# 
#     Ação .......: y = 1
#     Resultado ..: 1
# 
#     Ação .......: fn (x) -> x + y end.(9)
#     Resultado ..: 10
# 
# >>> Capítulo 4
# 
#     Ação .......: Cabeca.de [1, 2, 3, 4, 5]
#     Resultado ..: 1
# 
#     Ação .......: Cabeca.de []
#     Resultado ..: ** (ArgumentError) lista vazia
# 
#     Ação .......: Cabeca.de [5, 3, 1]
#     Resultado ..: 5
# 
#     Ação .......: Cauda.de [1, 2, 3, 4, 5]
#     Resultado ..: [2, 3, 4, 5]
# 
#     Ação .......: Cauda.de []
#     Resultado ..: ** (ArgumentError) lista vazia
# 
#     Ação .......: Cauda.de [5, 3, 1]
#     Resultado ..: [3, 1]
# 
#     Ação .......: Ultimo.de [1, 2, 3, 4, 5]
#     Resultado ..: 5
# 
#     Ação .......: Ultimo.de []
#     Resultado ..: ** (ArgumentError) lista vazia
# 
#     Ação .......: Ultimo.de [5, 3, 1]
#     Resultado ..: 1
# 
#     Ação .......: Arranjo.de [1, 2, 3, 4, 5]
#     Resultado ..: [1, 2, 3, 4]
# 
#     Ação .......: Arranjo.de []
#     Resultado ..: ** (ArgumentError) lista vazia
# 
#     Ação .......: Arranjo.de [5, 3, 1]
#     Resultado ..: [5, 3]
# 
#     Ação .......: Somar.de [1, 2, 3, 4, 5]
#     Resultado ..: 15
# 
#     Ação .......: Somar.de [5, 3, 1]
#     Resultado ..: 9
# 
#     Ação .......: Faixa.de 3, 6, 1
#     Resultado ..: [3, 4, 5, 6]
# 
#     Ação .......: Faixa.de 1, 5, 1
#     Resultado ..: [1, 2, 3, 4, 5]
# 
#     Ação .......: Faixa.de 1, 9, 2
#     Resultado ..: [1, 3, 5, 7, 9]
# 
#     Ação .......: Oposto.de (Faixa.de 3, 6, 1)
#     Resultado ..: [6, 5, 4, 3]
# 
#     Ação .......: Oposto.de  [1, 2, 3, 4, 5]
#     Resultado ..: [5, 4, 3, 2, 1]
# 
#     Ação .......: Complista.de (Faixa.de 1, 20, 1), fn (x) -> Par.de x end
#     Resultado ..: [2, 4, 6, 8, 10, 12, 14, 16, 18, 20] 
# 
#     Ação .......: Complista.de [1, 2, 3, 4, 5], fn (x) -> Par.de x end
#     Resultado ..: [2, 4] 
# 
#     Ação .......: Listamul.de 2, [2, 3, 4]
#     Resultado ..: [4, 6, 8] 
# 
#     Ação .......: Listamul.de 2, (Faixa.de 1, 5, 1)
#     Resultado ..: [2, 4, 6, 8, 10]
# 
#     Ação .......: Listapot.de 2, [2, 3, 4]
#     Resultado ..: [4, 9, 16] 
# 
#     Ação .......: Listapot.de 2, (Faixa.de 1, 5, 1)
#     Resultado ..: [1, 4, 9, 16, 25]
# 
#     Ação .......: Multiplo.de 15, 3
#     Resultado ..: true
# 
#     Ação .......: Multiplo.de 15, 4
#     Resultado ..: false
# 
#     Ação .......: Multiplo.de 15, 5
#     Resultado ..: true
#
#     Ação .......: Listamult.de [1, 2, 3, 4, 5, 6], 3
#     Resultado ..: [1, 3]
# 
#     Ação .......: Listamult.de (Faixa.de 1, 10, 1), 10 
#     Resultado ..: [1, 2, 5, 10]
#
#     Ação .......: Divisor.de 10
#     Resultado ..: [1, 2, 5, 10]
# 
#     Ação .......: Checa_primo.de 1
#     Resultado ..: false
# 
#     Ação .......: Checa_primo.de 2
#     Resultado ..: true
# 
#     Ação .......: Checa_primo.de 3
#     Resultado ..: true
# 
#     Ação .......: Checa_primo.de 4
#     Resultado ..: false
# 
#     Ação .......: Checa_primo.de 5
#     Resultado ..: true
# 
#     Ação .......: Lprimos.de 30
#     Resultado ..: [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
# 
#     Ação .......: Membro.de 2, [1, 2, 3]
#     Resultado ..: true
# 
#     Ação .......: Membro.de 6, [1, 2, 3]
#     Resultado ..: false
# 
#     Ação .......: Juncao.de [1, 2, 3], [4, 5, 6]
#     Resultado ..: [1, 2, 3, 4, 5, 6]
# 
#     Ação .......: Juncao.de [1, 2, 3], [1, 2, 4]
#     Resultado ..: [1, 2, 3, 1, 2, 4]
# 
#     Ação .......: Juncao.de [], [4, 5, 6]
#     Resultado ..: [4, 5, 6]
# 
#     Ação .......: Juncao.de [1, 2, 3], []
#     Resultado ..: [1, 2, 3]
# 
#     Ação .......: Juncao.de [], []
#     Resultado ..: []
# 
#     Ação .......: Unico.de [1, 1, 1, 1, 1, 2, 2, 2, 2, 3]
#     Resultado ..: [1, 2, 3]
# 
#     Ação .......: Insira.de 9, [7, 8, 6, 4, 5, 3]
#     Resultado ..: [7, 8, 6, 4, 5, 3, 9]
# 
#     Ação .......: Insira.de 2, [7, 8, 6, 4, 5, 3]
#     Resultado ..: [2, 7, 8, 6, 4, 5, 3]
# 
#     Ação .......: Classifica.de [9, 8, 7, 6, 5, 0, 4, 2, 1, 3]
#     Resultado ..: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
# 
#     Ação .......: Uniao.de [1, 2, 3], [4, 5, 6]
#     Resultado ..: [1, 2, 3, 4, 5, 6]
# 
#     Ação .......: Uniao.de [1, 2, 3], [1, 2, 4]
#     Resultado ..: [1, 2, 3, 4]
# 
#     Ação .......: Uniao.de [], [4, 5, 6]
#     Resultado ..: [4, 5, 6]
# 
#     Ação .......: Uniao.de [1, 2, 3], []
#     Resultado ..: [1, 2, 3]
# 
#     Ação .......: Uniao.de [], []
#     Resultado ..: []
# 
#     Ação .......: Interseccao.de [1, 2, 3, 4], [3, 4, 5, 6]
#     Resultado ..: [3, 4]
# 
#     Ação .......: Diferenca.de [1, 2, 3, 4, 5], [1, 2, 6, 7]
#     Resultado ..: [3, 4, 5]
# 
#     Ação .......: Sub_lista.de [1, 2, 3], [1, 2, 3, 4, 5]
#     Resultado ..: true
# 
#     Ação .......: Sub_lista.de [1, 2, 7], [1, 2, 3, 4, 5]
#     Resultado ..: false
# 
#     Ação .......: Sub_lista.de [], []
#     Resultado ..: true
# 
#     Ação .......: Sub_lista.de [], [1, 2, 3]
#     Resultado ..: true
# 
#     Ação .......: Sub_lista.de [1, 2, 3], []
#     Resultado ..: false
# 
#     Ação .......: Igualdade.de [1, 2, 3], [3, 2, 1]
#     Resultado ..: true
# 
#     Ação .......: Igualdade.de [1, 2, 3], [3, 2, 1, 0]
#     Resultado ..: false
# 
#     Ação .......: Pega_pos.de 4, [1, 2, 3, 4, 5]
#     Resultado ..: 1
# 
#     Ação .......: Pega_pos.de (Negativo.de 2), [1, 2, 3, 4, 5]
#     Resultado ..: ** (ArgumentError) elemento nao existe na lista
# 
#     Ação .......: Busca.de 4, [1, 2, 3, 4, 5]
#     Resultado ..: 3
# 
#     Ação .......: Busca.de 6, [1, 2, 3, 4, 5]
#     Resultado ..: ** (ArgumentError) elemento nao existe na lista
# 
#     Ação .......: Mostra.de 2, [1, 2, 3, 4, 5]
#     Resultado ..: 3
# 
#     Ação .......: Mostra.de 7, [1, 2, 3, 4, 5]
#     Resultado ..: Exception: Failure "indice fora da faixa".
# 
#     Ação .......: Lista_max.de [3, 2, 1, 5, 4]
#     Resultado ..: 5
# 
#     Ação .......: Lista_min.de [3, 2, 1, 5, 4]
#     Resultado ..: 1
# 
#     Ação .......: Replicar.de 5, 99
#     Resultado ..: [99, 99, 99, 99, 99]
# 
#     Ação .......: Comeco.de 3, [5, 4, 3, 2, 1]
#     Resultado ..: [5, 4, 3]
# 
#     Ação .......: Comeco.de 7, [5, 4, 3, 2, 1]
#     Resultado ..: [5, 4, 3, 2, 1]
# 
#     Ação .......: Comeco.de 0, [5, 4, 3, 2, 1]
#     Resultado ..: []
# 
#     Ação .......: Final.de 0, [5, 4, 3, 2, 1]
#     Resultado ..: [4, 3, 2, 1]
# 
#     Ação .......: Final.de 100, [5, 4, 3, 2, 1]
#     Resultado ..: []
# 
#     Ação .......: Final.de 3, [5, 4, 3, 2, 1]
#     Resultado ..: [2, 1]
# 
#     Ação .......: Separar.de [1, 2, 3, 4, 5]
#     Resultado ..: {[1, 2, 3], [4, 5]}
# 
#     Ação .......: Separar.de [1, 2, 3, 4, 5, 6]
#     Resultado ..: {[1, 2, 3], [4, 5, 6]}
# 
#     Ação .......: Separar.de [1]
#     Resultado ..: {[1], []}
# 
#     Ação .......: Separar.de []
#     Resultado ..: {[], []}
# 
#     Ação .......: Fatiar.de 3, 6, [1, 2, 3, 4, 5, 6, 7, 8, 9, 0]
#     Resultado ..: [4, 5, 6]
# 
#     Ação .......: Mapa.de [1, 2, 3, 4, 5], fn (x) -> x * 3 end
#     Resultado ..: [3, 6, 9, 12, 15] 
# 
#     Ação .......: Filtro.de fn (x) -> Impar.de x end, [1, 2, 3, 4]
#     Resultado ..: [1, 3] 
# 
#     Ação .......: Reducao.de [1, 2, 3, 4], (fn (x, y) -> Soma.de x, y end), 0
#     Resultado ..: 10
# 
#     Ação .......: Dobra_d.de (fn (x, y) -> 2 * x + y end), 5, [1, 2, 3]
#     Resultado ..: 17
# 
#     Ação .......: (para) Dobra_d.de (-), 7, [4, 7, 3, 5]
#                   (faca) Dobra_d.de (fn (x, y) -> x - y end), 7, [4, 7, 3, 5]
#     Resultado ..: 2
# 
#     Ação .......: Dobra_e.de (fn (x, y) -> 2 * x + y end), 5, [1, 2, 3]
#     Resultado ..: 51
# 
#     Ação .......: (para) Dobra_e.de (-), 7, [4, 7, 3, 5]
#                   (faca) Dobra_e.de (fn (x, y) -> x - y end), 7, [4, 7, 3, 5]
#     Resultado ..: -12
# 
#     Ação .......: Compacta.de [1, 2, 3], [4, 5, 6]
#     Resultado ..: [{1, 4}, {2, 5}, {3, 6}]
# 
#     Ação .......: Compacta.de [1, 2, 3, 4], [5, 6]
#     Resultado ..: [{1, 5}, {2, 6}]
# 
#     Ação .......: Compacta.de [1, 2], [3, 4, 5, 6]
#     Resultado ..: [{1, 3}, {2, 4}]
# 
#     Ação .......: Dcp_base.de [], [], [{8, 9}]
#     Resultado ..: {[8], [9]}
# 
#     Ação .......: Dcp_base.de [], [], [{8, 9}, {1, 2}]
#     Resultado ..: {[8, 1], [9, 2]}
# 
#     Ação .......: Descompacta.de [{1, 2}, {3, 4}]
#     Resultado ..: {[1, 3], [2, 4]}
# 
#     Ação .......: Descompacta.de [{1, 2}, {3, 4}, {5, 6}, {7, 8}]
#     Resultado ..: {[1, 3, 5, 7], [2, 4, 6, 8]}
#
#       ////   
#      (o o)     
# __ooO_(_)_Ooo_________________________________________________________________________
# |_____|______|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|
# ___|_____|______|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|___
# |_____|______|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|
# ___| :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: |___
# |___ ::::'###::::'##::::'##::'######:::'##::::'##::'######::'########::'#######:: ___|
# ___| :::'## ##::: ##:::: ##:'##... ##:: ##:::: ##:'##... ##:... ##..::'##.... ##: |___
# |___ ::'##:. ##:: ##:::: ##: ##:::..::: ##:::: ##: ##:::..::::: ##:::: ##:::: ##: ___|
# ___| :'##:::. ##: ##:::: ##: ##::'####: ##:::: ##:. ######::::: ##:::: ##:::: ##: |___
# |___ : #########: ##:::: ##: ##::: ##:: ##:::: ##::..... ##:::: ##:::: ##:::: ##: ___|
# ___| : ##.... ##: ##:::: ##: ##::: ##:: ##:::: ##:'##::: ##:::: ##:::: ##:::: ##: |___
# |___ : ##:::: ##:. #######::. ######:::. #######::. ######::::: ##::::. #######:: ___|
# ___| :..:::::..:::.......::::......:::::.......::::......::::::..::::::.......::: |___
# |___ :'##::::'##::::'###::::'##::: ##::'########::::'###::::'##::: ##::'#######:: ___|
# ___| : ###::'###:::'## ##::: ###:: ##::..... ##::::'## ##::: ###:: ##:'##.... ##: |___
# |___ : ####'####::'##:. ##:: ####: ##:::::: ##::::'##:. ##:: ####: ##: ##:::: ##: ___|
# ___| : ## ### ##:'##:::. ##: ## ## ##::::: ##::::'##:::. ##: ## ## ##: ##:::: ##: |___
# |___ : ##. #: ##: #########: ##. ####:::: ##::::: #########: ##. ####: ##:::: ##: ___|
# ___| : ##:.:: ##: ##.... ##: ##:. ###::: ##:::::: ##.... ##: ##:. ###: ##:::: ##: |___
# |___ : ##:::: ##: ##:::: ##: ##::. ##:: ########: ##:::: ##: ##::. ##:. #######:: ___|
# ___| :..:::::..::..:::::..::..::::..::........::..:::::..::..::::..:::.......:::: |___
# |_____|______|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|
# ___|_____|______|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|___
# |_____|______|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|



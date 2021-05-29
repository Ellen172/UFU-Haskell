--LISTA SOBRE TIPOS COMPOSTOS

--Usando as funções head e tail, defina a função terceiro que devolve o terceiro elemento de uma lista de inteiros

terceiro :: [Int] -> Int
terceiro lista = head (tail (tail lista))

{-
Considere a função  reverse do preludio-padrão:
> reverse [1 ,2 ,3][3 ,2 ,1]
Utilizando essa função (além das funções head e tail, crie as seguintes funções: 
-}

--(a) Função ultimo, que devolve o ultimo elemento de uma string. Exemplo:> ultimo " haskell "’l’

ultimo :: String -> Char
ultimo nome = head (reverse nome)

--(b) Função inicio, que devolve todos os elementos da string, exceto o ultimo. Exemplo:> inicio " haskell "" haskel "

inicio :: String -> String
inicio nome = reverse (tail (reverse nome))

{-
Implemente uma função que receba o primeiro e o último nome de alguém e retorne suas iniciaisem uma tupla.
Por exemplo:
> iniciais " Haskell " " Curry "
(’H’,’C ’)
-}

iniciais :: String -> String -> (Char, Char)
iniciais nome sobre = (head nome, head sobre)
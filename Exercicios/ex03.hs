--1.Fornecidos tres valores, a, b e c em uma tupla, implemente uma função que retorne quantosdesses tres são iguais. A reposta deve ser 3, se todos sao iguais; 2, se dois sao iguais e um e distintodos demais ou 0, se todos sao distintos entre si.

iguais :: (Int, Int, Int) -> Int
iguais (a, b, c) 
  | a==b && b==c = 3
  | a==b || b==c = 2
  | otherwise = 0
  
--2.Faça uma função que recebe dois valores reais em uma tupla e devolve o menor.

menor :: (Double,Double) -> Double
menor (a,b) 
  |a>b = b
  |otherwise = a
  
--3.Faça uma função que recebe três valores reais em uma tupla e devolve o menor.

menor3 :: (Double, Double, Double) -> Double
menor3 (a, b, c)
  |a<b && a<c = a
  |b<a && b<c = b
  |otherwise = c
  
--4.Escreva uma função que receba uma tupla-3 de três inteiros e retorne uma tupla-2 com o maior eo menor elemento dentre os três.

maior_menor :: (Int, Int, Int) -> (Int, Int)
maior_menor (a, b, c)
  | a<b && b<c = (c,a)
  | a<c && c<b = (b,a)
  | b<a && a<c = (c,b)
  | b<c && c<a = (a,b)
  | c<a && a<b = (b,c)
  | c<b && b<a = (a,c)

{-
5.Seja a seguinte equação do segundo grau: ax2 + bx + c = 0 sendo que a, b e c sao numeros reais ea≠0. Essa equação tem:
* duas raízes reais, se b2 > 4ac;
* uma raiz real, se b2 = 4ac; e
* nenhuma raiz real, se b2 < 4ac.
Faça uma função que, dados três coeficientes a, b, e c, informe quantas raízes a equação possui.
-}

raiz :: (Double, Double, Double) -> String
raiz (a, b, c)
  |(b^2) > (4*a*c) = "2 raizes"
  |(b^2) == (4*a*c) = "1 raiz"
  |(b^2) < (4*a*c) = "nenhuma raiz"
  
--6.Faça   uma   função   que,   dado   duas   datas   como   entrada,   determine   qual   delas   ocorreucronologicamente antes em relação a outra. Cada data é composta por um tupla de 3 númerosinteiros: ano, mês e dia. Saídas possíveis: ”Primeira data ocorreu antes dasegunda”ou ”Segunda data ocorreu antes da Primeira”.

data' :: (Int, Int, Int) -> (Int, Int, Int) -> String
data' (a, m, d) (a', m', d') 
  |a<a' = "Primera data ocorreu antes da Segunda"
  |a>a' = "Segunda data ocorreu antes da Primeira"
  |a==a' && m<m' = "Primera data ocorreu antes da Segunda"
  |a==a' && m>m' = "Segunda data ocorreu antes da Primeira"
  |a==a' && m==m' && d<d' = "Primera data ocorreu antes da Segunda"
  |a==a' && m==m' && d>d' = "Segunda data ocorreu antes da Primeira"
  |otherwise = "As duas datas ocorreram no mesmo dia, mes e ano"

--7.Faça uma função chamada ordena2 :: Int -> Int -> (Int, Int) que aceita dois valores inteiros comoargumentos e retorna-os como um par ordenado. Por exemplo, ordena2 5 3 e igual a  (3,5). Definaessa função utilizando Guardas.

ordena2 :: Int -> Int -> (Int, Int)
ordena2 a b 
  | a<b = (a,b)
  | otherwise = (b,a)

--8.Faça a função par que recebe um numero inteiro e devolve verdadeiro se o número for par efalso, caso contrario. Não se esqueça das definições de tipos.

par :: Int -> Bool
par x
  | mod x 2 == 0 = True
  |otherwise = False
  
--9.Utilizando a função do item anterior, faça a função impar que recebe um numero inteiro edevolve verdadeiro se o numero for ímpar e falso, caso contrario.

impar :: Int -> Bool 
impar x
  | par x == True = False
  |otherwise = True
  

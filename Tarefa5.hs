--TAREFA 5
--AS 08/09
--8
conta_digitos :: Int -> Int
conta_digitos n
  |n<10 = 1
  |otherwise = 1+conta_digitos (div n 10)
  
--10
potencia :: (Int, Int) -> Int
potencia (b, e)
  |e==1 = b
  |otherwise = b*potencia(b, (e-1))

--11
ackermann :: (Double, Double) -> Double
ackermann (m, n)
  |m==0 = n+1
  |m>0 && n==0 = ackermann ((m-1), 1)
  |m>0 && n>0 = ackermann (m, (n-1))

--AS 15/09
--2.h
ultimo :: [Int] -> Int
ultimo [x] = x
ultimo (x:xs) = ultimo (xs)

{-
EXEMPLO
ultimo [3, 14, 1, 5, 9] = ultimo [14, 1, 5, 9]
ultimo [14, 1, 5, 9] = ultimo [1, 5, 9]
ultimo [1, 5, 9] = ultimo [5, 9]
ultimo [5, 9] = ultimo [9]
ultimo [9] = 9
-}

--2.i
duplica :: [Int] -> [Int]
duplica [x] = x: [x]
duplica (x:xs) = x : x : duplica (xs)

{-
EXEMPLO
duplica [3 ,14 ,1 ,5 ,9] = 3 : 3 : duplica [14, 1, 5, 9]
3 : 3 : duplica [14, 1, 5, 9] = 3: 3: 14: 14: duplica [1, 5, 9]
3: 3: 14: 14: duplica [1, 5, 9] = 3: 3: 14: 14: 1: 1: duplica [5, 9]
3: 3: 14: 14: 1: 1: duplica [5, 9] = 3: 3: 14: 14: 1: 1: 5: 5: duplica [9]
3: 3: 14: 14: 1: 1: 5: 5: duplica [9] = 3: 3: 14: 14: 1: 1: 5: 5: 9: [9]
= [3, 3, 14, 14, 1, 1, 5, 5, 9, 9]
-}

--2.k
substituir_todos :: Int -> Int -> [Int] -> [Int]
substituir_todos a b [] = []
substituir_todos a b (x:xs) 
  |a==x = substituir_todos a b (b:xs)
  |otherwise = x: substituir_todos a b (xs) 

{-
EXEMPLO
substituir_todos 1 2 [3 ,14 ,1 ,5 ,1] = 3: substituir_todos 1 2 [14 ,1 ,5 ,1]
3: substituir_todos 1 2 [14 ,1 ,5 ,1] = 3: 14: substituir_todos 1 2 [1 ,5 ,1]
3: 14: substituir_todos 1 2 [1 ,5 ,1] = 3: 14: substituir_todos 1 2 [2 ,5 ,1]
3: 14: substituir_todos 1 2 [2 ,5 ,1] = 3: 14: 2: substituir_todos 1 2 [5 ,1]
3: 14: 2: substituir_todos 1 2 [5 ,1] = 3: 14: 2: 5: substituir_todos 1 2 [1]
3: 14: 2: 5: substituir_todos 1 2 [1] = 3: 14: 2: 5: substituir_todos 1 2 [2]
3: 14: 2: 5: substituir_todos 1 2 [2] = 3: 14: 2: 5: 2: substituir_todos 1 2 []
3: 14: 2: 5: 2: substituir_todos 1 2 [] = 3: 14: 2: 5: 2: []
 3: 14: 2: 5: 2: [] = [3 ,14 ,2 ,5 ,2]
-}

--2.n
maior :: [Int] -> Int
maior (x:xs) 
  |(x:xs) == [x] || x > head (xs)= x
  |otherwise = maior (xs)

{-
EXEMPLO
maior [3 ,14 ,1 ,5 ,9] = maior [14, 1, 5, 9]
maior [14, 1, 5, 9] = 14
-}

--3.b 
uniao :: [Int] -> [Int] -> [Int]
uniao [] a = a
uniao a [] = a
uniao (a:x) (b:xs)
  | a < b = a : uniao x (b:xs)
  | a == b = a : uniao x xs
  | otherwise = b : uniao (a:x) xs

--3.c 
inter :: [Int] -> [Int] -> [Int]
inter [] cab = []
inter x [] = []
inter (x : xs) (cab : cau) 
  |(x == cab) = x : inter xs cau
  |(x > cab) = inter (x : xs) cau
  |(x < cab) = inter xs (cab : cau)

--7
conta_posicao :: [Int] -> Int
conta_posicao [] = 0
conta_posicao (x : xs) = 1 + conta_posicao xs 

converte :: [Int] -> Int
converte [] = 0
converte (x : xs) = x * ( 2^( (conta_posicao (x : xs) )-1 ) ) + converte xs

--8
--numeros voltando ao contrario 1234 -> [4, 3, 2, 1]
digitos :: Int -> [Int]
digitos n 
  | n == 0 = []
  | otherwise = (digitos (div n 10)) ++ [mod n 10]
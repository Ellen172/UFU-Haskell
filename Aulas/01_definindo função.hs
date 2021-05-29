--Esse é um comentário de linha. 

{- 
Esse é um comentário de mais de uma linha.
Meu nome é Ellen Christina 
-}

dobro :: Int -> Int --Definindo o tipo da função.
dobro x = x * 2

quadruplo1 :: Int -> Int
quadruplo1 x = dobro x * 2

quadruplo2 :: Int -> Int
quadruplo2 x = dobro (dobro x)

soma2 :: Int -> Int -> Int
soma2 x y = x + y

soma4 :: Int -> Int -> Int -> Int -> Int
soma4 a b c d = (soma2 a b) + (soma2 c d) 

misterio :: Int -> Int -> Int -> Int -> Int 
misterio x y z w = soma2 (soma2 x y) (soma2 z w)

hipotenusa :: Int -> Int -> Int
hipotenusa ca co = sqrt( ca^2 + co^2)

-- Exercícios/Prova Oral
-- Aluna: Ellen Christina Amaral Santana

import Data.Char

--1. MAP

-- (a)
primeiros :: [(a,b)] -> [a]
primeiros [] = []
primeiros (x : xs) = fst x: primeiros xs

primeirosMap :: [(a,b)] -> [a]
primeirosMap (x : xs) = fst x: map fst xs


-- (b)
maiusculas :: String -> String
maiusculas [] = []
maiusculas (x:xs)
  | (isLower x) = (toUpper x): maiusculas xs
  | otherwise = x: maiusculas xs

maiusculasMap :: String -> String
maiusculasMap (x:xs) = toUpper x: map toUpper xs

--(c)
dobra :: Num a => [a] -> [a]
dobra [] = []
dobra (x : xs) = (x * 2) : dobra xs

dobraMap :: Num a => [a] -> [a]
dobraMap (x : xs) = (x * 2) : map (2*) xs

--(d)
hora_em_seg :: [Float] -> [Float]
hora_em_seg [] = []
hora_em_seg (x : xs) = (x * 3600) : hora_em_seg xs

hora_em_seg_map :: [Float] -> [Float]
hora_em_seg_map (x : xs) = (x * 3600) : map (3600*) xs

--7. FILTER

--(a)
pares :: [Int] -> [Int] 
pares [] = []
pares (x : xs)
  | even x == True = x : pares xs
  | otherwise = pares xs

paresFilter :: [Int] -> [Int]
paresFilter (x : xs) = filter (even) (x : xs)

--(b)
alfa :: String -> String 
alfa [] = []
alfa (x : xs)
  | isAlpha x = x : alfa xs
  | otherwise = alfa xs

alfaFilter :: String -> String
alfaFilter (x : xs) = filter (isAlpha) (x: xs)

--(c)
rmChar :: Char -> String -> String
rmChar n [] = [] 
rmChar n (x:xs) 
  | n == x = rmChar n xs
  | otherwise = x : rmChar n xs 

rmCharFilter :: Char -> String -> String
rmCharFilter n (x: xs) = filter (/= n) (x : xs)

--(d)
acima :: Int -> [Int] -> [Int] 
acima n [] = []
acima n (x: xs)
  | x <= n = acima n xs
  | otherwise = x : acima n xs

acimaFilter :: Int -> [Int] -> [Int] 
acimaFilter n [] = []
acimaFilter n (x : xs) = filter (>n) (x : xs) 

--(e)
desiguais :: Eq t => [(t,t)] -> [(t,t)] 
desiguais [] = []
desiguais (x: xs) 
  | fst x == snd x = desiguais xs
  | otherwise = x : desiguais xs

eDiferente :: Eq t => (t, t) -> Bool
eDiferente (x, y)
  | x /= y = True
  | otherwise = False

desiguaisFilter :: Eq t => [(t,t)] -> [(t,t)] 
desiguaisFilter xs = filter (eDiferente) xs

--8. FOLDR

--(a)
produto :: Num a => [a] -> a 
produto [] = 1
produto (x : xs) = x* produto xs

produtoFoldr :: Num a => [a] -> a
produtoFoldr (x : xs) = foldr (*) 1 (x : xs)

--(b)
e_logico :: [Bool] -> Bool 
e_logico [] = True
e_logico (x : xs) 
  | x == True = True && e_logico xs
  | otherwise = False

e_logico_foldr :: [Bool] -> Bool
e_logico_foldr (x : xs) = foldr (&&) x xs

--(c)
concatena :: [String] -> String 
concatena [] = []
concatena (x : xs) = x ++ concatena xs

concatenaFoldr :: [String] -> String
concatenaFoldr (x :xs) = foldr (++) "" (x: xs)

--(d)
maior :: Int -> [Int] -> Int 
maior n [] = n
maior n (x : xs)
  |( n > x )= maior n xs
  |otherwise = maior x xs

maiorFoldr :: Int -> [Int] -> Int 
maiorFoldr n (x : xs) = foldr (max) n (x : xs)
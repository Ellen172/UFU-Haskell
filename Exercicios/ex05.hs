--2.b
somatorio :: [Int] -> Int
somatorio [] = 0
somatorio (x:xs) = x + somatorio xs

{- EXEMPLO DE FUNCIONAMENTO 
somatorio [3, 14, 1, 5, 9]
[3, 14, 1, 5, 9] = 3 + [14, 1, 5, 9]
[14, 1, 5, 9] = 3 + 14 + [1, 5, 9]
[1, 5, 9] = 3 + 14 + 1 + [5, 9]
[5, 9] = 3 + 14 + 1 + 5 + [9]
[9] = 3 + 14 + 1 + 5 + 9 + []
[] = 3 + 14 + 1 + 5 + 9 + 0
somatorio = 32
-}

--2.g
n_esimo :: Int -> [Int] -> Int
n_esimo 0 (x:xs) = x
n_esimo p (x:xs) = (n_esimo (p-1) xs) 


{-EXEMPLO DE FUNCIONAMENTO
n-esimo 3 [3, 14, 1, 5, 9]
[3, 14, 1, 5, 9] = [14, 1, 5, 9] p=2
[14, 1, 5, 9] = [1, 5, 9] p=1
[1, 5, 9] = [5, 9] p=0
[5, 9] = 5 --x
-}

--2.s
pertence :: Int -> [Int] -> Bool
pertence p (x:xs) 
  |p == x = True
  |otherwise = pertence p xs


{-EXEMPLO DE FUNCIONAMENTO
pertence 3 [1, 2, 4, 5]
pertence 3 [2, 4, 5]
pertence 3 [4, 5]
pertence 3 [5]
pertence 3 = False
pertence 3 [1, 2, 3]
pertence 3 [2, 3]
pertence 3 [3] = True 
-}
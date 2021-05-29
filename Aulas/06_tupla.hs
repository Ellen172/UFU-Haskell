--AS 30/09

--Função que extrai o primeiro elemento de uma tupla
primeiros :: [(a, b)] -> [a]
primeiros (x : xs) = map fst (x : xs)

pares :: [Int] -> [Int]
pares (x : xs) = filter even (x : xs)

produto :: [Int] -> Int
produto (x : xs) = foldr (*) 1 (x : xs)
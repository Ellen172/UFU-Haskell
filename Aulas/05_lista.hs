--cabeça da lista
cab :: [Int] -> Int
cab (x:xs) = x

--cauda da lista
cau :: [Int] -> [Int]
cau (x:xs) = xs

--produto entre todos os elementos da lista
produto :: [Float] -> Float
produto [] = 1 --lista vazia
produto (x:xs) = x * produto xs 

produto' :: [Float] -> Float
produto' [x] = x --lista com um unico elemento
produto' (x:xs) = x * produto xs 

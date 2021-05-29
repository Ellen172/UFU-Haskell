--FUNÇÕES COM GUARDA

--Definir qual é o menor dentre os dois valores usando "if" e "else" 

menor :: Int -> Int -> Int 
menor x y 
  |x<=y = x --se x<=y for true retorna x
  |otherwise = y --se não retorna y


--Executando uma função que devolve o fatorial de um numero

fatorial :: Integer -> Integer
fatorial n
  |n==0 = 1
  |otherwise = n* fatorial (n-1)

fatorial' :: Integer -> Integer 
fatorial' 0 = 1
fatorial' n = n*fatorial(n-1)

--mdc
mdc :: (Int, Int) -> Int
mdc (m,n) 
  |n==0 = m 
  |otherwise = mdc(n, m `mod` n)
--TIPOS DE DADOS COMPOSTOS

--tupla: divindo dois valores e mostrando o resto dessa divisão.
modiv :: (Int, Int) -> (Int, Int) 
modiv (a, b) = (a `div` b, mod a b) 

--retornar primeiro valor
primeiro :: (Int, Int) -> Int
primeiro (a,b) = a

--retornar segundo valor
segundo :: (Int,Int) -> Int
segundo (a, b) = b

--recebe horas, minutos e segundos e devolve o valor inteiro em segundos.
segundos :: (Int, Int, Int) -> Int
segundos (a,b,c) = (a * 60 * 60) + ( b * 60 ) + c

--recebe um valor em segundos e devolve em horas, minutos e segundos.
horario :: Int -> (Int, Int, Int)
horario s = (div s 3600, div (mod s 3600) 60, mod (mod s 3600) 60 )

--15.Faça uma função que determine o volume de uma esfera

volume_esfera :: Double -> Double
volume_esfera x = (4 * pi * (x^3)) /3 

--16. Faça uma função que determine o valor da hipotenusa de um triangulo, onde são fornecidos os seus outros lados.

hipotenusa :: Double -> Double -> Double
hipotenusa ca co = sqrt( ca^2 + co^2)

--18. Dados dois pontos (xa, ya) e (xb, yb), implemente uma função que determine a distancia entre esses pontos. 

distancia_2pontos :: Double -> Double -> Double -> Double -> Double
distancia_2pontos xa ya xb yb = sqrt( ( (xa-xb) ^2) + ( (ya-yb) ^2) )

--Faça uma função que determine o quadrado de um numero.

quadrado :: Int -> Int
quadrado x = x^2

--20. Faça uma funçaõ que determine a quarta potência de um numero, usando a função que determina o quadrado de um numero.

quarta_potencia :: Int -> Int
quarta_potencia x = quadrado (quadrado x)

-- 27. Dados dois valores lógicos, faça uma função que implemente a fórmula: (p ∨ q) ∧ ¬(p ∧ q).

logico27 :: Bool -> Bool -> Bool 
logico27 p q = (p || q) && not (p && q) 

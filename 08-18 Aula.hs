-- 6. Faça uma função que determine a área de um retângulo.

area_retangulo :: Int -> Int -> Int
area_retangulo x y = x*y

--7. Faça uma função que determine a área de um quadrado 

area_quadrado :: Int -> Int
area_quadrado x = x*x

--8. Faça um função que determine a área de um triangulo

area_triangulo :: Double -> Double -> Double
area_triangulo b h = (b*h)/2

--9. Faça uma função que determine a área de um trapezio 
{-
area_trapezio :: Int -> Int -> Int -> Int
area_trapezio a b h = ((a+b)*h)/2
-}

--10. Faça um função que determine a área de um cirulo (Dica: Utilize a constante pi da linguagem Haskell para representar o valor de pi.)
{-
area_circulo :: Double -> Double
area_circulo r = pi * (r^2)
-}

--11. Faça uma função que determine a área da coroa circular.

area_coroa :: Double -> Double -> Double 
area_coroa a b = pi * (a**2) - pi * (b**2)

--12. Faça uma função que determine o volume de um cubo

volume_cubo :: Int -> Int
volume_cubo x = x^3

--13. Faça uma função que determine o volume de um paralelepípedo

volume_parale :: Int -> Int -> Int -> Int
volume_parale a b c = a*b*c

--14. Faça uma função que determine o volume de uma pirâmide regular

volume_piramide :: Double -> Double -> Double
volume_piramide a h = (a * a * h) /3

--15.Faça uma função que determine o volume de uma esfera

volume_esfera :: Double -> Double
volume_esfera x = (4 * pi * (x^3)) /3 

--16. Faça uma função que determine o valor da hipotenusa de um triangulo, onde são fornecidos os seus outros lados.

hipotenusa :: Double -> Double -> Double
hipotenusa ca co = sqrt( ca^2 + co^2)

--17. Dado um ponto (x,y), implemente uma função que determina a distância desse ponto à origem (0,0).

distancia_ponto :: Double -> Double -> Double
distancia_ponto x y = sqrt( (x ^2 ) + (y^2) )

--18. Dados dois pontos (xa, ya) e (xb, yb), implemente uma função que determine a distancia entre esses pontos. 

distancia_2pontos :: Double -> Double -> Double -> Double -> Double
distancia_2pontos xa ya xb yb = sqrt( ( (xa-xb) ^2) + ( (ya-yb) ^2) )

--Faça uma função que determine o quadrado de um numero.

quadrado :: Int -> Int
quadrado x = x^2

--19. Faça uma função que determine o cubo de um numero

cubo :: Int -> Int
cubo x = x^3

--20. Faça uma funçaõ que determine a quarta potência de um numero, usando a função que determina o quadrado de um numero.

quarta_potencia :: Int -> Int
quarta_potencia x = quadrado (quadrado x)

--21. Faça uma função que, dado um total de segundos, calcule o total de horas.

seg_hrs :: Double -> Double
seg_hrs x = x/60

--22.Faça uma função que, dado um total de segundos, calcule o total de minutos

seg_min :: Double -> Double 
seg_min x = (seg_hrs x)/60

--23.Dada um temperatura em graus Fahrenheit, converta-a em graus Celsius

fah_cel :: Double -> Double 
fah_cel x = (x-32)/1.8


--24.Dada uma temperatura em graus Kelvin, converta-a em graus Celsius

kel_cel :: Double -> Double
kel_cel x = x-273


--25.Dada uma temperatura em graus Fahrenheit, converta-a em graus Kelvin

fah_kel :: Double -> Double
fah_kel x = (x+459.67) * 5/9

--26. Faça uma função que, dada uma velocidade em quilômetros por hora, converta-a em metros por segundo.

km_ms :: Double -> Double
km_ms x = x/3.6

-- 27. Dados dois valores lógicos, faça uma função que implemente a fórmula: (p ∨ q) ∧ ¬(p ∧ q).

logico27 :: Bool -> Bool -> Bool 
logico27 p q = (p || q) && not (p && q) 

--28. Dados três valores lógicos, faça uma função que implemente a fórmula: (p ∨ q) ∧ r
{-
logico28 :: Bool -> Bool -> Bool -> Bool 
logico28 p q r = (p || q) && r
-}

--29. Dados três valores lógicos, faça uma função que implemente a fórmula: (p ∧ q) ∨ ¬(p ∧ r).

logico29 :: Bool -> Bool -> Bool -> Bool 
logico29 p q r = ( p && q ) || not ( p && r) 

--30. Dados quatro valores lógicos, faça uma função que implemente a fórmula: p ∨ (q ∧ r) ∨¬s.

logico30 :: Bool -> Bool -> Bool -> Bool -> Bool 
logico30 p q r s = p || (q && r) || not s 

--31. Dados quatro valores lógicos, faça uma função que implemente a fórmula: ¬(p ∨ q) ∧ (r ∨s) ∧ ¬r.

logico31 :: Bool -> Bool -> Bool -> Bool -> Bool
logico31 p q r s = not (p || q) && (r || s) && not r
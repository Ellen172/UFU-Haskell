--3º TRABALHO DE PROGRAMAÇÃO FUNCIONAL

type Nome = String
type Preco = Int
type CodBar = Int
type BaseDeDados = [(CodBar,Nome,Preco)]
type ListaDeCodigos = [CodBar]
type Recibo = [(Nome,Preco)]

tamLinha :: Int
tamLinha = 30

listaDeProdutos :: BaseDeDados
listaDeProdutos = [(1234, "Oleo DoBom, 1l" , 195),(4756, "Chocolate Cazzeiro, 250g" , 180),(3216, "Arroz DoBom, 5Kg", 213),(5823, "Balas Pedregulho, 1Kg" , 379),(4719, "Queijo Mineirim, 1Kg" , 449),(6832, "Iogurte Maravilha, 1Kg" , 499),(1112, "Rapadura QuebraDente, 1Kg", 80),(1111, "Sal Donorte, 1Kg", 221),(1113,"Cafe  DoBom, 1Kg", 285),(1115, "Biscoito Bibi, 1Kg", 80),(3814, "Sorvete QGelo, 1l", 695)]

acha :: BaseDeDados -> CodBar -> (Nome, Preco)
acha ((cod1,nome,preco):resto) cod2 
 | cod1 == cod2 = (nome,preco)
 | otherwise = acha resto cod2

fazRecibo :: ListaDeCodigos -> Recibo
fazRecibo xs = [acha listaDeProdutos x | x <- xs, x>0]

formataCentavos :: Preco -> String
formataCentavos preco 
  | (mod preco 100) < 10 = show (div preco 100) ++ "." ++ "0" ++ show (mod preco 100)
  | otherwise = show (div preco 100) ++ "." ++ show (mod preco 100)

formataLinha :: (Nome,Preco) -> String
formataLinha (nome,preco) = nome ++ (replicate qtdepts '.') ++ precoStr ++ "\n"
 where
  precoStr = formataCentavos preco
  qtdepts = tamLinha - length nome - length precoStr

formataLinhas :: [(Nome,Preco)] -> String
formataLinhas (x : xs) = show (map formataLinha (x : xs))

geraTotal :: Recibo -> Preco
geraTotal itens = sum (map snd itens)

formataTotal :: Preco -> String
formataTotal preco = "\nTotal" ++  (replicate qtdepts '.') ++ precoStr  
  where
    precoStr = formataCentavos preco
    qtdepts = tamLinha - length "Total" - length precoStr  

formataRecibo :: Recibo -> String
formataRecibo itens = formataLinhas itens ++ formataTotal (geraTotal itens) ++ "\n"

geraRecibo :: ListaDeCodigos -> String
geraRecibo itens = formataRecibo (fazRecibo itens)

main :: IO ()
main = do 
  x <- getLine 
  putStrLn ((geraRecibo (read x :: ListaDeCodigos))) 
  y <- getLine 
  putStrLn ("End")
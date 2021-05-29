type Dia = Int
type Mes = Int
type Ano = Int 
type Data = (Dia, Mes, Ano)

--definir se o ano é bissexto 
bissexto :: Ano -> Bool
bissexto ano 
  |(mod ano 4)==0 = True 
  |(mod ano 4)==0 && (mod ano 100)==0 = False
  |(mod ano 4)==0 && (mod ano 100)==0 && (mod ano 400)==0 = True
  |otherwise = False

--definir se o numero de dias de cada mes, considerando q em anos bissextos fevereiro tem 29 dias e em anos comuns 28 dias
numDeDiasEmCadaMesDeUmAno :: Ano -> [Int]
numDeDiasEmCadaMesDeUmAno ano 
  |bissexto ano==True = [31,29,31,30,31,30,31,31,30,31,30,31]
  |otherwise = [31,28,31,30,31,30,31,31,30,31,30,31] 

--definir o numero de dias q há entre a data especificada e 1 de janeiro de 2001, o ponto de partida  
numDeDias :: Data -> Int
numDeDias (dia, mes, ano) = dia + sum (take (mes-1) (numDeDiasEmCadaMesDeUmAno ano)) + (ano-2001)*365 + (ano-2001)`div`4

--definir o nome do dia da semana como um inteiro
nomeDoDia :: Int -> String 
nomeDoDia dia
  |dia==0 = "Domingo"
  |dia==1 = "Segunda"
  |dia==2 = "Terca"
  |dia==3 = "Quarta"
  |dia==4 = "Quinta"
  |dia==5 = "Sexta"
  |dia==6 = "Sabado"
  |otherwise = error "o dia deve ser um numero entre 0 e 6"

--definição final do programa
diaDaSemana :: Data -> String
diaDaSemana (dia, mes, ano) 
  |numDeDias (dia, mes, ano) <= 6 = nomeDoDia dia --se o numero de dias é menor ou igual a 6, ele é usado na função nomeDoDia que retorna o dia correspondente
  |mes>1 = diaDaSemana( (dia+sum (take (mes-1) (numDeDiasEmCadaMesDeUmAno ano))), (mes - (mes-1)), ano ) --se o mes for maior que 1, deverá ser reduzido e os seus correspondentes dias passarão para o elemento dia sendo somado ao que ele ja possui
  |ano>2001 = diaDaSemana (dia, mes+12, ano-1) --se o ano for maior q 2001, deverá ser reduzido e seus messes passarão para o elemento mes, somado ao q ja possui. Isso se repetirá ate o ano chegar a 2001
  |otherwise = diaDaSemana (dia-7, mes, ano) -- os elementos q entrarão nessa função será (dia, 1, 2001) portanto o numero de dias entre a data especificada e o dia 1 de janeiro de 2001 estará todo em 'dia'. Sendo assim do elemento dia precisará ser retirado 7 dias repetidamente ate que se torne um numero menor ou igual a 6.


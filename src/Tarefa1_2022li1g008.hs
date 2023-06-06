{- |
Module      : Tarefa1_2022li1g008
Description : Validação de um mapa
Copyright   : Gonçalo Oliveira Cruz <a104346@alunos.uminho.pt>
              Gonçalo Costa Magalhães <a104538@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g008 where

import LI12223
{- |A função 'mapaValido' verifica se um dado mapa obedece a todas as regras presentes na Tarefa 1, utilizando para isso algumas funções auxiliares.

== Exemplos de utilização:

>>> mapaValido (Mapa 5 [(Rio 1, [Tronco, Nenhum, Tronco, Nenhum, Tronco]), 
                        (Rio (-1), [Nenhum, Nenhum, Nenhum, Tronco, Tronco]),
                        (Estrada 2, [Nenhum, Carro, Carro, Carro, Nenhum])])
True
>>> mapaValido (Mapa 5 [(Rio 1, [Tronco, Nenhum, Tronco, Nenhum, Tronco]), 
                        (Rio 1, [Nenhum, Nenhum, Nenhum, Tronco, Tronco]),
                        (Estrada 2, [Nenhum, Carro, Carro, Carro, Nenhum])])
False
>>> mapaValido (Mapa 5 [(Rio 1, [Tronco, Nenhum, Arvore, Nenhum, Tronco]), 
                        (Rio (-1), [Nenhum, Nenhum, Nenhum, Tronco, Tronco]),
                        (Estrada 2, [Nenhum, Carro, Carro, Carro, Nenhum])])
False
-}
mapaValido :: Mapa -- ^ É escrito um tipo de mapa qualquer.
                -> Bool -- ^ A função irá avaliar se o mapa inserido é válido (devolvendo "True") ou se não o é (devolvendo "False").
mapaValido (Mapa l ((t,o:os):ts))  | velocity ((t,o:os):ts) && obstaculo ((t,o:os):ts) && dirRios ((t,o:os):ts) && troncos ((t,o:os):ts) && carros ((t,o:os):ts) && nenhuns ((t, o:os):ts) && largura l ((t,o:os):ts) && rios 0 ((t,o:os):ts) && estradas 0 ((t,o:os):ts) && relvas 0 ((t,o:os):ts) = True
                                   | otherwise = False 
{- |A função 'velocity' serve para indicar se a velocidade do terreno é possível.
-}
velocity::[(Terreno,[Obstaculo])] -- ^ A função recebe uma lista de pares, no qual o primeiro elemento de cada par é um terreno, e o segundo elemento é uma lista de obstáculos desse mesmo terreno.
            -> Bool -- ^ A função retornará valor lógico "False" caso a velocidade da Relva seja diferente de 0, e a velocidade de um Rio e de uma Estrada seja 0.
velocity [] = True
velocity  ((Relva,o:os):ts) | getvelocidade Relva == 0 = velocity ts
                            | otherwise = False
velocity ((Rio v1,o:os):ts) | getvelocidade (Rio v1) == 0 = False
                            | otherwise = velocity ts
velocity ((Estrada v1,o:os):ts) | getvelocidade (Estrada v1) == 0 = False
                                | otherwise = velocity ts                                                                         
{- |A função 'getvelocidade' serve para indicar qual a velocidade correspondente ao tipo de terreno.
-}
getvelocidade :: Terreno -- ^ A função recebe um tipo de terreno com a sua velocidade correspondente. 
                    -> Int -- ^ A função retorna a velocidade correspondente ao terreno que ela recebeu.
getvelocidade Relva = 0
getvelocidade (Rio v1) = v1
getvelocidade (Estrada v1) = v1
{- |A função 'obstaculo' verifica se não existem obstáculos em terrenos imprórios, por exemplo não podem existir carros ou árvores nos rios, nem troncos ou árvores nas estradas, e nem carros ou troncos na relva.
-}
obstaculo:: [(Terreno,[Obstaculo])] -- ^ Escreve-se uma lista de pares em que o primeiro elemento do par é o tipo de terreno e o segundo elemento é a lista de obstáculos presentes nesse terreno.
                -> Bool -- ^ Caso não existam obstáculos impróprios para cada tipo de terreno associado, o resultado será "True".
obstaculo [] = True 
obstaculo ((Rio v1,o:os):ts) | elem Carro (o:os) = False 
                             | elem Arvore (o:os) = False 
                             | otherwise = obstaculo ts 
obstaculo ((Estrada v1,o:os):ts) | elem Tronco (o:os) = False 
                                 | elem Arvore (o:os) = False 
                                 | otherwise = obstaculo ts
obstaculo ((Relva,o:os):ts) | elem Carro (o:os) = False 
                            | elem Tronco (o:os) = False 
                            | otherwise = obstaculo ts
{- |A função 'dirRios' verifica se rios contíguos/seguidos têm direções opostas, ou seja, se as velocidades de dois rios consecutivos têm sinais opostos.
-}
dirRios:: [(Terreno,[Obstaculo])] -- ^ Escreve-se uma lista de pares em que o primeiro elemento do par é o tipo de terreno e o segundo elemento é a lista de obstáculos presentes nesse terreno.
            -> Bool -- ^ Caso dois rios seguidos tenham valores de velocidade opostos, então o resultado será "True".
dirRios [] = True 
dirRios ((Rio v1, l):(Rio v2, o):ts) | v1<0 && v2>0 = dirRios ((Rio v2,o):ts)
                                     | v1>0 && v2<0 = dirRios ((Rio v2,o):ts)
                                     | otherwise = False 
dirRios ((_,o:os):ts) = dirRios ts
{- |A função 'troncos' tem como papel principal assegurar-se de que os troncos têm no máximo 5 unidades de comprimento.
-}
troncos::[(Terreno,[Obstaculo])] -- ^ Escreve-se uma lista de pares em que o primeiro elemento do par é o tipo de terreno e o segundo elemento é a lista de obstáculos presentes nesse terreno.
                    -> Bool -- ^ Caso um tronco tenha mais do que 5 unidades de comprimento, ou seja, caso a palavra "Tronco" apareça mais do que 5 vezes seguidas na lista,o resultado será "False".
troncos [] = True
troncos (h:t) |troncosaux3 h <=5 && troncosaux4 0 h = troncos t 
              |otherwise = False
{- |A função 'troncosaux1' é uma das funções auxiliares da função "troncos" para que juntamente com as outras funções auxiliares façam com que a função "troncos" retorne que uma lista de obstáculos é inválida caso os obstáculos dessa lista ao sairem do mapa e voltando a reaparecer do lado oposto não originem um tronco com mais do que 5 unidades de comprimento.
-}          
troncosaux1:: Int -- ^ Escreve-se o número 0.
                -> (Terreno, [Obstaculo]) -- ^ Escreve-se um par composto por um tipo de terreno e por uma lista de obstáculos desse mesmo terreno.
                    -> Int -- ^ A função revela qual o comprimento de de um tronco que esteja exatamente no início do rio.
troncosaux1 n (t, []) = n
troncosaux1 n (t, Tronco:os) = troncosaux1 (n+1) (t, os) 
troncosaux1 n (t, _:os) = n 
{- |A função 'troncosaux2' é mais uma das funções auxiliares da função "troncos".
-}
troncosaux2:: Int -- ^ Escreve-se o número 0.
                -> (Terreno, [Obstaculo]) -- ^ Escreve-se um par composto por um tipo de terreno e por uma lista de obstáculos desse mesmo terreno.
                    -> Int -- ^ A função revela qual o comprimento de de um tronco que esteja exatamente no final do rio.
troncosaux2 n (t, []) = n
troncosaux2 n (t, [o]) | o==Tronco = n+1
                       | otherwise = n
troncosaux2 n (t, o:os) | last os == Tronco = troncosaux2 (n+1) (t, init (o:os)) 
                        | otherwise = n
{- |A função 'troncosaux3' é mais uma das funções auxiliares da função "troncos".
-}
troncosaux3::(Terreno, [Obstaculo]) -- ^ Escreve-se um par composto por um tipo de terreno e por uma lista de obstáculos desse mesmo terreno.
                    -> Int -- ^ A função revela qual o comprimento de um tronco que tem uma parte na extremidade esquerda do rio e a outra parte na extremidade direita do rio.
troncosaux3 a = troncosaux1 0 a + troncosaux2 0 a
{- |A função 'troncosaux4' é mais uma das funções auxiliares da função "troncos".
-}
troncosaux4::Int -- ^ Escreve-se o número 0.
                ->(Terreno, [Obstaculo]) -- ^ Escreve-se um par composto por um tipo de terreno e por uma lista de obstáculos desse mesmo terreno.
                    ->Bool -- ^ Se um tronco tiver mais do que 5 unidades de comprimento, esta função retornará o resultado "False".
troncosaux4 6 p = False
troncosaux4 x (t,[]) = True
troncosaux4 x (t,Tronco:os) = troncosaux4 (x+1) (t,os)
troncosaux4 x (t,o:os) = troncosaux4 0 (t,os)
{- |A função 'carros' serve para limitar o comprimento dos carros no máximo a 3 unidades de comprimento.
-}
carros::[(Terreno,[Obstaculo])] -- ^ Escreve-se uma lista de pares em que o primeiro elemento do par é o tipo de terreno e o segundo elemento é a lista de obstáculos presentes nesse terreno.
                    -> Bool -- ^ Caso um carro tenha mais do que 3 unidades de comprimento, ou seja, caso a palavra "Carro" apareça mais do que 3 vezes seguidas na lista, o resultado será "False".
carros [] = True 
carros (h:t) |carrosaux3 h <=3 && carrosaux4 0 h  = carros t
             |otherwise = False
{- |A função 'carrosaux1' é uma das funções auxiliares da função "carros" para que juntamente com as outras funções auxiliares façam com que a função "carros" retorne que uma lista de obstáculos é inválida caso os obstáculos dessa lista ao sairem do mapa e voltando a reaparecer do lado oposto não originem um carro com mais do que 3 unidades de comprimento.
-}  
carrosaux1:: Int -- ^ Escreve-se o número 0.
                -> (Terreno, [Obstaculo]) -- ^ Escreve-se um par composto por um tipo de terreno e por uma lista de obstáculos desse mesmo terreno.
                    -> Int -- ^ A função revela qual o comprimento de de um carro que esteja exatamente no início da estrada.
carrosaux1 n (t, []) = n
carrosaux1 n (t, Carro:os) = carrosaux1 (n+1) (t, os) 
carrosaux1 n (t, _:os) = n 
{- |A função 'carrosaux2' é mais uma das funções auxiliares da função "carros".
-}
carrosaux2:: Int -- ^ Escreve-se o número 0.
                -> (Terreno, [Obstaculo]) -- ^ Escreve-se um par composto por um tipo de terreno e por uma lista de obstáculos desse mesmo terreno.
                    -> Int -- ^ A função revela qual o comprimento de de um carro que esteja exatamente no final da estrada.
carrosaux2 n (t, []) = n
carrosaux2 n (t, [o]) | o==Carro = n+1
                      | otherwise = n
carrosaux2 n (t, o:os) | last os == Carro = carrosaux2 (n+1) (t, init (o:os)) 
                       | otherwise = n
{- |A função 'carrosaux3' é mais uma das funções auxiliares da função "carros".
-}
carrosaux3::(Terreno, [Obstaculo]) -- ^ Escreve-se um par composto por um tipo de terreno e por uma lista de obstáculos desse mesmo terreno.
                    -> Int -- ^ A função revela qual o comprimento de um carro que tem uma parte na extremidade esquerda da estrada e a outra parte na extremidade direita da estrada.
carrosaux3 a = carrosaux1 0 a + carrosaux2 0 a
{- |A função 'carrosaux4' é mais uma das funções auxiliares da função "carros".
-}
carrosaux4::Int -- ^ Escreve-se o número 0.
                ->(Terreno, [Obstaculo]) -- ^ Escreve-se um par composto por um tipo de terreno e por uma lista de obstáculos desse mesmo terreno.
                    ->Bool -- ^ Se um carro tiver mais do que 3 unidades de comprimento, esta função retornará o resultado "False".
carrosaux4 4 p = False
carrosaux4 x (t,[]) = True
carrosaux4 x (t,Carro:os) = carrosaux4 (x+1) (t,os)
carrosaux4 x (t,o:os) = carrosaux4 0 (t,os)
{- |A função 'nenhuns' verifica se existe no mínimo um obstáculo "Nenhum", ou seja, uma linha não pode ser composta exclusivamente por obstáculos, precisando de haver pelo menos um espaço livre
-}
nenhuns:: [(Terreno,[Obstaculo])] -- ^ Escreve-se uma lista de pares em que o primeiro elemento do par é o tipo de terreno e o segundo elemento é a lista de obstáculos presentes nesse terreno. 
                -> Bool -- ^ Caso exista pelo menos um obstáculo "Nenhum" na lista de obstáculos para cada tipo de terreno associado, o resultado será "True". 
nenhuns [] = True
nenhuns ((t, []):ts) = False  
nenhuns ((t, o:os):ts) | o == Nenhum = nenhuns ts
                       | otherwise = nenhuns ((t, os):ts)
{- |A função 'largura' verifica se o número de obstáculos de uma lista corresponde à largura do mapa
-}
largura:: Int -- Insere-se o número correspondente à largura do mapa.
                -> [(Terreno,[Obstaculo])] -- ^ Escreve-se uma lista de pares em que o primeiro elemento do par é o tipo de terreno e o segundo elemento é a lista de obstáculos presentes nesse terreno.
                    -> Bool -- ^ Caso o número correspondente à largura do mapa seja igual ao comprimento da lista de obstáculos para cada tipo de terreno associado, o resultado será "True".
largura l [] = True 
largura l ((t,o:os):ts) | l==length (o:os) = largura l ts  
                        | otherwise = False
{- |A função 'rios' verifica se não existem mais do que 4 rios consecutivos
-}
rios :: Int -- ^ Escreve-se o número 0 que servirá para contabilizar o número de rios que aparecem seguidos, e cada vez que o número de rios seguidos for inferior ou igual a 4 e a seguir estiver escrito outro tipo de terreno, este número voltará a 0.
            -> [(Terreno,[Obstaculo])] -- ^ Escreve-se uma lista de pares em que o primeiro elemento do par é o tipo de terreno e o segundo elemento é a lista de obstáculos presentes nesse terreno. 
                -> Bool -- ^ Caso se encontrem mais do que 4 rios consecutivos, ou seja, caso a palavra "Rio" apareça mais do que 4 vezes em pares consecutivos, o resultado será "False".
rios 5 l = False
rios x [] = True 
rios x ((Rio v1,o:os):ts) = rios (x+1) ts 
rios x ((t,o:os):ts) = rios 0 ts
{- |A função 'estradas' verifica se não existem mais do que 5 estradas consecutivas
-}
estradas :: Int -- ^ Escreve-se o número 0 que servirá para contabilizar o número de estradas que aparecem seguidas, e cada vez que o número de estradas seguidas for inferior ou igual a 5 e a seguir estiver escrito outro tipo de terreno, este número voltará a 0.
                -> [(Terreno,[Obstaculo])] -- ^ Escreve-se uma lista de pares em que o primeiro elemento do par é o tipo de terreno e o segundo elemento é a lista de obstáculos presentes nesse terreno.
                    -> Bool -- ^ Caso se encontrem mais do que 5 estradas consecutivas, ou seja, caso a palavra "Estrada" apareça mais do que 5 vezes em pares consecutivos, o resultado será "False".
estradas 6 l = False
estradas x [] = True 
estradas x ((Estrada v1,o:os):ts) = estradas (x+1) ts 
estradas x ((t,o:os):ts) = estradas 0 ts
{- |A função 'relvas' verifica se não existem mais do que 5 relvas consecutivas
-}
relvas :: Int -- ^ Escreve-se o número 0 que servirá para contabilizar o número de relvas que aparecem seguidas, e cada vez que o número de relvas seguidas for inferior ou igual a 5 e a seguir estiver escrito outro tipo de terreno, este número voltará a 0.
                -> [(Terreno,[Obstaculo])] -- ^ Escreve-se uma lista de pares em que o primeiro elemento do par é o tipo de terreno e o segundo elemento é a lista de obstáculos presentes nesse terreno.
                    -> Bool -- ^ Caso se encontrem mais do que 5 relvas consecutivas, ou seja, caso a palavra "Relva" apareça mais do que 5 vezes em pares consecutivos, o resultado será "False".
relvas 6 l = False
relvas x [] = True 
relvas x ((Relva,o:os):ts) = relvas (x+1) ts 
relvas x ((t,o:os):ts) = relvas 0 ts 
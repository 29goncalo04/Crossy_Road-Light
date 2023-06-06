{-|
Module      : Tarefa2_2022li1g008
Description : Geração contínua de um mapa
Copyright   : Gonçalo Oliveira Cruz <a104346@alunos.uminho.pt>
              Gonçalo Costa Magalhães <a104538@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g008 where

import LI12223
import Data.List
import Tarefa1_2022li1g008
import System.Random

{- |A função 'estendeMapa' dá-nos um novo mapa válido com uma nova linha gerada aleatoriamente. 

== Exemplos de utilização:

>>> estendeMapa (Mapa 4 [(Relva,[Nenhum,Arvore,Arvore,Nenhum]),
                         (Rio (-3),[Tronco,Tronco,Nenhum,Tronco]),
                         (Rio 1,[Nenhum,Nenhum,Tronco,Nenhum]),
                         (Estrada 2,[Carro,Carro,Carro,Nenhum])]) 0
Mapa 4 [(Rio 5,[Nenhum,Nenhum,Nenhum,Tronco]),
        (Relva,[Nenhum,Arvore,Arvore,Nenhum]),
        (Rio (-3),[Tronco,Tronco,Nenhum,Tronco]),
        (Rio 1,[Nenhum,Nenhum,Tronco,Nenhum]),
        (Estrada 2,[Carro,Carro,Carro,Nenhum])] 

>>> estendeMapa (Mapa 6 [(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Arvore]),
                         (Relva,[Nenhum,Arvore,Nenhum,Nenhum,Arvore,Arvore]),
                         (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore]),
                         (Relva,[Arvore,Arvore,Arvore,Nenhum,Arvore,Nenhum]),
                         (Relva,[Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore])]) 95
Mapa 6 [(Estrada 1,[Carro,Carro,Nenhum,Carro,Nenhum,Carro]),
        (Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Arvore]),
        (Relva,[Nenhum,Arvore,Nenhum,Nenhum,Arvore,Arvore]),
        (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore]),
        (Relva,[Arvore,Arvore,Arvore,Nenhum,Arvore,Nenhum]),
        (Relva,[Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore])] 

>>> estendeMapa (Mapa 6 [(Rio 1,[Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum]),
                         (Rio (-2),[Nenhum,Tronco,Tronco,Nenhum,Tronco,Tronco]),
                         (Rio 4,[Tronco,Nenhum,Tronco,Tronco,Nenhum,Tronco]),
                         (Rio (-4),[Tronco,Tronco,Tronco,Nenhum,Nenhum,Tronco])]) 23
Mapa 6 [(Estrada (-1),[Nenhum,Nenhum,Nenhum,Carro,Carro,Carro]),
        (Rio 1,[Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum]),
        (Rio (-2),[Nenhum,Tronco,Tronco,Nenhum,Tronco,Tronco]),
        (Rio 4,[Tronco,Nenhum,Tronco,Tronco,Nenhum,Tronco]),
        (Rio (-4),[Tronco,Tronco,Tronco,Nenhum,Nenhum,Tronco])] 
-}
estendeMapa :: Mapa -- ^ A função recebe um mapa válido.
                  -> Int -- ^ A função recebe também um número inteiro entre 0 e 100 para gerar aleatoriamente uma nova linha.
                        -> Mapa -- ^ A função gera um novo mapa válido com a nova linha no topo.
estendeMapa (Mapa l ((terreno,o):t)) n | mapaValido (Mapa l (estendeLinha n l velocidade (newTerreno ,[]):(terreno,o):t)) = Mapa l (estendeLinha n l velocidade (newTerreno ,[]):(terreno,o):t)
                                       | otherwise = estendeMapa (Mapa l ((terreno,o):t)) (n+1)
                                       where listaTerreno = proximosTerrenosValidos (Mapa l ((terreno,o):t))
                                             newTerreno = geraTerrenoFromAleatorios (geraAleatorios (length listaTerreno-1) 1 n) listaTerreno
                                             velocidade = case terreno of 
                                                            Rio v1 -> v1*(-1)
                                                            Estrada v1 -> randomInt 9 n
                                                            Relva -> randomInt 7 n 
{- |A função 'estendeLinha' dá-nos uma nova linha gerada aleatoriamente. 
-}
estendeLinha ::Int -- ^ A função recebe um número inteiro a partir do qual inicia a aleatoriedade.
                  -> Int -- ^ A função recebe também um número inteiro correspondente ao comprimento da lista de obstáculos.
                        -> Velocidade -- ^ A função recebe também uma velocidade.
                              -> (Terreno, [Obstaculo]) -- ^ A função recebe um par, com um terreno como primeiro elemento e uma lista de obstáculos como segundo elemento.
                                    -> (Terreno, [Obstaculo]) -- ^ A função gera um novo par (nova linha do mapa) aleatório.
estendeLinha seed l v1 (terreno,o) | l == length o = (terreno,o)  
estendeLinha seed l v1 (Relva,o) = estendeLinha seed l v1 (Relva, obstaculos)
   where listaobstaculos= proximosObstaculosValidos l (Relva,o) 
         obstaculos = geraObsFromAleatorios (geraAleatorios (length listaobstaculos-1) l seed) listaobstaculos
estendeLinha seed l v1 (Rio v ,o) = estendeLinha seed l v1 (Rio v1 , obstaculos)  
   where listaobstaculos= proximosObstaculosValidos l (Rio v ,o)
         obstaculos = geraObsFromAleatorios (geraAleatorios (length listaobstaculos-1) l seed) listaobstaculos
estendeLinha seed l v1 (Estrada v,o) = estendeLinha seed l v1 (Estrada v1 , obstaculos)                             
   where listaobstaculos= proximosObstaculosValidos l (Estrada v ,o)
         obstaculos = geraObsFromAleatorios (geraAleatorios (length listaobstaculos-1) l seed) listaobstaculos        
{- |A função 'randomList' gera uma lista aleatória.
-}
randomList :: (Random a) => Int -> [a]
randomList seed = randoms (mkStdGen seed)
{- |A função 'randomInt' gera um número aleatório.
-}
randomInt:: Int -> Int -> Int
randomInt  y x = mod (randomList x !! x) y
{- |A função 'geraAleatorios' dá-nos uma lista de números escolhida aleatoriamente.
-}
geraAleatorios :: Int -- ^ A função recebe um número inteiro que limita o intervalo de aleatoriedade.
                        -> Int -- ^ A função recebe um número inteiro que indica o número de inteiros que queremos para fazer essa mesma aleatoriedade.
                              -> Int -- ^ A função recebe um número inteiro a partir do qual inicia a aleatoriedade.
                                    -> [Int] -- ^ A função gera uma lista de números inteiros. 
geraAleatorios x n seed = take n (randomRs (0,x) (mkStdGen seed)) 
{- |A função 'geraObsFromAleatorios' dá-nos uma lista de obstáculos escolhida aleatoriamente com base nos obstáculos válidos.
-}
geraObsFromAleatorios :: [Int] -- ^ A função recebe uma lista de números inteiros.
                              -> [Obstaculo] -- ^ A função recebe uma lista de obstáculos válida.
                                          -> [Obstaculo] -- ^ A função gera uma nova lista de obstáculos com base nos obstáculos válidos inseridos.
geraObsFromAleatorios [] l = []
geraObsFromAleatorios (h:t) l = (l !! h): geraObsFromAleatorios t l 
{- |A função 'geraTerrenoFromAleatorios' dá-nos um Terreno escolhido aleatoriamente.
-}
geraTerrenoFromAleatorios :: [Int] -- ^ A função recebe uma lista com um número inteiro.
                              -> [Terreno] -- ^ A função recebe também uma lista com um Terreno.
                                          -> Terreno -- ^ A função retorna esse mesmo Terreno.
geraTerrenoFromAleatorios [h] l = l !! h  
{- |A função 'proximosTerrenosValidos' indica os terrenos possíveis de inserir na seguinte linha de um mapa dado.
-}
proximosTerrenosValidos :: Mapa -- ^ A função recebe um mapa válido.
                              -> [Terreno] -- ^ A função retorna-nos uma lista de terrenos possíveis de inserir na seguinte linha. 
proximosTerrenosValidos (Mapa _ []) = [Rio 0, Estrada 0, Relva]
proximosTerrenosValidos (Mapa _ ((Rio _, _):(Rio _, _):(Rio _, _):(Rio _, _):ts)) = [Estrada 0, Relva]
proximosTerrenosValidos (Mapa _ ((Estrada _, _): (Estrada _, _): (Estrada _, _): (Estrada _, _): (Estrada _, _):ts)) = [Rio 0, Relva]
proximosTerrenosValidos (Mapa _ ((Relva, _): (Relva, _): (Relva, _): (Relva, _): (Relva, _):ts)) = [Rio 0, Estrada 0]
proximosTerrenosValidos (Mapa x (h:ts)) = proximosTerrenosValidos (Mapa x ts)
{- |A função 'proximosObstaculosValidos' indica os obstáculos possíveis de inserir numa lista de obstáculos dada.
-}
proximosObstaculosValidos :: Int -- ^ A função recebe um número inteiro correspondente à largura do mapa.
                              -> (Terreno, [Obstaculo]) -- ^ A função recebe também um par com um determinado terreno e a respetiva lista de obstáculos.
                                                      -> [Obstaculo] -- A função retorna-nos a lista de obstáculos possíveis de inserir na lista de obstáculos.
proximosObstaculosValidos x (Rio _, []) = [Nenhum, Tronco]
proximosObstaculosValidos x (Estrada _, []) = [Nenhum, Carro]
proximosObstaculosValidos x (Relva, []) = [Nenhum, Arvore]
proximosObstaculosValidos x (Rio _, o) | x == length o = []
                                       | (x - length o) == 1 && elem Tronco o == False && listaObstaculos o == True = [Tronco] 
                                       | (x - length o) >= 1 && elem Nenhum o && listaObstaculos o == True = [Nenhum, Tronco]
                                       | (x - length o) >= 1 && elem Nenhum o && listaObstaculos o == False = [Nenhum]
                                       | (x - length o) > 1 && elem Nenhum o == False && listaObstaculos o == True = [Nenhum, Tronco]
                                       | (x - length o) > 1 && elem Nenhum o == False && listaObstaculos o == False = [Nenhum]
                                       | (x - length o) == 1 && elem Nenhum o && listaObstaculos o == False = [Nenhum] 
                                       | otherwise = [Nenhum] 
proximosObstaculosValidos x (Estrada _, o) | x == length o = []
                                           | (x - length o) >= 1 && elem Nenhum o && listaObstaculos o == True = [Nenhum, Carro]
                                           | (x - length o) >= 1 && elem Nenhum o && listaObstaculos o == False = [Nenhum]
                                           | (x - length o) > 1 && elem Nenhum o == False && listaObstaculos o == True = [Nenhum, Carro]
                                           | (x - length o) > 1 && elem Nenhum o == False && listaObstaculos o == False = [Nenhum]
                                           | (x - length o) == 1 && elem Nenhum o && listaObstaculos o == False = [Nenhum] 
                                           | otherwise = [Nenhum]                                       
proximosObstaculosValidos x (Relva, o) | x == length o = []
                                       | (x - length o) >= 1 && elem Nenhum o = [Nenhum, Arvore]
                                       | (x - length o) > 1 && elem Nenhum o == False = [Nenhum, Arvore]
                                       | (x - length o) == 1 && elem Nenhum o = [Nenhum] 
                                       | otherwise = [Nenhum] 
{- |A função 'listaObstaculos' verifica se não existem mais do que 5 troncos consecutivos nem mais do que 3 carros consecutivos.
-}
listaObstaculos :: [Obstaculo] -- ^ A função recebe uma lista de obstáculos.
                        -> Bool -- ^ A função retorna o valor lógico "True" se não existirem mais do que 5 troncos consecutivos nem mais do que 3 carros consecutivos.
listaObstaculos [] = True
listaObstaculos l = tamanhoObstaculos (group l)
{- |A função 'tamanhoObstaculos' verifica se não existem listas de comprimento superior a 5 e com um "Tronco" como primeiro elemento dessa lista, e se não existem listas de comprimento superior a 3 e com um "Carro" como primeiro elemento dessa lista.
-}
tamanhoObstaculos :: [[Obstaculo]] -- ^ A função recebe uma lista de listas de obstáculos.
                         -> Bool -- ^ Caso o primeiro elemento de uma lista seja "Tronco" ou "Carro" e o comprimento da lista seja maior que 5 ou que 3, respetivamente, o resultado será "False".
tamanhoObstaculos [[]] = True
tamanhoObstaculos (h:t) | head h == Tronco && (length h > 5 ) = False
                        | head h == Carro && (length h > 3) = False
                        | otherwise = True
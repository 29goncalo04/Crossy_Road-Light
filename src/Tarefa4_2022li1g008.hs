{-|
Module      : Tarefa4_2022li1g008
Description : Determinar se o jogo terminou
Copyright   : Gonçalo Oliveira Cruz <a104346@alunos.uminho.pt>
              Gonçalo Costa Magalhães <a104538@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Tarefa4_2022li1g008 where
import LI12223
import Tarefa3_2022li1g008
import Data.Bool (Bool)
{- |A função 'jogoTerminou' indica se o jogo já terminou, ou seja se o jogador perdeu o jogo.

== Exemplos de utilização:

>>> jogoTerminou (Jogo (Jogador (0,1)) 
                  (Mapa 3 [(Relva,[Arvore,Arvore,Nenhum]),
                  (Rio 2,[Nenhum, Tronco, Tronco]),
                  (Estrada 1,[Nenhum,Carro,Nenhum])])) 

True

>>> jogoTerminou (Jogo (Jogador (1,2)) 
                  (Mapa 3 [(Relva,[Arvore,Arvore,Nenhum]),
                  (Rio 2,[Nenhum, Tronco, Tronco]),
                  (Estrada 1,[Nenhum,Carro,Nenhum])])) 

True

>>> jogoTerminou (Jogo (Jogador ((-1),2)) 
                  (Mapa 3 [(Relva,[Arvore,Arvore,Nenhum]),
                  (Rio 2,[Nenhum, Tronco, Tronco]),
                  (Estrada 1,[Nenhum,Carro,Nenhum])])) 

True

>>> jogoTerminou (Jogo (Jogador (1,1)) 
                  (Mapa 3 [(Relva,[Arvore,Arvore,Nenhum]),
                  (Rio 2,[Nenhum, Tronco, Tronco]),
                  (Estrada 1,[Nenhum,Carro,Nenhum])])) 

False
-} 
jogoTerminou :: Jogo -- ^ A função recebe um jogo (a posição do jogador e um mapa válido). 
                    -> Bool -- ^ A função retornará "True" se o jogador saiu fora do mapa ao ser arrastado por um tronco, ou se o jogador caiu à água, ou se ficou "debaixo" de um carro, utilizando para isso as funções auxiliares "jogadorFora" e "coincide".
jogoTerminou (Jogo (Jogador (x,y)) (Mapa l ((t,o:os):ts))) | jogadorFora (Mapa l ((t,o:os):ts)) (Jogador (x,y)) || coincide (Mapa l ((t,o:os):ts)) (Jogador (x,y)) = True
                                                           | otherwise = False
{- |A função 'jogadorFora' é uma função auxiliar da "jogoTerminou" e serve para indicar se o jogador já se encontra fora do mapa.
-}                                                           
jogadorFora :: Mapa -- ^ A função recebe um mapa válido. 
                -> Jogador -- ^ E recebe a posição onde o jogador se encontra.
                    -> Bool -- ^ A função retornará o valor lógico "True" caso as coordenadas do jogador se encontrem já fora do limites do mapa.
jogadorFora (Mapa l ((t,o:os):ts)) (Jogador (x,y)) | x < 0 || x > (length (o:os)-1) || y < 0 || y > (length ((t,o:os):ts)-1) = True
                                                   | otherwise = False
{- |A função 'coincide' é uma função auxiliar da "jogoTerminou" e serve para indicar se o jogador se encontra na água, ou seja se está num rio mas não está em cima de um tronco, ou se este se encontra "debaixo" de um carro (esta função utiliza as funções "getTerreno" e "encontraCoordenadas" definidas previamente na Tarefa 3).
-}
coincide :: Mapa -- ^ A função recebe um mapa válido. 
                -> Jogador -- ^ E recebe a posição onde o jogador se encontra.
                    -> Bool -- ^ A função retornará o valor lógico "True" caso as coordenadas do jogador sejam as mesmas que as de "Nenhum" quando este está num rio, ou seja caso o jogador esteja num rio mas não em cima de um tronco, ou então caso as coordenadas do jogador sejam as mesmas que as de um "Carro".
coincide (Mapa l ((t,o:os):ts)) (Jogador (x,y)) | getTerreno2 (getTerreno (encontraLinha y (Mapa l ((t,o:os):ts)))) == "Rio" && encontraCoordenadas (x,y) (Mapa l ((t,o:os):ts)) == Nenhum = True
                                                | encontraCoordenadas (x,y) (Mapa l ((t,o:os):ts)) == Carro = True
                                                | otherwise = False
{- |A função 'encontraLinha' é uma função auxiliar da "coincide" e tem como objetivo indicar qual a linha pretendida de um mapa válido (esta função utiliza as funções "encontrax" e "getMapa" definidas previamente na Tarefa 3).
-}
encontraLinha:: Int -- ^ A função recebe um inteiro que corresponderá ao número de ordem da linha do mapa pretendida e subtraída por 1.
                    -> Mapa -- ^ E recebe um mapa válido.
                        -> (Terreno,[Obstaculo]) -- ^ A função indica qual a linha do mapa pretendida, não esquecendo que se por exemplo quisermos a primeira linha do mapa, o número inteiro que devemos dar à função é o 0.
encontraLinha y l = encontrax y (getMapa l)
{- |A função 'getTerreno2' é uma função auxiliar da "coincide" e serve para indicar qual o nome do terreno em causa (excluindo assim a sua velocidade).
-} 
getTerreno2 :: Terreno -- ^ A função recebe um terreno com a sua velocidade associada, se assim for o caso (pois se o terreno for "Relva", este já não terá velocidade). 
                -> String -- ^ A função retornará o nome do terreno que recebeu, mas sem a sua velocidade e em forma de "String".
getTerreno2 (Rio v1) = "Rio"
getTerreno2 (Estrada v1) = "Estrada"
getTerreno2 Relva = "Relva"
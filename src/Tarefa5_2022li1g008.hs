{-|
Module      : Tarefa5_2022li1g008
Description : Criar efeito de deslize do mapa
Copyright   : Gonçalo Oliveira Cruz <a104346@alunos.uminho.pt>
              Gonçalo Costa Magalhães <a104538@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/23.
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Tarefa5_2022li1g008 where
import LI12223
import Tarefa2_2022li1g008
import Tarefa3_2022li1g008
{- |A função 'deslizaJogo' faz com que o mapa se vá movendo, ou seja, faz com que seja acrescentada uma linha no topo do mapa e seja retirada a última linha do mesmo ao mesmo tempo.

== Exemplos de utilização:

>>> deslizaJogo 2 (Jogo (Jogador (1,1))
                           (Mapa 3 [(Relva,[Nenhum,Nenhum,Nenhum]),
                           (Rio (-3),[Nenhum,Tronco,Nenhum]),
                           (Relva,[Nenhum,Nenhum,Nenhum])])) 
Jogo (Jogador (1,2)) 
(Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),
(Relva,[Nenhum,Nenhum,Nenhum]),
(Rio (-3),[Nenhum,Tronco,Nenhum])]) 

>>> deslizaJogo 86 (Jogo (Jogador (1,1))
                           (Mapa 3 [(Relva,[Nenhum,Nenhum,Nenhum]),
                           (Rio (-3),[Nenhum,Tronco,Nenhum]),
                           (Relva,[Nenhum,Nenhum,Nenhum])])) 
Jogo (Jogador (1,2)) 
(Mapa 3 [(Estrada 1,[Carro,Carro,Nenhum]),
(Relva,[Nenhum,Nenhum,Nenhum]),
(Rio (-3),[Nenhum,Tronco,Nenhum])]) 

>>> deslizaJogo 20 (Jogo (Jogador (1,1))
                           (Mapa 3 [(Relva,[Nenhum,Nenhum,Nenhum]),
                           (Rio (-3),[Nenhum,Tronco,Nenhum]),
                           (Relva,[Nenhum,Nenhum,Nenhum])])) 
Jogo (Jogador (1,2)) 
(Mapa 3 [(Rio 4,[Tronco,Nenhum,Nenhum]),
(Relva,[Nenhum,Nenhum,Nenhum]),
(Rio (-3),[Nenhum,Tronco,Nenhum])]) 

-} 
deslizaJogo :: Int -> -- ^ A função recebe também um número inteiro entre 0 e 100 para gerar aleatoriamente uma nova linha.
                    Jogo -- ^ A função recebe um jogo (a posição do jogador e um mapa válido). 
                        -> Jogo -- ^ A função retornará um novo jogo, com as mesmas coordenadas que o jogador tinha anteriormente mas agora com uma unidade somada ao "y", e o mesmo mapa inicialmente inserido mas com uma nova linha no topo e com a última linha removida.
deslizaJogo n (Jogo (Jogador (x,y)) (Mapa l ((t,o:os):ts))) = Jogo (Jogador (x,y+1)) (Mapa l (init (getMapa (estendeMapa (Mapa l ((t,o:os):ts)) n))))
module Tarefa3_2022li1g008_Spec where

import LI12223
import Tarefa3_2022li1g008
import Test.HUnit
import LI12223 (Direcao(Baixo, Direita, Esquerda, Cima), Obstaculo (Tronco, Nenhum), Terreno (Rio))

testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ test [
    "Teste 1" ~: Jogo (Jogador (2,1)) 
                      (Mapa 3 [(Rio 2,[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada 1,[Carro,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (2,1)) 
                                                                           (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                                                                           (Relva,[Arvore,Arvore,Nenhum]),
                                                                           (Estrada 1,[Nenhum,Nenhum,Carro])])) 
                                                                           (Move Esquerda),
    "Teste 2" ~: Jogo (Jogador (1,2)) 
                      (Mapa 3 [(Rio 2,[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada 1,[Carro,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (1,2)) 
                                                                           (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                                                                           (Relva,[Arvore,Arvore,Nenhum]),
                                                                           (Estrada 1,[Nenhum,Nenhum,Carro])])) 
                                                                           (Move Cima),
    "Teste 3" ~: Jogo (Jogador (3,0)) 
                      (Mapa 3 [(Rio 2,[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada 1,[Carro,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (1,0)) 
                                                                           (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                                                                           (Relva,[Arvore,Arvore,Nenhum]),
                                                                           (Estrada 1,[Nenhum,Nenhum,Carro])])) 
                                                                           (Move Baixo),
    "Teste 4" ~: Jogo (Jogador (1,1)) 
                      (Mapa 3 [(Rio 2,[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Nenhum,Arvore]),
                      (Estrada 1,[Carro,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (1,1)) 
                                                                           (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                                                                           (Relva,[Arvore,Nenhum,Arvore]),
                                                                           (Estrada 1,[Nenhum,Nenhum,Carro])])) 
                                                                           (Move Direita),
    "Teste 5" ~: Jogo (Jogador (1,0)) 
                      (Mapa 3 [(Rio 2,[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada 1,[Carro,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (0,0)) 
                                                                           (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                                                                           (Relva,[Arvore,Arvore,Nenhum]),
                                                                           (Estrada 1,[Nenhum,Nenhum,Carro])])) 
                                                                           (Move Esquerda), 
    "Teste 6" ~: Jogo (Jogador (1,0)) 
                      (Mapa 3 [(Rio 2,[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada 1,[Carro,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (1,0)) 
                                                                           (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                                                                           (Relva,[Arvore,Arvore,Nenhum]),
                                                                           (Estrada 1,[Nenhum,Nenhum,Carro])])) 
                                                                           (Move Cima),
    "Teste 7" ~: Jogo (Jogador (1,2)) 
                      (Mapa 3 [(Rio 2,[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada 1,[Carro,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (1,2)) 
                                                                           (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                                                                           (Relva,[Arvore,Arvore,Nenhum]),
                                                                           (Estrada 1,[Nenhum,Nenhum,Carro])])) 
                                                                           (Move Baixo),
    "Teste 8" ~: Jogo (Jogador (2,1)) 
                      (Mapa 3 [(Rio 2,[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada 1,[Carro,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (2,1)) 
                                                                           (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                                                                           (Relva,[Arvore,Arvore,Nenhum]),
                                                                           (Estrada 1,[Nenhum,Nenhum,Carro])])) 
                                                                           (Move Direita),
    "Teste 9" ~: Jogo (Jogador (2,0)) 
                      (Mapa 3 [(Rio 2,[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada 1,[Carro,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (0,0)) 
                                                                           (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                                                                           (Relva,[Arvore,Arvore,Nenhum]),
                                                                           (Estrada 1,[Nenhum,Nenhum,Carro])])) 
                                                                           (Parado),
    "Teste 10" ~: Jogo (Jogador (3,0)) 
                      (Mapa 3 [(Rio 2,[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada 1,[Carro,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (1,0)) 
                                                                           (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                                                                           (Relva,[Arvore,Arvore,Nenhum]),
                                                                           (Estrada 1,[Nenhum,Nenhum,Carro])])) 
                                                                           (Parado),
    "Teste 11" ~: Jogo (Jogador (-1,0)) 
                      (Mapa 3 [(Rio (-1),[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada 1,[Carro,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (0,0)) 
                                                                           (Mapa 3 [(Rio (-1),[Tronco,Tronco,Nenhum]),
                                                                           (Relva,[Arvore,Arvore,Nenhum]),
                                                                           (Estrada 1,[Nenhum,Nenhum,Carro])])) 
                                                                           (Parado),
    "Teste 12" ~: Jogo (Jogador (0,0)) 
                      (Mapa 3 [(Rio (-1),[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada 1,[Carro,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (1,0)) 
                                                                           (Mapa 3 [(Rio (-1),[Tronco,Tronco,Nenhum]),
                                                                           (Relva,[Arvore,Arvore,Nenhum]),
                                                                           (Estrada 1,[Nenhum,Nenhum,Carro])])) 
                                                                           (Parado),
    "Teste 13" ~: Jogo (Jogador (0,2)) 
                      (Mapa 3 [(Rio (-1),[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada 1,[Carro,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (0,2)) 
                                                                           (Mapa 3 [(Rio (-1),[Tronco,Tronco,Nenhum]),
                                                                           (Relva,[Arvore,Arvore,Nenhum]),
                                                                           (Estrada 1,[Nenhum,Nenhum,Carro])])) 
                                                                           (Parado),
    "Teste 14" ~: Jogo (Jogador (1,2)) 
                      (Mapa 3 [(Rio (-1),[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada 1,[Carro,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (0,2)) 
                                                                           (Mapa 3 [(Rio (-1),[Tronco,Tronco,Nenhum]),
                                                                           (Relva,[Arvore,Arvore,Nenhum]),
                                                                           (Estrada 1,[Nenhum,Nenhum,Carro])])) 
                                                                           (Move Direita),  
    "Teste 15" ~: Jogo (Jogador (0,2)) 
                      (Mapa 3 [(Rio (-1),[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada 1,[Carro,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (1,2)) 
                                                                           (Mapa 3 [(Rio (-1),[Tronco,Tronco,Nenhum]),
                                                                           (Relva,[Arvore,Arvore,Nenhum]),
                                                                           (Estrada 1,[Nenhum,Nenhum,Carro])])) 
                                                                           (Move Esquerda),
    "Teste 16" ~: Jogo (Jogador (2,2)) 
                      (Mapa 3 [(Rio (-1),[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada 1,[Nenhum,Nenhum,Carro])]) ~=? animaJogo (Jogo (Jogador (2,1)) 
                                                                           (Mapa 3 [(Rio (-1),[Tronco,Tronco,Nenhum]),
                                                                           (Relva,[Arvore,Arvore,Nenhum]),
                                                                           (Estrada 1,[Nenhum,Nenhum,Carro])])) 
                                                                           (Move Baixo),
    "Teste 17" ~: Jogo (Jogador (2,0)) 
                      (Mapa 3 [(Rio (-1),[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada 1,[Carro,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (2,1)) 
                                                                           (Mapa 3 [(Rio (-1),[Tronco,Tronco,Nenhum]),
                                                                           (Relva,[Arvore,Arvore,Nenhum]),
                                                                           (Estrada 1,[Nenhum,Nenhum,Carro])])) 
                                                                           (Move Cima),
    "Teste 18" ~: Jogo (Jogador (-1,0)) 
                      (Mapa 3 [(Rio (-1),[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada 1,[Carro,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (1,0)) 
                                                                           (Mapa 3 [(Rio (-1),[Tronco,Tronco,Nenhum]),
                                                                           (Relva,[Arvore,Arvore,Nenhum]),
                                                                           (Estrada 1,[Nenhum,Nenhum,Carro])])) 
                                                                           (Move Esquerda),
    "Teste 19" ~: Jogo (Jogador (3,0)) 
                      (Mapa 3 [(Rio 1,[Nenhum,Tronco,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada 1,[Carro,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (1,0)) 
                                                                           (Mapa 3 [(Rio 1,[Tronco,Tronco,Nenhum]),
                                                                           (Relva,[Arvore,Arvore,Nenhum]),
                                                                           (Estrada 1,[Nenhum,Nenhum,Carro])])) 
                                                                           (Move Direita),
    "Teste 20" ~: Jogo (Jogador (0,2)) 
                      (Mapa 3 [(Rio (-1),[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada (-2),[Carro, Nenhum, Nenhum])]) ~=? animaJogo (Jogo (Jogador (1,2)) 
                                                                               (Mapa 3 [(Rio (-1),[Tronco,Tronco,Nenhum]),
                                                                               (Relva,[Arvore,Arvore,Nenhum]),
                                                                               (Estrada (-2),[Nenhum,Nenhum,Carro])])) 
                                                                               (Move Esquerda),
    "Teste 21" ~: Jogo (Jogador (2,2)) 
                      (Mapa 3 [(Rio (-1),[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada (-2),[Nenhum, Nenhum, Carro])]) ~=? animaJogo (Jogo (Jogador (1,2)) 
                                                                               (Mapa 3 [(Rio (-1),[Tronco,Tronco,Nenhum]),
                                                                               (Relva,[Arvore,Arvore,Nenhum]),
                                                                               (Estrada (-2),[Nenhum,Nenhum,Carro])])) 
                                                                               (Move Direita),
    "Teste 22" ~: Jogo (Jogador (0,2)) 
                      (Mapa 3 [(Rio (-6),[Tronco,Tronco,Nenhum]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada (-2),[Carro, Nenhum, Nenhum])]) ~=? animaJogo (Jogo (Jogador (1,2)) 
                                                                               (Mapa 3 [(Rio (-6),[Tronco,Tronco,Nenhum]),
                                                                               (Relva,[Arvore,Arvore,Nenhum]),
                                                                               (Estrada (-2),[Nenhum,Nenhum,Carro])])) 
                                                                               (Move Esquerda),
    "Teste 23" ~: Jogo (Jogador (0,2)) 
                      (Mapa 3 [(Rio (-1),[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada (-8),[Carro, Nenhum, Nenhum])]) ~=? animaJogo (Jogo (Jogador (1,2)) 
                                                                               (Mapa 3 [(Rio (-1),[Tronco,Tronco,Nenhum]),
                                                                               (Relva,[Arvore,Arvore,Nenhum]),
                                                                               (Estrada (-8),[Nenhum,Nenhum,Carro])])) 
                                                                               (Move Esquerda),
    "Teste 24" ~: Jogo (Jogador (0,2)) 
                      (Mapa 3 [(Rio (-1),[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada (-3),[Carro, Nenhum, Nenhum])]) ~=? animaJogo (Jogo (Jogador (1,2)) 
                                                                               (Mapa 3 [(Rio (-1),[Tronco,Tronco,Nenhum]),
                                                                               (Relva,[Arvore,Arvore,Nenhum]),
                                                                               (Estrada (-3),[Nenhum,Nenhum,Carro])])) 
                                                                               (Move Esquerda),
    "Teste 25" ~: Jogo (Jogador (2,0)) 
                      (Mapa 3 [(Estrada (-2),[Carro,Nenhum,Carro]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada (-2),[Carro, Nenhum, Nenhum])]) ~=? animaJogo (Jogo (Jogador (2,1)) 
                                                                               (Mapa 3 [(Estrada (-2),[Carro,Carro,Nenhum]),
                                                                               (Relva,[Arvore,Arvore,Nenhum]),
                                                                               (Estrada (-2),[Nenhum,Nenhum,Carro])])) 
                                                                               (Move Cima),
    "Teste 26" ~: Jogo (Jogador (3,0)) 
                      (Mapa 3 [(Rio 2,[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada 1,[Carro,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (0,0)) 
                                                                           (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                                                                           (Relva,[Arvore,Arvore,Nenhum]),
                                                                           (Estrada 1,[Nenhum,Nenhum,Carro])])) 
                                                                           (Move Direita), 
    "Teste 27" ~: Jogo (Jogador (2,1)) 
                      (Mapa 3 [(Rio 2,[Nenhum,Tronco,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Estrada 1,[Carro,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (2,0)) 
                                                                           (Mapa 3 [(Rio 2,[Tronco,Nenhum,Tronco]),
                                                                           (Relva,[Arvore,Arvore,Nenhum]),
                                                                           (Estrada 1,[Nenhum,Nenhum,Carro])])) 
                                                                           (Move Baixo),
    "Teste 28" ~: Jogo (Jogador (2,1)) 
                      (Mapa 3 [(Rio 2,[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Rio (-1),[Tronco,Tronco,Nenhum])]) ~=? animaJogo (Jogo (Jogador (2,2)) 
                                                                           (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                                                                           (Relva,[Arvore,Arvore,Nenhum]),
                                                                           (Rio (-1),[Nenhum,Tronco,Tronco])])) 
                                                                           (Move Cima),  
    "Teste 29" ~: Jogo (Jogador (2,2)) 
                      (Mapa 3 [(Rio 2,[Tronco,Nenhum,Tronco]),
                      (Relva,[Arvore,Arvore,Nenhum]),
                      (Rio 1,[Tronco,Nenhum,Tronco])]) ~=? animaJogo (Jogo (Jogador (1,2)) 
                                                                           (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                                                                           (Relva,[Arvore,Arvore,Nenhum]),
                                                                           (Rio 1,[Nenhum,Tronco,Tronco])])) 
                                                                           (Move Cima)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
    ]
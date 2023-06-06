module Tarefa4_2022li1g008_Spec where

import LI12223
import Tarefa4_2022li1g008
import Test.HUnit
import LI12223 (Obstaculo(Nenhum))

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test [
    "Teste 1" ~: True ~=? jogoTerminou (Jogo (Jogador (-1,1)) 
                                            (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                                            (Relva,[Arvore,Arvore,Nenhum]),
                                            (Estrada 1,[Nenhum,Nenhum,Carro])])),
    "Teste 2" ~: True ~=? jogoTerminou (Jogo (Jogador (3,0)) 
                                            (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                                            (Relva,[Arvore,Arvore,Nenhum]),
                                            (Estrada 1,[Nenhum,Nenhum,Carro])])),
    "Teste 3" ~: True ~=? jogoTerminou (Jogo (Jogador (1,-2)) 
                                            (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                                            (Relva,[Arvore,Arvore,Nenhum]),
                                            (Estrada 1,[Nenhum,Nenhum,Carro])])),  
    "Teste 4" ~: True ~=? jogoTerminou (Jogo (Jogador (2,4)) 
                                            (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                                            (Relva,[Arvore,Arvore,Nenhum]),
                                            (Estrada 1,[Nenhum,Nenhum,Carro])])),  
    "Teste 5" ~: True ~=? jogoTerminou (Jogo (Jogador (-1,1)) 
                                            (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                                            (Relva,[Arvore,Arvore,Nenhum]),
                                            (Estrada 1,[Nenhum,Nenhum,Carro])])),  
    "Teste 6" ~: True ~=? jogoTerminou (Jogo (Jogador (2,0)) 
                                            (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                                            (Relva,[Arvore,Arvore,Nenhum]),
                                            (Estrada 1,[Nenhum,Nenhum,Carro])])), 
    "Teste 7" ~: True ~=? jogoTerminou (Jogo (Jogador (2,2)) 
                                            (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                                            (Relva,[Arvore,Arvore,Nenhum]),
                                            (Rio (-3),[Nenhum,Tronco,Nenhum])])), 
    "Teste 8" ~: True ~=? jogoTerminou (Jogo (Jogador (0,2)) 
                                            (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                                            (Relva,[Arvore,Arvore,Nenhum]),
                                            (Rio (-3),[Nenhum,Tronco,Nenhum])])),  
    "Teste 9" ~: False ~=? jogoTerminou (Jogo (Jogador (1,2)) 
                                            (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                                            (Relva,[Arvore,Arvore,Nenhum]),
                                            (Rio (-3),[Nenhum,Tronco,Nenhum])])),  
    "Teste 10" ~: True ~=? jogoTerminou (Jogo (Jogador (2,2)) 
                                            (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                                            (Relva,[Arvore,Arvore,Nenhum]),
                                            (Estrada 1,[Nenhum,Nenhum,Carro])])),
    "Teste 11" ~: False ~=? jogoTerminou (Jogo (Jogador (2,1)) 
                                            (Mapa 5 [(Rio 2,[Tronco,Tronco,Nenhum,Tronco,Tronco]),
                                            (Relva,[Arvore,Arvore,Nenhum,Arvore,Nenhum]),
                                            (Estrada 1,[Nenhum,Nenhum,Carro,Carro,Nenhum]),
                                            (Relva,[Nenhum,Nenhum,Nenhum,Arvore,Nenhum])])),
    "Teste 12" ~: False ~=? jogoTerminou (Jogo (Jogador (1,3)) 
                                            (Mapa 5 [(Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco]),
                                            (Relva,[Arvore,Arvore,Nenhum,Arvore,Nenhum]),
                                            (Estrada 3,[Nenhum,Nenhum,Carro,Carro,Nenhum]),
                                            (Relva,[Nenhum,Nenhum,Nenhum,Arvore,Nenhum])])),
    "Teste 13" ~: False ~=? jogoTerminou (Jogo (Jogador (4,2)) 
                                            (Mapa 5 [(Rio 2,[Tronco,Tronco,Nenhum,Tronco,Tronco]),
                                            (Relva,[Arvore,Arvore,Nenhum,Arvore,Nenhum]),
                                            (Estrada 1,[Nenhum,Nenhum,Carro,Carro,Nenhum]),
                                            (Estrada (-3),[Nenhum,Carro,Nenhum,Carro,Carro])]))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
    ]
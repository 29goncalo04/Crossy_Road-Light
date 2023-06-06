module Tarefa1_2022li1g008_Spec where

import LI12223
import Tarefa1_2022li1g008
import Test.HUnit
import LI12223 (Obstaculo(Carro))

testsT1 :: Test
testsT1 = TestLabel "Testes Tarefa 1" $ test [
    "Teste 1" ~: True ~=? mapaValido (Mapa 6
    [(Rio 1, [Tronco, Nenhum, Tronco, Nenhum, Tronco, Tronco]),
    (Rio (-1), [Tronco, Tronco, Tronco, Nenhum, Nenhum, Nenhum]),
    (Estrada 2, [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum]),
    (Relva, [Arvore, Arvore, Nenhum, Arvore, Nenhum, Nenhum]),
    (Estrada 1, [Nenhum, Carro, Carro, Carro, Nenhum, Nenhum]),
    (Estrada 4, [Carro, Nenhum, Carro, Nenhum, Carro, Carro])]),

    "Teste 2" ~: False ~=? mapaValido (Mapa 6
    [(Rio 1, [Tronco, Carro, Tronco, Nenhum, Tronco, Tronco]),
    (Rio (-1), [Tronco, Tronco, Tronco, Nenhum, Nenhum, Nenhum]),
    (Estrada 2, [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum]),
    (Relva, [Arvore, Arvore, Nenhum, Arvore, Nenhum, Nenhum]),
    (Estrada 1, [Nenhum, Carro, Carro, Carro, Nenhum, Nenhum]),
    (Estrada 4, [Carro, Nenhum, Carro, Nenhum, Carro, Carro])]),

    "Teste 3" ~: False ~=? mapaValido (Mapa 6
    [(Rio 1, [Tronco, Arvore, Tronco, Nenhum, Tronco, Tronco]),
    (Rio (-1), [Tronco, Tronco, Tronco, Nenhum, Nenhum, Nenhum]),
    (Estrada 2, [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum]),
    (Relva, [Arvore, Arvore, Nenhum, Arvore, Nenhum, Nenhum]),
    (Estrada 1, [Nenhum, Carro, Carro, Carro, Nenhum, Nenhum]),
    (Estrada 4, [Carro, Nenhum, Carro, Nenhum, Carro, Carro])]),

    "Teste 4" ~: False ~=? mapaValido (Mapa 6
    [(Rio 1, [Tronco, Nenhum, Tronco, Nenhum, Tronco, Tronco]),
    (Rio (-1), [Tronco, Tronco, Tronco, Nenhum, Nenhum, Nenhum]),
    (Estrada 2, [Tronco, Nenhum, Nenhum, Carro, Carro, Nenhum]),
    (Relva, [Arvore, Arvore, Nenhum, Arvore, Nenhum, Nenhum]),
    (Estrada 1, [Nenhum, Carro, Carro, Carro, Nenhum, Nenhum]),
    (Estrada 4, [Carro, Nenhum, Carro, Nenhum, Carro, Carro])]),

    "Teste 5" ~: False ~=? mapaValido (Mapa 6
    [(Rio 1, [Tronco, Nenhum, Tronco, Nenhum, Tronco, Tronco]),
    (Rio (-1), [Tronco, Tronco, Tronco, Nenhum, Nenhum, Nenhum]),
    (Estrada 2, [Arvore, Nenhum, Nenhum, Carro, Carro, Nenhum]),
    (Relva, [Arvore, Arvore, Nenhum, Arvore, Nenhum, Nenhum]),
    (Estrada 1, [Nenhum, Carro, Carro, Carro, Nenhum, Nenhum]),
    (Estrada 4, [Carro, Nenhum, Carro, Nenhum, Carro, Carro])]),

    "Teste 6" ~: False ~=? mapaValido (Mapa 6
    [(Rio 1, [Tronco, Nenhum, Tronco, Nenhum, Tronco, Tronco]),
    (Rio (-1), [Tronco, Tronco, Tronco, Nenhum, Nenhum, Nenhum]),
    (Estrada 2, [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum]),
    (Relva, [Carro, Arvore, Nenhum, Arvore, Nenhum, Nenhum]),
    (Estrada 1, [Nenhum, Carro, Carro, Carro, Nenhum, Nenhum]),
    (Estrada 4, [Carro, Nenhum, Carro, Nenhum, Carro, Carro])]),

    "Teste 7" ~: False ~=? mapaValido (Mapa 6
    [(Rio 1, [Tronco, Nenhum, Tronco, Nenhum, Tronco, Tronco]),
    (Rio (-1), [Tronco, Tronco, Tronco, Nenhum, Nenhum, Nenhum]),
    (Estrada 2, [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum]),
    (Relva, [Tronco, Arvore, Nenhum, Arvore, Nenhum, Nenhum]),
    (Estrada 1, [Nenhum, Carro, Carro, Carro, Nenhum, Nenhum]),
    (Estrada 4, [Carro, Nenhum, Carro, Nenhum, Carro, Carro])]),

    "Teste 8" ~: False ~=? mapaValido (Mapa 6
    [(Rio (-3), [Tronco, Nenhum, Tronco, Nenhum, Tronco, Tronco]),
    (Rio (-1), [Tronco, Tronco, Tronco, Nenhum, Nenhum, Nenhum]),
    (Estrada 2, [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum]),
    (Relva, [Arvore, Arvore, Nenhum, Arvore, Nenhum, Nenhum]),
    (Estrada 1, [Nenhum, Carro, Carro, Carro, Nenhum, Nenhum]),
    (Estrada 4, [Carro, Nenhum, Carro, Nenhum, Carro, Carro])]),

    "Teste 9" ~: False ~=? mapaValido (Mapa 6
    [(Rio 1, [Tronco, Tronco, Tronco, Tronco, Tronco, Tronco]),
    (Rio (-1), [Tronco, Tronco, Tronco, Nenhum, Nenhum, Nenhum]),
    (Estrada 2, [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum]),
    (Relva, [Arvore, Arvore, Nenhum, Arvore, Nenhum, Nenhum]),
    (Estrada 1, [Nenhum, Carro, Carro, Carro, Nenhum, Nenhum]),
    (Estrada 4, [Carro, Nenhum, Carro, Nenhum, Carro, Carro])]),

    "Teste 10" ~: False ~=? mapaValido (Mapa 7
    [(Rio 1, [Tronco, Tronco, Tronco, Tronco, Tronco, Nenhum, Tronco]),
    (Rio (-1), [Tronco, Tronco, Tronco, Nenhum, Nenhum, Nenhum, Nenhum]),
    (Estrada 2, [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum, Nenhum]),
    (Relva, [Arvore, Arvore, Nenhum, Arvore, Nenhum, Nenhum, Nenhum]),
    (Estrada 1, [Nenhum, Carro, Carro, Carro, Nenhum, Nenhum, Nenhum]),
    (Estrada 4, [Carro, Nenhum, Carro, Nenhum, Carro, Carro, Nenhum])]),

    "Teste 11" ~: False ~=? mapaValido (Mapa 6
    [(Rio 1, [Tronco, Tronco, Tronco, Tronco, Tronco, Nenhum]),
    (Rio (-1), [Tronco, Tronco, Tronco, Nenhum, Nenhum, Nenhum]),
    (Estrada 2, [Carro, Nenhum, Nenhum, Carro, Carro, Carro]),
    (Relva, [Arvore, Arvore, Nenhum, Arvore, Nenhum, Nenhum]),
    (Estrada 1, [Nenhum, Carro, Carro, Carro, Nenhum, Nenhum]),
    (Estrada 4, [Carro, Nenhum, Carro, Nenhum, Carro, Carro])]),

    "Teste 12" ~: False ~=? mapaValido (Mapa 6
    [(Rio 1, [Tronco, Nenhum, Tronco, Nenhum, Tronco, Tronco]),
    (Rio (-1), [Tronco, Tronco, Tronco, Nenhum, Nenhum, Nenhum]),
    (Estrada 2, [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum]),
    (Relva, [Arvore, Arvore, Nenhum, Arvore, Nenhum, Nenhum]),
    (Estrada 1, [Nenhum, Carro, Carro, Carro, Carro, Nenhum]),
    (Estrada 4, [Carro, Nenhum, Carro, Nenhum, Carro, Carro])]),

    "Teste 13" ~: False ~=? mapaValido (Mapa 6
    [(Rio 1, [Tronco, Nenhum, Tronco, Nenhum, Tronco, Tronco]),
    (Rio (-1), [Tronco, Tronco, Tronco, Nenhum, Nenhum, Nenhum]),
    (Estrada 2, [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum]),
    (Relva, [Arvore, Arvore, Arvore, Arvore, Arvore, Arvore]),
    (Estrada 1, [Nenhum, Carro, Carro, Carro, Nenhum, Nenhum]),
    (Estrada 4, [Carro, Nenhum, Carro, Nenhum, Carro, Carro])]),

    "Teste 14" ~: False ~=? mapaValido (Mapa 6
    [(Rio 1, [Tronco, Nenhum, Tronco, Nenhum, Tronco]),
    (Rio (-1), [Tronco, Tronco, Tronco, Nenhum, Nenhum, Nenhum]),
    (Estrada 2, [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum]),
    (Relva, [Arvore, Arvore, Nenhum, Arvore, Nenhum, Nenhum]),
    (Estrada 1, [Nenhum, Carro, Carro, Carro, Nenhum, Nenhum]),
    (Estrada 4, [Carro, Nenhum, Carro, Nenhum, Carro, Carro])]),

    "Teste 15" ~: False ~=? mapaValido (Mapa 6
    [(Rio 1, [Tronco, Nenhum, Tronco, Nenhum, Tronco, Tronco]),
    (Rio (-1), [Tronco, Tronco, Tronco, Nenhum, Nenhum, Nenhum]),
    (Rio 2, [Tronco, Nenhum, Nenhum, Tronco, Tronco, Nenhum]),
    (Rio (-5), [Tronco, Tronco, Nenhum, Tronco, Nenhum, Nenhum]),
    (Rio 3, [Nenhum, Tronco, Tronco, Tronco, Nenhum, Nenhum]),
    (Estrada 4, [Carro, Nenhum, Carro, Nenhum, Carro, Carro])]),

    "Teste 16" ~: False ~=? mapaValido (Mapa 6
    [(Estrada 1, [Carro, Nenhum, Carro, Nenhum, Carro, Carro]),
    (Estrada (-1), [Carro, Carro, Carro, Nenhum, Nenhum, Nenhum]),
    (Estrada 2, [Carro, Nenhum, Nenhum, Carro, Carro, Nenhum]),
    (Estrada (-1), [Carro, Carro, Nenhum, Carro, Nenhum, Nenhum]),
    (Estrada 1, [Nenhum, Carro, Carro, Carro, Nenhum, Nenhum]),
    (Estrada 4, [Carro, Nenhum, Carro, Nenhum, Carro, Carro])]),

    "Teste 17" ~: False ~=? mapaValido (Mapa 6
    [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore, Arvore]),
    (Relva, [Arvore, Arvore, Arvore, Nenhum, Nenhum, Nenhum]),
    (Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore, Nenhum]),
    (Relva, [Arvore, Arvore, Nenhum, Arvore, Nenhum, Nenhum]),
    (Relva, [Nenhum, Arvore, Arvore, Arvore, Nenhum, Nenhum]),
    (Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore, Arvore])])
    ]
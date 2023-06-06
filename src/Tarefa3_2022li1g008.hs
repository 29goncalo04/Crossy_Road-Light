{- |
Module      : Tarefa3_2022li1g008
Description : Movimentação do personagem e obstáculos
Copyright   : Gonçalo Oliveira Cruz <a104346@alunos.uminho.pt>
              Gonçalo Costa Magalhães <a104538@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Tarefa3_2022li1g008 where
import Data.List
import LI12223
import GHC.CmmToAsm.AArch64.Instr (x0)
{- |A função 'animaJogo' movimenta os obstáculos do terreno tendo em conta a velocidade do mesmo, e o personagem, de acordo com a jogada dada.

== Exemplos de utilização:

>>> animaJogo (Jogo (Jogador (2,1)) 
                (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                (Relva,[Arvore,Arvore,Nenhum]),
                (Estrada 1,[Nenhum,Nenhum,Carro])])) 
                (Move Esquerda)
Jogo (Jogador (2,1)) 
(Mapa 3 [(Rio 2,[Tronco,Nenhum,Tronco]),
(Relva,[Arvore,Arvore,Nenhum]),
(Estrada 1,[Carro,Nenhum,Nenhum])])

>>> animaJogo (Jogo (Jogador (0,2)) 
                (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                (Relva,[Arvore,Arvore,Nenhum]),
                (Estrada 2,[Nenhum,Nenhum,Carro])])) 
                (Move Baixo)
Jogo (Jogador (0,2)) 
(Mapa 3 [(Rio 2,[Tronco,Nenhum,Tronco]),
(Relva,[Arvore,Arvore,Nenhum]),
(Estrada 2,[Carro,Nenhum,Nenhum])])

>>> animaJogo (Jogo (Jogador (0,0)) 
                (Mapa 3 [(Rio 2,[Tronco,Tronco,Nenhum]),
                (Relva,[Arvore,Arvore,Nenhum]),
                (Estrada 1,[Nenhum,Nenhum,Carro])])) 
                (Parado)
Jogo (Jogador (2,0)) 
(Mapa 3 [(Rio 2,[Tronco,Nenhum,Tronco]),
(Relva,[Arvore,Arvore,Nenhum]),
(Estrada 1,[Carro,Nenhum,Nenhum])])
-}
animaJogo :: Jogo -- ^ A função recebe um jogo, ou seja, as coordenadas iniciais do jogador e o mapa onde este se encontra. 
                -> Jogada -- ^ E recebe também a jogada que o jogador irá descrever.
                    -> Jogo -- ^ A função retornará a nova posição do jogador consoante a jogada que este efetuou, e o novo mapa após as obstáculos se terem movido de acordo com a velocidade do terreno onde estão inseridos.
animaJogo (Jogo (Jogador (x,y)) (Mapa l ((t,o:os):ts))) j | j == Move Baixo && y/=length ((t,o:os):ts)-1 &&  encontraCoordenadas (x,y+1) (Mapa l ((t,o:os):ts)) == Arvore = Jogo (posicao (Mapa l ((t,o:os):ts)) (Jogador (x,y)) [Parado]) (mover (posicao (Mapa l ((t,o:os):ts)) (Jogador (x,y))[j]) (Mapa l ((t,o:os):ts)))
                                                          | j == Move Cima && y/=0 && encontraCoordenadas (x,y-1) (Mapa l ((t,o:os):ts)) == Arvore = Jogo (posicao (Mapa l ((t,o:os):ts)) (Jogador (x,y)) [Parado]) (mover (posicao (Mapa l ((t,o:os):ts)) (Jogador (x,y))[j]) (Mapa l ((t,o:os):ts)))
                                                          | encontraCoordenadas (x,y) (Mapa l ((t,o:os):ts)) == Tronco && j == Move Direita = Jogo (posicao (mover (Jogador (x,y)) (Mapa l ((t,o:os):ts))) (posicao (Mapa l ((t,o:os):ts)) (Jogador (x,y)) [Parado]) [j]) (mover (Jogador (x,y)) (Mapa l ((t,o:os):ts))) 
                                                          | encontraCoordenadas (x,y) (Mapa l ((t,o:os):ts)) == Tronco && j == Move Esquerda = Jogo (posicao (mover (Jogador (x,y)) (Mapa l ((t,o:os):ts))) (posicao (Mapa l ((t,o:os):ts)) (Jogador (x,y)) [Parado]) [j]) (mover (Jogador (x,y)) (Mapa l ((t,o:os):ts))) 
                                                          | otherwise = Jogo (posicao (Mapa l ((t,o:os):ts)) (Jogador (x,y)) [j]) (mover (posicao (Mapa l ((t,o:os):ts)) (Jogador (x,y))[j]) (Mapa l ((t,o:os):ts)))
{- |A função 'mover' faz com que cada obstáculo da lista de obstáculos de cada terreno vá mudando de posição de acordo com a velocidade desse mesmo terreno.
-}
mover:: Jogador -- ^ A função recebe a posição em que o jogador se encontra inicialmente.
            -> Mapa -- ^ A função recebe um tipo de mapa válido.
                -> Mapa -- ^ A função retorna o mesmo mapa mas com os obstáculos de cada terreno nas suas novas posições, uma vez que é a velociade do terreno que faz com que essas posições variem.
mover (Jogador (x,y)) (Mapa n l) = Mapa n (mover2 (Jogador (x,y)) l)
{- |A função 'mover2' tem exatamente o mesmo objetivo do que a função "mover" (sendo portanto uma função auxiliar da mesma), com a particularidade de que não é necessário escrever a palavra Mapa e nem a sua largura.
-}
mover2:: Jogador -- ^ A função recebe a posição em que o jogador se encontra inicialmente.
            ->[(Terreno, [Obstaculo])] -- ^ A função recebe uma lista de pares, cujo primeiro elemento de cada par é um tipo de terreno e o segundo elemento de cada par é a lista de obstáculos desse mesmo terreno.
                -> [(Terreno, [Obstaculo])] -- ^ A função retorna a mesma lista de pares, mas com os obstáculos de cada terreno nas suas novas posições, uma vez que é a velocidade do terreno que faz com que essas posições variem.
mover2 (Jogador (x,y)) [] = []
mover2 (Jogador (x,y)) ((Relva,o):ts) = (Relva,o): mover2 (Jogador (x,y)) ts 
mover2 (Jogador (x,y)) ((Rio v1, o):ts) = mover3 0 (Jogador (x,y)) ((Rio v1, o):ts)
mover2 (Jogador (x,y)) ((Estrada v1, o):ts) = mover3 0 (Jogador (x,y)) ((Estrada v1, o):ts)
{- |A função 'mover3' tem exatamente o mesmo objetivo do que a função "mover2", mas é uma função auxiliar da mesma.
-}
mover3:: Int -- ^ Escreve-se o número 0 que irá contabilizar o número de vezes que a lista de obstáculos se move de cada vez até que este número seja igual ao da velocidade do terreno.
            ->Jogador -- ^ A função recebe a posição em que o jogador se encontra inicialmente.
                ->[(Terreno, [Obstaculo])] -- ^ A função recebe uma lista de pares, cujo primeiro elemento de cada par é um tipo de terreno e o segundo elemento de cada par é a lista de obstáculos desse mesmo terreno.
                    ->[(Terreno, [Obstaculo])] -- ^ A função retorna a mesma lista de pares, mas com os obstáculos de cada terreno nas suas novas posições, uma vez que é a velocidade do terreno que faz com que essas posições variem.
mover3 k (Jogador (x,y)) [] = []
mover3 k (Jogador (x,y)) ((Relva, o):ts) | y==0 = (Relva, o): mover4 0 ts 
                                         | otherwise = (Relva, o):mover3 0 (Jogador (x,y-1)) ts 
mover3 k (Jogador (x,y)) ((Estrada v1, o):ts) | x < 0 || x > (length o -1) || y < 0 || y > length  ((Estrada v1, o):ts) -1 = mover4 k ((Estrada v1, o):ts)
mover3 k (Jogador (x,y)) ((Estrada v1, o):ts) | k==abs v1 = (Estrada v1, o): mover3 0 (Jogador (x,y-1)) ts
                                              | encontraCoordenadas2 (x,y) ((Estrada v1, o):ts)==Carro = (Estrada v1, o): mover4 0 ts
                                              | v1 < 0 = mover3 (k+1) (Jogador (x,y)) ((Estrada v1, drop 1 (o ++ take 1 o)) : ts)
                                              | v1 > 0 = mover3 (k+1) (Jogador (x,y)) ((Estrada v1, reverse (drop 1 (reverse o ++ take 1 (reverse o)))) : ts)
                                              | y==0 = (Estrada v1, o): mover4 0 ts 
mover3 k (Jogador (x,y)) ((Rio v1, o):ts) | y==0 = mover4 0 ((Rio v1, o):ts) 
                                          | k==abs v1 = (Rio v1, o): mover3 0 (Jogador (x,y-1)) ts                             
                                          | v1 < 0 = mover3 (k+1) (Jogador (x,y)) ((Rio v1, drop 1 (o ++ take 1 o)) : ts)
                                          | v1 > 0 = mover3 (k+1) (Jogador (x,y)) ((Rio v1, reverse (drop 1 (reverse o ++ take 1 (reverse o)))) : ts) 
{- |A função 'mover4' é uma função auxiliar da "mover3".
-}
mover4:: Int -- ^ Escreve-se o número 0 que irá contabilizar o número de vezes que a lista de obstáculos se move de cada vez até que este número seja igual ao da velocidade do terreno.
            ->[(Terreno, [Obstaculo])] -- ^ A função recebe uma lista de pares, cujo primeiro elemento de cada par é um tipo de terreno e o segundo elemento de cada par é a lista de obstáculos desse mesmo terreno.
                ->[(Terreno, [Obstaculo])] -- ^ A função retorna a mesma lista de pares, mas com os obstáculos de cada terreno nas suas novas posições, uma vez que é a velocidade do terreno que faz com que essas posições variem.
mover4 k [] = []
mover4 k ((Estrada v1, o):ts) | k==abs v1 = (Estrada v1, o): mover4 0 ts
                              | v1 < 0 = mover4 (k+1) ((Estrada v1, drop 1 (o ++ take 1 o)) : ts)
                              | v1 > 0 = mover4 (k+1) ((Estrada v1, reverse (drop 1 (reverse o ++ take 1 (reverse o)))) : ts)
mover4 k ((Relva, o):ts) = (Relva, o): mover4 0 ts 
mover4 k ((Estrada v1, o):ts) | k==abs v1 = (Estrada v1, o): mover4 0 ts
                              | v1 < 0 = mover4 (k+1) ((Estrada v1, drop 1 (o ++ take 1 o)) : ts)
                              | v1 > 0 = mover4 (k+1) ((Estrada v1, reverse (drop 1 (reverse o ++ take 1 (reverse o)))) : ts)
mover4 k ((Rio v1, o):ts) | k==abs v1 = (Rio v1, o): mover4 0 ts                              
                          | v1 < 0 = mover4 (k+1) ((Rio v1, drop 1 (o ++ take 1 o)) : ts)
                          | v1 > 0 = mover4 (k+1) ((Rio v1, reverse (drop 1 (reverse o ++ take 1 (reverse o)))) : ts) 
{- |A função 'posicao' serve para indicar qual será a nova posição do jogador após algumas jogadas.
-}
posicao:: Mapa -- ^ A função recebe um mapa válido.
            -> Jogador -- ^ A função recebe a posição em que o jogador se encontra inicialmente.
                -> [Jogada] -- ^ E recebe também um conjunto de jogadas que o jogador descreverá.
                    -> Jogador -- ^ A função retornará a posição onde o jogador se encontra após ter descrito determinadas jogadas.
posicao (Mapa m l) (Jogador (x,y)) [] = Jogador (x,y)
posicao (Mapa m l) (Jogador (x,y)) _ | x<0 || x>m-1 = Jogador (x,y)
posicao (Mapa m l) (Jogador (x,y)) (Parado:ts) | encontraCoordenadas (x,y) (Mapa m l) == Tronco = posicao (Mapa m l) (segueTronco (Mapa m l) (Jogador (x,y)) Parado) ts 
                                               | otherwise = posicao (Mapa m l) (Jogador (x,y)) ts
posicao (Mapa m l) (Jogador (x,y)) (Move Cima:ts) | y==0 || encontraCoordenadas (x,y-1) (Mapa m l) == Arvore = posicao (Mapa m l) (Jogador (x,y)) ts 
                                                  | otherwise = posicao (Mapa m l) (Jogador (x,y-1)) ts 
posicao (Mapa m l) (Jogador (x,y)) (Move Baixo:ts) | y==length l-1 || encontraCoordenadas (x,y+1) (Mapa m l) == Arvore = posicao (Mapa m l) (Jogador (x,y)) ts 
                                                   | otherwise = posicao (Mapa m l) (Jogador (x,y+1)) ts 
posicao (Mapa m l) (Jogador (x,y)) (Move Direita:ts) | x==m-1 && encontraCoordenadas (x,y) (Mapa m l) /= Tronco = posicao (Mapa m l) (Jogador (x,y)) ts 
                                                     | x==m-1 = Jogador (m,y)
                                                     | encontraCoordenadas (x+1,y) (Mapa m l) == Arvore = posicao (Mapa m l) (Jogador (x,y)) ts 
                                                     | otherwise = posicao (Mapa m l) (Jogador (x+1,y)) ts  
posicao (Mapa m l) (Jogador (x,y)) (Move Esquerda:ts) | x==0 && encontraCoordenadas (x,y) (Mapa m l) /= Tronco = posicao (Mapa m l) (Jogador (x,y)) ts 
                                                      | x==0 = Jogador (-1,y)
                                                      | encontraCoordenadas (x-1,y) (Mapa m l) == Arvore = posicao (Mapa m l) (Jogador (x,y)) ts 
                                                      | otherwise = posicao (Mapa m l) (Jogador (x-1,y)) ts 
{- |A função 'segueTronco' serve para dizer as posições em que o jogador se vai situar caso este esteja parado em cima de um tronco.
-}
segueTronco:: Mapa -- ^ A função recebe um mapa válido. 
                -> Jogador -- ^ Recebe também as coordenadas de onde o jogador se situa inicialmente (salientando que esta função apenas será chamada caso o jogador se situe em cima de um tronco).
                    -> Jogada -- ^ E a jogada efetuada pelo jogador, que neste caso como ele está parado em cima do tronco, a jogada será "Parado".
                        -> Jogador -- ^ A função retornará as coordenadas de onde o jogador se encontra após ser arrastado pelo tronco.
segueTronco m (Jogador (x,y)) Parado | encontraCoordenadas (x,y) m == Tronco = Jogador (x+getvelocidade(getTerreno(encontrax y (getMapa m))),y)
                                     | otherwise = Jogador (x,y)
{- |A função 'encontrax' devolve um elemento de uma lista que se encontra na posição escolhida.
-}
encontrax :: Int -- ^ A função recebe um número que irá corresponder à posição de um elemento na lista.
                -> [a] -- ^ E recebe uma lista.
                    -> a -- ^ E devolve o elemento dessa lista que se situa na posição na posição inserida
encontrax 0 (x:xs) = x
encontrax n (x:xs) = encontrax (n-1) xs
{- |A função 'getListaObs' serve para referir a lista de obstáculos presente numa linha do mapa.
-}
getListaObs :: (Terreno, [Obstaculo]) -- ^ A função recebe uma linha do mapa, ou seja, um par onde o primeiro elemento é o tipo de terreno e o segundo elemento é a lista de obstáculos presentes nesse terreno.
                    -> [Obstaculo] -- ^ A função retorna a lista de obstáculos do par que recebeu inicialmente.
getListaObs (t, l) = l
{- |A função 'getTerreno' serve para referir o tipo de terreno presente numa linha do mapa.
-}
getTerreno :: (Terreno, [Obstaculo]) -- ^ A função recebe uma linha do mapa, ou seja, um par onde o primeiro elemento é o tipo de terreno e o segundo elemento é a lista de obstáculos presentes nesse terreno.
                    -> Terreno -- ^ A função retorna o tipo de terreno do par que recebeu inicialmente.
getTerreno (t, l) = t
{- |A função 'getMapa' serve para referir as linhas presentes num mapa.
-}
getMapa :: Mapa -- ^ A função recebe um tipo de mapa válido.
                -> [(Terreno, [Obstaculo])] -- ^ E devolve todas as linhas desse mesmo mapa.
getMapa (Mapa largura l) = l
{- |A função 'encontraCoordenadas' serve para indicar qual o obstáculo presente nas coordenadas especificadas.
-}
encontraCoordenadas:: Coordenadas -- ^ A função recebe um par de coordenadas.
                            -> Mapa -- ^ E recebe também um mapa válido.
                                -> Obstaculo -- ^ A função devolve qual o obstáculo do mapa que se encontra nas coordenadas que a função recebeu.
encontraCoordenadas (x,y) l = encontrax x (getListaObs(encontrax y (getMapa l)))
{- |A função 'encontraCoordenadas2' desempenha extamente o mesmo papel que a função "encontraCoordenadas" só que em vez de receber um mapa recebe as linhas de um mapa.
-}
encontraCoordenadas2:: Coordenadas -- ^ A função recebe um par de coordenadas.
                            -> [(Terreno, [Obstaculo])] -- ^ E recebe também as linhas de um mapa válido.
                                -> Obstaculo -- ^ A função devolve qual o obstáculo do mapa que se encontra nas coordenadas que a função recebeu.
encontraCoordenadas2 (x,y) l = encontrax x (getListaObs(encontrax y l))
{- |A função 'getvelocidade' serve para indicar qual a velocidade correspondente ao tipo de terreno.
-}
getvelocidade :: Terreno -- ^ A função recebe um tipo de terreno com a sua velocidade correspondente. 
                    -> Int -- ^ A função retorna a velocidade correspondente ao terreno que ela recebeu.
getvelocidade Relva = 0
getvelocidade (Rio v1) = v1
getvelocidade (Estrada v1) = v1
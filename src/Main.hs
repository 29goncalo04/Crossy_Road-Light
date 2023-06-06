{- |
Module      : Tarefa6_2022li1g008
Description : Aplicação gráfica completa
Copyright   : Gonçalo Oliveira Cruz <a104346@alunos.uminho.pt>
              Gonçalo Costa Magalhães <a104538@alunos.uminho.pt>

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2022/23.
-}
module Tarefa6_2022li1g008 where
import System.Random
import Data.List
import LI12223
import Tarefa1_2022li1g008
import Tarefa2_2022li1g008
import Tarefa3_2022li1g008
import Tarefa4_2022li1g008
import Tarefa5_2022li1g008
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Opcao = Jogar
            | NovoJogo
            | Pausa
            | Sair 
            | Abandonar

data Menu = MenuInicial Opcao
          | ModoJogo 
          | ModoBot
          | ModoBotJogo
          | PerdeuJogo            

type World = (Menu, Jogo, Imagens, Tempo, TempoLinha, Pontuação)
type TempoLinha = Float
type Tempo = Float
type Imagens = [Picture]
type Pontuação = Int
{- |A função 'jogoInicial' representa qual é o mapa inicial e quais as coordenadas iniciais do jogador.
-}
jogoInicial :: Jogo -- ^ A função devolve um Jogo.
jogoInicial = Jogo (Jogador (6,6)) (Mapa 13 [ (Estrada (-1), [Nenhum,Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Carro,Carro]),
                                              (Estrada 2, [Nenhum,Carro,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Carro]),
                                              (Relva, [Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore]),
                                              (Rio 2, [Tronco,Tronco,Nenhum,Tronco,Tronco,Nenhum,Tronco,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum]),
                                              (Rio (-1), [Nenhum,Tronco,Nenhum,Tronco,Tronco,Nenhum,Tronco,Nenhum,Tronco,Tronco,Nenhum,Tronco,Nenhum]),
                                              (Estrada 1, [Carro,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Carro,Nenhum]),
                                              (Relva, [Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum])])
{- |A função 'window' é simplesmente o tipo de janela onde o jogo será executado.
-}
window :: Display
window = FullScreen
{- |A função 'fr' corresponde ao frame rate.
-}
fr :: Int
fr = 120
{- |A função 'drawState' converte o estado atual do jogo num valor do tipo Picture.
-}
drawState :: World -- ^ A função utiliza um valor do tipo World para saber qual o estado atual do jogo. 
                 -> Picture -- ^ E retorna um valor do tipo Picture consoante o estado do jogo.
drawState (PerdeuJogo, Jogo (Jogador (x,y)) mapa, i, t, tl, k) = Pictures [Translate (-225) (-50) (Color red (Text "Perdeu")), Color yellow (Translate (-150) (-200) (Scale 0.3 0.3 (Text ("Total points: " ++ show k)))) , Color black (Translate 500 510 (Scale 0.17 0.17 (Text "Pressione a tecla Enter para continuar")))]
drawState (MenuInicial Jogar, Jogo (Jogador (x,y)) mapa, i, t, tl, k) = Pictures [Translate (-200) 170 (drawOption "Assistir Bot"), Color white (Translate (-40) 0 (drawOption "Jogar")), Translate 0 (-200) (drawOption "Sair"), Color black (Translate 500 510 (Scale 0.17 0.17 (Text "Pressione a tecla Enter para selecionar"))), Color black (Translate (-950) (-530) (Scale 0.17 0.17 (Text "Pode colocar o jogo em pausa premindo a tecla Space")))]
drawState (MenuInicial NovoJogo, Jogo (Jogador (x,y)) mapa, i, t, tl, k) = Pictures [Color white (Translate (-190) 200 (drawOption "Menu Inicial")), Translate (-130) 0 (drawOption "Continuar"), Translate 30 (-200) (drawOption "Sair"), Color black (Translate 500 510 (Scale 0.17 0.17 (Text "Pressione a tecla Enter para selecionar")))]
drawState (MenuInicial Pausa, Jogo (Jogador (x,y)) mapa, i, t, tl, k) = Pictures [Translate (-190) 200 (drawOption "Menu Inicial"), Color white (Translate (-130) 0 (drawOption "Continuar")), Translate 30 (-200) (drawOption "Sair"), Color black (Translate 500 510 (Scale 0.17 0.17 (Text "Pressione a tecla Enter para selecionar")))]
drawState (MenuInicial Abandonar, Jogo (Jogador (x,y)) mapa, i, t, tl, k) = Pictures [Translate (-190) 200 (drawOption "Menu Inicial"), Translate (-130) 0 (drawOption "Continuar"), Color white (Translate 30 (-200) (drawOption "Sair")), Color black (Translate 500 510 (Scale 0.17 0.17 (Text "Pressione a tecla Enter para selecionar")))]
drawState (MenuInicial Sair, Jogo (Jogador (x,y)) mapa, i, t, tl, k) = Pictures [Translate (-200) 170 (drawOption "Assistir Bot"), Translate (-40) 0 (drawOption "Jogar"), Color white (Translate 0 (-200) (drawOption "Sair")), Color black (Translate 500 510 (Scale 0.17 0.17 (Text "Pressione a tecla Enter para selecionar"))), Color black (Translate (-950) (-530) (Scale 0.17 0.17 (Text "Pode colocar o jogo em pausa premindo a tecla Space")))]
drawState (ModoBot, Jogo (Jogador (x,y)) mapa, i, t, tl, k) = Pictures [Color white (Translate (-200) 170 (drawOption "Assistir Bot")), Translate (-40) 0 (drawOption "Jogar"), Translate 0 (-200) (drawOption "Sair"), Color black (Translate 500 510 (Scale 0.17 0.17 (Text "Pressione a tecla Enter para selecionar"))), Color black (Translate (-950) (-530) (Scale 0.17 0.17 (Text "Pode colocar o jogo em pausa premindo a tecla Space")))]
drawState (ModoJogo, Jogo (Jogador (x,y)) mapa, i, t, tl, k) = translate (-900) 450 (Pictures (desenhaMapa mapa (0,0) i ++ [translate (fromIntegral x*150) (fromIntegral y*(-150)) (last i)] ++ [Color red (Translate 1520 40 (Scale 0.3 0.3 (Text ("Total points: " ++ show k))))]))
drawState (ModoBotJogo, Jogo (Jogador (x,y)) mapa, i, t, tl, k) = translate (-900) 450  (Pictures (desenhaMapa mapa (0,0) i ++ [translate (fromIntegral x*150) (fromIntegral y*(-150)) (last i)] ++ [Color red (Translate 1520 40 (Scale 0.3 0.3 (Text ("Total points: " ++ show k))))])) 
{- |A função 'drawOption' é uma função auxiliar da "drawState" que serve para ajudar a posicionar a "String" indicada.
-}
drawOption :: String -- ^ A função recebe uma string.
                 -> Picture -- ^ E depois transforma-a num valor do tipo Picture.
drawOption option = Translate (-150) 0 (Scale 1 1 (Text option))        
{- |A função 'desenhaMapa' tem como objetivo transformar o mapa do jogo num valor do tipo Picture.
-}
desenhaMapa :: Mapa -- ^ A função recebe um mapa.
                    -> (Float,Float) -- ^ Recebe também um par de Floats que inicialmente será (0,0) e que serve para a função saber em que coordenadas irá desenhar cada obstáculo. 
                        -> Imagens -- ^ E recebe ainda o conjunto de todas as imagens possíveis de serem utilizadas.
                            -> [Picture] -- ^ A função retorna uma lista de Pictures.
desenhaMapa (Mapa _ []) _ _= []                                 
desenhaMapa (Mapa l (h:t)) (a,b) imagens = desenhaLinha h (a,b) imagens ++ desenhaMapa (Mapa l t) (a,b-150) imagens
{- |A função 'desenhaLinha' é uma função auxiliar da "desenhaMapa" e tem como objetivo transformar uma linha do mapa do jogo num valor do tipo Picture.
-}
desenhaLinha :: (Terreno,[Obstaculo]) -- ^ A função recebe uma linha do mapa. 
                    -> (Float,Float) -- ^ Recebe também um par de Floats que inicialmente será (0,0) e que serve para a função saber em que coordenadas irá desenhar cada obstáculo. 
                        -> Imagens -- ^ E recebe ainda o conjunto de todas as imagens possíveis de serem utilizadas.
                            -> [Picture] -- ^ A função retorna uma lista de Pictures.
desenhaLinha (terreno,[]) (a,b) imagens = []
desenhaLinha (terreno,h:t) (a,b) imagens = desenhaObsTerr (terreno, h) (a,b) imagens : desenhaLinha (terreno, t) (a+150,b) imagens
{- |A função 'desenhaObsTerr' é uma função auxiliar da "desenhaLinha" e tem como objetivo transformar os obstáculos e os terrenos do jogo num valor do tipo Picture.
-}
desenhaObsTerr :: (Terreno,Obstaculo) -- ^ A função recebe o tipo de terreno com o respetivo obstáculo a desenhar (se este for "Nenhum", a função apenas irá desenhar o tipo de terreno).
                      -> (Float,Float) -- ^ Recebe também um par de Floats que inicialmente será (0,0) e que serve para a função saber em que coordenadas irá desenhar cada obstáculo. 
                         -> Imagens -- ^ E recebe ainda o conjunto de todas as imagens possíveis de serem utilizadas.
                           -> Picture -- ^ A função retorna os obstáculos e os terrenos sobre a forma do tipo Picture.
desenhaObsTerr (_, Arvore) (a,b) [carro,relva,rio,tronco,estrada,arvore,jogador] = Pictures [translate a b relva, translate a b arvore]    
desenhaObsTerr (Relva, Nenhum) (a,b) [carro,relva,rio,tronco,estrada,arvore,jogador] = translate a b relva
desenhaObsTerr (_, Carro) (a,b) [carro,relva,rio,tronco,estrada,arvore,jogador] = Pictures  [translate a b estrada, translate a b carro]
desenhaObsTerr (Estrada x, Nenhum) (a,b) [carro,relva,rio,tronco,estrada,arvore,jogador] = translate a b estrada                                       
desenhaObsTerr (_, Tronco) (a,b) [carro,relva,rio,tronco,estrada,arvore,jogador] = Pictures [translate a b rio ,translate a b tronco]
desenhaObsTerr (Rio x, Nenhum) (a,b) [carro,relva,rio,tronco,estrada,arvore,jogador] = translate a b rio  
{- |A função 'event' calcula o próximo estado após reagir a um evento.
-}
event :: Event -- ^ A função recebe um evento (neste caso quando teclas em específico são pressionadas).
            -> World -- ^ E recebe um valor do tipo World para saber qual o estado atual do jogo.
                -> World -- ^ A função retorna o novo estado do jogo através do tipo World.
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuInicial Sair, jogo, i, t, tl, k) = (MenuInicial Jogar, jogo, i, t, tl, k)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuInicial Sair, jogo, i, t, tl, k) = (ModoBot, jogo, i, t, tl, k)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial Sair, jogo, i, t, tl, k) = error "Fim de Jogo"
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial Jogar, jogo, i, t, tl, k) = (ModoJogo, jogoInicial, i, 0, 0, 0)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo, jogo, i, t, tl, k) = (MenuInicial Jogar, jogoInicial, i, t, tl, 0)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuInicial Jogar, jogo, i, t, tl, k) = (ModoBot, jogo, i, t, tl, k)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuInicial Jogar, jogo, i, t, tl, k) = (MenuInicial Sair, jogo, i, t, tl, k)
event (EventKey (SpecialKey KeyUp) Down _ _) (ModoBot, jogo, i, t, tl, k) = (MenuInicial Sair, jogo, i, t, tl, k)
event (EventKey (SpecialKey KeyDown) Down _ _) (ModoBot, jogo, i, t, tl, k) = (MenuInicial Jogar, jogo, i, t, tl, k)
event (EventKey (SpecialKey KeyEnter) Down _ _) (ModoBot, jogo, i, t, tl, k) = (ModoBotJogo, jogo, i, t, tl, k)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuInicial Pausa, jogo, i, t, tl, k) = (MenuInicial Abandonar, jogo, i, t, tl, k)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuInicial Pausa, jogo, i, t, tl, k) = (MenuInicial NovoJogo, jogo, i, t, tl, k)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial Pausa, jogo, i, t, tl, k) = (ModoJogo, jogo, i, t, tl, k)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuInicial Abandonar, jogo, i, t, tl, k) = (MenuInicial NovoJogo, jogo, i, t, tl, k)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuInicial Abandonar, jogo, i, t, tl, k) = (MenuInicial Pausa, jogo, i, t, tl, k)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial Abandonar, jogo, i, t, tl, k) = error "Fim de Jogo"
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuInicial NovoJogo, jogo, i, t, tl, k) = (MenuInicial Pausa, jogo, i, t, tl, k)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuInicial NovoJogo, jogo, i, t, tl, k) = (MenuInicial Abandonar, jogo, i, t, tl, k)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial NovoJogo, jogo, i, t, tl, k) = (MenuInicial Jogar, jogoInicial, i, 0, 0, 0)
event (EventKey (SpecialKey KeySpace) Down _ _) (ModoJogo, jogo, i, t, tl, k) = (MenuInicial Pausa, jogo, i, t, tl, k)  
event (EventKey (SpecialKey KeyUp) Down _ _)    (ModoJogo, Jogo jogador mapa, i, t, tl, k) = (ModoJogo, Jogo (posicao mapa jogador [Move Cima]) mapa, i, t, tl, k)  
event (EventKey (SpecialKey KeyDown) Down _ _)  (ModoJogo, Jogo jogador mapa, i, t, tl, k) = (ModoJogo, Jogo (posicao mapa jogador [Move Baixo]) mapa, i, t, tl, k)  
event (EventKey (SpecialKey KeyLeft) Down _ _)  (ModoJogo, Jogo jogador mapa, i, t, tl, k) = (ModoJogo, Jogo (posicao mapa jogador [Move Esquerda]) mapa, i, t, tl, k)
event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo, Jogo jogador mapa, i, t, tl, k) = (ModoJogo, Jogo (posicao mapa jogador [Move Direita]) mapa, i, t, tl, k)
event _ w = w
{- |A função 'deslizaJogo2' faz exatamente o mesmo que a função "deslizaJogo", mas não permite que sejam geradas linhas cuja velocidade seja superior a 3 e nem relvas com mais do que 4 árvores e nem estradas com mais de 3 carros, e apenas é chamada quando a pontuação do jogador for inferior ou igual a 200, ou seja serve para que a partir desse valor a dificuldade do jogo aumente.
-}
deslizaJogo2 :: Int -- ^ A função recebe um número inteiro para gerar aleatoriamente uma nova linha.
                    -> Jogo -- ^ A função recebe um jogo (a posição do jogador e um mapa válido). 
                        -> Jogo -- ^ A função retornará um novo jogo, com as mesmas coordenadas que o jogador tinha anteriormente mas agora com uma unidade somada ao "y", e o mesmo mapa inicialmente inserido mas com uma nova linha no topo (com as novas propriedade agora definidas) e com a última linha removida.
deslizaJogo2 n (Jogo (Jogador (x,y)) (Mapa l ((t,o:os):ts))) = Jogo (Jogador (x,y+1)) (Mapa l (init (getMapa (novoMapa n (Mapa l ((t,o:os):ts))))))
{- |A função 'novoMapa' é uma função auxiliar da "deslizaJogo2" e faz exatamente o mesmo que a função "estendeMapa", mas não permite que sejam geradas linhas cuja velocidade seja superior a 3.
-}
novoMapa :: Int -- ^ A função recebe um número inteiro para gerar aleatoriamente uma nova linha.
                -> Mapa -- ^ A função recebe também um mapa válido.
                    -> Mapa -- ^ A função gera um novo mapa válido com a nova linha no topo (cuja velocidade tem de ser inferior a 3).
novoMapa n (Mapa l ((terreno,o):t)) | length (elemIndices Arvore (getListaObs (head (getMapa (estendeMapa (Mapa l ((terreno,o):t)) n))))) >4 = novoMapa (n+21) (Mapa l ((terreno,o):t))
                                    | getvelocidade2 (fst (head (getMapa (estendeMapa (Mapa l ((terreno,o):t)) n)))) >3 || length (elemIndices Carro (getListaObs (head (getMapa (estendeMapa (Mapa l ((terreno,o):t)) n))))) >3  =  novoMapa (n+21) (Mapa l ((terreno,o):t))
                                    | otherwise = estendeMapa (Mapa l ((terreno,o):t)) n 
{- |A função 'deslizaJogo3' faz exatamente o mesmo que a função "deslizaJogo", mas não permite que sejam geradas linhas cuja velocidade seja superior a 6 e nem relvas com mais do que 4 árvores e nem estradas com mais de 5 carros, e apenas é chamada quando a pontuação do jogador for superior a 200, ou seja serve para que a partir desse valor a dificuldade do jogo aumente.
-}
deslizaJogo3 :: Int -- ^ A função recebe um número inteiro para gerar aleatoriamente uma nova linha.
                    -> Jogo -- ^ A função recebe um jogo (a posição do jogador e um mapa válido). 
                        -> Jogo -- ^ A função retornará um novo jogo, com as mesmas coordenadas que o jogador tinha anteriormente mas agora com uma unidade somada ao "y", e o mesmo mapa inicialmente inserido mas com uma nova linha no topo (com as novas propriedade agora definidas) e com a última linha removida.
deslizaJogo3 n (Jogo (Jogador (x,y)) (Mapa l ((t,o:os):ts))) = Jogo (Jogador (x,y+1)) (Mapa l (init (getMapa (novoMapa2 n (Mapa l ((t,o:os):ts))))))
{- |A função 'novoMapa2' é uma função auxiliar da "deslizaJogo2" e faz exatamente o mesmo que a função "estendeMapa", mas não permite que sejam geradas linhas cuja velocidade seja superior a 3.
-}
novoMapa2 :: Int -- ^ A função recebe um número inteiro para gerar aleatoriamente uma nova linha.
                -> Mapa -- ^ A função recebe também um mapa válido.
                    -> Mapa -- ^ A função gera um novo mapa válido com a nova linha no topo (cuja velocidade tem de ser inferior a 6).
novoMapa2 n (Mapa l ((terreno,o):t)) | length (elemIndices Arvore (getListaObs (head (getMapa (estendeMapa (Mapa l ((terreno,o):t)) n))))) >4 = novoMapa2 (n+21) (Mapa l ((terreno,o):t))
                                     | getvelocidade2 (fst (head (getMapa (estendeMapa (Mapa l ((terreno,o):t)) n)))) >6 || length (elemIndices Carro (getListaObs (head (getMapa (estendeMapa (Mapa l ((terreno,o):t)) n))))) >5  =  novoMapa2 (n+21) (Mapa l ((terreno,o):t))
                                     | otherwise = estendeMapa (Mapa l ((terreno,o):t)) n                                     
{- |A função 'getvelocidade2' é uma função auxiliar da "novoMapa" e faz exatamente o mesmo que a função "getvelocidade".
-}
getvelocidade2 :: Terreno -- ^ A função recebe um tipo de terreno com a sua velocidade correspondente. 
                    -> Int -- ^ A função retorna a velocidade correspondente ao terreno que ela recebeu.
getvelocidade2 Relva = 0
getvelocidade2 (Rio v1) = v1
getvelocidade2 (Estrada v1) = v1
{- |A função 'reageTempoGloss' altera o estado do jogo em funçao do tempo.
-}
reageTempoGloss :: Float -- ^ A função recebe um número que corresponde à quantidade de tempo que passa.
                        -> World -- ^ E recebe um valor do tipo World para saber qual o estado atual do jogo.
                             -> World -- ^ A função retorna o novo estado do jogo através do tipo World.
reageTempoGloss n (ModoBotJogo, Jogo (Jogador (x,y)) (Mapa l o), i, t, tl, k) | jogoTerminou (Jogo (Jogador (x,y)) (Mapa l o)) = (PerdeuJogo, Jogo (Jogador (x,y)) (Mapa l o), i, 0, 0, k)
                                                                              | t==5.5 = (ModoBotJogo, deslizaJogo (x*13+y) (Jogo (Jogador (x,y)) (Mapa l o)) , i, 0, tl, k)
                                                                              | tl==2.5 = (ModoBotJogo, animaJogo (Jogo (Jogador (x,y)) (Mapa l o)) (Move Cima), i, t, 0, k+5)
                                                                              | otherwise = (ModoBotJogo, Jogo (Jogador (x,y)) (Mapa l o), i, fromIntegral (round ((t + n) * 100)) / 100, fromIntegral (round ((tl + n) * 100)) / 100, k)
reageTempoGloss n (ModoJogo, Jogo (Jogador (x,y)) (Mapa l o), i, t, tl, k) | jogoTerminou (Jogo (Jogador (x,y)) (Mapa l o)) = (PerdeuJogo, Jogo (Jogador (x,y)) (Mapa l o), i, 0, 0, k)
                                                                           | t==5.5 && k>200 = (ModoJogo, deslizaJogo3 (x*3+y) (Jogo (Jogador (x,y)) (Mapa l o)) , i, 0, tl, k)
                                                                           | t==5.5 && k<=200 = (ModoJogo, deslizaJogo2 (x*3+y) (Jogo (Jogador (x,y)) (Mapa l o)) , i, 0, tl, k)
                                                                           | tl==2.5 = (ModoJogo, animaJogo (Jogo (Jogador (x,y)) (Mapa l o)) Parado, i, t, 0, k+5)
                                                                           | otherwise = (ModoJogo, Jogo (Jogador (x,y)) (Mapa l o), i, fromIntegral (round ((t + n) * 100)) / 100, fromIntegral (round ((tl + n) * 100)) / 100, k)
reageTempoGloss n a = a                                                                                                                                                          
{- |A função 'main' é a função principal.
-}
main :: IO ()
main = do
    jogador <- loadBMP "jogador.bmp"
    arvore  <- loadBMP "arvore.bmp"
    carro   <- loadBMP "carro.bmp"
    tronco  <- loadBMP "tronco.bmp" 
    relva   <- loadBMP "relva.bmp"
    estrada <- loadBMP "estrada.bmp"
    rio     <- loadBMP "rio.bmp"
    play 
        window 
        blue
        fr 
        (MenuInicial Jogar, jogoInicial, [carro,relva,rio,tronco,estrada,arvore,jogador], 0, 0, 0)  
        drawState 
        event 
        reageTempoGloss
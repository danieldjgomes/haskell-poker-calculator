module JogadasPoker (
    jogadorTemPar,
    jogadorTemTrinca,
    jogadorTemQuadra,
    jogadorTemFlush,
    geraSequencia,
    jogadorTemStraight,
    jogadorTemStraightFlush,
    jogadorTemParExato
)  where

import Carta

jogadorTemPar :: (Carta, Carta) -> [Carta] -> Bool
jogadorTemPar mao mesa = temParMao mao || temParComAMesa mao mesa
  where
      temParComAMesa m (c:cs) = mesmoNumero (fst m) c || mesmoNumero (snd m) c || temParComAMesa m cs
      temParComAMesa _ [] = False

jogadorTemParExato :: (Carta, Carta) -> [Carta] -> Double
jogadorTemParExato mao mesa 
    | temParMao mao || temParComAMesa mao mesa = 1
    | length mesa >= 5 = 0
    | otherwise = 1 - fromIntegral (product [40..(44 - length mesa)]) / fromIntegral (product [46..(50 - length mesa)])
  where
      temParComAMesa m (c:cs) = mesmoNumero (fst m) c || mesmoNumero (snd m) c || temParComAMesa m cs
      temParComAMesa _ [] = False

jogadorTemTrinca :: (Carta, Carta) -> [Carta] -> Bool
jogadorTemTrinca mao mesa = (temParMao mao && temTrincaComAMesa (fst mao) mesa) || temTrincaComUmaNaMaoEMesa (fst mao) mesa || temTrincaComUmaNaMaoEMesa (snd mao) mesa
    where 
        temTrincaComAMesa cartaMao (c:cs) = mesmoNumero cartaMao c || temTrincaComAMesa cartaMao cs
        temTrincaComAMesa _ [] = False
        temTrincaComUmaNaMaoEMesa cartaMao cs = length [c | c <- cs, mesmoNumero cartaMao c] == 2

jogadorTemQuadra :: (Carta, Carta) -> [Carta] -> Bool
jogadorTemQuadra mao mesa = (temParMao mao && temQuadraComAMesa (fst mao) mesa) || temQuadraComUmaNaMaoEMesa (fst mao) mesa || temQuadraComUmaNaMaoEMesa (snd mao) mesa
    where 
        temQuadraComAMesa cartaMao cs = length [c | c <- cs, mesmoNumero cartaMao c] == 2
        temQuadraComUmaNaMaoEMesa cartaMao cs = length [c | c <- cs, mesmoNumero cartaMao c] == 3

jogadorTemFlush :: (Carta, Carta) -> [Carta] -> Bool 
jogadorTemFlush mao mesa = (temMesmoNaipeMao mao && temFlushComDuasNaMao (fst mao) mesa) || temFlushComUmaNaMao (fst mao) mesa || temFlushComUmaNaMao (snd mao) mesa
    where 
        temFlushComDuasNaMao cartaMao cs = length [c | c <- cs, mesmoNaipe cartaMao c] >= 3
        temFlushComUmaNaMao cartaMao cs = length [c | c <- cs, mesmoNaipe cartaMao c] >= 4
        
jogadorTemStraight :: (Carta, Carta) -> [Carta] -> Bool 
jogadorTemStraight mao mesa = not (null [s | s <- geraSequencia (ordena ([fst mao] ++ [snd mao] ++ mesa)), temStraightSequencia s])

geraSequencia :: [Carta] -> [[Carta]]
geraSequencia n = [geraTodasSequencias n i | i <- [0..(length n-1)]]
geraTodasSequencias :: [Carta] -> Int -> [Carta]

geraTodasSequencias n i 
    | length n < 5 = error "Lista muito curta, sao necessarias pelo menos 5 cartas"
    | length [i..ultimo] < 5 =  take 5 (drop i n) ++ take (5-length [i..ultimo]) n
    | otherwise = take 5 (drop i n)
        where ultimo = length n -1

temStraightSequencia :: [Carta] -> Bool
temStraightSequencia (x:y:xs) 
    | y `ehSequencia` x = temStraightSequencia (y:xs)
    | otherwise = False
temStraightSequencia [_] = True
temStraightSequencia [] = False

jogadorTemStraightFlush :: (Carta, Carta) -> [Carta] -> Bool 
jogadorTemStraightFlush mao mesa = jogadorTemFlush mao mesa && jogadorTemStraight mao mesa

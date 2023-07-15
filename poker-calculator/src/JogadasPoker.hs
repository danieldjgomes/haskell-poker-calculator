module JogadasPoker (
    jogadorTemPar,
    jogadorTemTrinca,
    jogadorTemQuadra,
    jogadorTemFlush
)  where
import Carta

jogadorTemPar :: (Carta, Carta) -> [Carta] -> Bool
jogadorTemPar mao mesa = temParMao mao || temParComAMesa mao mesa
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
        temFlushComDuasNaMao cartaMao cs = length [c | c <- cs, mesmoNaipe cartaMao c] == 3
        temFlushComUmaNaMao cartaMao cs = length [c | c <- cs, mesmoNaipe cartaMao c] == 4
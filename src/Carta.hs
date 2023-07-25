module Carta (
  Carta(..), -- Exportar o tipo Carta e seu construtor
  Naipe(..), -- Exportar o tipo Naipe e seus construtores
  Numero(..), -- Exportar o tipo Numero e seus construtores
  mesmoNaipe,
  mesmoNumero,
  criarCarta,
  temParMao,
  temMesmoNaipeMao,
  ordena,
  numeroCarta,
  ehSequencia,
  criarTodasCartas,
  pegaProbabilidade,
  -- simulaJogada,
  comprarNCartas,
  comprarCarta,
  removeCartasCompradas
) where


import Data.List (sortBy,transpose)
import Data.Function (on)
import System.Random ( randomRIO )
import System.IO.Unsafe (unsafePerformIO)

data Naipe = Copas | Ouros | Paus | Espadas deriving (Eq)

data Numero = A | K | Q | J | T | Nove | Oito | Sete | Seis | Cinco | Quatro | Tres | Dois deriving (Eq)

data Carta = Carta Numero Naipe deriving (Eq)

mesmoNaipe :: Carta -> Carta -> Bool
mesmoNaipe (Carta _ naipe1) (Carta _ naipe2) = naipe1 == naipe2

mesmoNumero :: Carta -> Carta -> Bool
mesmoNumero (Carta numero1 _) (Carta numero2 _) = numero1 == numero2

criarCarta :: Numero -> Naipe -> Carta
criarCarta = Carta

temParMao :: (Carta, Carta) -> Bool
temParMao (c1, c2) = mesmoNumero c1 c2

temMesmoNaipeMao :: (Carta, Carta) -> Bool
temMesmoNaipeMao (c1, c2) = mesmoNaipe c1 c2

ordena :: [Carta] -> [Carta]
ordena = sortBy (comparaCarta `on` numeroCarta)

comparaCarta :: Int -> Int -> Ordering
comparaCarta a b
    | a == b = EQ
    | a == b + 1 = GT
    | otherwise = LT

ehSequencia :: Carta -> Carta -> Bool
ehSequencia (Carta Dois _) (Carta A _) = True
ehSequencia cartaA cartaB = comparaCarta (numeroCarta cartaA) (numeroCarta cartaB) == GT

numeroCarta :: Carta -> Int
numeroCarta (Carta numero _) = case numero of
    A      ->  14
    K      ->  13
    Q      ->  12
    J      ->  11
    T      ->  10
    Nove   ->  9
    Oito   ->  8
    Sete   ->  7
    Seis   ->  6
    Cinco  ->  5
    Quatro ->  4
    Tres   ->  3
    Dois   ->  2

instance Show Carta where
    show (Carta numero naipe) = showNumero numero ++ " " ++ showNaipe naipe

showNumero :: Numero -> String
showNumero A        = "A"
showNumero K        = "K"
showNumero Q        = "Q"
showNumero J        = "J"
showNumero T        = "T"
showNumero Nove     = "9"
showNumero Oito     = "8"
showNumero Sete     = "7"
showNumero Seis     = "6"
showNumero Cinco    = "5"
showNumero Quatro   = "4"
showNumero Tres     = "3"
showNumero Dois     = "2"

showNaipe :: Naipe -> String
showNaipe Copas     = "♥"
showNaipe Ouros     = "♦"
showNaipe Paus      = "♣"
showNaipe Espadas   = "♠"

criarTodasCartas :: [Carta]
criarTodasCartas = [Carta numero naipe | numero <- todosNumeros, naipe <- todosNaipes]
  where
    todosNumeros = [A, Dois, Tres, Quatro, Cinco, Seis, Sete, Oito, Nove, T, J, Q, K]
    todosNaipes = [Copas, Ouros, Paus, Espadas]

comprarCarta :: [Carta] -> Carta
comprarCarta cartasDisponiveis =
  if null cartasDisponiveis
    then error "O baralho está vazio. Impossível comprar mais cartas."
    else cartaComprada
  where
    numCartas = length cartasDisponiveis
    randomIndex = unsafePerformIO $ randomRIO (0, numCartas - 1)
    cartaComprada = cartasDisponiveis !! randomIndex

-- pegaProbabilidade :: (Carta, Carta) -> [Carta] -> ((Carta, Carta) -> [Carta] -> Bool) -> Double
-- pegaProbabilidade mao mesa jogada = fromIntegral acertos / fromIntegral numSimulacoes
--   where
--     numSimulacoes = 95000
--     acertos = simulaNJogadas mao mesa jogada numSimulacoes

-- simulaNJogadas :: (Carta,Carta) -> [Carta] ->  ((Carta,Carta) -> [Carta] -> Bool) -> Int -> Int
-- simulaNJogadas mao mesa jogada quantidade
--     | quantidade <= 0 = 0
--     | jogada mao (mesa++cartasCompradas) = 1 + simulaNJogadas mao mesa jogada (quantidade-1)
--     | otherwise = simulaNJogadas mao mesa jogada (quantidade-1)
--       where
--         cartasCompradas = comprarNCartas (removeCartasCompradas criarTodasCartas  cartasEmJogo) (7-length cartasEmJogo)
--         cartasEmJogo = [fst mao,snd mao]++mesa

pegaProbabilidade :: (Carta, Carta) -> [Carta] -> [(Carta, Carta) -> [Carta] -> Bool] -> [Double]
pegaProbabilidade mao mesa jogadas = dividePorN numSimulacoes acertosSumarizados
  where
    numSimulacoes = 500000
    acertos = simulaNJogadas mao mesa jogadas numSimulacoes
    acertosSumarizados = somarListasElemento acertos

simulaNJogadas :: (Carta,Carta) -> [Carta] ->  [(Carta,Carta) -> [Carta] -> Bool] -> Int -> [[Int]]
simulaNJogadas mao mesa jogadas quantidade
    | quantidade <= 0 = [[ 0 | _ <- [1..length jogadas]]]
    | otherwise = map (\jogada -> if jogada mao (mesa++cartasCompradas) then 1 else 0) jogadas : simulaNJogadas mao mesa jogadas (quantidade-1)
      where
        cartasCompradas = comprarNCartas (removeCartasCompradas criarTodasCartas  cartasEmJogo) (7-length cartasEmJogo)
        cartasEmJogo = [fst mao,snd mao]++mesa


removeCartasCompradas :: [Carta] -> [Carta] -> [Carta]
removeCartasCompradas baralho cartasEmJogo = filter (`notElem` cartasEmJogo) baralho

comprarNCartas :: [Carta] -> Int -> [Carta]
comprarNCartas _ 0 = []  -- Retorna uma lista vazia quando quantidade é igual a 0
comprarNCartas baralho quantidade
  | quantidade > 0 = cartaComprada : comprarNCartas (removeCartasCompradas baralho [cartaComprada]) (quantidade - 1)
  | otherwise = []  -- Corrigido para retornar uma lista vazia quando quantidade é negativa
  where
    cartaComprada = comprarCarta baralho


somarListasElemento ::  [[Int]] -> [Int]
somarListasElemento listas = [sum elementos | elementos <- transpose listas]
  where
    transpose :: [[a]] -> [[a]]
    transpose [] = repeat []
    transpose (xs:xss) = zipWith (:) xs (transpose xss)


dividePorN :: Int -> [Int] -> [Double]
dividePorN n listaInteiros = [fromIntegral x / fromIntegral n | x <- listaInteiros]
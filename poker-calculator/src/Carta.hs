module Carta (
  Carta,
  mesmoNaipe,
  mesmoNumero,
  criarCarta,
  temParMao,
  temMesmoNaipeMao
) where

data Naipe = Copas | Ouros | Paus | Espadas deriving (Eq, Show)

data Numero = A | K | Q | J | T | Nove | Oito | Sete | Seis | Cinco | Quatro | Tres | Dois  deriving (Eq, Show)

data Carta = Carta Numero Naipe deriving (Eq, Show)

mesmoNaipe :: Carta -> Carta -> Bool
mesmoNaipe (Carta _ naipe1) (Carta _ naipe2) = naipe1 == naipe2

mesmoNumero :: Carta -> Carta -> Bool
mesmoNumero (Carta numero1 _) (Carta numero2 _) = numero1 == numero2

criarCarta :: Numero -> Naipe -> Carta
criarCarta = Carta

temParMao :: (Carta, Carta) -> Bool
temParMao (c1,c2) = mesmoNumero c1 c2

temMesmoNaipeMao :: (Carta, Carta) -> Bool
temMesmoNaipeMao (c1,c2) = mesmoNaipe c1 c2
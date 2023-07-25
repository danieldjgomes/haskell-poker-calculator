module Main where

import Carta
import JogadasPoker
import Text.Printf
import JogadasPoker (jogadorTemParExato)

formatDoubles :: [Double] -> String
formatDoubles [v1, v2, v3, v4, v5, v6] = printf "┌---------------------------------┐ \n|                                 | \n| Par              %.6f     | \n| Trinca           %.6f      | \n| Quadra           %.6f       | \n| Flush            %.6f       | \n| Straight         %.6f       | \n| FullHouse        -              | \n| Straight Flush   %.6f       | \n| Royal Flush      -              | \n|                                 | \n└---------------------------------┘ \n\n\n\n" (v1*100) (v2*100) (v3*100) (v4*100) (v5*100) (v6*100)

-- Função para obter as cartas do jogador
obterCartasJogador :: IO (Carta, Carta)
obterCartasJogador = do
  putStrLn "Digite as cartas do jogador (Número e Naipe separados por espaço):"
  input <- getLine
  let cartas = words input
  case cartas of
    [num1, naipe1, num2, naipe2] -> do
      let carta1 = criarCarta (parseNumero num1) (parseNaipe naipe1)
          carta2 = criarCarta (parseNumero num2) (parseNaipe naipe2)
      return (carta1, carta2)
    _ -> do
      putStrLn "Entrada inválida. Digite novamente."
      obterCartasJogador

-- Função auxiliar para converter a representação do Número em tipo Numero
parseNumero :: String -> Numero
parseNumero "A" = A
parseNumero "K" = K
parseNumero "Q" = Q
parseNumero "J" = J
parseNumero "T" = T
parseNumero "9" = Nove
parseNumero "8" = Oito
parseNumero "7" = Sete
parseNumero "6" = Seis
parseNumero "5" = Cinco
parseNumero "4" = Quatro
parseNumero "3" = Tres
parseNumero "2" = Dois
parseNumero _   = error "Número inválido"

-- Função auxiliar para converter a representação do Naipe em tipo Naipe
parseNaipe :: String -> Naipe
parseNaipe "Copas"   = Copas
parseNaipe "Ouros"   = Ouros
parseNaipe "Paus"    = Paus
parseNaipe "Espadas" = Espadas
parseNaipe _         = error "Naipe inválido"

main :: IO ()
main = do
  putStr "\n\n\n"
  putStr "┌--------------------------------------------------------------------┐\n"
  putStr "|                                                                    |\n"
  putStr "|  :::::::::   ::::::::  :::    ::: :::::::::: :::        :::        |\n"
  putStr "|  :+:    :+: :+:    :+: :+:   :+:  :+:        :+:        :+:        |\n"
  putStr "|  +:+    +:+ +:+    +:+ +:+  +:+   +:+        +:+        +:+        |\n"
  putStr "|  +#++:++#+  +#+    +:+ +#++:++    +#++:++#   +#+        +#+        |\n"
  putStr "|  +#+        +#+    +#+ +#+  +#+   +#+        +#+        +#+        |\n"
  putStr "|  #+#        #+#    #+# #+#   #+#  #+#        #+#        #+#        |\n"
  putStr "|  ###         ########  ###    ### ########## ########## ########## |\n"
  putStr "|                                                                    |\n"
  putStr "|            Uma calculadora de Poker feita em haskell               |\n"
  putStr "|                                                                    |\n"
  putStr "└--------------------------------------------------------------------┘\n"
  putStr "\n\n\n"


  -- Obter as cartas do jogador
  (carta1, carta2) <- obterCartasJogador
  printf "\n\n\nMao: \n\n"
  printf  "┌-----┐ ┌-----┐\n"
  printf  "|     | |     |\n"
  printf  "| %s | | %s |\n" (show carta1) (show carta2)
  printf  "|     | |     |\n"
  printf  "└-----┘ └-----┘\n"

  -- Pré-flop: Calcular probabilidades de diferentes jogadas
  let mao = (carta1, carta2)
  let baralho = removeCartasCompradas criarTodasCartas [carta1, carta2]
  let probPar = pegaProbabilidade mao [] [jogadorTemPar,jogadorTemTrinca,jogadorTemQuadra,jogadorTemFlush,jogadorTemStraight,jogadorTemStraightFlush]
  putStrLn "\n\n\n"
  putStrLn "Probabilidades Pré-Flop:"
  putStrLn $ formatDoubles probPar
  print (jogadorTemParExato mao [])

  jogar mao []
  where
    jogar mao mesa = do
      putStrLn "Deseja inserir novas cartas da mesa? (S/N)"
      input <- getLine
      case input of
        "S" -> do
          novasCartas <- obterNovasCartasMesa
          let mesaAtualizada = mesa ++ novasCartas

          putStrLn "Mesa atualizada:"
          print mesaAtualizada

          -- Calcular as probabilidades após cada novo estágio do jogo
          let probPar = pegaProbabilidade mao mesaAtualizada [jogadorTemPar]
          let probTrinca = pegaProbabilidade mao mesaAtualizada [jogadorTemTrinca]
          let probQuadra = pegaProbabilidade mao mesaAtualizada [jogadorTemQuadra]
          let probFlush = pegaProbabilidade mao mesaAtualizada [jogadorTemFlush]
          let probStraight = pegaProbabilidade mao mesaAtualizada [jogadorTemStraight]
          let probStraightFlush = pegaProbabilidade mao mesaAtualizada [jogadorTemStraightFlush]

          let probPar = pegaProbabilidade mao mesaAtualizada [jogadorTemPar,jogadorTemTrinca,jogadorTemQuadra,jogadorTemFlush,jogadorTemStraight,jogadorTemStraightFlush]
          putStrLn "\n\n\n"
          putStrLn "Probabilidades Pré-Flop:"
          putStrLn $ formatDoubles probPar
          print (jogadorTemParExato mao mesaAtualizada)


          -- Perguntar se o jogador deseja continuar
          jogar mao mesaAtualizada
        "N" -> putStrLn "Obrigado por jogar!"
        _ -> do
          putStrLn "Entrada inválida. Digite 'S' para inserir novas cartas da mesa ou 'N' para encerrar o jogo."
          jogar mao mesa

-- Função para obter novas cartas da mesa
obterNovasCartasMesa :: IO [Carta]
obterNovasCartasMesa = do
  putStrLn "Digite as novas cartas da mesa (Número e Naipe separados por espaço):"
  input <- getLine
  let cartas = words input
  if even (length cartas)
    then do
      let novasCartas = [criarCarta (parseNumero (cartas !! i)) (parseNaipe (cartas !! (i + 1))) | i <- [0, 2 .. length cartas - 1]]
      return novasCartas
    else do
      putStrLn "Entrada inválida. Insira um número par de cartas."
      obterNovasCartasMesa

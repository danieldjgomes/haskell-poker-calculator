import Test.Tasty
import Test.Tasty.HUnit

import JogadasPoker
import Carta

-- Função para criar uma carta para uso nos testes
criarCartaExemplo :: Numero -> Naipe -> Carta
criarCartaExemplo = Carta

-- Testes para jogadorTemPar
testJogadorTemPar :: TestTree
testJogadorTemPar = testGroup "jogadorTemPar"
  [ testCase "Teste 1" $ jogadorTemPar (criarCartaExemplo A Copas, criarCartaExemplo K Copas) [criarCartaExemplo Q Ouros, criarCartaExemplo J Ouros, criarCartaExemplo T Paus] @?= False
  , testCase "Teste 2" $ jogadorTemPar (criarCartaExemplo A Copas, criarCartaExemplo K Copas) [criarCartaExemplo Q Ouros, criarCartaExemplo J Ouros, criarCartaExemplo A Paus] @?= True
  , testCase "Teste 3" $ jogadorTemPar (criarCartaExemplo A Copas, criarCartaExemplo K Ouros) [criarCartaExemplo Q Paus, criarCartaExemplo T Espadas, criarCartaExemplo T Paus] @?= False
  , testCase "Teste 4" $ jogadorTemPar (criarCartaExemplo A Copas, criarCartaExemplo A Ouros) [criarCartaExemplo Q Ouros, criarCartaExemplo J Ouros, criarCartaExemplo T Paus] @?= True
  , testCase "Teste 5" $ jogadorTemPar (criarCartaExemplo A Copas, criarCartaExemplo K Ouros) [criarCartaExemplo K Paus, criarCartaExemplo K Espadas, criarCartaExemplo T Paus] @?= True
  , testCase "Teste 6" $ jogadorTemPar (criarCartaExemplo A Copas, criarCartaExemplo T Ouros) [criarCartaExemplo Q Paus, criarCartaExemplo T Espadas, criarCartaExemplo T Paus] @?= True
  , testCase "Teste 7" $ jogadorTemPar (criarCartaExemplo A Copas, criarCartaExemplo T Ouros) [] @?= False
  , testCase "Teste 8" $ jogadorTemPar (criarCartaExemplo A Copas, criarCartaExemplo A Ouros) [] @?= True
  , testCase "Teste 9" $ jogadorTemPar (criarCartaExemplo A Copas, criarCartaExemplo K Ouros) [] @?= False
  ]

-- Testes para jogadorTemTrinca
testJogadorTemTrinca :: TestTree
testJogadorTemTrinca = testGroup "jogadorTemTrinca"
  [ testCase "Teste 1" $ jogadorTemTrinca (criarCartaExemplo A Copas, criarCartaExemplo K Copas) [criarCartaExemplo Q Ouros, criarCartaExemplo J Ouros, criarCartaExemplo T Paus] @?= False
  , testCase "Teste 2" $ jogadorTemTrinca (criarCartaExemplo A Copas, criarCartaExemplo K Copas) [criarCartaExemplo Q Ouros, criarCartaExemplo A Ouros, criarCartaExemplo A Paus] @?= True
  , testCase "Teste 3" $ jogadorTemTrinca (criarCartaExemplo T Copas, criarCartaExemplo K Copas) [criarCartaExemplo A Ouros, criarCartaExemplo A Espadas, criarCartaExemplo A Paus] @?= False
  -- Adicione mais testes aqui
  ]

-- Testes para jogadorTemQuadra
testJogadorTemQuadra :: TestTree
testJogadorTemQuadra = testGroup "jogadorTemQuadra"
  [ testCase "Teste 1" $ jogadorTemQuadra (criarCartaExemplo A Copas, criarCartaExemplo K Copas) [criarCartaExemplo Q Ouros, criarCartaExemplo J Ouros, criarCartaExemplo T Paus] @?= False
  , testCase "Teste 2" $ jogadorTemQuadra (criarCartaExemplo A Copas, criarCartaExemplo K Copas) [criarCartaExemplo Q Ouros, criarCartaExemplo A Ouros, criarCartaExemplo A Paus] @?= False
  , testCase "Teste 3" $ jogadorTemQuadra (criarCartaExemplo A Copas, criarCartaExemplo K Copas) [criarCartaExemplo A Ouros, criarCartaExemplo A Paus, criarCartaExemplo A Espadas] @?= True
  -- Adicione mais testes aqui
  ]

testJogadorTemFlush :: TestTree
testJogadorTemFlush = testGroup "jogadorTemFlush"
  [ testCase "Teste 1" $ jogadorTemFlush (criarCartaExemplo A Copas, criarCartaExemplo K Copas) [criarCartaExemplo Q Ouros, criarCartaExemplo J Ouros, criarCartaExemplo T Paus] @?= False
  , testCase "Teste 2" $ jogadorTemFlush (criarCartaExemplo A Copas, criarCartaExemplo K Copas) [criarCartaExemplo Q Copas, criarCartaExemplo J Copas, criarCartaExemplo T Paus] @?= False
  , testCase "Teste 3" $ jogadorTemFlush (criarCartaExemplo A Copas, criarCartaExemplo K Copas) [criarCartaExemplo Q Copas, criarCartaExemplo J Copas, criarCartaExemplo T Copas] @?= True
  , testCase "Teste 4" $ jogadorTemFlush (criarCartaExemplo A Copas, criarCartaExemplo K Copas) [criarCartaExemplo A Copas, criarCartaExemplo J Copas, criarCartaExemplo T Copas] @?= True
  , testCase "Teste 5" $ jogadorTemFlush (criarCartaExemplo A Paus, criarCartaExemplo K Paus) [criarCartaExemplo Q Copas, criarCartaExemplo J Copas, criarCartaExemplo T Copas, criarCartaExemplo Nove Copas, criarCartaExemplo Oito Copas] @?= False
  , testCase "Teste 6" $ jogadorTemFlush (criarCartaExemplo A Ouros, criarCartaExemplo K Ouros) [criarCartaExemplo Q Ouros, criarCartaExemplo J Ouros, criarCartaExemplo T Ouros, criarCartaExemplo Nove Ouros, criarCartaExemplo Oito Ouros] @?= True
  , testCase "Teste 7" $ jogadorTemFlush (criarCartaExemplo A Copas, criarCartaExemplo K Ouros) [criarCartaExemplo Q Paus, criarCartaExemplo J Espadas, criarCartaExemplo T Paus, criarCartaExemplo Nove Copas, criarCartaExemplo Oito Copas] @?= False
  , testCase "Teste 8" $ jogadorTemFlush (criarCartaExemplo A Copas, criarCartaExemplo K Ouros) [] @?= False
  , testCase "Teste 9" $ jogadorTemFlush (criarCartaExemplo A Copas, criarCartaExemplo K Ouros) [criarCartaExemplo Q Ouros, criarCartaExemplo J Ouros, criarCartaExemplo T Ouros] @?= False
  ]


testEhSequencia :: TestTree
testEhSequencia = testGroup "ehSequencia"
  [ testCase "Teste 1" $ criarCartaExemplo A Copas `ehSequencia` criarCartaExemplo A Copas  @?= False
  , testCase "Teste 2" $ criarCartaExemplo Dois Copas `ehSequencia` criarCartaExemplo A Copas  @?= True
  , testCase "Teste 3" $ criarCartaExemplo Quatro Copas `ehSequencia` criarCartaExemplo T Ouros  @?= False
  , testCase "Teste 4" $ criarCartaExemplo T Ouros `ehSequencia` criarCartaExemplo J Copas  @?= False
  , testCase "Teste 5" $ criarCartaExemplo J Espadas `ehSequencia` criarCartaExemplo T Copas  @?= True
  , testCase "Teste 6" $ criarCartaExemplo A Copas `ehSequencia` criarCartaExemplo Tres Paus  @?= False
  , testCase "Teste 7" $ criarCartaExemplo Dois Copas `ehSequencia` criarCartaExemplo A Espadas  @?= True
  -- Adicione mais testes aqui
  ]


-- Testes para jogadorTemStraight
testJogadorTemStraight :: TestTree
testJogadorTemStraight = testGroup "jogadorTemStraight"
  [ testCase "Teste 1" $ jogadorTemStraight (criarCartaExemplo Dois Copas, criarCartaExemplo K Copas) [criarCartaExemplo Q Ouros, criarCartaExemplo J Ouros, criarCartaExemplo T Paus] @?= False
  , testCase "Teste 2" $ jogadorTemStraight (criarCartaExemplo A Copas, criarCartaExemplo K Copas) [criarCartaExemplo Q Ouros, criarCartaExemplo J Ouros, criarCartaExemplo T Copas] @?= True
  , testCase "Teste 3" $ jogadorTemStraight (criarCartaExemplo A Copas, criarCartaExemplo K Copas) [criarCartaExemplo Dois Ouros, criarCartaExemplo Tres Ouros, criarCartaExemplo Quatro Paus] @?= True
  , testCase "Teste 4" $ jogadorTemStraight (criarCartaExemplo A Copas, criarCartaExemplo K Copas) [criarCartaExemplo Dois Ouros, criarCartaExemplo Tres Ouros, criarCartaExemplo Quatro Paus, criarCartaExemplo Cinco Copas] @?= True
  , testCase "Teste 5" $ jogadorTemStraight (criarCartaExemplo A Copas, criarCartaExemplo Dois Ouros) [criarCartaExemplo Tres Ouros, criarCartaExemplo Quatro Paus, criarCartaExemplo Cinco Copas] @?= True
  , testCase "Teste 6" $ jogadorTemStraight (criarCartaExemplo Dois Ouros, criarCartaExemplo Tres Ouros) [criarCartaExemplo Quatro Paus, criarCartaExemplo Cinco Copas, criarCartaExemplo A Copas] @?= True
  , testCase "Teste 7" $ jogadorTemStraight (criarCartaExemplo A Copas, criarCartaExemplo K Copas) [criarCartaExemplo Dois Ouros, criarCartaExemplo Tres Ouros, criarCartaExemplo Quatro Paus, criarCartaExemplo Cinco Copas, criarCartaExemplo Seis Copas, criarCartaExemplo Sete Copas] @?= True
  , testCase "Teste 8" $ jogadorTemStraight (criarCartaExemplo A Copas, criarCartaExemplo K Copas) [criarCartaExemplo Dois Ouros, criarCartaExemplo Tres Ouros, criarCartaExemplo Quatro Paus, criarCartaExemplo Cinco Copas, criarCartaExemplo Seis Copas, criarCartaExemplo Sete Copas, criarCartaExemplo Oito Copas] @?= True
  -- Adicione mais testes aqui
  ]

-- Testes para jogadorTemStraightFlush
testJogadorTemStraightFlush :: TestTree
testJogadorTemStraightFlush = testGroup "jogadorTemStraightFlush"
    [ testCase "Teste 1" $ jogadorTemStraightFlush (criarCartaExemplo A Copas, criarCartaExemplo K Copas) [criarCartaExemplo Q Ouros, criarCartaExemplo J Ouros, criarCartaExemplo T Paus] @?= False
    , testCase "Teste 2" $ jogadorTemStraightFlush (criarCartaExemplo A Copas, criarCartaExemplo K Copas) [criarCartaExemplo Q Copas, criarCartaExemplo J Copas, criarCartaExemplo T Copas] @?= True
    , testCase "Teste 3" $ jogadorTemStraightFlush (criarCartaExemplo A Copas, criarCartaExemplo K Copas) [criarCartaExemplo Q Copas, criarCartaExemplo J Copas, criarCartaExemplo T Copas, criarCartaExemplo Nove Copas, criarCartaExemplo Oito Copas] @?= True
    , testCase "Teste 4" $ jogadorTemStraightFlush (criarCartaExemplo Quatro Ouros, criarCartaExemplo Tres Ouros) [criarCartaExemplo Dois Ouros, criarCartaExemplo A Ouros, criarCartaExemplo K Ouros, criarCartaExemplo Q Ouros, criarCartaExemplo J Ouros] @?= True
    , testCase "Teste 5" $ jogadorTemStraightFlush (criarCartaExemplo Nove Ouros, criarCartaExemplo Oito Ouros) [criarCartaExemplo Dois Ouros, criarCartaExemplo Cinco Ouros, criarCartaExemplo Sete Ouros, criarCartaExemplo Seis Ouros, criarCartaExemplo Quatro Ouros] @?= False
    -- Add more test cases here
    ]

main :: IO ()
main = defaultMain $ testGroup "Testes"
  [ testJogadorTemPar
  , testJogadorTemTrinca
  , testJogadorTemQuadra
  , testJogadorTemFlush
  , testJogadorTemStraight
  , testEhSequencia
  , testJogadorTemStraightFlush
  ]

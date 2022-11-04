module Spiral (matrizEspiral) where

import Data.Char

-- Exercicio 2. Crie uma função chamada splitem que recebe um inteiro n e uma lista e devolva uma tupla onde o primeiro elemento será composta dos n primeiros elementos da lista de entrada e o segundo elemento será composto de todos os outros elementos da lista de entrada. 


splitem :: Ord a => a -> [a] -> ([a],[a])
splitem n [] = ([], [])
splitem n (y:lista)
  | y < n     = (y:menor, maior)
  | otherwise = (menor, y:maior)
                where (menor, maior) = splitem n lista
-- ==========================================================
--Exercicio 3 . A Cifra de Cesar é uma técnica de criptografia inocente que consiste na troca dos caracteres utilizados para  escrever uma  mensagem  a  partir de um deslocamento. Desta  forma,  se  o deslocamento for de 3 caracteres o A vira D. Sua tarefa será criar uma função que recebe um  inteiro  e  uma  string  e  devolve  uma  string  criptografada  segundo  a  Cifra  de  Cesar. 

letraMaiuscula m = words (map (toUpper) m)

letraModificada c n = chr((mod ((ord c - ord 'A') + n) 26) + ord 'A')
palavraModificada w n = map (`letraModificada` n) w

cifrar m n = unwords(map (`palavraModificada` n) (letraMaiuscula m))
-- ==========================================================
--Exercicio 4. Uma matriz espiral é uma tabela onde linhas e colunas são preenchidas na forma de uma matriz,  da  esquerda  para  direita  e  de  cima para baixo  por  números  inteiros  naturais.  Sua tarefa  será  criar  uma  função  chamada  de  espiral  que  recebe  um  inteiro  e  devolve  uma matriz espiral


linha :: Int -> Int -> [Int]

linha n k

  | n == 0 = []

  | n == 1 = [1]

  | k == 0 = [1..n]

  | k == n-1 = reverse [2*(n-1)+1 .. 3*(n-1)+1]

  | otherwise = [4*(n-1)-(k-1)] ++ (map (4*(n-1)+)(linha (n-2) (k-1))) ++ [n+k]

matrizEspiral :: Int -> [[Int]]

matrizEspiral tamanho = map (linha tamanho) [0..tamanho-1] 
-- ===========================================================
--Exercicio 5. Crie uma função chamada de difquadrado que recebe uma lista com não menos 10 números naturais e devolva a diferença entre o quadrado da soma e a soma dos quadrados destes números. 

somadosquadrados :: [Int] -> Int
somadosquadrados [] = 0
somadosquadrados (x:xs) = x^2 + somadosquadrados xs

soma :: [Int] -> Int
soma [] = 0
soma (x:xs) = x + soma xs

-- verificando se a lista tem pelo menos 10 elementos
verifica :: [Int] -> Bool
verifica [] = False
verifica (x:xs) = if length (x:xs) >= 10 then True else False

difquadrado :: [Int] -> Int
difquadrado (x:xs) = if verifica (x:xs) then (soma (x:xs))^2 - somadosquadrados (x:xs) else 0







main = do
  print(cifrar "programar funcionalmente" 3)
  print(matrizEspiral 4)
  print(splitem 4 [1,2,3,4,5,6,7,8,9,10])
  print(difquadrado [1,2,3,4,5,6,7,8,9,10])

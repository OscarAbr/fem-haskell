module Main where


import Operations
import Perfs
import Lib


{-}
main = do putStrLn "1: Rapide, 2: Lent"
          choice <- getLine
          putStrLn "Taille de la matrice"
          size <- getLine
          let n = read size :: Double
          if choice == "1"
              then putStrLn ("La méthode rapide donne " ++ show (rapide (matrixTest n)))
              else putStrLn ("La méthode lente donne " ++ show (lente (matrixTest n)))
              -}

-- how to compile: ghc -dynamic --make app/Main.hs src/Perfs.hs src/Invert.hs src/Operations.hs -O2 
main = do putStrLn "Rapide pour n=200"
          let n = 200
          putStrLn ("Résultat: " ++ show (rapide (matrixTest n)))
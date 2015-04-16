{-
  A Haskell program to solve Sodoku puzzles
  Ray Qiu <ray.qiu@gmail.com>, April 2015

  With parallelism built in, use with +RTS -s -Nx
-}

import           Control.Applicative
import           System.IO
import           Sudoku

main :: IO ()
main = do
  let a = [[0,0,0,0,0,0,0,0,0],
           [0,0,0,0,0,0,5,2,3],
           [0,0,0,0,0,0,0,1,8],
           [0,0,0,0,0,0,0,0,0],
           [0,0,9,0,7,4,0,6,0],
           [0,0,4,6,1,0,0,0,7],
           [0,5,8,0,4,3,0,0,0],
           [0,4,0,0,2,0,0,3,0],
           [0,6,7,0,8,1,0,9,4]] :: Sudoku
  showSolution a
  let b = [[9,0,6,0,7,0,4,0,3],
           [0,0,0,4,0,0,2,0,0],
           [0,7,0,0,2,3,0,1,0],
           [5,0,0,0,0,0,1,0,0],
           [0,4,0,2,0,8,0,6,0],
           [0,0,3,0,0,0,0,0,5],
           [0,3,0,7,0,0,0,5,0],
           [0,0,7,0,0,5,0,0,0],
           [4,0,5,0,1,0,7,0,8]] :: Sudoku
  showSolution b
  let c = [[0,0,0,0,3,7,6,0,0],
           [0,0,0,6,0,0,0,9,0],
           [0,0,8,0,0,0,0,0,4],
           [0,9,0,0,0,0,0,0,1],
           [6,0,0,0,0,0,0,0,9],
           [3,0,0,0,0,0,0,4,0],
           [7,0,0,0,0,0,8,0,0],
           [0,1,0,0,0,9,0,0,0],
           [0,0,2,5,4,0,0,0,0]] :: Sudoku
  showSolution c
  sudoku17 <- openFile "sudoku17.txt" ReadMode
  sudoku17s <- lines <$> hGetContents sudoku17
  mapM_ (showSolution . textToSudoku) $ take 20 sudoku17s

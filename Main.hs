{-
  A Haskell program to solve Sodoku puzzles
  Ray Qiu <ray.qiu@gmail.com>, April 2015

  With parallelism built in, use with +RTS -s -Nx
-}

import           Control.Applicative
import           Control.Parallel.Strategies
import           Data.Char                   (digitToInt)
import           System.IO

type Matrix a = [Row a]
type Row a = [a]
type Sudoku = Matrix Digit
type Digit = Int

blank :: Digit -> Bool
blank = (== 0)

solve :: Sudoku -> [Sudoku]
solve = filter isValid . search . choices

isValid :: Sudoku -> Bool
isValid g = all noDups (rows g) &&
            all noDups (cols g) &&
            all noDups (boxes g)

{-| Fill in  in every empty cell -}
choices :: Sudoku -> Matrix [Digit]
choices = let choice d = if blank d then [1..9] else [d]
          in  map (map choice)

search :: Matrix [Digit] -> [Sudoku]
search cm
  | not (isSafe pm)  = []
  | isComplete pm  = [map (map head) pm]
  | otherwise      = (concat . parMap rpar search) (expand pm) --concatMap search (expand pm)
    where pm = prune cm

{-| Expand a non-singleton cell at a time -}
expand :: Matrix [Digit] -> [Matrix [Digit]]
expand s = let (rows1, row:rows2) = break (any isSmallest) s
               -- ^ row contains the Row that has the smallest cell (cell has the least number of Digits)
               (row1, cs:row2)    = break isSmallest row
               -- ^ cs is the smallest cell
               isSmallest xs      = length xs == minimum (counts s)
               counts             = filter (/= 1) . map length . concat
               -- ^ remove singletons, but leave empty cells for a quicker resolution
           in  [rows1 ++ [row1 ++ [c]:row2] ++ rows2 | c <- cs]
           -- ^ put everything back but expand the smallest cell cs

isSingleton :: [a] -> Bool
isSingleton xs = length xs == 1

isComplete :: Matrix [Digit] -> Bool
isComplete = all (all isSingleton)

isSafe :: Matrix [Digit] -> Bool
isSafe s = let ok xs = noDups $ filter isSingleton xs
           in  all ok (rows s) &&
               all ok (cols s) &&
               all ok (boxes s)

noDups :: (Eq a) => [a] -> Bool
noDups []     = True
noDups (x:xs) = notElem x xs && noDups xs

rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols = foldr (zipWith (:)) (repeat [])

boxes :: Matrix a -> Matrix a
boxes = map concat . concatMap cols . group 3 . map (group 3)

group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs = if n > length xs
               then [xs]
               else take n xs : group n (drop n xs)

prune :: Matrix [Digit] -> Matrix [Digit]
prune = let pruneBy f = f . map prune' . f
        in  pruneBy boxes . pruneBy cols . pruneBy rows
          where
            prune' :: Row [Digit] -> Row [Digit]
            prune' r = let singles = concat $ filter isSingleton r
                         in  map (rm singles) r
                           where
                             rm :: [Digit] -> [Digit] -> [Digit]
                             rm _ [x] = [x]
                             rm ds xs  = filter (`notElem` ds) xs

{-| Each Sudoku is formated as a single string of 81 digits that are concatenated row by row -}
textToSudoku :: String -> Sudoku
textToSudoku s =
  let gs = group 9 s
  in  foldr (zipWith (:)) (repeat []) $ foldr (zipWith (:) . map digitToInt) (repeat []) gs

sudokuToString :: Sudoku -> String
sudokuToString [] = ""
sudokuToString (xs:xss) =
  p xs ++ "\n" ++ sudokuToString xss
    where p [] = []
          p (y:ys)
            | y /= 0    = show y ++ " " ++ p ys
            | otherwise = '\x2592' : " " ++ p ys

showSolution :: Sudoku -> IO ()
showSolution s = do
  putStrLn "Sudoku puzzle:"
  putStrLn $ sudokuToString s
  let sol = solve s
  case length sol of
    0 -> putStrLn "No solution found!"
    1 -> do
      putStrLn "Single solution:"
      putStrLn $ sudokuToString $ head sol
    _ -> do
      putStrLn "Multiple solutions:"
      mapM_ (putStrLn . sudokuToString) sol

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
  mapM_ (showSolution . textToSudoku) $ take 100 sudoku17s

{-
  A Haskell program to solve Sodoku puzzles
  Ray Qiu <ray.qiu@gmail.com>, April 2015

  With parallelism built in, use with +RTS -s -Nx
-}

import           Control.Applicative
import           Options.Applicative
import           System.IO
import           System.Exit
import           Sudoku

data CmdArgs = CmdArgs {
  _version   :: Bool,
  _file      :: Maybe String,
  _sudokuStr :: Maybe String
} deriving (Show)

cmdArgs :: Parser CmdArgs
cmdArgs = CmdArgs
  <$> switch
        (long "version"
          <> short 'v'
          <> help "Show the version text")
  <*> optional (strOption
        (long "file"
        <> short 'f'
        <> metavar "<file-name>"
        <> help "Name of the Sudoku text file"))
  <*> optional (argument str
        (metavar "<Sudoku-text>"
        <> help "Sodoku puzzle text"))

argsParser :: ParserInfo CmdArgs
argsParser =
  info (helper <*> cmdArgs)
    (fullDesc
      <> progDesc "Sudoku puzzle solver written in Haskell")

evalArgs :: CmdArgs -> IO ()
evalArgs (CmdArgs True _ _) = do
  putStrLn "sudoku: Version v0.1.0.0, 2015, Ray Qiu <ray.qiu@gmail.com>"
  exitSuccess
evalArgs (CmdArgs False Nothing Nothing) = do
  putStrLn "Usage: use option -h|--help for the help text."
  exitFailure
evalArgs (CmdArgs False (Just _)  (Just _)) = do
  putStrLn "Error: Only one command line argument at a time!"
  exitFailure
evalArgs (CmdArgs False _  (Just s)) = do
  mapM_ (showSolution . textToSudoku) $ words s
evalArgs (CmdArgs False (Just f)  _) = do
  h <- openFile f ReadMode
  hs <- lines <$> hGetContents h
  mapM_ (showSolution . textToSudoku) hs

main :: IO ()
main = do
  args <- customExecParser (prefs showHelpOnError) argsParser
  evalArgs args

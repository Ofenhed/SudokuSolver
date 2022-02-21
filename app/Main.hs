module Main where

import SudokuObject
import System.IO (readFile)
import Intelligence

main :: IO ()
main = do
  board <- readFile "puzzle.in"
  let b = readBoard board :: Board
  putStrLn $ maybe "Unsolvable" show $ solve b

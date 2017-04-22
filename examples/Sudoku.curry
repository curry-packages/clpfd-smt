{-# OPTIONS_CYMAKE -Wno-incomplete-patterns #-}

module Sudoku where

import XFD.Solvers.SMT.Z3
-- import CLP.FD
import List (transpose)

-- Solving a Su Doku puzzle represented as a matrix of numbers (possibly free
-- variables):
sudoku :: [[FDExpr]] -> [Int]
sudoku m = solveFD [] (concat m) $
              allC allDifferent m  /\             -- all rows contain different digits
              allC allDifferent (transpose m) /\  -- all columns have different digits
              allC allDifferent (squares m)       -- all 3x3 squares are different
 where
  -- translate a matrix into a list of small 3x3 squares
    squares :: [[a]] -> [[a]]
    squares [] = []
    squares (l1:l2:l3:ls) = group3Rows [l1,l2,l3] ++ squares ls

    group3Rows l123 = if null (head l123) then [] else
      concatMap (take 3) l123 : group3Rows (map (drop 3) l123)

-- read a Su Doku specification written as a list of strings containing digits
-- and spaces
readSudoku :: [String] -> [[FDExpr]]
readSudoku = (flip rows) vars
  where
    vars = domain 1 9
    rows []     _ = []
    rows (s:ss) lst = (row s (take l lst)):(rows ss (drop l lst))
      where
        l = length (filter (==' ') s)
        row [] _ = []
        row (x:xs) vs = if x==' '
                          then (head vs):(row xs (tail vs))
                          else (fd (ord x - ord '0')):(row xs vs)

-- show a solved Su Doku matrix
showSudoku :: [[Int]] -> String
showSudoku = unlines . map (concatMap (\i->[chr (i + ord '0'),' ']))

-- the main function, e.g., evaluate (main s1):
main :: [[Char]] -> IO ()
main s = putStrLn (showSudoku (toMatrix m (sudoku m)))
 where m = readSudoku s

       toMatrix [] xs = [xs]
       toMatrix (r:rs) xs = let rn = length r
                             in take rn xs : toMatrix rs (drop rn xs)

s1 :: [[Char]]
s1 = ["9  2  5  ",
      " 4  6  3 ",
      "  3     6",
      "   9  2  ",
      "    5  8 ",
      "  7  4  3",
      "7     1  ",
      " 5  2  4 ",
      "  1  6  9"]

s2 :: [[Char]]
s2 = ["819  5   ",
      "  2   75 ",
      " 371 4 6 ",
      "4  59 1  ",
      "7  3 8  2",
      "  3 62  7",
      " 5 7 921 ",
      " 64   9  ",
      "   2  438"]

s3 :: [[Char]]
s3 = ["    63 8 ",
      "   1     ",
      "327   1  ",
      "9  2   3 ",
      "  6   4  ",
      " 3   4  9",
      "  8   627",
      "     6   ",
      " 4 51    "]

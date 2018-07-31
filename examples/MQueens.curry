module MQueens where

import XFD.Solvers.SMT.Z3

import Data.List (transpose,elemIndex)

queens :: Int -> (Int, [Int])
queens n =
  let vars  = take (n*n) (domain 0 1)
      field = [ take n (drop (i*n) vars) | i <- [0..(n-1)] ]
      obj   = head (domain 1 n)
      (o:solved) = solveFD [Minimize obj] (obj:vars) $
                        controlled n vars /\
                        sum vars Equ obj /\
                        allSafe n field
      pos = [ maybe 0 (+1) $ elemIndex 1 (take n $ drop (i*n) solved) | i <- [0..(n-1)] ]
  in (o,pos)

allSafe :: Int -> [[FDExpr]] -> FDConstr
allSafe n qs =
    andC [ sum (head $ drop i qs) Leq (fd 1) | i <- [0..(n-1)] ] /\
    andC [ sum (head $ drop j (transpose qs)) Leq (fd 1) | j <- [0..(n-1)] ] /\
    andC [ sum (mdia k) Leq (fd 1) | k <- [(1-n)..(n-1)] ] /\
    andC [ sum (pdia k) Leq (fd 1) | k <- [0..(2*n-2)] ]
  where
    mdia k = [ (head $ drop i qs) !! j | i <- [0..(n-1)], j <- [0..(n-1)], i-j == k ]
    pdia k = [ (head $ drop i qs) !! j | i <- [0..(n-1)], j <- [0..(n-1)], i+j == k ]


controlled :: Int -> [FDExpr] -> FDConstr
controlled n field = andC $ [ controlled' i j | i <- [0..(n-1)], j <- [0..(n-1)] ]
  where
    controlled' i j = sum (vars i j) Geq (fd 1)
    vars i j =
      (take n $ drop (i*n) field) ++
      ([ field !! (j + m*n) | m <- [0..(n-1)] ]) ++
      ([ field !! (s*n + t) | s <- [0..(n-1)], t <- [0..(n-1)], s-t == i-j ]) ++
      ([ field !! (s*n + t) | s <- [0..(n-1)], t <- [0..(n-1)], s+t == i+j ])

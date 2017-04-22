module MagicSeries (
    magic, magicWo, Option(..)
  ) where

-- import XFD.Solvers.SMT.Z3
import CLP.FD

magic :: Int -> [Int]
magic n = magicWo n []

magicWo :: Int -> [Option] -> [Int]
magicWo n opt =
  let vs = take n $ domain 0 (n-1)
      is = map fd (take n [0..])
  in solveFD opt vs $
       constrain vs vs is /\
       sum vs Equ (fd n) /\
       scalarProduct is vs Equ (fd n)


constrain :: [FDExpr] -> [FDExpr] -> [FDExpr] -> FDConstr
constrain []     _  _      = true
constrain (x:xs) vs (i:is) = count i vs Equ x /\ constrain xs vs is

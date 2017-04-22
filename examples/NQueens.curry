module NQueens (
    queens, queensWo, Option(..)
  ) where

import XFD.Solvers.SMT.Z3
-- import CLP.FD

queens :: Int -> [Int]
queens n = queensWo n []

queensWo :: Int -> [Option] -> [Int]
queensWo n opt =
  let qs = take n $ domain 1 n
  in  solveFD opt qs (allSafe qs)

allSafe :: [FDExpr] -> FDConstr
allSafe []     = true
allSafe (q:qs) = safe q qs (fd 1) /\ allSafe qs

safe :: FDExpr -> [FDExpr] -> FDExpr -> FDConstr
safe _ []      _ = true
safe q (q1:qs) p = no_attack q q1 p /\ safe q qs (p +# fd 1)

no_attack :: FDExpr -> FDExpr -> FDExpr -> FDConstr
no_attack q1 q2 p = q1 /=# q2 /\ q1 /=# q2 +# p /\ q1 /=# q2 -# p

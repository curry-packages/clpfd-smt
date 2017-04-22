module XFD (

  -- solve functions
  solveFD, solveFDwith,

  -- FD constraints
  module XFD.FD,

  -- SMTLib
  --module SMTFD.SMTLib

  module XFD.Solver
  ) where

import Unsafe
import XFD.FD
import XFD.SMTLib
import XFD.Solver hiding (solveFDwith, solveFDAllwith, solveFDOnewith)

import XFD.Solvers.Z3
import XFD.Solvers.CVC4

data Solvers = Z3 | CVC4
type SolverOptions = [Option]

-- solveFD :: Solvers -> [FDExpr] -> FDConstr -> [[Int]]
-- solveFD Z3   exs c = unsafePerformIO $ solveSMT z3Config exs c
-- solveFD CVC4 exs c = unsafePerformIO $ solveSMT cvc4Config exs c
solveFD :: SolverOptions -> [FDExpr] -> FDConstr -> [[Int]]
solveFD = solveFDwith Z3

solveFDwith :: Solvers -> SolverOptions -> [FDExpr] -> FDConstr -> [[Int]]
solveFDwith s opt exs c =
  let (sol, cfg) = case s of
                    Z3    -> (solveSMT, z3Config)
                    CVC4  -> (solveSMT, cvc4Config)
  in unsafePerformIO $ sol cfg opt exs c

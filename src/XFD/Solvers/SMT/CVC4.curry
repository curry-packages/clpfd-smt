module XFD.Solvers.SMT.CVC4 (
    module XFD.FD
  , Option(..)
  , solveFD, solveFDAll, solveFDOne
  ) where

import XFD.FD

import XFD.Solver
import XFD.SMTLib

solverConfig :: SolverConfig
solverConfig = defaultConfig
                  { executable  = "cvc4"
                  , flags       = ["--interactive", "--no-interactive-prompt", "--lang=smt2", "-q"]
                  , solveWith   = solveSMT
                  }

solveFD :: SolverArgs [Int]
solveFD = solveFDwith solverConfig

solveFDAll :: SolverArgs [[Int]]
solveFDAll = solveFDAllwith solverConfig

solveFDOne :: SolverArgs [Int]
solveFDOne = solveFDOnewith solverConfig

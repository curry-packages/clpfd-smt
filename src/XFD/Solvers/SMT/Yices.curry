module XFD.Solvers.SMT.Yices (
    module XFD.FD
  , Option(..)
  , solveFD, solveFDAll, solveFDOne
  ) where

import XFD.FD

import XFD.Solver
import XFD.SMTLib

solverConfig :: SolverConfig
solverConfig = defaultConfig
                  { executable  = "yices-smt2"
                  , flags       = ["--incremental"]
                  , solveWith   = solveSMT
                  }

solveFD :: SolverArgs [Int]
solveFD = solveFDwith solverConfig

solveFDAll :: SolverArgs [[Int]]
solveFDAll = solveFDAllwith solverConfig

solveFDOne :: SolverArgs [Int]
solveFDOne = solveFDOnewith solverConfig

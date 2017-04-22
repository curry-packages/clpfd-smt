module XFD.Solvers.SMT.Z3 (
    module XFD.FD
  , Option(..)
  , solveFD, solveFDAll, solveFDOne
  ) where

import XFD.FD

import XFD.Solver
import XFD.SMTLib (solveSMT)

solverConfig :: SolverConfig
solverConfig = defaultConfig
                  { executable  = "z3"
                  , flags       = ["-smt2", "-in"]
                  , solveWith   = solveSMT
                  , smtLogic    = "QF_NIA"
                  }

solveFD :: SolverArgs [Int]
solveFD = solveFDwith solverConfig

solveFDAll :: SolverArgs [[Int]]
solveFDAll = solveFDAllwith solverConfig

solveFDOne :: SolverArgs [Int]
solveFDOne = solveFDOnewith solverConfig

module XFD.Solvers.SMT.MathSAT (
    module XFD.FD
  , Option(..)
  , solveFD, solveFDAll, solveFDOne
  ) where

import XFD.FD

import XFD.Solver
import XFD.SMTLib

solverConfig :: SolverConfig
solverConfig = defaultConfig
                  { executable  = "mathsat"
                  , flags       = ["-input=smt2", "2>&1"] -- hacky, redirect MathSATs stderr to stdout
                  , solveWith   = solveSMT
                  }

solveFD :: SolverArgs [Int]
solveFD = solveFDwith solverConfig

solveFDAll :: SolverArgs [[Int]]
solveFDAll = solveFDAllwith solverConfig

solveFDOne :: SolverArgs [Int]
solveFDOne = solveFDOnewith solverConfig


module XFD.Solvers.SAT.Z3 (
    module XFD.FD
  , Option(..)
  , solveFD, solveFDAll, solveFDOne
  ) where

import XFD.FD

import XFD.Solver
import XFD.Dimacs

solverConfig :: SolverConfig
solverConfig = Config { executable  = "z3"
                      , flags       = ["-dimacs", "-in"]
                      , solveWith   = solveDimacs
                      }

solveFD :: SolverArgs [Int]
solveFD = solveFDwith solverConfig

solveFDAll :: SolverArgs [[Int]]
solveFDAll = solveFDAllwith solverConfig

solveFDOne :: SolverArgs [Int]
solveFDOne = solveFDOnewith solverConfig

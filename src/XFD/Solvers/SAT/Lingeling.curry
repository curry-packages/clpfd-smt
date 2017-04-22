module XFD.Solvers.SAT.Lingeling (
    module XFD.FD
  , Option(..)
  , solveFD, solveFDAll, solveFDOne
  , solverConfig
  ) where

import XFD.FD

import XFD.Solver
import XFD.Dimacs

solverConfig :: SolverConfig
solverConfig = Config { executable  = "lingeling"
                      , flags       = ["-q"]
                      , solveWith   = solveDimacs
                      }

solveFD :: SolverArgs [Int]
solveFD = solveFDwith solverConfig

solveFDAll :: SolverArgs [[Int]]
solveFDAll = solveFDAllwith solverConfig

solveFDOne :: SolverArgs [Int]
solveFDOne = solveFDOnewith solverConfig

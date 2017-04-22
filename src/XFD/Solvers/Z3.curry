module XFD.Solvers.Z3 where

import XFD.Solver

z3Config :: SolverConfig
z3Config = Config { executable     = "z3"
                  , flags          = ["-smt2", "-in"]
                  }

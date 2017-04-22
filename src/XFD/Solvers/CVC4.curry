module XFD.Solvers.CVC4 where

import XFD.Solver

cvc4Config :: SolverConfig
cvc4Config = Config { executable = "cvc4"
                    , flags = ["--produce-models", "--interactive", "--no-interactive-prompt", "--lang smt", "-q"]
                    }

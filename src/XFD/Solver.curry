--- This module defines the basic options for solvers used to solve
--- FD constraints.

module XFD.Solver where

import System.IO.Unsafe

import XFD.FD (FDConstr, FDExpr (FDVar))
import qualified XFD.SMTLib.Types as SMT (Option(..))

--------------------------------------------------------------------------------
-- types and type synonyms for solvers

-- arguments and return type for a solver
type SolverArgs a = [Option] -> [FDExpr] -> FDConstr -> a

-- an implementation additionally gets the config and has IO return type
type SolverImpl   = SolverConfig -> SolverArgs (IO [[Int]])

-- configuration with fields for executable, command line flags, solver
-- implementation and settings for SMT solvers
data SolverConfig = Config
                  { executable  :: String
                  , flags       :: [String]
                  , solveWith   :: SolverImpl
                  , smtOptions  :: Maybe [SMT.Option]
                  , smtLogic    :: String
                  }

-- options to pass to the solver
data Option = Debug Int
              -- how many solutions?
            | All
            | First
            | FirstN Int
              -- keep the generated program
            | Persist String
              -- optimize a FD variable
            | Minimize FDExpr
            | Maximize FDExpr

-- options for the solver implemtation to work with; extracted from notations above
-- Int: debugging level
-- Int: number of solutions
-- (Bool, String): keep? generated program if yes: in file named filename
-- (Bool, FDExpr): minimize? given variable
-- (Bool, FDExpr): maximize? given varialbe
type ExtractedOptions = (Int, Int , (Bool, String), (Bool, FDExpr), (Bool, FDExpr))

--------------------------------------------------------------------------------
-- default configuration for SMT solvers
defaultConfig :: SolverConfig
defaultConfig = Config
              { flags      = []
              , smtOptions = Just [SMT.ProduceModels True]
              , smtLogic   = "QF_LIA"
              }

-- default options, can be modified by giving [Option] to solveFD functions
defaultOptions :: ExtractedOptions
defaultOptions =  ( 0           -- default debugging level of 0
                  , (-1)        -- number of solutions, (-1) means All
                  , (False, _)  -- (persist?, filename)
                  , (False, _)  -- (minimize?, var)
                  , (False, _)  -- (maximize?, var)
                  )

--- Takes a list of options and returns a tuple containing the extracted values
--- to work with.
getSolverOptions :: [Option] -> ExtractedOptions
getSolverOptions options = getOpts options defaultOptions
  where
    getOpts []     opts = opts
    getOpts (o:os) (d, n, (p, f), mi, ma) = case o of
      Debug   i   -> getOpts os (i, n   , (p, f), mi, ma)
      All         -> getOpts os (d, (-1), (p, f), mi, ma)
      First       -> getOpts os (d, 1   , (p, f), mi, ma)
      FirstN  i   -> getOpts os (d, i   , (p, f), mi, ma)
      Persist s   -> getOpts os (d, n   , (True, s), mi, ma)
      Minimize v  -> getOpts os (d, n, (p, f), (True, assertVar v), ma)
      Maximize v  -> getOpts os (d, n, (p, f), mi, (True, assertVar v))
    assertVar v = case v of
      FDVar _ _ _ _ -> v
      -- _             -> error "not an FDVar"

--------------------------------------------------------------------------------
-- helpers to build solveFD funtions

--- Get list of solutions from solver and combine them using the (?) operator to
--- imitate non-determinism as `CLP.FD.solveFD` implements it.
solveFDwith :: SolverConfig -> SolverArgs [Int]
solveFDwith cfg opt vars constr =
  let solveF    = solveWith cfg
      solutions = unsafePerformIO $ solveF cfg opt vars constr
  in foldr1 (?) solutions

--- Same as solveFDwith, but append option First to list of options so only one
--- solution is returned.
solveFDOnewith :: SolverConfig -> SolverArgs [Int]
solveFDOnewith cfg opt vars constr =
  let solveF    = solveWith cfg
      options   = opt ++ [First]
      solutions = unsafePerformIO $ solveF cfg options vars constr
  in head solutions

--- Same as solveFDwith, but append option All to list of options so all
--- solutions are returned; also returns list of solutions non-deterministically.
solveFDAllwith :: SolverConfig -> SolverArgs [[Int]]
solveFDAllwith cfg opt vars constr =
  let solveF    = solveWith cfg
      options   = opt ++ [All]
      solutions = unsafePerformIO $ solveF cfg options vars constr
  in solutions

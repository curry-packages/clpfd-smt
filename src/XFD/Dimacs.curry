module XFD.Dimacs (
  module XFD.Dimacs,
  module XFD.FD,
  module XFD.Solver
  ) where

import Control.Monad.Trans.State
import System.IO
import IOExts

import Dimacs.Types
import Dimacs.Build
import Dimacs.Pretty
import Dimacs.Parser

import Dimacs.FromFD
import XFD.FD
import XFD.Solver


solveDimacs :: SolverImpl--SolverConfig -> SolverOptions -> [FDExpr] -> FDConstr -> IO [[Int]]
solveDimacs cfg options vars constr = do
  let (simples, simpleNum, simpleConstr) = simplifyConstr constr
      fdVars = filterFDVars (allFDVars constr ++ allFDVars simpleConstr)--listFDVars' (vars ++ allFDVars simpleConstr)
      (boolean, (nVars, lVars)) = convert $ convertFDVars fdVars >>
                                            convertSimplifications simples simpleNum >>= \b ->
                                            convertFDConstr simpleConstr >>= \c ->
                                            return (b ./\ c)
      baseDimacs = prettyDimacs nVars boolean
      exec = unwords $ (executable cfg):(flags cfg)
      names = map (\(FDVar n _ _ _) -> n) vars
      (debug, numSol, (persist, filename), _, _) = getSolverOptions options
  when (debug > 0) $ putStrLn $ "debugging level " ++ (show debug)
  when persist $ do
                  putStrLn $ "saving program in file '" ++ filename ++ "'"
                  writeFile filename $ baseDimacs
  when (debug > 1) $ putStr "starting solver... "
  (inH, outH, _) <- execCmd exec
  when (debug > 1) $ putStrLn "done"
  when (debug > 1) $ putStr "writing program to stdin... "
  hPutStr inH $ baseDimacs
  hFlush inH
  hClose inH
  when (debug > 1) $ putStrLn "done"
  when (debug > 1) $ putStrLn "searching solutions"
  solutions <- getSolutions outH exec nVars boolean names lVars numSol debug 1
  when (debug > 1) $ putStrLn $ "found " ++ (show (length solutions)) ++ " solutions"
  putStrLn ""
  return solutions


getSolutions :: Handle -> String -> Int -> Boolean -> [String]
                  -> [(String, [Boolean])] -> Int -> Int -> Int -> IO [[Int]]
getSolutions outH' exec nVars boolean vars lVars n debug nSol
 | n == 0 = return []
 | otherwise
 = do when (debug > 1) $ putStrLn "waiting for solver response"
      response <- hGetContents outH'
      when (debug > 1) $ putStrLn "received"
      when (debug > 1) $ putStrLn response
      case parse response of
        Left  e  -> putStrLn e >> return []
        Right [] -> return []
        Right bs ->
          do let solution = map getFDVal $ extractSolution vars lVars bs
                 boolean' = excludeSolution vars lVars (tail bs) boolean
                 dimacs   = prettyDimacs nVars boolean'
             when (debug > 1) $ putStrLn dimacs
             (inH, outH, _) <- execCmd exec
             hPutStr inH dimacs
             hFlush inH
             hClose inH
             when (debug > 0) $ putStrLn $ "Solution #" ++ (show nSol) ++ ": " ++ (show solution)
             sols <- getSolutions outH exec nVars boolean' vars lVars (n-1) debug (nSol+1)
             return (solution:sols)

excludeSolution :: [String] -> [(String, [Boolean])] -> [Boolean] -> Boolean
                -> Boolean
excludeSolution vars lVars bs boolean =
  boolean ./\ (Or $ invert (neededVars vars))
 where
  neededVars [] = []
  neededVars (v:vs) = extractVars (lookupVar v lVars) ++ neededVars vs
  extractVars [] = []
  extractVars l@(v:_) = take (length l) $ drop ((num v)-1) bs
  num var = case var of
    (Var i) -> i
    _       -> error "should not happen"
  invert [] = []
  invert (v:vs) = (case v of
    (Var _) -> no v
    (Not x) -> x
    _       -> v) : invert vs


solveD :: [Option] -> [FDExpr] -> FDConstr -> IO [[Int]]
solveD = solveDimacs z3Dimacs

z3Dimacs :: SolverConfig
z3Dimacs = Config { executable = "z3"
                  , flags = ["-in", "-dimacs"]
                  }

lingeling :: SolverConfig
lingeling = Config { executable = "lingeling"
                   , flags = ["-q"]
                   }

--------------------------------------------------------------------------------

testExtraction :: [FDExpr]
testExtraction =
  let sol   = [va 1,nv 2,va 3,nv 4,va 5,nv 6,va 7,nv 8, va 9, nv 10, va 11]
      vars  = take 2 $ domain 0 9
      names = map (\(FDVar n _ _ _) -> n) vars
      (_, (_, lVars)) = convert $ convertFDVars vars
  in extractSolution names lVars sol

-- testExclusion :: String
-- testExclusion =
--   let sol   = [va 1,nv 2,va 3,nv 4,va 5,nv 6,va 7,nv 8, va 9, nv 10, va 11]
--       vars  = take 2 $ domainFromList ["x", "y"] 0 9
--       names = map (\(FDVar n _ _ _) -> n) vars
--       (_, (_, lVars)) = convert $ convertFDVars vars
--   in excludeSolution names lVars sol

test :: [FDExpr] -> FDConstr -> IO ()
test vs c =
  let (simps, i, nc) = simplifyConstr c
      (b, (n, _)) = convert $ (convertFDVars $ filterFDVars (vs ++ allFDVars nc))
                        >> (convertSimplifications simps i)
                        >>= \s -> convertFDConstr nc
                        >>= \t -> return (s ./\ t)
  in putStrLn $ prettyDimacs (i+n) b

-- test :: [FDExpr] -> FDConstr -> IO ()
-- test vs c =
--   let ((s@(SimplifyAdd z x y):simps), i, nc) = simplifyConstr c
--       (j, lVars) = execState (convertFDVars $ listFDVars' (vs ++ allFDVars nc)) (1, [])
--   in fst $ arithmetic s lVars (map va [j ..])

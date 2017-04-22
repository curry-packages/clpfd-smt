--- This module provides operations to communicate with SMT solvers.

module XFD.SMTLib
  ( solveSMT
  ) where

import XFD.SMTLib.Pretty
import XFD.SMTLib.Types
import XFD.SMTLib.Build
import XFD.SMTLib.RDParser
import XFD.SMTLib.FromFD

import XFD.FD
import XFD.Solver

import IO
import IOExts
import List (last, init)

-- reads lines from an input handle until a line that equals `s` is read:
hGetContentsUntil :: Handle -> String -> IO String
hGetContentsUntil h s = do line <- hGetLine h
                           let l = unstring line
                           if l == s
                              then return []
                              else do ls <- hGetContentsUntil h s
                                      return (l ++ "\n" ++ ls)
  where
    -- some solvers put quotes around echo output...
    unstring :: String -> String
    unstring s' = case (s', head s', last s') of
      (_:tr@(_:_), '"', '"') -> init tr
      _                      -> s'

-- combination of `hPutStr` and `hFlush`:
hPutStrFlush :: Handle -> String -> IO ()
hPutStrFlush h s = hPutStr h s >> hFlush h

-- string to delimit answers to read one at a time
outputDelimiter :: String
outputDelimiter = "===STOP-READING==="

-- get contents until `outputDelimiter`
hGetDelimited :: Handle -> IO String
hGetDelimited h = hGetContentsUntil h outputDelimiter

-- show SMT-LIB with delimiter echo at the end
delimSMT :: SMT -> String
delimSMT cmds = showSMT (cmds =>> echo outputDelimiter)


--- Prints output of the external solver given a list of FD variables (second
--- argument) w.r.t. constraint (third argument).
--- The first argument contains the configuration for the solver to use.
solveSMT :: SolverImpl
solveSMT _ _ [] _ = return [[]]
solveSMT cfg options vars@(_:_) constr = do
  let pre  = setOptions (smtOptions cfg) =>> setLogic (smtLogic cfg)
      cmds = declare (allFDVars constr) =>> assert ( constr )
      smt  = pre =>> cmds
      exec = unwords $ (executable cfg):(flags cfg)
      (debug, numSol, (persist, filename), mini, maxi) = getSolverOptions options
  when (debug > 0) $ putStrLn $ "debugging level " ++ (show debug)
  when (debug > 0) $ do
    putStrLn $ "using solver " ++ (executable cfg)
    putStrLn $ "  called with options " ++ unwords (flags cfg)
    putStrLn $ "  with logic " ++ (smtLogic cfg)
  when persist $ do
    putStrLn $ "saving program in file '" ++ filename ++ "'"
    writeFile filename $ showSMT smt
  when (debug > 1) $ putStr "starting solver... "
  (inH, outH, _) <- execCmd exec
  when (debug > 1) $ putStrLn "done"
  when (debug > 1) $ putStr "writing program to stdin... "
  hPutStrFlush inH $ delimSMT $ smt
  when (debug > 1) $ putStrLn "done"
  checkForError outH debug
  when (debug > 1) $ putStrLn "searching solutions"
  solutions <- getSolutions inH outH vars numSol debug 1 mini maxi
  when (debug > 1) $ putStrLn $ "found " ++ (show (length solutions)) ++ " solutions"
  hPutStrFlush inH $ showSMT exit
  hClose inH
  hClose outH
  return solutions

-- check for any error message from the solver; uses `error` to get cancel I/O
-- computation; use directly after writing commands without check-sat to catch
-- problems with constraint like non linear arithmetic with solvers that only
-- support linear
checkForError :: Handle -> Int -> IO ()
checkForError outH debug = do
  when (debug > 1) $ putStr "checking for errors raised by solver... "
  response <- hGetDelimited outH
  if (response /= "")
    then do
      case parse response of
        Left  e -> error e
        Right p -> case p of
          CmdGenResponse (Error msg) -> do
            when (debug > 1) $ putStrLn ("got error message")
            error msg
          _                          -> error "unexpected response"
    else do
      when (debug > 1) $ putStrLn "all clear"

-- print solution number and values
putSolution :: Int -> [Int] -> IO ()
putSolution n s = putStrLn $ "Solution #" ++ (show n) ++ ": " ++ (show s)

-- check satisfiability and get solution iff answer is sat; exclude said
-- solution and look for next one if `n` is not 0
-- returns list of solutions
getSolutions :: Handle -> Handle -> [FDExpr] -> Int -> Int
             -> Int -> (Bool, FDExpr) -> (Bool, FDExpr) -> IO [[Int]]
getSolutions inH outH fds n debug nSol (mini, miv) (maxi, mav)
 | n == 0 = return []
 | otherwise = do
    hPutStrFlush inH $ delimSMT checkSat
    when (debug > 1) $ putStr "waiting for checksat response... "
    response <- hGetDelimited outH
    when (debug > 1) $ putStrLn "received"
    case parse response of
      Left  e -> putStr e >> return []
      Right p -> case p of
        CmdGenResponse (Error msg) -> do
          putStrLn $ "error from solver: " ++ msg
          return []
        CmdCheckSatResponse Sat -> do
          when mini $ optimize inH outH debug miv (<#)
          when maxi $ optimize inH outH debug mav (>#)
          hPutStrFlush inH $ delimSMT (getValue fds)
          when (debug > 1) $ putStr "waiting for value response... "
          values <- hGetDelimited outH
          when (debug > 1) $ putStrLn "received"
          let vs  = case parse values of
                      Left e -> error e
                      Right vr -> convertValueResponse vr
              sol = extractValues vs
          when (debug > 1) $ putStr "writing excluded solution... "
          hPutStrFlush inH $ showSMT (excludeSolution vs)
          when (debug > 1) $ putStrLn "done"
          when (debug > 0) $ putSolution nSol sol
          liftIO (sol:) $ getSolutions inH outH fds (n-1) debug (nSol+1) (False, _) (False, _)
        _ -> return []

-- optimize variable according to relation; `(<#)` for minimiing value of
-- variable `ov`, `(>#)` for maximizing it
optimize :: Handle -> Handle -> Int -> FDExpr -> (FDExpr -> FDExpr -> FDConstr) -> IO ()
optimize inH outH debug ov relop = do
    when (debug > 1) $ putStrLn $ "optimizing value for variable "
                                  ++ getFDVarName optv
    hPutStrFlush inH $ delimSMT (getValue [optv])
    when (debug > 1) $ putStr "waiting for value response... "
    values <- hGetDelimited outH
    when (debug > 1) $ putStrLn "received"
    let vs = case parse values of
              Left e -> error e
              Right vr -> convertValueResponse vr
        sol = head $ extractValues vs
    optval <- optimize' sol
    when (debug > 1) $ putStrLn $ "found optimal value to be " ++ show optval
    hPutStrFlush inH $ showSMT (assert (optv =# fd optval))
    hPutStrFlush inH $ delimSMT checkSat
    when (debug > 1) $ putStr "waiting for checksat response... "
    response <- hGetDelimited outH
    when (debug > 1) $ putStrLn "received"
    case parse response of
      Left  e -> error e
      Right p -> case p of
        CmdGenResponse (Error msg) -> error $ "error from solver: " ++ msg
        CmdCheckSatResponse Sat -> return ()
        _ -> error "error occured"
  where
    optv = case ov of
      FDVar _ _ _ _ -> ov
      _             -> error "optimize: not an FDVar"
    optimize' :: Int -> IO Int
    optimize' val = do
      hPutStrFlush inH $ showSMT $ push 1
      hPutStrFlush inH $ showSMT (assert (optv `relop` fd val))
      hPutStrFlush inH $ delimSMT checkSat
      when (debug > 1) $ putStr "waiting for checksat response... "
      response <- hGetDelimited outH
      when (debug > 1) $ putStrLn "received"
      case parse response of
        Left  e -> error e
        Right p -> case p of
          CmdGenResponse (Error msg) -> error $ "error from solver: " ++ msg
          CmdCheckSatResponse Sat -> do
            hPutStrFlush inH $ delimSMT (getValue [optv])
            when (debug > 1) $ putStr "waiting for value response... "
            values <- hGetDelimited outH
            when (debug > 1) $ putStrLn "received"
            let vs  = case parse values of
                        Left e -> error e
                        Right vr -> convertValueResponse vr
                sol = head $ extractValues vs
            hPutStrFlush inH $ showSMT $ pop 1
            optimize' sol
          _ -> do
            hPutStrFlush inH $ showSMT $ pop 1
            return val

-- convert value response to list of FD variables and integers
convertValueResponse :: CmdResponse -> [FDExpr]
convertValueResponse rsp = case rsp of
    CmdGetValueResponse vr -> map extractVP vr
    _ -> []
  where
    extractVP (ValuationPair n v) = toFD n v --(name n, value v False)
    toFD n v = case n of
      TermQualIdentifier (QIdentifier (ISymbol s)) -> FDVar s _ _ (value v False)
      TermSpecConstant (SpecConstantNumeral _)     -> fd (value v False)
      TermQualIdentifierT (QIdentifier (ISymbol "-")) [i] -> fd (value i True)
      _ -> error "not a variable name or integer"
    value t neg = case t of
      TermSpecConstant (SpecConstantNumeral v) -> if neg then (-v) else v
      TermQualIdentifierT (QIdentifier (ISymbol "-")) [v] -> value v True
      _ -> error "not a value"

-- get list of values of a list of FD expressions
extractValues :: [FDExpr] -> [Int]
extractValues = map getFDVal

-- produce assertion to exclude values for a list of FD variables
excludeSolution :: [FDExpr] -> SMT
excludeSolution vs = assert (orC $ map exclude (filterVars vs))
  where
    exclude x = case x of
      FDVar _ _ _ v -> x /=# fd v
      _ -> error "solutin of non FDVars cannot be excluded"

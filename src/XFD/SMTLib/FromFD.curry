--- This module defines operation to translate FD models into SMT-LIB models

module XFD.SMTLib.FromFD where

import XFD.SMTLib.Types
import XFD.SMTLib.Build
import XFD.SMTLib.Pretty

import XFD.FD

type SMT = SMTLib -> SMTLib

infixl 1 =>>

(=>>) :: SMT -> SMT -> SMT
a =>> b = \cmds -> b $ a cmds

showSMT :: SMT -> String
showSMT cmds = showSMTLib $ cmds []

setOptions :: Maybe [Option] -> SMT
setOptions Nothing   = \cmds -> cmds
setOptions (Just os) = \cmds -> cmds ++ (map SetOption os)

setLogic :: String -> SMT
setLogic l = \cmds -> cmds ++ [SetLogic l]

exit :: SMT
exit = \cmds -> cmds ++ [Exit]

-- add Echo command to list
echo :: String -> SMT
echo s = \cmds -> cmds ++ [Echo s]

-- add Pop command to list
pop :: Int -> SMT
pop lvl = \cmds -> cmds ++ [Pop lvl]

-- add Push command to list
push :: Int -> SMT
push lvl = \cmds -> cmds ++ [Push lvl]

-- add CheckSat command to list
checkSat :: SMT
checkSat = \cmds -> cmds ++ [CheckSat]

-- add GetModel command to list
getModel :: SMT
getModel = \cmds -> cmds ++ [GetModel]

getValue :: [FDExpr] -> SMT
getValue fds = \cmds -> cmds ++ [GetValue $ map convertExpr fds]

-- add Declare commands for a list of variables and Assert commands for their
-- domain
declare :: [FDExpr] -> SMT
declare []       = id
declare vs@(_:_) = (foldr1 (=>>) (map declareConst vars))
               =>> (foldr1 (=>>) (map (assert . domainConstr) vars))
  where
    vars = filterFDVars vs
    domainConstr :: FDExpr -> FDConstr
    domainConstr v@(FDVar _ l u _) = (fd l) <=# v /\ v <=# (fd u)
    domainConstr (FDInt _)         = error "Cannot assert domain of FDInt"
    domainConstr (FDBinExp _ _ _)  = error "Cannot assert domain of FDBinExp"
    domainConstr (FDAbs _)         = error "Cannot assert domain of FDAbs"

--- add Assert command with the given constraint to list
assert :: FDConstr -> SMT
assert c = \cmds -> cmds ++ [Assert $ convertConstr c]

--- add DeclareConst command for given FDVar to list
declareConst :: FDExpr -> SMT
declareConst (FDVar n _ _ _)  = \cmds -> cmds ++ [declareInt n]
declareConst (FDInt _)        = error "Cannot declare FDInt"
declareConst (FDBinExp _ _ _) = error "Cannot declare FDBinExp"
declareConst (FDAbs _)        = error "Cannot declare FDAbs"

--------------------------------------------------------------------------------
-- Converion from FD to SMTLib

-- transforms a FD constraint into a SMT-Lib term
convertConstr :: FDConstr -> Term
convertConstr constr =
  case constr of
    FDTrue               -> termQIdent "true"
    FDFalse              -> termQIdent "false"
    FDRelCon   Neq e1 e2 -> convertConstr $ notC $ e1 =# e2 --SMT-Lib has no `/=`
    FDRelCon   rel e1 e2 -> termQIdentT (relSymb rel) $ map (convertExpr) [e1, e2]
    FDAnd       e1 e2    -> termQIdentT "and" $ map (convertConstr) [e1, e2]
    FDOr        e1 e2    -> termQIdentT "or"  $ map (convertConstr) [e1, e2]
    FDNot           c    -> termQIdentT "not" $ [convertConstr c]
    FDAllDiff      xs    -> termQIdentT "distinct" $ map (convertExpr) xs
    FDSum       vs rel c -> termQIdentT (relSymb rel) [termQIdentT "+" (map convertExpr ((fd 0):vs)), convertExpr c]
    FDScalar cs vs rel v -> convertConstr $ sum (scalarProd cs vs) rel v
    FDCount   v vs rel c -> termQIdentT (relSymb rel) [countSum v vs, convertExpr c]
  where
    relSymb :: FDRel -> String
    relSymb rel = case rel of
      Lt  -> "<"
      Leq -> "<="
      Gt  -> ">"
      Geq -> ">="
      _   -> "=" -- Equ; Neq cannot happen, is caught beforehand
    scalarProd :: [FDExpr] -> [FDExpr] -> [FDExpr]
    scalarProd cs vs = zipWith (*#) cs vs
    countSum :: FDExpr -> [FDExpr] -> Term
    countSum v vs =
      let one  = convertExpr $ fd 1
          zero = convertExpr $ fd 0
          comparisons  = map (\x -> convertConstr $ x =# v) vs
          reifications = map (\x -> termQIdentT "ite" [x, one, zero]) comparisons
      in  termQIdentT "+" reifications

-- transforms a FD expression into a SMT-Lib term
convertExpr :: FDExpr -> Term
convertExpr expr =
  case expr of
    FDVar     n _ _ _   -> termQIdent n
    FDInt     i         -> case i < 0 of
      True  -> termQIdentT "-" [termSpecConstNum (-i)]
      False -> termSpecConstNum i
    FDBinExp  op e1 e2  -> termQIdentT (opSymb op) $ map (convertExpr) [e1, e2]
    FDAbs     e         -> termQIdentT "abs" $ [convertExpr e]
  where
    opSymb op = case op of
      Plus  -> "+"
      Minus -> "-"
      Times -> "*"


--------------------------------------------------------------------------------
-- Conversion from SMTLib to FD

convertCmdResponseToFD :: CmdResponse -> [FDExpr]
convertCmdResponseToFD res =
  case res of
    CmdGetModelResponse mr  -> map convertModelResponseToFD mr
    CmdGetValueResponse vr  -> map convertValueResponseToFD vr
    _                       -> []

convertModelResponseToFD :: ModelResponse -> FDExpr
convertModelResponseToFD (MRDefineFun funDef) = convertFunDefToFD funDef

convertFunDefToFD :: FunDef -> FDExpr
convertFunDefToFD (FunDef n _ _ v) = FDVar n _ _ (getVal v)
  where
    getVal :: Term -> Int
    getVal t = case t of
      (TermSpecConstant (SpecConstantNumeral i)) -> i
      (TermQualIdentifierT (QIdentifier (ISymbol "-")) [TermSpecConstant (SpecConstantNumeral i)]) -> -i
      _ -> 0

convertValueResponseToFD :: ValuationPair -> FDExpr
convertValueResponseToFD (ValuationPair t1 t2) = FDVar (getName t1) _ _ (getVal t2)
  where
    getName t = case t of
      (TermQualIdentifier (QIdentifier (ISymbol s))) -> s
      _ -> error "name missing"
    getVal :: Term -> Int
    getVal t = case t of
      (TermSpecConstant (SpecConstantNumeral i)) -> i
      (TermQualIdentifierT (QIdentifier (ISymbol "-")) [TermSpecConstant (SpecConstantNumeral i)]) -> -i
      _ -> 0

------------------------------------------------------------------------------
--- Library for finite domain constraint solving.
---
--- An FD problem is specified as an expression of type `FDConstr`
--- using the constraints and expressions offered in this library.
--- FD variables are created by the operation `domain`.
--- An FD problem is solved by calling `solveFD` with labeling options,
--- the FD variables whose values should be included in the output,
--- and a constraint. Hence, the typical program structure to solve
--- an FD problem is as follows:
---
---     main :: [Int]
---     main =
---       let fdvars  = take n (domain u o)
---           fdmodel = {description of FD problem}
---        in solveFD {options} fdvars fdmodel
--- 
--- where `n` are the number of variables and `[u..o]` is the
--- range of their possible values.
---
--- Note that this library defines the operation to describe an FD problem.
--- The actual `solveFD` operations are defined in the solver libraries
--- which also exports this library.
---
--- @author Michael Hanus, Tom Hueser
--- @version March 2016
------------------------------------------------------------------------------

module XFD.FD where

import List (nubBy)
import qualified Integer as I (abs)

infixl 7 *#
infixl 6 +#, -#
infix  4 =#, /=#, <#, <=#, >#, >=#
infixr 3 /\
infixr 2 \/


--------------------------------------------------------------------------------
-- Operations to construct basic constraints.

--- Returns an infinite list of named FD variables with given domain
--- @param prefix - prefix to use for generated names
--- @param lo     - minimum value for all variables in xs
--- @param up     - maximum value for all variables in xs
domainWPrefix :: String -> Int -> Int -> [FDExpr]
domainWPrefix prf lo up = genFDVars (nameList prefix 1) lo up
  where
    prefix :: String
    prefix = prf ++ (num lo) ++ "_" ++ (num up) ++ "_"

    num :: Int -> String
    num i = if i < 0 then "-" ++ (show (- i)) else show i

    nameList :: String -> Int -> [String]
    nameList pr i = (pr ++ (show i)) : nameList pr (i+1)

    genFDVars :: [String] -> Int -> Int -> [FDExpr]
    genFDVars (n:ns) l u = FDVar n l u _ : genFDVars ns l u
    genFDVars []     _ _ = [] -- can never choose this rule

--- Returns infinite list of named FD variables with given domain and default
--- prefix `"fdv_"`
--- @param lo     - minimum value for all variables in xs
--- @param up     - maximum value for all variables in xs
domain :: Int -> Int -> [FDExpr]
domain = domainWPrefix "fdv_"

--- Ints as FDExpr
fd :: Int -> FDExpr
fd x = FDInt x

--- Negated FD constraint
notC :: FDConstr -> FDConstr
notC c = FDNot c

--- Addition of FD expressions.
(+#) :: FDExpr -> FDExpr -> FDExpr
x +# y = FDBinExp Plus x y

--- Subtraction of FD expressions.
(-#) :: FDExpr -> FDExpr -> FDExpr
x -# y = FDBinExp Minus x y

--- Multiplication of FD expressions.
(*#) :: FDExpr -> FDExpr -> FDExpr
x *# y = FDBinExp Times x y

--- Equality of FD expressions.
(=#) :: FDExpr -> FDExpr -> FDConstr
x =# y = FDRelCon Equ x y

--- Disequality of FD expressions.
(/=#) :: FDExpr -> FDExpr -> FDConstr
x /=# y = FDRelCon Neq x y

--- "Less than" constraint on FD expressions.
(<#) :: FDExpr -> FDExpr -> FDConstr
x <# y = FDRelCon Lt x y

--- "Less than or equal" constraint on FD expressions.
(<=#) :: FDExpr -> FDExpr -> FDConstr
x <=# y = FDRelCon Leq x y

--- "Greater than" constraint on FD expressions.
(>#) :: FDExpr -> FDExpr -> FDConstr
x ># y = FDRelCon Gt x y

--- "Greater than or equal" constraint on FD expressions.
(>=#) :: FDExpr -> FDExpr -> FDConstr
x >=# y = FDRelCon Geq x y

--- The always satisfied FD constraint.
true :: FDConstr
true = FDTrue

--- The always unsatisfied constraint
false :: FDConstr
false = FDFalse

--- Conjunction of FD constraints.
(/\) :: FDConstr -> FDConstr -> FDConstr
c1 /\ c2 = FDAnd c1 c2

(\/) :: FDConstr -> FDConstr -> FDConstr
c1 \/ c2 = FDOr c1 c2

--- Conjunction of a list of FD constraints.
andC :: [FDConstr] -> FDConstr
andC = foldr (/\) true

--- Disnjunction of a list of FD constraints.
orC :: [FDConstr] -> FDConstr
orC = foldr (\/) false

--- Maps a constraint abstraction to a list of FD constraints and joins them.
allC :: (a -> FDConstr) -> [a] -> FDConstr
allC c = andC . map c

--- "All different" constraint on FD variables.
--- @param xs - list of FD variables
--- @return satisfied if the FD variables in the argument list xs
---         have pairwise different values.
allDifferent :: [FDExpr] -> FDConstr
allDifferent vs = FDAllDiff vs

--- Relates the sum of FD variables with some integer of FD variable.
sum :: [FDExpr] -> FDRel -> FDExpr -> FDConstr
sum vs rel v = FDSum vs rel v

--- `(scalarProduct cs vs relop v)` is satisfied if `(sum (cs*vs) relop v)`
--- is satisfied.
--- The first argument must be a list of integers. The other arguments are as
--- in 'sum'.
scalarProduct :: [FDExpr] -> [FDExpr] -> FDRel -> FDExpr -> FDConstr
scalarProduct cs vs rel v = FDScalar cs vs rel v

--- `(count v vs relop c)` is satisfied if `(n relop c)`,
--- where `n` is the number of elements in the
--- list of FD variables `vs` that are equal to `v`, is satisfied.
--- The first argument must be an integer. The other arguments are as
--- in 'sum'.
count :: FDExpr -> [FDExpr] -> FDRel -> FDExpr -> FDConstr
count v vs rel c = FDCount v vs rel c


--- Absolute value of expression.
abs :: FDExpr -> FDExpr
abs e = FDAbs e

--------------------------------------------------------------------------------
-- Abstract types to represent FD constraints as data.

-- Abstract type for FD expressions:
data FDExpr = FDVar String Int Int Int
            | FDInt Int
            | FDBinExp FDOp FDExpr FDExpr
            | FDAbs     FDExpr

data FDOp = Plus | Minus | Times

--- Possible relations between FD values.
--- @cons Equ - Equal
--- @cons Neq - Not equal
--- @cons Lt  - Less than
--- @cons Leq - Less than or equal
--- @cons Gt  - Greater than
--- @cons Geq - Greater than or equal
data FDRel = Equ | Neq | Lt | Leq | Gt | Geq

-- Abstract type for FD constraints:
data FDConstr = FDTrue
              | FDFalse
              | FDRelCon  FDRel FDExpr FDExpr
              | FDAnd     FDConstr FDConstr
              | FDOr      FDConstr FDConstr
              | FDNot     FDConstr
              | FDAllDiff [FDExpr]
              | FDSum     [FDExpr]          FDRel FDExpr
              | FDScalar  [FDExpr] [FDExpr] FDRel FDExpr
              | FDCount   FDExpr   [FDExpr] FDRel FDExpr

--------------------------------------------------------------------------------

--- Get the name of a FDVar.
getFDVarName :: FDExpr -> String
getFDVarName var = case var of
  FDVar n _ _ _ -> n
  _ -> error "non FDVar has no name"

--- Get value (possibly an unbound variable) of an FD expression.
getFDVal :: FDExpr -> Int
getFDVal var = case var of
  FDVar _ _ _ v -> v
  FDInt i       -> i
  FDBinExp fdop fde1 fde2 -> (arithOp fdop) (valOf fde1) (valOf fde2)
  FDAbs e       -> getFDVal e
 where
  valOf e = case e of
    FDInt i -> i
    FDBinExp op e1 e2 -> (arithOp op) (valOf e1) (valOf e2)
    _ -> error $ "FD variable or value expected but FD expression found:\n"++
                 show e
  arithOp Plus = (+)
  arithOp Minus = (-)
  arithOp Times = (*)


--- Compute set of all variables occurring in a constraint.
allFDVars :: FDConstr -> [FDExpr]
allFDVars = (nubBy fdVarEq) . allFDVars'

-- compute (multi-set) of all variables occurring in a constraint:
allFDVars' :: FDConstr -> [FDExpr]
allFDVars' FDTrue  = []
allFDVars' FDFalse = []
allFDVars' (FDRelCon _ fde1 fde2)     = allEFDVars fde1 ++ allEFDVars fde2
allFDVars' (FDAnd      c1 c2)         = allFDVars' c1 ++ allFDVars' c2
allFDVars' (FDOr       c1 c2)         = allFDVars' c1 ++ allFDVars' c2
allFDVars' (FDNot c)                  = allFDVars' c
allFDVars' (FDAllDiff fdvars)         = filterVars fdvars
allFDVars' (FDSum fdvars _ fdv)       = filterVars (fdvars ++ [fdv])
allFDVars' (FDScalar cs fdvars _ fdv) = filterVars (cs ++ fdvars ++ [fdv])
allFDVars' (FDCount fdv fdvars _ c)   = filterVars (fdvars ++ [fdv,c])

--- Compute a set of all variables occuring in a list of expressions
filterFDVars :: [FDExpr] -> [FDExpr]
filterFDVars = (nubBy fdVarEq) . filterVars

-- eq relation on FDVars:
fdVarEq :: FDExpr -> FDExpr -> Bool
fdVarEq v1 v2 = case (v1, v2) of
  ((FDVar n1 _ _ _), (FDVar n2 _ _ _)) -> n1 == n2
  _                                    -> False

-- filter variables in a list of FD expressions
filterVars :: [FDExpr] -> [FDExpr]
filterVars = concatMap allEFDVars

allEFDVars :: FDExpr -> [FDExpr]
allEFDVars e = case e of
  FDVar _ _ _ _    -> [e]
  FDInt _          -> []
  FDBinExp _ e1 e2 -> allEFDVars e1 ++ allEFDVars e2
  FDAbs         ex -> allEFDVars ex

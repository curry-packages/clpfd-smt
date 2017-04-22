module XFD.Dimacs.Build where

import List (nub)

import XFD.Dimacs.Types

infixr 3 ./\
infixr 2 .\/

va :: Int -> Boolean
va = Var

no :: Boolean -> Boolean
no = Not

nv :: Int -> Boolean
nv = no . va

(./\) :: Boolean -> Boolean -> Boolean
a ./\ b = case (a, b) of
  (Var _, Var _) -> And [a,b]
  (Var _, Not _) -> And [a,b]
  (Var _, And l) -> And $ a:l
  (Var _, Or  _) -> And [a,b]
  (_    , Var _) -> b ./\ a
  (Not _, Not _) -> And [a,b]
  (Not _, And l) -> And $ a:l
  (Not _, Or  _) -> And [a,b]
  (_    , Not _) -> b ./\ a
  (And p, And q) -> And $ p ++ q
  (And l, Or  _) -> And $ b:l
  (Or  _, Or  _) -> And [a,b]
  (Or  _, And _) -> b ./\ a

(.\/) :: Boolean -> Boolean -> Boolean
a .\/ b = case (a, b) of
  (Var _, Var _) -> Or [a,b]
  (Var _, Not _) -> Or [a,b]
  (Var _, And _) -> Or [a,b]
  (Var _, Or  l) -> Or $ a:l
  (_    , Var _) -> b .\/ a
  (Not _, Not _) -> Or [a,b]
  (Not _, And _) -> Or [a,b]
  (Not _, Or  l) -> Or $ a:l
  (_    , Not _) -> b ./\ a
  (And _, And _) -> Or [a,b]
  (And _, Or  l) -> Or $ a:l
  (Or  p, Or  q) -> Or $ p ++ q
  (Or  _, And _) -> b .\/ a

xor :: Boolean -> Boolean -> Boolean
a `xor` b = (no a ./\ b) .\/ (a ./\ no b)

--------------------------------------------------------------------------------

nnf :: Boolean -> Boolean
nnf (Var x) = Var x
nnf n@(Not f) = case f of
  (Var _)  -> n
  (Not g)  -> nnf g
  (And fs) -> Or  (map (nnf . Not) fs)
  (Or  fs) -> And (map (nnf . Not) fs)
nnf (And fs) = And (map nnf fs)
nnf (Or  fs) = Or  (map nnf fs)

cnf :: Boolean -> Boolean
cnf (Var x) = Or [Var x]
cnf (Not v) = Or [Not v]
cnf (And fs) = And (map cnf fs)
cnf (Or  []) = Or  []
cnf (Or  [f]) = cnf f
cnf (Or  (f1:f2:fs)) = dist (cnf f1) (cnf (Or (f2:fs)))

dist :: Boolean -> Boolean -> Boolean
dist xs ys = case (xs, ys) of
  (And [], _)       -> And []
  (_, And [])       -> And []
  (And [f1], f2)    -> dist f1 f2
  (f1, And [f2])    -> dist f1 f2
  (And (f1:fs), f2) -> And [dist f1 f2, dist (And fs) f2]
  (f1, And (f2:fs)) -> And [dist f1 f2, dist f1 (And fs)]
  (f1, f2)          -> Or  [f1,f2]


flatten :: Boolean -> Boolean
flatten f = case f of
  (And fs) -> And (flattenAnd fs)
  (Or  fs) -> Or  (flattenOr  fs)
  _        -> f

flattenAnd :: [Boolean] -> [Boolean]
flattenAnd cs = case cs of
  []            -> []
  ((And fs):gs) -> flattenAnd (fs ++ gs)
  (f:fs)        -> flatten f : flattenAnd fs

flattenOr :: [Boolean] -> [Boolean]
flattenOr ds = case ds of
  []            -> []
  ((Or  fs):gs) -> flattenOr (fs ++ gs)
  (f:fs)        -> f: flattenOr fs

nubB :: Boolean -> Boolean
nubB bs = case bs of
  (And fs) -> And (map nubB fs)
  (Or  fs) -> Or  (nub fs)
  _        -> bs

toCNF :: Boolean -> Boolean
toCNF = filterTauts . nubB . flatten . cnf . nnf

filterTauts :: Boolean -> Boolean
filterTauts b = case b of
  (And ls)  -> And $ filter (not . isTaut) ls
  _         -> b

isTaut :: Boolean -> Bool
isTaut b = case b of
    (Or ls) -> isTaut' ls
    _       -> False
  where
    isTaut' []     = False
    isTaut' (x:xs) = containsInverted x xs || isTaut' xs
    containsInverted x xs = case x of
      (Var i)       -> nv i `elem` xs
      (Not (Var i)) -> va i `elem` xs
      _             -> False

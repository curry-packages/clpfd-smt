module XFD.Dimacs.FromFD where

import Integer as I

import XFD.FD
import XFD.State
import XFD.Dimacs.Types
import XFD.Dimacs.Build


ilog2 :: Int -> Int
ilog2 n | n>0 = if n<2 then 0 else 1 + ilog2 (n `div` 2)

numBits :: Int -> Int
numBits = (2+) . ilog2

numBitsFor :: Int -> Int -> Int
numBitsFor l u = numBits $ max ((I.abs l)-1) (I.abs u)

dTrue, dFalse :: Boolean
dTrue = va 1
dFalse = nv 1

type DimacsState = (Int, [(String, [Boolean])])
type ConvertState a = State DimacsState a

convertFDVars :: [FDExpr] -> ConvertState ()
convertFDVars fdvs = (mapS_ convertFDVar $ reverse fdvs)

convertFDVar :: FDExpr -> ConvertState ()
convertFDVar var = case var of
  (FDVar n l u _) -> modifyS addVar
    where
      addVar :: DimacsState -> DimacsState
      addVar (i, vs) = (i+neededBits, (n, map va [(i+1)..(i+neededBits)]):vs)
      neededBits :: Int
      neededBits = numBitsFor l u
  _               -> error "should not happen"

type SimplificationState a = State [Boolean] a

convertSimplifications :: [Simplification] -> Int -> ConvertState Boolean
convertSimplifications simps i s@(_, lVars) =
  let bsimps = evalState (mapS (convertSimplification lVars) simps) (map va [i ..])
  in (foldr (./\) dTrue bsimps, s)

convertSimplification :: [(String, [Boolean])] -> Simplification -> SimplificationState Boolean
convertSimplification lVars simp bools = (arithmetic simp lVars) bools

convertFDConstr :: FDConstr -> ConvertState Boolean
convertFDConstr constr = case constr of
    FDTrue               -> returnS $ dTrue
    FDFalse              -> returnS $ dFalse
    FDRelCon   rel e1 e2 -> (convertFDRel rel) e1 e2
    FDAnd       c1 c2    -> liftS2 (./\) (convertFDConstr c1) (convertFDConstr c2)
    FDOr        c1 c2    -> liftS2 (.\/) (convertFDConstr c1) (convertFDConstr c2)
    FDNot           c    -> liftS no $ convertFDConstr c
    _                    -> error $ "can't convert " ++ show constr
    -- FDAllDiff      xs    -> error "all diff vars"
    -- FDSum       vs rel c -> error "sum"
    -- FDScalar cs vs rel v -> error "scalar"
    -- FDCount   v vs rel c -> error "count"

convertFDRel :: FDRel -> FDExpr -> FDExpr -> ConvertState Boolean
convertFDRel rel = case rel of
  Equ -> convertEqual
  _  -> error "not yet implemented"

convertEqual :: FDExpr -> FDExpr -> ConvertState Boolean
convertEqual e1 e2 = case (e1, e2) of
  (FDInt i, FDInt j) -> if i == j then returnS (dTrue) else returnS (dFalse) -- i /= j is not possible
  _                  -> booleanEqual e1 e2

booleanEqual :: FDExpr -> FDExpr -> ConvertState Boolean
booleanEqual e1 e2 = case (e1, e2) of
  (FDInt i, FDVar n _ _ _)       -> getS `bindS` \(_, vars) ->
                                    returnS $ setEq (toBits i)
                                                    (lookupVar n vars)
  (FDVar _ _ _ _, FDInt _)       -> booleanEqual e2 e1
  (FDVar n _ _ _, FDVar m _ _ _) -> getS `bindS` \(_, vars) ->
                                    returnS $ setEq (lookupVar m vars)
                                                    (lookupVar n vars)
  _                              -> error "should not happen"


setEq :: [Boolean] -> [Boolean] -> Boolean
setEq xss yss = case (xss, yss) of
    ([x], [y])        -> eqVars x y
    (xs@(_:_), [y])   -> foldr1 (./\) $ zipWith (eqVars) xs (replicate (length xs) y)
    ([y], xs@(_:_))   -> setEq xs [y]
    ((x:xs), (y:ys))  -> eqVars x y ./\ setEq xs ys
    _ -> error "empty lists should not happen"
  where
    eqVars :: Boolean -> Boolean -> Boolean
    eqVars x y = x ./\ y .\/ no x ./\ no y


toBits :: Int -> [Boolean]
toBits i
  | i < 0 = negative (toBits (-i)) True
  | otherwise = toBits' i ++ [dFalse]
  where
    toBits' :: Int -> [Boolean]
    toBits' j = case j of
      0 -> [dFalse]
      1 -> [dTrue]
      _ -> let b = if j `mod` 2 == 1 then dTrue else dFalse
           in  b : toBits' (j `div` 2)
    negative []     _ = []
    negative (x:xs) b = case (x, b) of
      (Var _, True)         -> dTrue  : negative xs False
      (Var _, False)        -> dFalse : negative xs False
      (Not (Var _), True)   -> dFalse : negative xs True
      (Not (Var _), False)  -> dTrue  : negative xs False
      _ -> error "XFD.Dimacs.toBits, in where: negative only takes Var"

lookupVar :: String -> [(String, [Boolean])] -> [Boolean]
lookupVar v vs = maybe [] id (lookup v vs)

convert :: ConvertState a -> (a, DimacsState)
convert state = runState state (1, [])

-- fullAdder :: Boolean -> Boolean -> Boolean -> Boolean -> Boolean
-- fullAdder a b c s = (no s .\/ add) ./\ (no add .\/ s)
--   where
--     add = (no a ./\ b ./\ no c) .\/ (a ./\ no b ./\ no c)
--           .\/ (a ./\ no b ./\ c) .\/ (a ./\ b ./\ c)

fullAdder :: Boolean -> Boolean -> Boolean -> Boolean -> Boolean -> Boolean --(Boolean, Boolean)
fullAdder a b cin s cout = res ./\ carry --(res, carry)
  where
    res = (s .\/ a .\/ b .\/ no cin)
      ./\ (s .\/ a .\/ no b .\/ cin)
      ./\ (s .\/ no a .\/ b .\/ cin)
      ./\ (s .\/ no a .\/ no b .\/ no cin)
      ./\ (no s .\/ a .\/ b .\/ cin)
      ./\ (no s .\/ a .\/  no b .\/ no cin)
      ./\ (no s .\/ no a .\/ b .\/ no cin)
      ./\ (no s .\/ no a .\/ no b .\/ cin)
    carry = (cout .\/ a .\/ no b .\/ no cin)
        ./\ (cout .\/ no a .\/ b .\/ no cin)
        ./\ (cout .\/ no a .\/ no b .\/ cin)
        ./\ (cout .\/ no a .\/ no b .\/ no cin)
        ./\ (no cout .\/ a .\/ b .\/ cin)
        ./\ (no cout .\/ a .\/ b .\/ no cin)
        ./\ (no cout .\/ a .\/ no b .\/ cin)
        ./\ (no cout .\/ no a .\/ b .\/ cin)

arithmetic :: Simplification -> [(String, [Boolean])] -> [Boolean] -> (Boolean, [Boolean])
arithmetic (SimplifyAdd z x y) vars = adder z x y vars
arithmetic (SimplifySub _ _ _) _ = error "XFD.Dimacs.arithmetic: Subtractions not yet supported"
arithmetic (SimplifyMul _ _ _) _ = error "XFD.Dimacs.arithmetic: Multiplications not yet supported"

-- add :: [Boolean] -> [Boolean] -> [Boolean] -> [Boolean] -> Boolean -> Boolean
-- add zss xss yss c cin = case (zss, xss, yss) of
--   ([zm,zn], [xn], [yn]) -> fullAdder xn yn cin zn zm --let (r, ca) = fullAdder xn yn cin zn zm in r ./\ ca
--   ([zn], [xn], [yn]) ->     fullAdder xn yn cin zn (head c)--let (r, ca) = fullAdder xn yn cin zn (head c) in r ./\ ca
--   ((z:zs@(_:_)), (x:xs@(_:_)), (y:ys@(_:_))) -> fullAdder x y cin z (head c)./\ add zs xs ys (tail c) (head c) --let (r, ca) = fullAdder x y cin z (head c)
--                               -- in (r ./\ ca) ./\ add zs xs ys (tail c) (head c)
--   _ -> error $ "XFD.Dimacs.adder where add: lists mismatched" ++ show (zss,xss,yss)

adder :: FDExpr -> FDExpr -> FDExpr -> [(String, [Boolean])] -> [Boolean] -> (Boolean, [Boolean])
adder vz vx vy lVars vars = case (vz,vx,vy) of
    (FDVar zn _ _ _, FDVar xn _ _ _, FDVar yn _ _ _) -> adder' zn xn yn
    _ -> error $ "XFD.Dimacs.adder: found non FDVar in " ++ show (vz,vx,vy)
  where
    adder' :: String -> String -> String -> (Boolean, [Boolean])
    adder' zn xn yn =
      let z = lookupVar zn lVars
          (x,y) = padd (lookupVar xn lVars) (lookupVar yn lVars) -- TODO: hier kommt eine zu kurze liste raus
          lz = (length z)
          c = (take lz vars)
      in (add z x y c dFalse, drop lz vars)

    add :: [Boolean] -> [Boolean] -> [Boolean] -> [Boolean] -> Boolean -> Boolean
    add zss xss yss c cin = case (zss, xss, yss) of
      ([zm,zn], [xn], [yn]) -> fullAdder xn yn cin zn zm --let (r, ca) = fullAdder xn yn cin zn zm in r ./\ ca
      ([zn], [xn], [yn]) ->    fullAdder xn yn cin zn (head c) --let (r, ca) = fullAdder xn yn cin zn (head c) in r ./\ ca
      ((z:zs@(_:_)), (x:xs@(_:_)), (y:ys@(_:_))) -> fullAdder x y cin z (head c)./\ add zs xs ys (tail c) (head c) --let (r, ca) = fullAdder x y cin z (head c)
                                  -- in (r ./\ ca) ./\ add zs xs ys (tail c) (head c)
      _ -> error $ "XFD.Dimacs.adder where add: lists mismatched" ++ show (zss,xss,yss)

    padd :: [Boolean] -> [Boolean] -> ([Boolean], [Boolean])
    padd x y | lx == ly = (x, y)
             | lx < ly  = (padd' x, y)
             | lx > ly  = (x, padd' y)
      where
        lx = length x
        ly = length y
        ld = I.abs (lx - ly)
        padd' []  = []
        padd' [v] = replicate ld v
        padd' (v:vs@(_:_)) = v : padd' vs

--------------------------------------------------------------------------------

data Simplification = SimplifyAdd FDExpr FDExpr FDExpr
                    | SimplifySub FDExpr FDExpr FDExpr
                    | SimplifyMul FDExpr FDExpr FDExpr

-- extra code gen, number of additional vars, simplified a
type Simplified a = ([Simplification], Int, a) --TODO: add list of new FDVars

simplifyConstr :: FDConstr -> Simplified FDConstr
simplifyConstr constr = simpleC ([], 0, constr)
  where
    simpleC :: Simplified FDConstr -> Simplified FDConstr
    simpleC p = case p of
      (_, _, FDTrue)  -> p
      (_, _, FDFalse) -> p
      (s, i, FDNot c) -> let (s', i', c') = simpleC (s, i, c) in (s', i', notC c')
      (s, i, FDAnd a b) -> let (s1, i1, a') = simpleC (s, i, a)
                               (s2, i2, b') = simpleC (s1, i1, b)
                           in (s2, i2, a' /\ b')
      (s, i, FDOr  a b) -> let (s1, i1, a') = simpleC (s, i, a)
                               (s2, i2, b') = simpleC (s1, i1, b)
                           in (s2, i2, a' \/ b')
      (s, i, FDRelCon r a b) -> let (s1, i1, a') = simpleE (s, i, a)
                                    (s2, i2, b') = simpleE (s1, i1, b)
                             in (s2, i2, FDRelCon r a' b')

      -- AllDifferent, Count, Sum, Scalar
      (_, _, c) -> error $ "XFD.Dimacs.simplifyConstr where simpleC: not yet implemented for " ++ show c

    simpleE :: Simplified FDExpr -> Simplified FDExpr
    simpleE p = case p of
      (s, n, FDBinExp op a b) -> case (a,b) of
          (FDInt i, FDInt j) -> let (o,_) = oc op in (s, n, fd (i `o` j))
          _  -> let (s1, n1, a') = simpleE (s, n, a)
                    (s2, n2, b') = simpleE (s1, n1, b)
                    (o,c) = oc op
                    ab@(FDVar _ l u _) = newDomainVar o a' b' (length s2)
                    n3 = (numBitsFor l u)
                    s' = c ab a' b'
                in (s':s2, n2+n3, ab)
      (_, _, FDVar _ _ _ _) -> p
      (_, _, FDInt _)       -> p

    oc :: FDOp -> ((Int -> Int -> Int), (FDExpr -> FDExpr -> FDExpr -> Simplification))
    oc Plus  = ((+), SimplifyAdd)
    oc Minus = ((-), SimplifySub)
    oc Times = ((*), SimplifyMul)

    newDomainVar :: (Int -> Int -> Int) -> FDExpr -> FDExpr -> Int -> FDExpr
    newDomainVar op a b n = (head . take 1) $ case (a, b) of
      (FDVar an al au _, FDVar bn bl bu _) -> domainWPrefix (prefix an bn) (al `op` bl) (au `op` bu)
      (FDVar an al au _, FDInt i)          -> domainWPrefix (prefix an (show i)) (al `op` i) (au `op` i)
      (FDInt i, FDVar an al au _)          -> domainWPrefix (prefix (show i) an) (al `op` i) (au `op` i)
      _ -> error "XFD.Dimacs.simplifyConstr where newDomainVar: only FDVar or FDInt"
     where
       prefix x y = "sVar_"++(show n)++"_"++x++y

--------------------------------------------------------------------------------

-- extractSolution (n:ns) lVars sol =
extractSolution :: [String] -> [(String, [Boolean])] -> [Boolean] -> [FDExpr]
extractSolution []     _     _   = []
extractSolution (n:ns) lVars sol = case (lookupVar n lVars, sol) of
  ([], _)     -> extractSolution ns lVars sol
  (_, [])     -> []
  (vars, _) -> (FDVar n _ _ (val vars sol)) : extractSolution ns lVars sol
    where
      val l s = getVal ((num (head l))-1) (length l) s
      num var = case var of
        (Var i) -> i
        _       -> error "should not happen"
      getVal l u s = binVal (take u $ drop l s)

binVal :: [Boolean] -> Int
binVal [] = 0
binVal bits@(_:_) = binVal' bits 1
  where
    binVal' []            _   = 0
    binVal' (v:vs)  val = case (v, null vs) of
      (Var _, False)    -> val + (binVal' vs (val*2))
      (Var _, True)     -> (-val)
      (Not (Var _), _)  -> binVal' vs (val*2)
      _ -> error "XFD.Dimacs.binVal, in where: binVal' only takes Vars"


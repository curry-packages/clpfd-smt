-- A kid goes into a grocery store and buys four items. The cashier
-- charges $7.11.  The kid pays and is about to leave when the cashier
-- calls the kid back, and says "Hold on, I multiplied the four items
-- instead of adding them; I'll try again...  Gosh, with adding them the
-- price still comes to $7.11"! What were the prices of the four items?

module Grocery where

import XFD.Solvers.SMT.Z3
-- import CLP.FD

grocery :: [Int]
grocery =
  let gs@[a,b,c,d] = take 4 $ domain 0 711
  in solveFDOne [] gs $ andC $
    [ a+#b+#c+#d =# fd 711
    , (a*#b)*#(c*#d) =# fd 711000000
    , sorted gs
    ]

sorted :: [FDExpr] -> FDConstr
sorted []      = true
sorted [_]     = true
sorted (x1:x2:xs)  = x1 <=# x2 /\ sorted (x2:xs)

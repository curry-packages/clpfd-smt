module SendMoreMoney where

import XFD.Solvers.SMT.Z3

sendMoreMoney :: [Int]
sendMoreMoney =
 let xs@[s,e,n,d,m,o,r,y] = take 8 (domain 0 9)

     constraints =
        s ># fd 0 /\
        m ># fd 0 /\
        allDifferent xs /\
           fd 1000  *# s +# fd 100  *# e +# fd 10  *# n +# d
        +# fd 1000  *# m +# fd 100  *# o +# fd 10  *# r +# e
        =# fd 10000 *# m +# fd 1000 *# o +# fd 100 *# n +# fd 10 *# e +# y
 in solveFD [] xs constraints

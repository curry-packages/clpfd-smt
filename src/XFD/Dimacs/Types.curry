module XFD.Dimacs.Types where

data Boolean = Var Int
             | Not Boolean
             | And [Boolean]
             | Or  [Boolean]

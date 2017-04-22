module XFD.Dimacs.Pretty where

import Pretty

import XFD.Dimacs.Types
import XFD.Dimacs.Build

prettyDimacs :: Int -> Boolean -> String
prettyDimacs nv = pPrint . (ppDimacs nv) . toCNF

prettySolution :: Boolean -> String
prettySolution = pPrint . (line <>) . ppCNF

ppDimacs :: Int -> Boolean -> Doc
ppDimacs nv b =
  let nd = case b of
            Or  _ -> 1
            And l -> length l
            _     -> error "Dimacs.Pretty: need formula in CNF"
  in (text "p cnf" <+> int nv <+> int nd) $$ (ppCNF b)

ppCNF :: Boolean -> Doc
ppCNF b = case b of
  Var  i -> int i
  Not  n -> text "-" <> ppCNF n
  And bs -> vsep $ map ppCNF bs
  Or  bs -> (hsep $ map ppCNF bs) <+> text "0"

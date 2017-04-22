--- This module defines a pretty printer for SMT data types in order to
--- show them in the SMT-LIB2 syntax.

module XFD.SMTLib.Pretty (
    showSMTLib
  ) where

import XFD.SMTLib.Types
import Pretty


--- Shows a pretty printed version of a list of SMT-LIB commands.
showSMTLib :: SMTLib -> String
showSMTLib = unlines . map (pPrint . ppCmd)

--- The document `(hsepA f xs)` maps a pretty-printing function `f` to a list
--- `xs` and concatenate the resulting documents horizontally.
hsepA :: (a -> Doc) -> [a] -> Doc
hsepA ppA = hsep . map (ppA)

--- pretty-print a SMT-LIB command.
ppCmd :: Command -> Doc
ppCmd cmd = parens $ case cmd of
  DeclareConst name sort  -> text "declare-fun" <+> text name
                                                <+> text "()" <+> ppSort sort
  Assert       term       -> text "assert" <+> ppTerm term
  CheckSat                -> text "check-sat"
  GetModel                -> text "get-model"
  GetValue     terms      -> text "get-value" <+> parens (hsepA ppTerm terms)
  Echo         string     -> text "echo" <+> (dquotes $ text string)
  Exit                    -> text "exit"
  Pop          level      -> text "pop" <+> int level
  Push         level      -> text "push" <+> int level
  SetLogic     logic      -> text "set-logic" <+> ppLogic logic
  SetOption    option     -> text "set-option" <+> text ":" <> ppOption option

--- pretty-print a logic name (just a string).
ppLogic :: String -> Doc
ppLogic = text

--- pretty-print an option.
ppOption :: Option -> Doc
ppOption opt = case opt of
  ProduceModels b -> text "produce-models" <+> ppBool b

--- pretty-print Boolean values.
ppBool :: Bool -> Doc
ppBool True  = text "true"
ppBool False = text "false"

--- pretty-print a term.
ppTerm :: Term -> Doc
ppTerm term = case term of
  TermSpecConstant    sc        -> ppSpec sc
  TermQualIdentifier  qi        -> ppQi qi
  TermQualIdentifierT qi terms  -> parens $ ppQi qi <+> hsepA (ppTerm) terms

--- pretty-print a qualified identifier.
ppQi :: QualIdentifier -> Doc
ppQi qi = case qi of
  QIdentifier i -> ppIden i

--- pretty-print an identifer
ppIden :: Identifier -> Doc
ppIden i = case i of
  ISymbol s -> text s

--- pretty-print a sort.
ppSort :: Sort -> Doc
ppSort sort = case sort of
  SortId          i       -> ppIden i
  SortIdentifiers i sorts -> parens $ ppIden i <+> hsepA (ppSort) sorts

--- pretty-print a contant.
ppSpec :: SpecConstant -> Doc
ppSpec spec = case spec of
  SpecConstantNumeral i -> int i

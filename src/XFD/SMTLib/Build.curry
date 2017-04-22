--- This module defines some auxiliary operation to construct
--- SMT-LIB formulas.

module XFD.SMTLib.Build where

import XFD.SMTLib.Types

declareConst :: String -> String -> Command
declareConst n s = DeclareConst n (SortId (ISymbol s))

declareInt :: String -> Command
declareInt = flip declareConst "Int"

termSpecConstNum :: Int -> Term
termSpecConstNum i = TermSpecConstant (specConstNum i)

specConstNum :: Int -> SpecConstant
specConstNum i = SpecConstantNumeral i

termQIdent  :: String -> Term
termQIdent sym = TermQualIdentifier (qIdent sym)

termQIdentT :: String -> [Term] -> Term
termQIdentT sym ts = TermQualIdentifierT (qIdent sym) ts

qIdent :: String -> QualIdentifier
qIdent sym = QIdentifier $ ident sym

ident :: String -> Identifier
ident sym = ISymbol sym

--- This module implements a scanner for reading programs and formulas
--- in the SMT-LIB2 syntax.

module XFD.SMTLib.Scanner (
    scan, Token(..)
  ) where

import Numeric
import Data.Char
import Data.List (split)

data Token
    -- identifier, literals
  = Id      String
  | Number  Int
  | Str     String
    -- delimiters
  | LParen
  | RParen
   -- operators
  | OP_Minus
   -- keywords
  | KW_sat
  | KW_unsat
  | KW_unknown
  | KW_model
  | KW_defineFun
  | KW_error
   -- other
  | EOF
  deriving (Eq, Show)

keywords :: [(String, Token)]
keywords =
  [ ("sat"        , KW_sat)
  , ("unsat"      , KW_unsat)
  , ("unknown"    , KW_unknown)
  , ("model"      , KW_model)
  , ("define-fun" , KW_defineFun)
  , ("error"      , KW_error)
  ]

specialOrIdent :: String -> Token
specialOrIdent s = maybe (Id s) id (lookup s keywords)

scan :: String -> [Token]
scan str = case str of
    ""      -> [EOF]
    ('(':s) -> LParen : scan s
    (')':s) -> RParen : scan s
    ('-':s) -> OP_Minus : scan s
    ('"':s) -> scanString s
    cs      -> scan' cs
  where
    scan' :: String -> [Token]
    scan' "" = []
    scan' x@(c:cs) | isDigit  c = scanNum x
                   | isSymbol c = scanSpecialOrIdent x
                   | otherwise  = scan cs

isNumber :: Char -> Bool
isNumber c = isDigit c || c == '-'

isSymbol :: Char -> Bool
isSymbol c = isAlpha c || c `elem` symbols

symbols :: [Char]
symbols = ['-', '_', '!']

scanSpecialOrIdent :: String -> [Token]
scanSpecialOrIdent xs =
  let (ident, rest) = span (\c -> (isAlphaNum c) || (c `elem` symbols)) xs
  in  specialOrIdent ident : scan rest

scanNum :: String -> [Token]
scanNum xs =
  case readInt xs of
    [(num, rest)]  -> Number num : scan rest
    _              -> error "SMTLib.Scanner, scanNum: should not be possible"

scanString :: String -> [Token]
scanString xs =
  let [string, rest] = split (=='"') xs
  in Str string : scan rest

--- This module implements a parser for programs and formulas in the
--- SMT-LIB2 syntax. It is based on the system library `XFD.Parser`
--- which defines deterministic parsing combinators.

module XFD.SMTLib.RDParser where

import XFD.Parser
import XFD.SMTLib.Scanner
import XFD.SMTLib.Types
import XFD.SMTLib.Build


parse :: String -> Either ParseError CmdResponse
parse s = case parseCmdResult $ scan s of
  Left e        -> Left e
  Right ([], x) -> Right x
  Right _       -> Left "incomplete parse"

parseCmdResult :: Parser Token CmdResponse
parseCmdResult =  parseCmdCheckSatResponse
              <|> parseCmdGetValueResponse
              <|> parseCmdGenResponse
              <* terminal EOF

parseCmdCheckSatResponse :: Parser Token CmdResponse
parseCmdCheckSatResponse []     = eof []
parseCmdCheckSatResponse (t:ts) = case t of
  KW_sat     -> yield (CmdCheckSatResponse Sat) ts
  KW_unsat   -> yield (CmdCheckSatResponse Unsat) ts
  KW_unknown -> yield (CmdCheckSatResponse Unknown) ts
  _          -> unexpected t ts

parseCmdGetValueResponse :: Parser Token CmdResponse
parseCmdGetValueResponse []     = eof []
parseCmdGetValueResponse (t:ts) = case t of
  LParen -> CmdGetValueResponse <$> parseGetValueResponse <* terminal RParen $ ts
  _      -> unexpected t ts

parseGetValueResponse :: Parser Token GetValueResponse
parseGetValueResponse = some parseValuePair

parseValuePair :: Parser Token ValuationPair
parseValuePair = terminal LParen *> liftP2 ValuationPair parseTerm parseTerm <* terminal RParen

parseTerm :: Parser Token Term
parseTerm [] = eof []
parseTerm (t:ts) = case t of
  Number i -> yield (termSpecConstNum i) ts
  Id  name -> yield (termQIdent name) ts
  LParen   -> terminal OP_Minus *> (termQIdentT "-" <$> (some parseTerm)) <* terminal RParen $ ts
  _        -> unexpected t ts

parseCmdGenResponse :: Parser Token CmdResponse
parseCmdGenResponse []     = eof []
parseCmdGenResponse (t:ts) = case t of
  LParen -> CmdGenResponse <$> parseErrorResponse <* terminal RParen $ ts
  _      -> unexpected t ts

parseErrorResponse :: Parser Token GenResponse
parseErrorResponse []     = eof []
parseErrorResponse (t:ts) = case t of
  KW_error -> Error <$> parseString $ ts
  _        -> unexpected t ts

parseString :: Parser Token String
parseString [] = eof []
parseString (t:ts) = case t of
  Str string -> yield string ts
  _          -> unexpected t ts

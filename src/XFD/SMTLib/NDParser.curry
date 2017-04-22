--- This module implements a parser for programs and formulas in the
--- SMT-LIB2 syntax. It is based on the system library `Parser`
--- which defines functional logic parsing combinators.

module XFD.SMTLib.NDParser where

import Parser
import XFD.SMTLib.Scanner
import XFD.SMTLib.Types
import XFD.SMTLib.Build

type ParseError = String

parse :: String -> Either ParseError CmdResponse
parse s = let ts  = scan s
              val free
              parseResult = parseCmdResult val ts == []
          in case parseResult of
            False -> Left "incomplete parse"
            True  -> Right val

parseCmdResult :: ParserRep CmdResponse Token
parseCmdResult = (
         parseCmdCheckSatResponse
    <||> parseCmdGetValueResponse
    -- <||> parseCmdGetModelResponse
    <||> parseCmdGenResponse
    ) rsp <*> terminal EOF >>> rsp
  where rsp free

parseCmdCheckSatResponse :: ParserRep CmdResponse Token
parseCmdCheckSatResponse = parseCheckSatResponse csr >>> CmdCheckSatResponse csr
  where csr free

parseCheckSatResponse :: ParserRep CheckSatResponse Token
parseCheckSatResponse =  terminal KW_sat      >>> Sat
                    <||> terminal KW_unsat    >>> Unsat
                    <||> terminal KW_unknown  >>> Unknown

-- GetValueResponse
parseCmdGetValueResponse :: ParserRep CmdResponse Token
parseCmdGetValueResponse = parseGetValueResponse gvr >>> CmdGetValueResponse gvr
  where gvr free

parseGetValueResponse :: ParserRep GetValueResponse Token
parseGetValueResponse =  terminal LParen
                     <*> mSome (\_ -> parseValuePair) vp
                     <*> terminal RParen >>> vp
  where vp free

parseValuePair :: ParserRep ValuationPair Token
parseValuePair =  terminal LParen
              <*> parseTerm t1
              <*> parseTerm t2
              <*> terminal RParen >>> ValuationPair t1 t2
  where t1, t2 free

-- Terms
parseTerm :: ParserRep Term Token
parseTerm = parseTSPC <||> parseTQId

parseTSPC :: ParserRep Term Token
parseTSPC = terminal (Number n) >>> termSpecConstNum n
  where n free

parseTQId :: ParserRep Term Token
parseTQId = terminal (Id name) >>> termQIdent name
  where name free


parseCmdGenResponse :: ParserRep CmdResponse Token
parseCmdGenResponse = parseGenResponse gr >>> CmdGenResponse gr
  where gr free

parseGenResponse :: ParserRep GenResponse Token
parseGenResponse = parseErrorResponse

parseErrorResponse :: ParserRep GenResponse Token
parseErrorResponse = terminal LParen
                 <*> terminal KW_error
                 <*> parseString s
                 <*> terminal RParen >>> Error s
  where s free

parseString :: ParserRep String Token
parseString = terminal (Str s) >>> s
  where s free


-- new star and some parser
mStar :: (() -> ParserRep rep token) -> ParserRep [rep] token
mStar p =    (p ()) x <*> (mStar p) xs >>> (x:xs)
       <||> empty               >>> []         where x,xs free

mSome :: (() -> ParserRep rep token) -> ParserRep [rep] token
mSome p = (p ()) x <*> (mStar p) xs >>> (x:xs)        where x,xs free



-- model responses are no longer used; parsers are kept here in case they might
-- be used again
parseCmdGetModelResponse :: ParserRep CmdResponse Token
parseCmdGetModelResponse = parseGetModelResponse gmr >>> CmdGetModelResponse gmr
  where gmr free

parseGetModelResponse :: ParserRep GetModelResponse Token
parseGetModelResponse = terminal LParen
                    <*> terminal KW_model
                    <*> mStar (\_ -> parseModelResponse) mr
                    <*> terminal RParen >>> mr
  where mr free

parseModelResponse :: ParserRep ModelResponse Token
parseModelResponse = parseFunDef fd >>> MRDefineFun fd
  where fd free

parseFunDef :: ParserRep FunDef Token
parseFunDef = terminal LParen
          <*> terminal KW_defineFun
          <*> terminal (Id name)
          <*> terminal LParen <*> terminal RParen
          <*> terminal (Id sort)
          <*> parseTerm t
          <*> terminal RParen >>> (FunDef name [] (SortId (ISymbol sort)) t)
  where name, sort, t free

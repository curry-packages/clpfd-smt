--- This module defines data types to represent SMT programs and formulas
--- in Curry.

module XFD.SMTLib.Types where

type SMTLib = [Command]

data Command = DeclareConst String Sort
             | Assert Term
             | CheckSat
             | GetModel
             | GetValue [Term]
             | Echo String
             | Exit
             | Pop Int
             | Push Int
             | SetLogic String
             | SetOption Option

data Option = ProduceModels Bool

data Term = TermSpecConstant SpecConstant
          | TermQualIdentifier QualIdentifier
          | TermQualIdentifierT QualIdentifier [Term]

data QualIdentifier = QIdentifier Identifier

data Identifier = ISymbol String

data SpecConstant = SpecConstantNumeral Int

data Sort = SortId Identifier
          | SortIdentifiers Identifier [Sort]

data SortedVar = SV String Sort

data FunDef = FunDef String [SortedVar] Sort Term


-- Response

data CmdResponse = CmdGenResponse GenResponse
                 | CmdCheckSatResponse CheckSatResponse
                 | CmdGetModelResponse GetModelResponse
                 | CmdGetValueResponse GetValueResponse

data GenResponse = Success
                 | Unsupported
                 | Error String

data CheckSatResponse = Sat
                      | Unsat
                      | Unknown

-- Get Model Response

type GetModelResponse = [ModelResponse]

data ModelResponse = MRDefineFun FunDef

-- Get Valuation Pair

type GetValueResponse = [ValuationPair]

data ValuationPair = ValuationPair Term Term

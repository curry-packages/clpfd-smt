--- This module implements some deterministic parsing combinators.

module XFD.Parser where

type Parser token a = [token] -> Either ParseError ([token], a)
type ParseError = String

--- Combine parsers with resulting representation of first one.
(<*) :: Parser token a -> Parser token b -> Parser token a
a <* b = const <$> a <*> b

--- Combine parsers with resulting representation of second one.
(*>) :: Parser token a -> Parser token b -> Parser token b
a *> b = const id <$> a <*> b

(<*>) :: Parser token (a -> b) -> Parser token a -> Parser token b
a <*> b = \ts -> case a ts of
            Left e         -> Left e
            Right (ts', f) -> case b ts' of
              Left e         -> Left e
              Right (ts2, x) -> Right (ts2, f x)

--- Combines two parsers in an alternative manner.
(<|>) :: Parser token a -> Parser token a -> Parser token a
a <|> b = \ts -> case a ts of
  Right (ts', x) -> Right (ts', x)
  Left e         -> case b ts of
    Left e' -> Left $ "parse error: " ++ e' ++ " | " ++ e
    Right (ts2, y) -> Right (ts2, y)

--- Apply unary function `f` to result of parser `p`
(<$>) :: (a -> b) -> Parser token a -> Parser token b
f <$> p = yield f <*> p

--- Apply binary function `f` to results of parsers `p1` and `p2`
liftP2 :: (a -> b -> r) -> Parser token a -> Parser token b -> Parser token r
liftP2 f p1 p2 = \ts -> case p1 ts of
                     Left e         -> Left e
                     Right (ts', x) -> case p2 ts' of
                       Left e -> Left e
                       Right (ts2, y) -> Right (ts2, f x y)

--- A parser with `x` as representation while consuming no tokens.
yield :: a -> Parser token a
yield x ts = Right (ts, x)

--- A parser recognizing a particular terminal symbol.
terminal :: token -> Parser token ()
terminal _ [] = eof []
terminal x (t:ts) = case x == t of
  True  -> Right (ts, ())
  False -> unexpected t ts

--- Returns parse error about unexpected end-of-file
eof :: Parser token a
eof _ = Left "unexpected end-of-file"

--- Returns parse error about unexpected token `t`
unexpected :: token -> Parser token a
unexpected t _ = Left $ "unexpected token " ++ show t

--- A star combinator for parsers. The returned parser
--- repeats zero or more times a parser p and
--- returns the representation of all parsers in a list.
star :: Parser token a -> Parser token [a]
star p = (\ts -> case p ts of
  Left e -> Left e
  Right (ts', x) -> (x:) <$> star p $ ts')
  <|> yield []

--- A some combinator for parsers. The returned parser
--- repeats the argument parser at least once.
some :: Parser token a -> Parser token [a]
some p = \ts -> case p ts of
  Left e -> Left e
  Right (ts', x) -> (x:) <$> star p $ ts'

-- same as <$>
-- liftP :: (a -> r) -> Parser token a -> Parser token r
-- liftP f m = \ts -> let res = m ts
--                    in case res of
--                      Left e         -> Left e
--                      Right (ts', x) -> Right (ts', f x)

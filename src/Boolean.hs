module Boolean where

import Parser
import Grammar

data BoolExpression = BTrue | BFalse
                    | BVar String
                    | BOr  BoolExpression BoolExpression
                    | BAnd BoolExpression BoolExpression
                    | BNot BoolExpression
                    | BExp Expression
                    | REqual BoolExpression BoolExpression
                    | RNotEqual BoolExpression BoolExpression
                    | RGreaterThan BoolExpression BoolExpression
                    | RLessThan BoolExpression BoolExpression
                    | RGreaterThanOrEqualTo BoolExpression BoolExpression
                    | RLessThanOrEqualTo BoolExpression BoolExpression
	                deriving (Show)

relOp :: Parser (BoolExpression -> BoolExpression -> BoolExpression)
relOp = token(accept ">=") >>> (\_ -> RGreaterThanOrEqualTo)
    <|> token(accept "<=") >>> (\_ -> RLessThanOrEqualTo)
    <|> token(literal '>') >>> (\_ -> RGreaterThan)
    <|> token(literal '<') >>> (\_ -> RLessThan)
    <|> token(accept "==") >>> (\_ -> REqual)
    <|> token(accept "!=") >>> (\_ -> RNotEqual)

bLiteral :: Parser BoolExpression
bLiteral = accept "true"  >>> (\_ -> BTrue)
       <|> accept "false" >>> (\_ -> BFalse)

boolOp :: Parser (BoolExpression -> BoolExpression -> BoolExpression)
boolOp = token (accept "&&") >>> (\_ -> BAnd)
     <|> token (accept "||") >>> (\_ -> BOr)


boolExpression :: Parser BoolExpression
boolExpression = token(bFactor) +> boolExpression'
boolExpression' e = boolOp <+> bFactor >>> buildRelOp e +> boolExpression'
                <|> result e

relExpression :: Parser BoolExpression
relExpression = bExpression +> relExpression'
relExpression' e = relOp <+> bExpression >>> buildRelOp e +> relExpression'
                <|> result e               

buildRelOp :: BoolExpression -> 
                ((BoolExpression -> BoolExpression -> BoolExpression)
                , BoolExpression) 
                -> BoolExpression
buildRelOp expressionA (op, expressionB) = op expressionA expressionB

--Consider moving some of this to Parser.hs

bFactor :: Parser BoolExpression
bFactor = relExpression
      <|> bNot <+> bLiteral >>> (\(n, lit) -> n lit)
      <|> bLiteral
      <|> bNot <+> bVar >>> (\(n, lit) -> n lit)
      <|> bVar

bVar :: Parser BoolExpression
bVar = letters >>> BVar

bNot :: Parser (BoolExpression -> BoolExpression)
bNot = token(literal '!') >>> (\_ -> BNot)

bExpression :: Parser BoolExpression
bExpression = token expression >>> BExp
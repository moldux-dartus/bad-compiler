module Main (main) where

import Lib
import Data.Char


main :: IO ()
main = putStrLn "fuck you"

--Types

data Expression = 
    Num Integer
    | Var String
    | Add Expression Expression
    | Sub Expression Expression
    | Mul Expression Expression
    | Div Expression Expression
    deriving (Show)

type Parser a = String -> Maybe (a, String) 

data Assign = Assign String Expression
    deriving Show

--Infixes

--Returns a parser if the predicate is true
infix 7 <=>
(<=>) :: Parser a -> (a -> Bool) -> Parser a  
(parser <=> predicate) input = 
    case parser input of
        Nothing -> Nothing
        Just (a, rest) -> if predicate a 
                            then Just (a, rest) 
                            else Nothing

--Either parser succeeds
infixl 3 <|>
(<|>) :: Parser a -> Parser a -> Parser a 
(parserA <|> parserB) input = 
    case parserA input of
        Nothing -> parserB input
        result -> result

--Both parsers succeed
infixl 6 <+>
(<+>) :: Parser a -> Parser b -> Parser (a, b)
(parserA <+> parserB) input = 
    case parserA input of
        Nothing -> Nothing
        Just (resultA, remainder) -> case parserB remainder of  
            Nothing -> Nothing
            Just (resultB, cs) -> Just ((resultA, resultB), cs)

--Both parsers succeeds; returns only left
infixl 6 <+->
(<+->) :: Parser a -> Parser b -> Parser a
(parserA <+-> parserB) input = 
    case parserA input of
        Nothing -> Nothing
        Just (resultA, remainder) -> case parserB remainder of  
            Nothing -> Nothing
            Just (_, cs) -> Just (resultA, cs)

--Both parsers succeeds; returns only right
infixl 6 <-+>
(<-+>) :: Parser a -> Parser b -> Parser b
(parserA <-+> parserB) input = 
    case parserA input of
        Nothing -> Nothing
        Just (resultA, remainder) -> case parserB remainder of  
            Nothing -> Nothing
            Just (resultB, cs) -> Just (resultB, cs)

--Applies the function (transformer) if the parser succeeds e.g. (char >>> toUpper) "a"
infixl 5 >>>
(>>>) :: Parser a -> (a -> b) -> Parser b 
(parser >>> transformer) input = 
    case parser input of
            Nothing -> Nothing
            Just (resultA, remainder) -> Just ((transformer resultA), remainder)

infix 4 +>
(+>) :: Parser a -> (a -> Parser b) -> Parser b 
(parser +> function) input = 
    case parser input of
        Nothing -> Nothing
        Just (a, cs) -> function a cs

--Parsers

char :: Parser Char
char [] = Nothing
char (x:xs) = Just (x, xs)

digit :: Parser Char
digit = char <=> isDigit

digits :: Parser String
digits = iter digit

number :: Parser Integer
number = literal '-' <-+> digits >>> (\n -> -1 * (read n :: Integer))
        <|> digits >>> (\n -> read n :: Integer)

space :: Parser Char
space = char <=> isSpace

letter :: Parser Char
letter = char <=> isAlpha

literal :: Char -> Parser Char
literal c = char <=> (==c)

letters :: Parser String
letters = iter letter

token :: Parser a -> Parser a
token = (<+-> iterS space)

iter :: Parser Char -> Parser String
iter m = (iterS m) <=> (/="")
iterS m = m <+> iterS m >>> (\(x,y) -> x:y)
        <|> result []

--Operators

addOp :: Parser (Expression -> Expression -> Expression)
addOp = token(literal '+') >>> (\_ -> Add)
    <|> token(literal '-') >>> (\_ -> Sub)

expression :: Parser Expression
expression = token(term) +> expression'
expression' e = addOp <+> term >>> buildOp e +> expression'
                      <|> result e

mulOp :: Parser (Expression -> Expression -> Expression)
mulOp = token(literal '*') >>> (\_ -> Mul)
    <|> token(literal '/') >>> (\_ -> Div)

term :: Parser Expression
term = token(factor) +> term'
term' e = mulOp <+> term >>> buildOp e +> term'
                <|> result e

factor :: Parser Expression
factor = token (literal '(') <-+> token(expression) <+-> token(literal ')')
        <|> number >>> Num
        <|> letters >>> Var

buildOp :: Expression -> ((Expression -> Expression -> Expression), Expression) -> Expression
buildOp expressionA (op, expressionB) = op expressionA expressionB

parse :: String -> Assign
parse s = Assign id expr
    where (id, expr) = case assign s of
                            Nothing -> error "Invalid assignemnt"
                            Just ((a, b), _) -> (a, b)

assign :: Parser (String, Expression)
assign = token(letters) <+-> token(literal '=') <+> expression

result :: a -> Parser a 
result a cs = Just (a, cs)

module Main (main) where

import Lib
import Data.Char


main :: IO ()
main = putStrLn "fuck you"

data Expression = 
    Num Char
    | Add Expression Expression
    | Sub Expression Expression
    deriving (Show)

type Parser a = String -> Maybe (a, String) 

data Assign = Assign Char Expression
    deriving Show

infix 7 <=>
(<=>) :: Parser a -> (a -> Bool) -> Parser a  
(parser <=> predicate) input = 
    case parser input of
        Nothing -> Nothing
        Just (a, rest) -> if predicate a 
                            then Just (a, rest) 
                            else Nothing

infixl 3 <|>
(<|>) :: Parser a -> Parser a -> Parser a 
(parserA <|> parserB) input = 
    case parserA input of
        Nothing -> parserB input
        result -> result

infixl 6 <+>
(<+>) :: Parser a -> Parser b -> Parser (a, b)
(parserA <+> parserB) input = 
    case parserA input of
        Nothing -> Nothing
        Just (resultA, remainder) -> case parserB remainder of  
            Nothing -> Nothing
            Just (resultB, cs) -> Just ((resultA, resultB), cs)

infixl 6 <+->
(<+->) :: Parser a -> Parser b -> Parser a
(parserA <+-> parserB) input = 
    case parserA input of
        Nothing -> Nothing
        Just (resultA, remainder) -> case parserB remainder of  
            Nothing -> Nothing
            Just (_, cs) -> Just (resultA, cs)

infixl 6 <-+>
(<-+>) :: Parser a -> Parser b -> Parser b
(parserA <-+> parserB) input = 
    case parserA input of
        Nothing -> Nothing
        Just (resultA, remainder) -> case parserB remainder of  
            Nothing -> Nothing
            Just (resultB, cs) -> Just (resultB, cs)

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

char :: Parser Char
char [] = Nothing
char (x:xs) = Just (x, xs)

digit :: Parser Char
digit = char <=> isDigit

space :: Parser Char
space = char <=> isSpace

letter :: Parser Char
letter = char <=> isAlpha

literal :: Char -> Parser Char
literal c = char <=> (==c)

parse :: String -> Assign
parse s = Assign id expr
    where (id, expr) = case assign s of
                            Nothing -> error "Invalid assignemnt"
                            Just ((a, b), _) -> (a, b)

assign :: Parser (Char, Expression)
assign = letter <+-> literal '=' <+> expression

term :: Parser Expression
term = digit >>> Num

result :: a -> Parser a 
result a cs = Just (a, cs)

expression :: Parser Expression
expression = term +> expression'

expression' e = addOp <+> term >>> buildOp e +> expression'
                <|> result e

addOp :: Parser (Expression -> Expression -> Expression)
addOp = literal '+' >>> (\_ -> Add)
    <|> literal '-' >>> (\_ -> Sub)

buildOp :: Expression -> ((Expression -> Expression -> Expression), Expression) -> Expression
buildOp expressionA (op, expressionB) = op expressionA expressionB
module Parser 
where

import Data.Char

type Parser a = String -> Maybe (a, String) 

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

result :: a -> Parser a 
result a cs = Just (a, cs)

notSpace :: Parser Char
notSpace = char <=> (not . isSpace)

acceptWord :: String -> Parser String
acceptWord w = token (letters <=> (==w))

accept :: String -> Parser String
accept w = token ((iter notSpace) <=> (==w))
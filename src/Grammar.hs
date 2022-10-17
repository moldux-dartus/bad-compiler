module Grammar 
where

import Parser

data Expression = 
    Num Integer
    | Var String
    | Add Expression Expression
    | Sub Expression Expression
    | Mul Expression Expression
    | Div Expression Expression
    deriving (Show)

data Assign = Assign String Expression
    deriving Show

data Program = Program Block deriving (Show)

type Block = [Statement]

data Statement = Statement Assign 
                         | Branch Condition Block
                         | Branch2 Condition Block Block
                         | While Condition Block
--                         |DoUntil Condition (reader exercise)
                         deriving (Show)

type Condition = String

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

statement :: Parser Statement
statement = assign >>> Statement 
                    <|> ifelse 
                    <|> ifthen 
                    <|> while

ifthen :: Parser Statement
ifthen = accept "if" <-+> condition <+> block <+-> accept "end" >>> buildBranch
            where buildBranch (c, b) = Branch c b 

ifelse :: Parser Statement
ifelse = accept "if" 
            <-+> condition <+> block 
            <+-> accept "else" <+> block 
            <+-> accept "end" >>> buildBranch
    where buildBranch ((c, b1), b2) = Branch2 c b1 b2

while :: Parser Statement
while = accept "while" <-+> condition <+> block <+-> accept "end" >>> buildWhile
    where buildWhile (c, b) = While c b

condition = tempPlaceHolder

tempPlaceHolder :: Parser String
tempPlaceHolder = token letters <=> (\x -> not $ any (==x) keywords)
    where keywords = ["if", "else", "end", "while", "until"]

block :: Parser Block
block = iterS statement

program :: Parser Program
program = block <+-> accept "end" >>> Program

assign :: Parser Assign
assign = token(letters) <+-> token(literal '=') <+> expression >>> (\(x,y) -> Assign x y)

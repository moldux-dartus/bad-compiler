module Main (main) where

import Lib
import Data.Char
import System.Environment
import Parser
import Grammar
import Generator

main :: IO ()
main = getArgs >>= e . head

parse :: String -> Assign
parse s = Assign id expr
    where (id, expr) = case assign s of
                            Nothing -> error "Invalid assignemnt"
                            Just ((a, b), _) -> (a, b)

p = putStrLn . show . parse

e = putStrLn . emit . parse

--(\(Assign _ rest) -> rest)

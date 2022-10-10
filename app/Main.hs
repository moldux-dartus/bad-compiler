module Main (main) where

import Lib
import Data.Char
import System.Environment
import Parser
import Grammar

main :: IO ()
main = getArgs >>= p . head

parse :: String -> Assign
parse s = Assign id expr
    where (id, expr) = case assign s of
                            Nothing -> error "Invalid assignemnt"
                            Just ((a, b), _) -> (a, b)

p = putStrLn . show . parse
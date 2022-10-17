module Main (main) where

import Lib
import Data.Char
import System.Environment
import Parser
import Grammar
import Generator
import Boolean

main :: IO ()
main = undefined --getArgs >>= e . head

parse :: String -> Program
parse s = case program s of
            Nothing -> error "Invalid program"
            Just (a, b) -> a 

p = putStrLn . show . parse

--e = putStrLn . emit . parse

--

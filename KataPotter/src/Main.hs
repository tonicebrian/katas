module Main where

import System.Environment

import KattaPotter

main :: IO()
main = do
    args <- getArgs
    putStrLn $ show $ price $ (map (read) args :: [Int])


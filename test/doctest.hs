module Main where

import Test.DocTest

main :: IO ()
main = doctest ["src/Parser.hs", "src/Enumerator.hs"]

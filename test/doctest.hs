module Main where

import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Regex/Parser.hs", "src/Regex/Enumerator.hs"]

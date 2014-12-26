{- Simple example that demonstrates how regex enumerator can be used together
 - with QuickCheck. -}

import Test.QuickCheck
import Regex.Enumerator
import Control.Monad (forM)

-- Define palindrome type
newtype Palin = Palin String deriving Show

-- Define generator for palindrome
instance Arbitrary Palin where
  arbitrary = do x <- growingElements [1 .. 100]
                 let Right l = enumerate "((aba)*)(c+)\\1e?\\1\\3\\1"
                 return . Palin $ l !! x

-- Function that we would like to make sure is correct.
-- palindrome checks if string, passed in as paramater, is created from
-- alternating characters 'a' and 'b'. For example "abba" is OK and
-- "abab" is not OK.
palindrome :: String -> Bool
palindrome word = take half word == reverse (drop half word)
  where half = length word `div` 2

-- Property of the function: must say True to all Palin
palinProp :: Palin -> Bool
palinProp (Palin x) = palindrome x == True

-- Fixed function
palindrome' :: String -> Bool
palindrome' word = take half word == reverse (drop (half + rem) word)
  where half = length word `div` 2
        rem  = length word `mod` 2

-- Try again
palinProp' :: Palin -> Bool
palinProp' (Palin x) = palindrome' x == True

-- Run this thingy and show first one fails and second one is OK
main = do
  putStrLn "==========================================="
  putStrLn "Testing first implementation ten times ..."
  putStrLn "-------------------------------------------"
  forM [1 .. 5] $ \x -> do quickCheck palinProp
  putStrLn "\n==========================================="
  putStrLn "Testing second implementation ten times ..."
  putStrLn "-------------------------------------------"
  forM [1 .. 5] $ \x -> do quickCheck palinProp'
  putStrLn "\nDONE"

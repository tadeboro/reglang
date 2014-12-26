{- |
Module      :  $Header$
Description :  Parse regular expression
Copyright   :  Aleš Omerzel, Tadej Borovšak
License     :  GPL-3

Maintainer  :  omerzel.vales@gmail.com
Stability   :  unstable

Parse string representation of regular expresion into representation, used
by enumerator.

Parser currently operates on ASCII strings and supports operators ".", "?",
"*", "+", "()" and "|".
-}
module Regex.Parser
  ( Rexp(..)
  , parseRexp)
  where

import Regex.Parser.Internal
import Text.ParserCombinators.Parsec (eof)
import Text.Parsec.Prim (runParser)
import Control.Applicative ((<*))

-- | Parse string representation of regular expression into tree form.
-- If an error occurs during parsing, location of error is returned as
-- string. If parsing is successful, parser returns tuple of regular
-- expression and number of groups that have been encountered.
--
-- >>> parseRexp "Abcd"
-- Right (Cat (Cat (Cat (Sym 'A') (Sym 'b')) (Sym 'c')) (Sym 'd'),0)
--
-- >>> parseRexp "a(bc)?d"
-- Right (Cat (Cat (Sym 'a') (Alt Eps (Group 0 (Cat (Sym 'b') (Sym 'c'))))) (Sym 'd'),1)
--
-- >>> parseRexp "a(b(c(d)))"
-- Right (Cat (Sym 'a') (Group 0 (Cat (Sym 'b') (Group 1 (Cat (Sym 'c') (Group 2 (Sym 'd')))))),3)
--
-- >>> parseRexp "ab+c"
-- Right (Cat (Cat (Sym 'a') (Cat (Sym 'b') (Clo (Sym 'b')))) (Sym 'c'),0)
--
-- >>> parseRexp "a|"
-- Left "Error at line 1, column 3."
parseRexp :: String -> Either String (Rexp, Int)
parseRexp input =
  case runParser exprP 0 "" input of
       Left e -> Left $ errMsg e
       Right r -> Right r

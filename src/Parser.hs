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
module Parser
  ( Rexp(..)
  , parseRexp)
  where

import Parser.Internal
import Text.ParserCombinators.Parsec (parse, eof)
import Control.Applicative ((<*))

-- | Parse string representation of regular expression into tree form.
-- If an error occurs during parsing, location of error is returned as
-- string.
--
-- >>> parseRexp "Abcd"
-- Right (Cat (Cat (Cat (Sym 'A') (Sym 'b')) (Sym 'c')) (Sym 'd'))
--
-- >>> parseRexp "a(bc)?d"
-- Right (Cat (Cat (Sym 'a') (Alt Eps (Group (Cat (Sym 'b') (Sym 'c'))))) (Sym 'd'))
--
-- >>> parseRexp "ab+c"
-- Right (Cat (Cat (Sym 'a') (Cat (Sym 'b') (Clo (Sym 'b')))) (Sym 'c'))
--
-- >>> parseRexp "a|"
-- Left "Error at line 1, column 3."
parseRexp :: String -> Either String Rexp
parseRexp input =
  case parse (exprP <* eof) "(Test)" input of
       Left e -> Left $ errMsg e
       Right r -> Right r

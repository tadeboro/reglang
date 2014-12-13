{- |
Module      :  $Header$
Description :  Parse regular expression
Copyright   :  Aleš Omerzel, Tadej Borovšak
License     :  GPL-3

Maintainer  :  omerzel.vales@gmail.com
Stability   :  unstable

Parse string representation of regular expresion into representation, used
by enumerator.
-}
module Parser (parseRexp) where

import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Applicative ((<*))

import Enumerator (Rexp(..))


-- Characters that can be used in regular expressions
charClass = " !\"#$%&'()*+,-./0123456789:;<=>?" ++
            "@ABCDEFGHIJKLMNOPQRSTUVWXZY[\\]^_" ++
            "`abcdefghijklmnopqrstuvwxzy{|}~"
anyExpr = foldl1 Alt $ map Sym charClass

-- Parser (and grammar)
-- Regexp: expr -> or
exprP = altP

-- Alternation: or -> and | and '|' or
altP = catP `chainl1` op
  where op = char '|' >> return Alt

-- Catenation: and -> rep | rep and
catP = repP `chainl1` op
  where op = return Cat

-- Repetitions: rep -> base '*' | base '?' | base '+'
repP = try zeromoreP <|> try zerooneP <|> try onemoreP <|> baseP
  where zeromoreP = do l <- baseP
                       char '*'
                       return $ Clo l
        zerooneP  = do l <- baseP
                       char '?'
                       return $ Alt Eps l
        onemoreP  = do l <- baseP
                       char '+'
                       return $ Cat l $ Clo l

-- Base: base -> group | sym | any | esc
baseP = choice [groupP, symP, anyP, escP]

-- Captured group: group -> '(' expr ')'
groupP = between (char '(') (char ')') exprP

-- Symbols in regex
symP = liftM Sym $ noneOf "().|\\?*+"

-- Wildcard character
anyP :: GenParser Char st Rexp
anyP = char '.' >> return anyExpr

-- Escaped character
escP :: GenParser Char st Rexp
escP = char '\\' >> liftM Sym anyChar

-- | Function that 
parseRexp :: String -> Either String Rexp
parseRexp input =
  case parse (exprP <* eof) "(Test)" input of
       Left e -> Left $ "Error parsing regex: " ++ (show e)
       Right r -> Right r

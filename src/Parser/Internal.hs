{- |
Module      :  $Header$
Description :  Internal regular expression parser routins
Copyright   :  Aleš Omerzel, Tadej Borovšak
License     :  GPL-3

Maintainer  :  omerzel.vales@gmail.com
Stability   :  unstable

Implementation details of regular expression parser. This file should not
be imported directly by application writers. Use Parser module instead.

Grammar that parser uses (terminals are in double quotes):

>  expr --> alt
>  alt --> cat | cat "|" alt
>  cat --> rep | cat  --> rep cat
>  rep --> base "*" | base "?" | base "+" | base
>  base --> group | sym | any | esc
>  group --> "(" expr ")"
>  sym --> {any ASCII symbol without "().|\+*?"}
>  any --> "."
>  esc --> "\" {any from "0123456789"} | "\" {any symbol from "().|\+*?}

Functions ending in P are direct equivalents of grammar rules.
-}
module Parser.Internal where

import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Char

-- | Constructors for regular expressions.
data Rexp = Nil               -- ^Empty language
          | Eps               -- ^Empty string
          | Sym Char          -- ^Symbol of the alphabet
          | Clo Rexp          -- ^Kleene closure
          | Cat Rexp Rexp     -- ^Catenation
          | Alt Rexp Rexp     -- ^Alternation
          | Group Rexp        -- ^Group operation
          | GroupRef Int      -- ^Group reference operation
            deriving (Show, Eq, Ord)

-- | Characters that have special meaning in regular expressions and need
-- to be escaped in string representation.
specialChars :: [Char]
specialChars = "().|\\?*+"

-- | Characters that can be used unescaped in string representation of
-- regular expression.
normalChars :: [Char]
normalChars = " !\"#$%&',-/0123456789:;<=>" ++
              "@ABCDEFGHIJKLMNOPQRSTUVWXZY[]^_" ++
              "`abcdefghijklmnopqrstuvwxzy{}~"

-- | All chars that can be used in parsed form of regular expression
allChars :: [Char]
allChars = specialChars ++ normalChars

-- | Regular expression that matches any character ("." in string
-- representation).
anyExpr :: Rexp
anyExpr = foldl1 Alt $ map Sym allChars

-- Parsers for parts of regular expression
exprP = altP

altP = catP `chainl1` op
  where op = char '|' >> return Alt

catP = repP `chainl1` op
  where op = return Cat

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

baseP = choice [groupP, symP, anyP, escP]

groupP = do char '('
            e <- exprP
            char ')'
            return $ Group e

symP = liftM Sym $ oneOf normalChars

anyP = char '.' >> return anyExpr

escP = try backrefP <|> charP
  where backrefP = do char '\\'
                      d <- digit
                      return $ GroupRef (digitToInt d)
        charP = do char '\\'
                   c <- oneOf specialChars
                   return $ Sym c

-- | Helper to display error position.
errMsg :: ParseError -> String
errMsg e = "Error at line " ++ line ++ ", column " ++ col ++ "."
  where line = show . sourceLine . errorPos $ e
        col = show . sourceColumn . errorPos $ e

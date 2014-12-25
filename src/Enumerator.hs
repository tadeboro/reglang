{- |
Module      :  $Header$
Description :  Enumerate all words of regular language
Copyright   :  Aleš Omerzel, Tadej Borovšak
License     :  GPL-3

Maintainer  :  omerzel.vales@gmail.com
Stability   :  unstable

Enumerate all words of regular language. This implementation lists shorter
words before longer ones, ensuring that all words of a language are listed
in finite amount of time. Words of the same length are sorted alphabetically.
-}
module Enumerator
  ( enumerate
  , enumerate1
  ) where

import Enumerator.Internal
import Parser

-- | Enumerate regular expression, passed in as a 'String'.
--
-- >>> enumerate "a|t"
-- Right ["a","t"]
--
-- >>> enumerate "T(es)?t"
-- Right ["Tt","Test"]
--
-- >>> let Right tmp = enumerate "a(bc)+a"
-- >>> take 5 tmp
-- ["abca","abcbca","abcbcbca","abcbcbcbca","abcbcbcbcbca"]
--
-- >>> enumerate "?"
-- Left "Error at line 1, column 1."
enumerate :: String -> Either String [String]
enumerate regex = case parseRexp regex of
                  Left err -> Left err
                  Right expr -> Right $ enumerate1 expr

-- | Enumerate regular expression, passed in as a 'Rexp'. Proper
-- representation can be obtained from string using 'parseRexp'
-- function.
--
-- >>> take 5 $ enumerate1 $ Clo (Sym 'b')
-- ["","b","bb","bbb","bbbb"]
enumerate1 :: Rexp -> [String]
enumerate1 = enumNFA . rexp2nfa . deNil

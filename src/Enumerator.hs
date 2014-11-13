module Enumerator 
(
-- * Tip
Rexp
-- * Funkcija
--enumR
)
where

data Rexp = Nil               -- empty language
          | Eps               -- empty string
          | Sym Char          -- symbol of the alphabet
          | Clo Rexp          -- Kleene closure
          | Cat Rexp Rexp     -- catenation
          | Alt Rexp Rexp     -- alternation
            deriving (Show,Eq,Ord)

--enumR :: Rexp -> [String]
--enumR r = undefined

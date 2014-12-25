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

import Parser
import Data.Char

--infixr 5 +++   -- catenate LOL data
infixl 6 \/    -- set union

-- | Type for non-deterministic finite automata is list of states.
type NFA = [State]

-- | Datatype, representing NFA state and transitions from this state.
-- State holds identifier, which makes it possible to compare two states
-- easily; action, which happens when we enter this state; and NFA, which
-- represents all possible transitions from this state.
--
-- States are ordered by action and then by identificator. This is done
-- mainily because this ordering returns strings, produced by automaton,
-- in alphabetical order.
--
-- When using groups and backreferences in regular expressions,
-- alphabetical order is not quarateed and words of automaton will
-- almost certainly be shuffled a bit.
data State = State Ident Action NFA
instance Eq State where
  (State i _ _) == (State i' _ _) = i == i'
instance Ord State where
  (State i c _) <= (State i' c' _) = (c, i) <= (c', i')

-- | NFA states are labeled with integers.
type Ident = Int

-- | Union where elements are sorted by length and lexicographically.	
(\/) :: Ord a => [a] -> [a] -> [a]
[] \/ ys = ys
xs \/ [] = xs
xs@(x:xt) \/ ys@(y:yt) =
  case compare x y of
    LT -> x : xt \/ ys
    EQ -> x : xt \/ yt
    GT -> y : xs \/ yt
-- | Actions that can be executed when we visit states in NFA.
data Action = Symbol Char -- ^Append character to existing word
            | Open        -- ^Add new group to memory
            | Close       -- ^Close most recently activated group
            | Ref Int     -- ^Reference existing group
            | None        -- ^Marker for final state
            deriving (Ord, Eq)

-- | Bypass check function.
bp :: Bool -> NFA -> NFA
bp True ds = ds
bp False _ = []

-- | Function which determines starting state which is actually set as
-- destination state.
rexp2nfa :: Rexp -> NFA
rexp2nfa r = fs \/ bp b ds
  where ds = [State 0 None []];
        (fs, _, b) = rexp2nfa' r 1 ds

-- | Perform single step of conversion from Rexp to NFA. Input of this
-- function is regular expession to convert, next available identificator
-- and destination states. Function returns set of starting states, next
-- available state indetificator and bypass flag that signals, if this
-- regular expression can produce empty string.
rexp2nfa' :: Rexp -> Ident -> NFA -> (NFA, Ident, Bool)
rexp2nfa' Nil          n _  = ([], n, False)
rexp2nfa' Eps          n _  = ([], n, True)
rexp2nfa' (Sym c)      n ds = ([State n (Symbol c) ds], n + 1, False)
rexp2nfa' (GroupRef x) n ds = ([State n (Ref x) ds], n + 1, False)
rexp2nfa' (Cat x y)    n ds = (fs2 \/ bp b2 fs1, n2, b1 && b2)
  where (fs1, n1, b1) = rexp2nfa' y n ds;
        (fs2, n2, b2) = rexp2nfa' x n1 (fs1 \/ bp b1 ds);
rexp2nfa' (Alt x y)    n ds = (fs1 \/ fs2, n2, b1 || b1)
  where (fs1, n1, b1) = rexp2nfa' y n  ds;
        (fs2, n2, b2) = rexp2nfa' x n1 ds;
rexp2nfa' (Clo x)      n ds = (fs, n', True)
  where (fs, n', _) = rexp2nfa' x n (fs \/ ds)
rexp2nfa' (Group x)    n ds = (start, n' + 1, b)
  where end = [State n Close ds]
        (fs, n', b) = rexp2nfa' x (n + 1) end
        start = [State n' Open fs]

-- | Function that takes list of states and joins states with the same
-- action. This removes duplicated word production and accelerates speed
-- of algorithm, since transitions, which would produce exactly the same
-- words, are joined together.
grp :: NFA -> NFA
grp (m @ (State _ c ds) : ms @ (State _ c' ds' : mt)) =
  if c == c'
     then grp (State (-1) c (ds \/ ds') : mt)
     else m : grp ms
grp ms = ms

-- | Function that returns true if automaton accepts current word. This is
-- determined by looking for None action, which indicates transition to
-- end state.
accept :: NFA -> Bool
accept ds = None `elem` [a | (State _ a _) <- ds]

-- Groups represent memory in which we store partial matches in regular
-- expression. Since groups can be nested, we need indices of all currently
-- active groups in which we add characters as we proceed.
--
-- Grups are ordered in reverse: string on i-th position corresponds to
-- contents of (size - i - 1)-th group.
--
--             .--- Number of groups in memory
--             |     .--- Content of each group
--             |     |         .--- Currently active groups
type Groups = (Int, [String], [Int])

-- Add new group to memory. This happens when we encounter start of the
-- group in regular expression. Newly created group is marked as active.
addGroup :: Groups -> (Groups, String)
addGroup (n, gs, as) = ((n + 1, "" : gs, n : as), "")
ng = addGroup (3, ["a", "b", "c"], [2,0])

-- Close group. This action is triggered when we encounter end of group
-- marker in regular expression.
closeGroup :: Groups -> (Groups, String)
closeGroup (n, gs, a : as) = ((n, gs, as), "")

-- Insert character into all active groups.
insert :: Groups -> Char -> Groups
insert g @ (n, gs, as) c = (n, insert' g c [], as)
  where insert' (_, [], _) _ acc = reverse acc
        insert' (n, g : gs, a : as) c acc =
          if n - a - 1 == 0
             then insert' (n - 1, gs, as) c ((g ++ [c]) : acc)
             else insert' (n - 1, gs, a : as) c (g : acc)
atg = insert (3, ["a", "b", "c"], [2,0]) 'c'

-- | Get group
getGroup :: Groups -> Int -> String
getGroup (len, gs, _) n = gs !! (len - n - 1)
gg1 = getGroup (3, ["a", "b", "c"], [2,0]) 0
gg2 = getGroup (3, ["a", "b", "c"], [2,0]) 2

-- | If character is digit it returns group with id of the digit otherwise it returns input character.
-- If character is not a special character then it's added to all grouops it's located.
checkChar :: Groups -> Char -> (Groups, String)
checkChar groups c =
	if isDigit c
	-- if digit then return group
	then (groups, getGroup groups ((digitToInt c) - 1))
	-- if '(' then start new group
	else if c == '<' then addGroup groups
	-- if ')' then close last group
	else if c == '>' then closeGroup groups
	else (insert groups c, [c])

-- test checkChar	
cc = checkChar (2, ["ac", "bc"], [1]) '1'

-- | Generates length ordered list of strings from automata.
visit :: [(String, NFA, Groups)] -> [String]
visit [] = []
visit ((x, ds, groups) : ws) =
  -- x = a word,
  -- ds = destination states, where we can go at this point,
  -- grp helps us to extract those dest. states
  -- [ ] = list of (word+c, ds'), where c is character
  -- visit -> ws+[ ]
  -- xs = list of valid words
  let xs = visit (ws ++ [(x++s',ds',groups') | (State _ c ds') <- grp ds, (groups', s') <- [checkChar groups c] ])
  -- returns list of x
  -- returns list of words (because x = word)
  in if accept ds then x:xs else xs

-- | Removes different production of the same length words. Makes parsing more
-- efficient.
deNil :: Rexp -> Rexp
deNil (Cat x y) =
  case (deNil x, deNil y) of
    (Nil, _) -> Nil
    (_, Nil) -> Nil
    (x', y') -> Cat x' y'
deNil (Alt x y) =
  case (deNil x, deNil y) of
    (Nil, y') -> y'
    (x', Nil) -> x'
    (x', y') -> Alt x' y'
deNil (Clo x) =
  case deNil x of
    Nil -> Eps
    x' -> Clo x'
deNil x = x	
	
-- | Properly initialise visit call. It starts with empty word.
enumNFA :: NFA -> [String]
enumNFA starts = visit [("", starts, (0, [], []))]

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

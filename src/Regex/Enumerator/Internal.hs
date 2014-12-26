{- |
Module      :  $Header$
Description :  Internal workings of regex enumerator
Copyright   :  Aleš Omerzel, Tadej Borovšak
License     :  GPL-3

Maintainer  :  omerzel.vales@gmail.com
Stability   :  unstable

This is private implementation file for enumerator and thus should not be
used in application code. Use Enumerator module.
-}
module Regex.Enumerator.Internal where

import Regex.Parser
import Data.Char
import Data.Maybe

infixl 6 \/

-- | Ordered union operation. It takes 2 ordered lists and returns
-- ordered union. If inputs are not already sorted, sort order of
-- results in undefined.
--
-- NOTE: \\/ operation has the same precedence as addition and higher
-- precedence than : and ++.
(\/) :: Ord a => [a] -> [a] -> [a]
[] \/ ys = ys
xs \/ [] = xs
xs @ (x : xt) \/ ys @ (y : yt) =
  case compare x y of
    LT -> x : xt \/ ys
    EQ -> x : xt \/ yt
    GT -> y : xs \/ yt

-- | Non-deterministic finite automata data type.
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
data State = State Ident Action NFA deriving Show
instance Eq State where
  (State i _ _) == (State i' _ _) = i == i'
instance Ord State where
  (State i c _) <= (State i' c' _) = (c, i) <= (c', i')

-- | NFA states are labeled with integers.
type Ident = Int

-- | Actions that can be executed when we visit states in NFA.
data Action = Accept      -- ^Marker for final state
            | Symbol Char -- ^Append character to existing word
            | Open Int    -- ^Reset nth group for new round of insertions
            | Close Int   -- ^Close nth group
            | Ref Int     -- ^Reference existing group
            deriving (Ord, Eq, Show)

-- | Bypass check function.
bp :: Bool -> NFA -> NFA
bp True ds = ds
bp False _ = []

-- | Convert regular expression into non-deterministic finite automaton.
rexp2nfa :: Rexp -> NFA
rexp2nfa r = fs \/ bp b ds
  where ds = [State 0 Accept []];
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
rexp2nfa' (Alt x y)    n ds = (fs1 \/ fs2, n2, b1 || b2)
  where (fs1, n1, b1) = rexp2nfa' y n  ds;
        (fs2, n2, b2) = rexp2nfa' x n1 ds;
rexp2nfa' (Clo x)      n ds = (fs, n', True)
  where (fs, n', _) = rexp2nfa' x n (fs \/ ds)
rexp2nfa' (Group id x) n ds = (start \/ bp b ds, n' + 1, b)
  where end = [State n (Close id) ds]
        (fs, n', b) = rexp2nfa' x (n + 1) end
        start = [State n' (Open id) (fs \/ bp b end)]

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
-- determined by looking for Accept action, which indicates transition to
-- end state.
accept :: NFA -> Bool
accept ds = Accept `elem` [a | (State _ a _) <- ds]

-- | Memory represent memory in which we store partial matches in regular
-- expression. Since groups can be nested, we need a mask that tells us which
-- groups are currently active.
--
-- If group hasn't been yet activated (it's activation state in NFA hasn't
-- been visited yet), contents is Nothing.
type Memory = ([Maybe String], [Bool])
--              |               '--- Mask of currently active groups
--              '--- Content of each group

-- | Funtion that allocates and initializes memory for n grops. All memory
-- locations (or groups) are initialy set to hold Nothing and to be inactive.
initMemory :: Int -> Memory
initMemory n = (replicate n Nothing, replicate n False)

-- | Function that resets memory of nth group and marks it active.
-- FIXME: This is really bad implementation, but should be OK for start
resetGroup :: Int -> Memory -> Memory
resetGroup n (gs, as) =
  (take n gs ++ [Just ""] ++ drop (n + 1) gs,
   take n as ++ [True]    ++ drop (n + 1) as)

-- | Close group. This action is triggered when we encounter end of group
-- marker in regular expression.
-- FIXME: Again, really bad implementation
closeGroup :: Int -> Memory -> Memory
closeGroup n (gs, as) = (gs, take n as ++ [False] ++ drop (n + 1) as)

-- | Append string onto all active groups.
insert :: String -> Memory -> Memory
insert s (gs, as) = (insert' gs as [], as)
  where insert' [] [] acc = reverse acc
        insert' (g : gs) (a : as) acc =
          if a
            then insert' gs as (Just (fromJust g ++ s) : acc)
            else insert' gs as (g : acc)

-- | Obtain string that belongs to nth group. If nth group has not been
-- activated yet, Nothing is returned.
getGroup :: Int -> Memory -> Maybe String
getGroup n (gs, _) = if n >= length gs || n < 0 then Nothing else gs !! n

-- | Visit function is main engine of enumerator. It first checks if current
-- word is accepted by automaton and inserts it into returned list. The tail
-- of result is then generated recursivelly by applying action to current
-- word and moving to next state.
visit :: [(String, NFA, Memory)] -> [String]
visit [] = []
visit ((x, ds, memory) : ws) = if accept ds then x : xs else xs
  where xs = visit (ws ++ catMaybes [genNextState x s memory | s <- grp ds])

-- | Generate next state (update currently constructed word and memory).
genNextState :: String -> State -> Memory -> Maybe (String, NFA, Memory)
genNextState word (State _ a ds) memory =
  case a of
    Symbol c -> Just (word ++ [c], ds, insert [c] memory)
    Open n   -> Just (word, ds, resetGroup n memory)
    Close n  -> Just (word, ds, closeGroup n memory)
    Accept   -> Just (word, ds, memory) -- Should newer be executed
    Ref n    -> do s <- getGroup n memory
                   return (word ++ s, ds, insert s memory)

-- | deNil removes different production of the same length words. Makes
-- generating more efficient.
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

-- | Properly initialise visit call. It starts with empty word, to which char
-- will be added, and empty memory.
enumNFA :: Int -> NFA -> [String]
enumNFA n starts = visit [("", starts, initMemory n)]

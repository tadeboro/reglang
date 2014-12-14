{- |
Module      :  $Header$
Description :  Enumerate all words of regular language
Copyright   :  Aleš Omerzel, Tadej Borovšak
License     :  GPL-3

Maintainer  :  omerzel.vales@gmail.com
Stability   :  unstable

Enumerate all words of regular language. This implementation lists shorter
words before longer ones, ensuring that all words of a language are listed
in finite amount of time.
-}
module Enumerator
  ( Rexp(..)
  , enumerate
  , sandwich)
  where

infixr 5 +++   -- catenate LOL data
infixl 6 \/    -- set union

-- | Constructors for regular expressions.
data Rexp = Nil               -- ^Empty language
          | Eps               -- ^Empty string
          | Sym Char          -- ^Symbol of the alphabet
          | Clo Rexp          -- ^Kleene closure
          | Cat Rexp Rexp     -- ^Catenation
          | Alt Rexp Rexp     -- ^Alternation
            deriving (Show,Eq,Ord)

-- | Type for non-deterministic finite automata is list of states.
type NFA = [State]

-- | State contains identificator, character and non-deterministic finite
-- automata (which is list of states). State derives Show class.
-- States are equal if they have the same identificator. For determining
-- ordering of states, we compare their symbols first and after that the
-- identificators. That gives us words of equal lengths sorted in alphabetical
-- order.
data State = State Ident Char NFA deriving Show
instance Eq State where
  (State i _ _) == (State i' _ _) = i == i'
instance Ord State where
  (State i c _) <= (State i' c' _) = (c, i) <= (c', i')

-- | NFA states are labeled with integers.
type Ident = Int

-- | Length-ordered list. Shorter lists come before longer lists when sorting.
data LOL a = LOL [a] deriving (Eq, Show)
instance Ord a => Ord (LOL a) where
  LOL x <= LOL y = (length x, x) <= (length y, y)

-- | Concatenation of length ordered lists.
(+++) :: LOL a -> LOL a -> LOL a
LOL x +++ LOL y = LOL (x++y)			
			
-- | Length-ordered string. Specialization of LOL. 		
type LOS = LOL Char				
	
-- | Union where elements are sorted by length and lexicographically.	
(\/) :: Ord a => [a] -> [a] -> [a]
[] \/ ys = ys
xs \/ [] = xs
xs@(x:xt) \/ ys@(y:yt) =
  case compare x y of
    LT -> x : xt \/ ys
    EQ -> x : xt \/ yt
    GT -> y : xs \/ yt

-- | General crossproduct of two list with a given function.
xprod :: (Ord a, Ord b, Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]
xprod _ [] _ = []
xprod _ _ [] = []
xprod f (x : xt) ys@(y : yt) =
  f x y : xprod f [x] yt \/ xprod f xt ys

-- | Closure.	
closure :: Ord a => (a -> a -> a) -> a -> [a] -> [a]
closure f z [] = [z]
closure f z xs@(x : xt) =
  if x == z
     then closure f z xt
     else z : xprod f xs (closure f z xs)

-- | OR operator.	
alt :: [LOS] -> [LOS] -> [LOS]
alt = (\/)

-- | Sequencing.
cat :: [LOS] -> [LOS] -> [LOS]
cat = xprod (+++)

-- | Closure operator.
clo :: [LOS] -> [LOS]
clo = closure (+++) (LOL "")	
	
-- | Bypass check function.
bp :: Bool -> NFA -> NFA
bp True ds = ds
bp False _ = []

-- | Function which determines starting state which is actually set as
-- destination state.
rexp2nfa :: Rexp -> NFA
rexp2nfa r = fs \/ bp b ds
  where ds = [State 0 '~' []];
        (fs, _, b) = rexp2nfa' r 1 ds

-- | Function takes regular expression, identifcator and destinations states.
-- Returns automat (seznam stanj) with meta informations (identificator,
-- bypass value).	
rexp2nfa' :: Rexp -> Ident -> NFA -> (NFA, Ident, Bool)
rexp2nfa' Nil       n _  = ([], n, False)
rexp2nfa' Eps       n _  = ([], n, True)
rexp2nfa' (Sym c)   n ds = ([State n c ds], succ n, False)
rexp2nfa' (Cat x y) n ds = (fs' \/ bp b' fs, n'', b && b')
  where (fs , n' , b ) = rexp2nfa' y n ds;
        (fs', n'', b') = rexp2nfa' x n' (fs \/ bp b ds);
rexp2nfa' (Alt x y) n ds = (fs \/ fs', n'', b || b')
  where (fs , n' , b ) = rexp2nfa' y n ds;
        (fs', n'', b') = rexp2nfa' x n' ds;
rexp2nfa' (Clo x)   n ds = (fs, n', True)
  where (fs, n', b) = rexp2nfa' x n (fs \/ ds)
	
-- | It takes list of states and joins states with the same character. This
-- removes duplicated word production and accelerates speed of algorithm.
grp :: NFA -> NFA
grp (m@(State _ c ds) : ms@(State _ c' ds' : mt)) =
  if c == c'
     then grp (State (-1) c (ds \/ ds') : mt)
     else m : grp ms
grp ms = ms

-- | Tells us if the word is part of automata language. State 0 is set as
-- the only final state.
accept :: NFA -> Bool
accept ds = 0 `elem` [i | (State i _ _) <- ds]

-- | Generates length ordered list of strings from automata.
visit :: [(String,NFA)] -> [String]
visit [] = []
visit ((x,ds):ws) =
  -- x = a word, 
  -- ds = destination states, where we can go at this point, 
  -- grp helps us to extract those dest. states
  -- [ ] = list of (word+c, ds'), where c is character
  -- visit -> ws+[ ]
  -- xs = list of valid words
  let xs = visit (ws ++ [(x++[c],ds') | (State _ c ds') <- grp ds])
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
enumNFA starts = visit [("", starts)]
	
-- | Exposes implemented functionality. It is user friendly function.
enumerate :: Rexp -> [String]
enumerate = enumNFA . rexp2nfa . deNil

a = Sym 'a'
b = Sym 'b'
c = Sym 'c'
aa = Cat a a
bb = Cat b b
ab = Cat a b
ba = Cat b a
a_a = Alt a a
b_b = Alt b b
a_b = Alt a b	
	
sandwich = Cat a (Cat (Clo b) a)

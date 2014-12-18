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

infixr 5 +++   -- catenate LOL data
infixl 6 \/    -- set union

-- | Type for non-deterministic finite automata is list of states.
type NFA = [State]

-- | Defines type of all groups. Its behaviour is just like stack.
-- The first (the top) field is RESERVED for number of groups. The number is stored as string.
type GROUPS = (Int, [String])

-- | Defines type of memory which contains indexes of activated groups at certain state.
type CURGROUPS = [Int]

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


-- | Define new group. Character is here just to simplify checkChar funciton. 
newGroup :: GROUPS -> CURGROUPS -> (GROUPS, CURGROUPS, String) 
-- First inserted group
newGroup (_,[]) _ = ((1, [""]), [0], "")
-- Later added groups
newGroup (n, xs) curGroup = 
	(((n+1), "":xs), n:curGroup, "")
	
ng = newGroup (3,["a", "b", "c"]) [2,0] 	
-- | Close group
closeGroup :: GROUPS -> CURGROUPS -> (GROUPS, CURGROUPS, String)
closeGroup group (x:xs) = (group, xs, "") 

-- | Add into groups
addToGroups :: GROUPS -> CURGROUPS -> Char -> GROUPS
addToGroups groups [] _ = groups
addToGroups (n, y:ys) (x:xs) c =
	let
		i = n - x - 1
		(_, zs) = addToGroups (n-1, ys) (if i == 0 then xs else x:xs) c
	in
		if i == 0 then (n, (y++[c]):zs)	else (n, y:zs)

-- test addToGroups		
atg = addToGroups (3,["a", "b", "c"]) [2,0] 'c'

-- | Get group 
getGroup :: GROUPS -> Int -> String
getGroup (len,x:xs) n =
	if len - n - 1 == 0 then x else getGroup (len-1, xs) n  

-- | If character is digit it returns group with id of the digit otherwise it returns input character.
-- If character is not a special character then it's added to all grouops it's located.
checkChar :: GROUPS -> CURGROUPS -> Char -> (GROUPS, CURGROUPS, String)
checkChar groups curgroups c =
	if isDigit c
	-- if digit then return group
	then (groups, curgroups, getGroup groups ((digitToInt c) - 1))
	-- if '(' then start new group
	else if c == '<' then newGroup groups curgroups
	-- if ')' then close last group
	else if c == '>' then closeGroup groups curgroups
	else 
		let groups' = addToGroups groups curgroups c in (groups', curgroups, [c])

-- test checkChar	
cc = checkChar (2, ["ac", "bc"]) [1] '1'

-- | Generates length ordered list of strings from automata.
visit :: [(String,NFA,GROUPS,CURGROUPS)] -> [String]
visit [] = []
visit ((x,ds,groups, curgroups):ws) =
  -- x = a word, 
  -- ds = destination states, where we can go at this point, 
  -- grp helps us to extract those dest. states
  -- [ ] = list of (word+c, ds'), where c is character
  -- visit -> ws+[ ]
  -- xs = list of valid words
  let xs = visit (ws ++ [(x++s',ds',groups', curgroups') | (State _ c ds') <- grp ds, (groups', curgroups', s') <- [checkChar groups curgroups c] ])
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
enumNFA starts = visit [("", starts, (0,[]), [])]

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

a = Sym 'a'
b = Sym 'b'
c = Sym 'c'
p1 = Sym '('
p2 = Sym ')'
n1 = Sym '1'
n2 = Sym '2'

e = enumerate1
g = Cat (Cat p1 (Cat (Clo a) p2)) n1
g2 = Cat (Cat p1 (Cat ( Cat (Cat p1 (Cat (Clo a) p2)) b) p2)) n2
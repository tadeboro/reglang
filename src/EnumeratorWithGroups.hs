module Enumerator 
(
-- * Tip
Rexp(..),
-- * Funkcija
enum,
-- * Test
sandwich
)
where

infixr 5 +++   -- catenate LOL data     -- H98 (++ in H1.4)
infixl 6 \/    -- set union

-- | Constructors for regular expressions.
data Rexp = 
			Nil               -- ^Empty language
          | Eps               -- ^Empty string
          | Sym Char          -- ^Symbol of the alphabet
          | Clo Rexp          -- ^Kleene closure
          | Cat Rexp Rexp     -- ^Catenation
          | Alt Rexp Rexp     -- ^Alternation
		  | Group [Char] Rexp -- ^Group (name the regular expression)
		  | Paste [Char]	  -- ^Paste the group with a given name
            deriving (Show,Eq,Ord)
			


-- | Type for non-deterministic finite automata is list of states.
type NFA = [State]

-- | State contains identificator, character and non-deterministic finite automata (which is list of states).
-- State derives Show class.
data State = State Ident Char NFA deriving Show

-- | States are equal if they have the same identificator.
instance Eq State where
    (State i _ _) == (State i' _ _) = i==i'
	
-- | For determining when State1 is less then State2 we compare their strings first and after that the identificators.
-- That gives us words of equal lengths sorted in alphabetical order.
instance Ord State where
    (State i c _) <= (State i' c' _) = (c,i) <= (c',i')

-- | Identificator has Int type.
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
xs@(x:xt) \/ ys@(y:yt) = case compare x y of
    LT -> x : xt\/ys
    EQ -> x : xt\/yt
    GT -> y : xs\/yt

-- | General crossproduct of two list with a given function.
xprod :: (Ord a, Ord b, Ord c) => (a->b->c) -> [a] -> [b] -> [c]
xprod _ [] _ = []
xprod _ _ [] = []
xprod f (x:xt) ys@(y:yt) =
    (f x y) : (xprod f [x] yt) \/ (xprod f xt ys)

-- | Closure.	
closure :: Ord a => (a->a->a) -> a -> [a] -> [a]
closure f z [] = [z]
closure f z xs@(x:xt) = if x==z
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

-- | Function which determines starting state which is actually set as destination state.
r2n :: Rexp -> NFA        
r2n r = let {
    ds = [State 0 '~' []];
    (fs, _, b) = r2n' r 1 ds
    } in fs \/ (bp b ds)
	
-- | Find element in memory for groups
getGroup :: (Eq a) => [(a,b)] -> a -> Maybe b
getGroup [] _ = Nothing
getGroup ((k, v):xs) element = 
	if k == element then  Just v else getGroup xs element
	
-- | Insert element into memory for groups
addGroup :: (Eq a) => [(a,b)] -> (a,b) ->  [(a,b)]
addGroup memory element = element:memory 

-- | Function takes regular expression, identifcator and destinations states. Returns automat (seznam stanj) with meta informations (identificator, bypass value).	
r2n' :: Rexp -> Ident -> NFA -> (NFA,Ident,Bool)
r2n' Nil n _ = ([], n, False)
r2n' Eps n ds = ([], n, True)
r2n' (Sym c) n ds = ([State n c ds], succ n, False)
r2n' (Cat x y) n ds = let {
    (fs, n', b) = r2n' y n ds;
    (fs', n'', b') = r2n' x n' (fs\/(bp b ds));
    } in (fs'\/(bp b' fs), n'', b&&b')
r2n' (Alt x y) n ds = let {
    (fs, n', b) = r2n' y n ds;
    (fs', n'', b') = r2n' x n' ds;
    } in (fs\/fs', n'', b||b')
r2n' (Clo x) n ds = let {
    (fs, n', b) = r2n' x n (fs\/ds)
    } in (fs, n', True)
	
r2n' (Group name x) n ds = let {
    (fs, n', b) = r2n' x n ds;			-- Calculate rexp as normal
	-- add to memory
    } in (fs, n', True)
	
-- | It takes list of states and joins states with the same character. This removes duplicated word production. 
-- That accelerates speed of algorithm. 
grp :: NFA -> NFA
grp (m@(State _ c ds) : ms@((State _ c' ds'):mt)) =
    if c==c' then grp ((State (-1) c (ds\/ds')):mt)
    else m : grp ms
grp ms = ms

-- | Tells us if the word is part of automata language. State 0 is set as the only final state. 
accept :: NFA -> Bool
accept ds = 0 `elem` [i | (State i _ _) <- ds]

-- | Generates length ordered list of strings from automata. 
visit :: [(String,NFA)] -> [String]  
visit [] = []
visit ((x,ds):ws) =
    let xs = visit (ws ++ [(x++[c],ds') | (State _ c ds') <- grp ds])
    in if accept ds then x:xs else xs

-- | Removes different production of the same length words. Makes parsing more efficient. 
deNil :: Rexp -> Rexp
deNil (Cat x y) = case (deNil x, deNil y) of
    (Nil, _) -> Nil
    (_, Nil) -> Nil
    (x', y') -> Cat x' y'
deNil (Alt x y) = case (deNil x, deNil y) of
    (Nil, y') -> y'
    (x', Nil) -> x'
    (x', y') -> Alt x' y'
deNil (Clo x) = case deNil x of
    Nil -> Eps
    x' -> Clo x'
deNil x = x	
	
-- | Properly initialise visit call. It starts with empty word.
enumA :: NFA -> [String]
enumA starts = visit [("",starts)]
	
-- | Exposes implemented functionality. It is user friendly function. 
enum :: Rexp -> [String]
enum = enumA . r2n . deNil

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
--enumR :: Rexp -> [String]
--enumR r = undefined


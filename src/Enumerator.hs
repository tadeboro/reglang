module Enumerator 
(
-- * Tip
Rexp,
NFA,
-- * Funkcija
enum,
r2n,

sandwich
)
where

infixr 5 +++   -- catenate LOL data     -- H98 (++ in H1.4)
infixl 6 \/    -- set union


data Rexp = Nil               -- empty language
          | Eps               -- empty string
          | Sym Char          -- symbol of the alphabet
          | Clo Rexp          -- Kleene closure
          | Cat Rexp Rexp     -- catenation
          | Alt Rexp Rexp     -- alternation
            deriving (Show,Eq,Ord)

type NFA = [State]
data State = State Ident Char NFA deriving Show

instance Eq State where
    (State i _ _) == (State i' _ _) = i==i'
instance Ord State where
    (State i c _) <= (State i' c' _) = (c,i) <= (c',i')

type Ident = Int

-- | Length-ordered list
-- Shorter lists come before longer lists when sorting			
data LOL a = LOL [a] deriving (Eq, Show)
instance Ord a => Ord (LOL a) where
    LOL x <= LOL y = (length x, x) <= (length y, y)

(+++) :: LOL a -> LOL a -> LOL a       
LOL x +++ LOL y = LOL (x++y)			
			
-- | Length-ordered string			
type LOS = LOL Char				
	
-- | *is sorted	
(\/) :: Ord a => [a] -> [a] -> [a]
[] \/ ys = ys
xs \/ [] = xs
xs@(x:xt) \/ ys@(y:yt) = case compare x y of
    LT -> x : xt\/ys
    EQ -> x : xt\/yt
    GT -> y : xs\/yt

-- | more general crossproduct*
xprod :: (Ord a, Ord b, Ord c) => (a->b->c) -> [a] -> [b] -> [c]
xprod _ [] _ = []
xprod _ _ [] = []
xprod f (x:xt) ys@(y:yt) =
    (f x y) : (xprod f [x] yt) \/ (xprod f xt ys)

closure :: Ord a => (a->a->a) -> a -> [a] -> [a]
closure f z [] = [z]
closure f z xs@(x:xt) = if x==z
    then closure f z xt
    else z : xprod f xs (closure f z xs)

alt :: [LOS] -> [LOS] -> [LOS]
alt = (\/)
cat :: [LOS] -> [LOS] -> [LOS]
cat = xprod (+++)               
clo :: [LOS] -> [LOS]
clo = closure (+++) (LOL "")	
	

bp :: Bool -> NFA -> NFA
bp True ds = ds
bp False _ = []

r2n :: Rexp -> NFA        
r2n r = let {
    ds = [State 0 '~' []];
    (fs, _, b) = r2n' r 1 ds
    } in fs \/ (bp b ds)

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
	

grp :: NFA -> NFA
grp (m@(State _ c ds) : ms@((State _ c' ds'):mt)) =
    if c==c' then grp ((State (-1) c (ds\/ds')):mt)
    else m : grp ms
grp ms = ms

accept :: NFA -> Bool
accept ds = 0 `elem` [i | (State i _ _) <- ds]

visit :: [(String,NFA)] -> [String]     
visit [] = []
visit ((x,ds):ws) =
    let xs = visit (ws ++ [(x++[c],ds') | (State _ c ds') <- grp ds])
    in if accept ds then x:xs else xs


enum :: NFA -> [String]
enum starts = visit [("",starts)]
	
	

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


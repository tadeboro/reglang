module Enumerator 
(
-- * Tip
Rexp
-- * Funkcija
--enumR
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
	
	
--enumR :: Rexp -> [String]
--enumR r = undefined


-- Program to accompany JFP Functional Pearl
-- "Enumerating the strings of a regular language"
-- M. Douglas McIlroy

-- The code in the paper works with Hugs 1.4; the code here has
-- been updated to work with Hugs 98 as well.  Changes, flagged
-- H98 and H1.4, are necessary because 
--      "Word" conflicts with H98 standard prelude,
--      ++ is not overloadable in H98.

infixr 5 +++   -- catenate LOL data     -- H98 (++ in H1.4)
infixl 6 \/    -- set union

-- SET OPERATIONS

(\/) :: Ord a => [a] -> [a] -> [a]
xprod :: (Ord a, Ord b, Ord c) => (a->b->c) -> [a] -> [b] -> [c]
closure :: Ord a => (a->a->a) -> a -> [a] -> [a]

[] \/ ys = ys
xs \/ [] = xs
xs@(x:xt) \/ ys@(y:yt) = case compare x y of
    LT -> x : xt\/ys
    EQ -> x : xt\/yt
    GT -> y : xs\/yt

xprod _ [] _ = []
xprod _ _ [] = []
xprod f (x:xt) ys@(y:yt) =
    (f x y) : (xprod f [x] yt) \/ (xprod f xt ys)

closure f z [] = [z]
closure f z xs@(x:xt) = if x==z
    then closure f z xt
    else z : xprod f xs (closure f z xs)

-- GENERATE LANGUAGE FROM REGULAR EXPRESSION

data LOL a = LOL [a]          -- length-ordered list
    deriving (Eq, Show)
instance Ord a => Ord (LOL a) where
    LOL x <= LOL y = (length x, x) <= (length y, y)
(+++) :: LOL a -> LOL a -> LOL a        -- H98
LOL x +++ LOL y = LOL (x++y)

-- instance Monad LOL                   -- H1.4
-- instance MonadZero LOL
-- instance MonadPlus LOL where
--     (LOL x) ++ (LOL y) = LOL (x++y)

type LOS = LOL Char           -- length-ordered string

data Rexp = Nil               -- empty language
          | Eps               -- empty string
          | Sym Char          -- symbol of the alphabet
          | Clo Rexp          -- Kleene closure
          | Cat Rexp Rexp     -- catenation
          | Alt Rexp Rexp     -- alternation
            deriving (Show,Eq,Ord)

enumR :: Rexp -> [String]
enumR r = [x | (LOL x) <- enumR' r]

enumR' :: Rexp -> [LOS]
enumR' Nil       = []
enumR' Eps       = [LOL ""]
enumR' (Sym a)   = [LOL [a]]
enumR' (Clo x)   = clo (enumR' x)
enumR' (Cat x y) = cat (enumR' x) (enumR' y)
enumR' (Alt x y) = alt (enumR' x) (enumR' y)

alt, cat :: [LOS] -> [LOS] -> [LOS]
clo :: [LOS] -> [LOS]
alt = (\/)
cat = xprod (+++)               -- H98
clo = closure (+++) (LOL "")

-- CONSTRUCT AUTOMATON FROM REGULAR EXPRESSION

-- Eliminate Nil from all but top level.

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

-- A nondeterministic finite automaton (NFA)
-- is known by the set of states it starts in.
-- A single state accepts a single character and
-- moves to a subautomaton (i.e. to a set of states).
-- Each state has a distinguishing (Int) identifier.
-- The unique final state has identifier 0, a dummy
-- accepted character, and no moves: (State 0 '~' []).

type NFA = [State]
data State = State Ident Char NFA
type Ident = Int

accept :: NFA -> Bool
accept ds = 0 `elem` [i | (State i _ _) <- ds]

-- States are ordered by character then identifier;
-- sets of states are kept in order.

instance Eq State where
    (State i _ _) == (State i' _ _) = i==i'
instance Ord State where
    (State i c _) <= (State i' c' _) = (c,i) <= (c',i')

-- Return destination states ds if bypass flag is
-- true, else empty list.

bp :: Bool -> NFA -> NFA
bp True ds = ds
bp False _ = []

r2n :: Rexp -> NFA        -- reg expr to nondet automaton
r2n r = let {
    ds = [State 0 '~' []];
    (fs, _, b) = r2n' r 1 ds
    } in fs \/ (bp b ds)

r2n' :: Rexp -> Ident -> NFA -> (NFA,Ident,Bool)
--  (fs,n',b) = r2n' r n ds
--  r regular expression
--  n number available as identifier
--  ds destination states
--  fs first states (states in r that accept chars)
--  b bypass flag (true if r's language includes Eps)
--  n' next number available as identifier
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

-- GENERATE LANGUAGE FROM AUTOMATON

enumA :: NFA -> [String]
enumA starts = visit [("",starts)]
    
-- type Word = (String,NFA)             -- H1.4
-- visit :: [Word] -> [String]

visit :: [(String,NFA)] -> [String]     -- H98
visit [] = []
visit ((x,ds):ws) = let { xs = visit (ws ++
        [(x++[c],ds') | (State _ c ds') <- grp ds])
    } in if accept ds then x:xs else xs

-- Group moves by character (states with identical chars
-- are adjacent in a state list).  A group is repesented 
-- as a (possibly fictitious) state.  The Ident of a group
-- is never looked at, allowing -1 to be used repeatedly.

grp :: NFA -> NFA
grp (m@(State _ c ds) : ms@((State _ c' ds'):mt)) =
    if c==c' then grp ((State (-1) c (ds\/ds')):mt)
    else m : grp ms
grp ms = ms

-- Enumerate strings of a regular expression by automaton.

enumRA :: Rexp -> [String]
enumRA = enumA . r2n . deNil

-- INSPECTION AND TEST

-- Create a pair of printable lists that describe an NFA:
--    identifiers of start states
--    state descriptions (identifier, character, moves)

list :: NFA -> ([Ident],[(Ident,Char,[Ident])])
list ds = (map ident ds, list' ds [])
list' :: NFA -> [Ident] -> [(Ident,Char,[Ident])]
list' [] _ = []
list' ((State i c ds):xs) is = if i `elem` is
    then list' xs is
    else (i,c,map ident ds) : list' (xs++ds) (i:is)

ident :: State -> Ident
ident (State i _ _) = i

-- Compare the first n strings of the language of
-- regular expression r, as calculated by the direct
-- enumerator enumR and the automaton-based enumRA.

check :: Int -> Rexp -> Bool
check n r = take n (enumR r) == take n (enumRA r)

-- test n e r  (Almost) silently generate the first n strings in
-- the language of r by enumerator e (enumR or enumRA).
-- This allows stress/timing tests without the cost of printing.

test n e r = length (take n (e r))

-- dexprs n   Exhaustively list regular expressions in primitives
-- a and b with operator nesting depth at most n.

dexprs :: Int -> [Rexp]
dexprs 0 = [a, b]
dexprs i = let x = dexprs (i-1) 
    in x \/ (xprod Cat x x) \/ (xprod Alt x x) \/ (map Clo x)

-- sexprs n  Exhaustively list regular expressions of
-- size n built from primitives in sexprs 1.
-- cumsexprs n  Cumulatively list expressions of size n or less.

sexprs :: Int -> [Rexp]
sexprs 0 = []
sexprs 1 = [Nil, Eps, a, b]
sexprs n = map Clo (sexprs (n-1)) ++ 
    foldr (++) [] [xprod Cat p q ++ xprod Alt p q |
        (p,q) <- [((sexprs i), (sexprs (n-1-i))) |
                                        i <- [1..(n-1)]]]

cumsexprs :: Int -> [Rexp]
cumsexprs 1 = sexprs 1
cumsexprs n = cumsexprs (n-1) \/ sexprs n

-- dcheck (scheck) Check first m strings for every
-- regular expression of depth (size) n or less.

dcheck, scheck :: Int -> Int -> Bool
dcheck m n = and (map (check m) (dexprs n))
scheck m n = and (map (check m) (cumsexprs n))

-- Test expressions

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

sandwich = Cat a (Cat (Clo b) a)  -- a b* a
even_a = Clo (Alt sandwich b)     -- (ab*a|b)*

rep 1 x = x
rep n x = Cat x (rep (n-1) x)     -- x^n (catenation of n strings in x)

-- An NFA whose equivalent DFA is exponentially large

aho n = (Cat (Clo a_b) (Cat a (rep n a_b))) -- (a|b)*a(a|b)^n


-- Used as an example in Darrell Raymond, "Grail: a
-- C++ library for finite-state machines and regular expressions"
-- Tech report #358, Dept of CS, U of Western Ontario (March 1994)

grail = Cat (Clo (Alt a b)) (Cat a (Cat b c))
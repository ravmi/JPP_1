module RegExtra where
import Mon
import Reg
import Data.List

data AB = A | B deriving(Eq,Ord,Show)

infix 4 ===
class Equiv a where
  (===) :: a -> a -> Bool

instance (Eq c) => Equiv (Reg c) where
   r1 === r2 = (simpl r1) == (simpl r2)

-- todo
instance Mon (Reg c) where
  m1 = Empty
  x <> y = Empty -- simpl $ (simpl x) :> (simpl y)
  
simpl :: Eq c => Reg c -> Reg c
simpl (x :> y) = fromList $ fixList $ map simpl $ (toList x) ++ toList y where
    toList :: Reg c -> [Reg c]
    toList (x :> y) = (toList x) ++ (toList y)
    toList x = [x]

    -- mozna usuwac ,jesli obok jest many takei samo[wazne]
    fixList :: Eq c => [Reg c] -> [Reg c]
    fixList = cleanEps . cleanEmpty

    cleanEmpty :: Eq c => [Reg c] -> [Reg c]
    cleanEmpty = filter (/=Empty)

    cleanEps :: Eq c => [Reg c] -> [Reg c]
    cleanEps l
        | all (Eps==) l = [Eps]
        | otherwise = filter (/=Eps) l

    fromList :: [Reg c] -> Reg c
    fromList [] = Empty
    fromList [x] = x
    fromList (x:xs) = x :> (fromList xs)

simpl (x :| y) = fromList $ fixList $ map simpl $ (toList x) ++ toList y where
    toList :: Reg c -> [Reg c]
    toList (x :| y) = (toList x) ++ (toList y)
    toList x = [x]

    -- mozna dodac, ze usuwamy a, aa itd, jesli jest many a
    -- mozna dodac clean eps
    -- mozna usunac epsy, jesli jest Many (łatwe!)
    fixList :: Eq c => [Reg c] -> [Reg c]
    fixList = nub . cleanEmpty

    cleanEmpty :: Eq c => [Reg c] -> [Reg c]
    cleanEmpty = filter (/=Empty)
     
    fromList :: [Reg c] -> Reg c
    fromList [] = Empty
    fromList [x] = x
    fromList (x:xs) = x :| (fromList xs)

simpl (Many Eps) = Eps
simpl (Many Empty) = Empty
simpl (Many x) = Many (simpl x)
simpl x = x

nullable :: Reg c -> Bool
nullable (x :| y) = (nullable x) || (nullable y)
nullable Eps = True
nullable x = False

empty :: Reg c -> Bool 
empty Empty = True
empty r = False

-- pomyslec nad optymalizacja zemy tak nie spamowac simplem (mozna upsrawnic ders,
-- tylko obtoczys dwoma simplami)
-- nie rozumiem kiedyz uzywac epsa, a kiedy empyty
der :: Eq c => c -> Reg c -> Reg c
der c = simpl . (der' c) . simpl  where
    der' :: Eq c => c -> Reg c -> Reg c
    der' c ((Lit x) :> y)
        | x == c = y
        | otherwise = Empty
    -- to pisalem szybko, moze byc blad gdzies - popraweione
    der' c ((Many x) :> y) = der' c $ x :> (Many x) :> y
    der' c (x :| y) = (der' c x) :| (der' c y)
    der' c (Many x) = der' c $ x :> (Many x)
    der' c Empty = Empty
    der' c Eps = Empty

-- foldl czy foldr
ders :: Eq c => [c] -> Reg c -> Reg c
ders l r = foldl (flip der) r l

accepts :: Eq c => Reg c -> [c] -> Bool
accepts r w = nullable $ ders w r

mayStart :: Eq c => c -> Reg c -> Bool
mayStart c r = not $ empty $ der c r

match :: Eq c => Reg c -> [c] -> Maybe [c]
match r w = Nothing

search :: Eq c => Reg c -> [c] -> Maybe [c]
search r w = Nothing

findall :: Eq c => Reg c -> [c] -> [[c]]
findall r w = []

char :: Char -> Reg Char
char = Lit

string :: [Char] -> Reg Char
string = foldr1 (:>) . map Lit

alts :: [Char] -> Reg Char
alts = foldr1 (:|) . map Lit

letter = alts ['a'..'z'] :| alts ['A'..'Z']
digit = alts ['0'..'9']
number = digit :> Many digit
ident = letter :> Many (letter :| digit)

many1 r = r :> Many r

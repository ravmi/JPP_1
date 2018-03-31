module RegExtra where
import Mon
import Reg
import Data.List

data AB = A | B deriving(Eq,Ord,Show)

infix 4 ===
class Equiv a where
  (===) :: a -> a -> Bool

---

instance (Eq c) => Equiv (Reg c) where
   r1 === r2 = (simpl r1) == (simpl r2)

instance Mon (Reg c) where
  m1 = Eps
  x <> y = x :> y
  
simpl :: Eq c => Reg c -> Reg c
simpl (x :> y) = fromList $ fixList $ map simpl $ (toList x) ++ toList y where
    toList :: Reg c -> [Reg c]
    toList (x :> y) = (toList x) ++ (toList y)
    toList x = [x]

    fixList :: Eq c => [Reg c] -> [Reg c]
    fixList = cleanEps . checkEmpty

    checkEmpty :: Eq c => [Reg c] -> [Reg c]
    checkEmpty l
        | any empty l = []
        | otherwise = l

    cleanEps :: Eq c => [Reg c] -> [Reg c]
    cleanEps [] = []
    cleanEps l
        | all equalsEps l = [Eps]
        | otherwise = filter (not . equalsEps) l

    fromList :: [Reg c] -> Reg c
    fromList [] = Empty
    fromList [x] = x
    fromList (x:xs) = x :> (fromList xs)

simpl (x :| y) = fromList $ fixList $ map simpl $ (toList x) ++ toList y where
    toList :: Reg c -> [Reg c]
    toList (x :| y) = (toList x) ++ (toList y)
    toList x = [x]

    fixList :: Eq c => [Reg c] -> [Reg c]
    fixList = nub . cleanEmpty

    cleanEmpty :: Eq c => [Reg c] -> [Reg c]
    cleanEmpty = filter (/=Empty)
     
    fromList :: [Reg c] -> Reg c
    fromList [] = Empty
    fromList [x] = x
    fromList (x:xs) = x :| (fromList xs)

simpl (Many x)
    | (equalsEps x) || (empty x) = Eps
    | otherwise = Many (simpl x)
simpl x = x


nullable :: Reg c -> Bool
nullable (x :| y) = (nullable x) || (nullable y)
nullable (x :> y)= (nullable x) && (nullable y)
nullable (Many x) = True
nullable Eps = True
nullable Empty = False
nullable (Lit x) = False


-- Is the argument regex that accepts only Eps?
equalsEps :: Reg c -> Bool
equalsEps Eps = True
equalsEps (x :> y) = equalsEps x && equalsEps y
equalsEps (x :| y) = (epsx && epsy) || (epsx && emptyy) || (emptyx && epsy) where
    emptyx = empty x
    emptyy = empty y
    epsx = equalsEps x
    epsy = equalsEps y
equalsEps (Many x) = (equalsEps x) || (empty x)
equalsEps (Lit x) = False
equalsEps Empty = False


empty :: Reg c -> Bool 
empty (x :> y) = (empty x) || (empty y)
empty (x :| y) = (empty x) && (empty y)
empty (Many x) = False
empty Empty = True
empty r = False


-- simpl format has some properties:
-- both :> and :| "go to the right"
-- there are no Eps in :>
-- there are no Many Empty
-- tere are no Many Eps
der :: Eq c => c -> Reg c -> Reg c
der c r = cutFirst c $ simpl r
cutFirst :: Eq c => c -> Reg c -> Reg c
cutFirst c Empty = Empty
cutFirst c Eps = Empty
cutFirst c (Lit x) = Empty
cutFirst c (Many x)
    | equalsEps cutx = Many x
    | nullable cutx = cutx :> (Many x)
    | otherwise = Empty where
    cutx = cutFirst c x
cutFirst c (x :| y) = simpl $ cutFirst c x :| (cutFirst c y)
cutFirst c (x :> y)
    | equalsEps cutx = y
    | nullable cutx = cutx :> y
    | otherwise = Empty
    where
    cutx = cutFirst c x

-- foldl czy foldr
-- posprawdzac funkcje, ktore nie maja eq, wiec nie sa simply
ders :: Eq c => [c] -> Reg c -> Reg c
ders l r = simpl $ foldl (flip der) r l

accepts :: Eq c => Reg c -> [c] -> Bool
accepts r [] = nullable r
accepts r w = nullable $ ders y x
    where 
    x = r
    y = w

mayStart :: Eq c => c -> Reg c -> Bool
mayStart c r = not $ empty $ der c r

match :: Eq c => Reg c -> [c] -> Maybe [c]
match r w
    | found /= [] = Just found
    | nullable r = Just []
    | otherwise = Nothing
    where
    found = match' r w

match' :: Eq c => Reg c -> [c] -> [c]
match' r [] = []
match' r (x:xs)
    | nullable reduced = x:(match' reduced xs)
    | otherwise = [] where
            reduced = der x r

suffixes :: Eq c => [c] -> [[c]]
suffixes [] = [[]]
suffixes l = l : (tails (tail l))

search :: Eq c => Reg c -> [c] -> Maybe [c]
search r w = foldl (sumMaybe) Nothing $ map (match r) (suffixes w) where
    sumMaybe Nothing l = l
    sumMaybe l Nothing = l
    sumMaybe (Just l1) (Just l2) = Just (l1 ++ l2)


findall :: Eq c => Reg c -> [c] -> [[c]]
findall r w = []

---

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

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

instance Mon (Reg c) where
  m1 = Eps
  x <> y = x :> y

simpl :: Eq c => Reg c -> Reg c
simpl (x :> y) = fromList $ fixList $ map simpl $ (toList x) ++ toList y
    where
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
simpl (x :| y) = fromList $ fixList $ map simpl $ (toList x) ++ toList y
    where
    toList :: Reg c -> [Reg c]
    toList (x :| y) = (toList x) ++ (toList y)
    toList x = [x]

    fixList :: Eq c => [Reg c] -> [Reg c]
    fixList = nub . cleanEmpty

    cleanEmpty :: Eq c => [Reg c] -> [Reg c]
    cleanEmpty = filter (not . empty)

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

equalsEps :: Reg c -> Bool
equalsEps Eps = True
equalsEps (x :> y) = equalsEps x && equalsEps y
equalsEps (x :| y) = (epsx && epsy) || (epsx && emptyy) || (emptyx && epsy)
    where
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

der :: Eq c => c -> Reg c -> Reg c
der c r = cutFirst c $ simpl r

cutFirst :: Eq c => c -> Reg c -> Reg c
cutFirst c Empty = Empty
cutFirst c Eps = Empty
cutFirst c (Lit x)
    | c==x = Eps
    | otherwise = Empty
cutFirst c (Many x)
    | equalsEps cutx = Many x
    | not $ empty cutx = cutx :> (Many x)
    | otherwise = Empty
        where
        cutx = cutFirst c x
cutFirst c (x :| y) = cutFirst c x :| (cutFirst c y)
cutFirst c (x :> y)
    | nullable x = (cutFirst c y) :| (cutx :> y)
    | empty cutx = Empty
    | otherwise = cutx :> y
        where
        cutx = cutFirst c x

ders :: Eq c => [c] -> Reg c -> Reg c
ders l r = foldr der r l

accepts :: Eq c => Reg c -> [c] -> Bool
accepts r [] = nullable r
accepts r w = nullable $ ders w r

mayStart :: Eq c => c -> Reg c -> Bool
mayStart c r = not $ empty $ der c r

sufixes :: Eq c => [c] -> [[c]]
sufixes [] = [[]]
sufixes l@(x:xs) = l:(sufixes xs)

match :: Eq c => Reg c -> [c] -> Maybe [c]
match r []
    | nullable r = Just []
    | otherwise = Nothing
match r (c:cs) = case match derc cs of
    Just rest -> Just $ c:rest
    Nothing -> if nullable r then Just [] else Nothing
    where
        derc = der c r

search :: Eq c => Reg c -> [c] -> Maybe [c]
search r w = firstSomething $ map (match r) $ sufixes w
    where
    firstSomething :: Eq c => [Maybe [c]] -> Maybe [c]
    firstSomething [] = Nothing
    firstSomething (Nothing:xs) = firstSomething xs
    firstSomething ((Just x):_) = Just x

findall :: Eq c => Reg c -> [c] -> [[c]]
findall r w = cleanIncluded (map (match r) $ sufixes w) 0
    where
    -- removes shorter words included in accepted ones
    cleanIncluded :: Eq c => [Maybe [c]] -> Int -> [[c]]
    cleanIncluded [] _ = []
    cleanIncluded (Nothing:ls) expected = cleanIncluded ls (expected-1)
    cleanIncluded ((Just l):ls) expected
        | llen >= expected = l : (cleanIncluded ls llen)
        | otherwise = cleanIncluded ls $ expected - 1
        where
            llen = length l

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

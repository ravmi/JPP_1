import Data.List
data Reg c = Lit c |
 Reg c :> Reg c |
 Reg c :| Reg c |
 Many (Reg c) |
 Eps |
 Empty deriving (Eq,Show)


simpl :: Eq c => Reg c -> Reg c
simpl (x :> y) = fromList $ fixList $ map simpl $ (toList x) ++ toList y where
    toList :: Reg c -> [Reg c]
    toList (x :> y) = (toList x) ++ (toList y)
    toList x = [x]

    -- mozna usuwac ,jesli obok jest many takei samo[wazne]
    fixList :: Eq c => [Reg c] -> [Reg c]
    fixList = filter (/=Eps)

    fromList :: [Reg c] -> Reg c
    fromList [] = Eps
    fromList [x] = x
    fromList (x:xs) = x :> (fromListAnd xs)

simpl (x :| y) = fromList $ fixList $ map simpl $ (toList x) ++ toList y where
    toList :: Reg c -> [Reg c]
    toList (x :| y) = (toList x) ++ (toList y)
    toList x = [x]

    -- mozna dodac, ze usuwamy a, aa itd, jesli jest many a
    -- mozna dodac cleaneps
    -- mozna dodac epsy, jesli jest Many (łatwe!)
    fixList :: Eq c => [Reg c] -> [Reg c]
    fixList = nub
     
    fromList :: [Reg c] -> Reg c
    fromList [x] = x
    fromList (x:xs) = x :| (fromList xs)

simpl x = x










q = (((Lit 1) :| (Lit 4)) :| (Eps)) :| ((Eps) :| (Lit 12))

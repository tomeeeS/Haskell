data Section = Section { getA :: Int, getB :: Int, getC :: Int } 
    deriving (Show)
data SectionForRoad =
    S { forThisRoad :: (Int,Label),
        forOther :: (Int,Label),
        across :: Int } 
    deriving (Show)
type RoadSystem = [Section] 

heathrowToLondon :: RoadSystem  
heathrowToLondon = 
    [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0] 

-- which road (C is the crossing between A and B main roads)
data Label = A | B | C 
    deriving (Show, Eq) 
type Path = ([Label], Int) -- road labels on path and the total cost
type BestPaths = (Path, Path) -- for A and for B

forB :: Section -> SectionForRoad
forB (Section a b c) = S (a, A) (b, B) c

forA :: Section -> SectionForRoad
forA (Section a b c) = S (b, B) (a, A) c

addSection :: Path -> SectionForRoad -> Path
addSection (rs, i) (S (tc, tl) (oc, ol) c)
    | oc + c < tc = (rs ++ [ol, C], i + oc + c)
    | otherwise = (rs ++ [tl], i + tc)

addSections :: BestPaths -> Section -> BestPaths
addSections (pa, pb) s = (addSection pa $ forA s, addSection pb $ forB s)

solve :: RoadSystem -> Path
solve rs = bestOf bestPaths where
    bestPaths = foldl addSections (([], 0), ([], 0)) rs
    bestOf (a, b) = if snd a < snd b then a else b

t1 :: RoadSystem 
t1 = [Section 9 3 2]


testAddSection :: Bool
testAddSection = addSection ([], 0) (S (9, A) (3, B) 2) == ([B, C], 5)

testAddRoads :: Path
testAddRoads = undefined

test = [
    testAddSection,
    solve t1 == ([B], 3),
    solve heathrowToLondon == ([B, C, A, C, ], 65)]
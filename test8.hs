
findElem :: Foldable t => (a -> Bool) -> t a -> Maybe a
findElem f t = foldr (\a b -> if f a then Just a else b) Nothing t

bl = findElem (==10) [1, 3, 10] == Just 10

main :: IO ()
main = return ()
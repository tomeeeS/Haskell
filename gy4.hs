import Prelude

filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe f [] = Just []
filterMaybe f (x:xs) = case f x of
    Just b -> case filterMaybe f xs of
        Nothing -> Nothing
        Just xs' -> Just (if b then x:xs' else xs')
    Nothing -> Nothing

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show

mapMaybeTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree f (Leaf a) = 
    case f a of
        Nothing -> Nothing
        Just b -> Just $ Leaf b
    -- fmap Leaf (f a)

mapMaybeTree f (Node l r) = case mapMaybeTree f l of
    Nothing -> Nothing
    Just l' -> case mapMaybeTree f r of
        Nothing -> Nothing
        Just r' -> Just (Node l' r')

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just a) f = f a

filterMaybe' :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe' _ [] = Just []
filterMaybe' f (x:xs) =
    bind (f x) $ \b ->
    bind (filterMaybe' f xs) $ \xs' ->
    Just (if b then x:xs' else xs')
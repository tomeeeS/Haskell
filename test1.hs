{-# options_ghc -Wincomplete-patterns #-}  -- helps detecting missing branches of a pattern matching function

data Color = Red | Green | Blue
    deriving Show

instance Eq Color where
    (==) Red Red = True
    (==) Green Green = True
    (==) Blue Blue = True
    (==) _ _ = False
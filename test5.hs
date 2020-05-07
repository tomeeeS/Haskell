f1 :: Monad m => m (a -> b) -> m a -> m b
f1  = (<*>)

f2 :: Monad m => m (m (m a)) -> m a
f2 mmma = 
    mmma >>= \mma ->
    mma >>= \ma -> ma
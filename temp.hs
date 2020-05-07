
data Maybe' a = Nothing' | Just' a
  deriving Show  -- opcionálisan: deriving (Functor)

instance Functor Maybe' where
  fmap f ma = ma >>= \a -> return (f a)                  -- Maybe' b

instance Applicative Maybe' where
  pure = return
  mf <*> ma =        -- (<*>) kiejtése "ap"
    mf >>= \f ->
    ma >>= \a ->
    return (f a)

instance Monad Maybe' where
  return = Just'
  Just' a  >>= f = f a
  Nothing' >>= _ = Nothing'

main :: IO ()
main = return ()
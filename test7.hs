{-# LANGUAGE DeriveFunctor #-}

import Control.Monad (ap)
import Data.String

newtype State s a = State {runState :: s -> (a, s)}
  deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  State f >>= g = State $ \s -> case f s of
    (a, s') -> runState (g a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= put . f

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma



f1 :: (a -> a) -> State (a, b) ()
f1 f = State $ \s -> ((), (f (fst s), snd s))

f2 :: State s a -> State s b -> State s (a, b)
f2 (State f) (State g) =
    State $ \s -> ( (fst (f s), fst (g s)), s)
  --  do
  --a <- ma
  --b <- mb
  --pure (a, b)


a :: Either Int Int
a = Left 1

b = fromLeft 0 a
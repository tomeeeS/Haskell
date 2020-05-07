
import Control.Monad -- ap

newtype State s a = State {runState :: s -> (a, s)}


-- feladat 1: írd meg a következő instance-t
instance Functor (State s) where
  fmap f (State g) = 
      State $ \sa -> 
      (f . fst . g, snd . g, sa)

-- feladat 2: definiáld a következő függvényeket.
-- A lényeg, hogy típushelyes legyen a megoldás, rossz definíciót írni
-- tudomásom szerint nem lehet (kivéve loop/undefined)
get :: State s s
get = get (loop)

evalState :: State s a -> s -> a
evalState = undefined

returnState :: a -> State s a
returnState = undefined

-- feladat 3: írj két-két *különböző* definíciót a következő függvényekhez!
execState :: State s a -> s -> s
execState (State f) = snd (f s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

constBindState :: State s a -> (a -> State s b) -> State s b
constBindState (State f) (State g) = 
    State $ \s ->
    case f s of (a, s') -> g s'

-- extra feladat:
bindState :: State s a -> (a -> State s b) -> State s b
bindState (State f) g = 
    State $ \s ->
    case f s of
        State g' -> g' s'  -- vagy (a, s') -> runState (g a ) s'

------------------------------------------------------------

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  return = returnState
  (>>=)  = bindState

------------------------------------------------------------

-- list push/pop

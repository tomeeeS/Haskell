{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}

import Control.Monad (ap)
import Data.String
import Data.Either
import Data.Maybe

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

  
type Name = String

data Exp
  = Add Exp Exp     -- e + e
  | Mul Exp Exp     -- e * e
  | Not Exp         -- not e     (Bool negáció)
  | IntLit Int      -- n
  | BoolLit Bool    -- b
  | Eq Exp Exp      -- e == e   (egyenlőségvizsgálat, Int-re vagy Bool-ra)
  | Lt Exp Exp      -- e < e    (Int rendezés vizsgálata)
  | And Exp Exp     -- e && e   (Bool és)
  | Or Exp Exp      -- e || w   (Bool vagy)
  | Var Name        --          (változónév)
  deriving (Show, Eq)

type Program = [Statement]

infix 3 :=
data Statement
  = Name := Exp              -- értékadás mint infix konstruktor
  | If Exp Program Program
  | While Exp Program
  deriving Show


instance Num Exp where
  (+)         = Add
  (*)         = Mul
  fromInteger = IntLit . fromInteger
  negate e    = Mul e (IntLit (-1))
  abs         = undefined
  signum      = undefined

instance IsString Exp where
  fromString = Var

true :: Exp
true = BoolLit True

false :: Exp
false = BoolLit False


type Val = Either Int Bool
type Env = [(Name, Val)]



-------- written by me:

toInt :: Exp -> Env -> Int
toInt e env = fromLeft 0 $ evalExp e env

toBool :: Exp -> Env -> Bool
toBool e env = fromRight True $ evalExp e env

areInts :: Exp -> Exp -> Env -> Bool
areInts e1 e2 env = 
  isLeft (evalExp e1 env) &&
  isLeft (evalExp e2 env)

areBools :: Exp -> Exp -> Env -> Bool
areBools e1 e2 env =
  isRight (evalExp e1 env) &&
  isRight (evalExp e2 env)

areOfSameType :: Exp -> Exp -> Env -> Bool
areOfSameType e1 e2 env = 
  areInts e1 e2 env ||
  areBools e1 e2 env

-- int operation on ints
intOp :: Exp -> Exp -> Env -> (Int -> Int -> Int) -> Either Int Bool
intOp e1 e2 env op = if areInts e1 e2 env then
  Left $ op (toInt e1 env) (toInt e2 env) else
  undefined

-- bool operation on bools
boolOp :: Exp -> Exp -> Env -> (Bool -> Bool -> Bool) -> Either Int Bool
boolOp e1 e2 env op = if areBools e1 e2 env then
  Right $ op (toBool e1 env) (toBool e2 env) else
  undefined

-- bool operation on ints
intsBoolOp :: Exp -> Exp -> Env -> (Int -> Int -> Bool) -> Either Int Bool
intsBoolOp e1 e2 env op = if areInts e1 e2 env then
  Right $ op (toInt e1 env) (toInt e2 env) else
  undefined

evalExp :: Exp -> Env -> Val
evalExp exp' env = case exp' of
  Add e1 e2 -> intOp e1 e2 env (+)
  Mul e1 e2 -> intOp e1 e2 env (*)
  Not e     -> Right (not $ toBool e env)
  IntLit i  -> Left i
  BoolLit b -> Right b
  Eq e1 e2
    | areBools e1 e2 env -> boolOp e1 e2 env (==)
    | areInts e1 e2 env -> intsBoolOp e1 e2 env (==)
    | otherwise -> undefined
  Lt e1 e2 -> intsBoolOp e1 e2 env (<)
  And e1 e2 -> boolOp e1 e2 env (&&)
  Or e1 e2  -> boolOp e1 e2 env (||)
  Var name  -> fromJust $ lookup name env


evalStatement :: Statement -> State Env ()
evalStatement = undefined

evalProgram :: Program -> State Env ()
evalProgram = undefined

-- Program futtatása üres környezetből indulva:
runProgram :: Program -> Env
runProgram prog = execState (evalProgram prog) []



-------- tests:

t = [t1_1, t1_2, t2_1, t2_2]

exp1 :: Exp
exp1 = 123 + 432 * 54 -- Add (IntLit 123) (Mul (IntLit 432) (IntLit 54))
t1_1 = exp1 == (Add (IntLit 123) (Mul (IntLit 432) (IntLit 54)))
t1_2 = evalExp exp1 [] == Left 23451

exp2 :: Exp
exp2 = 20 + "x" -- 
t2_1 = exp2 == (Add (IntLit 20) (Var "x"))
t2_2 = evalExp exp2 [("x", Left 10)] == Left 30


prog1 :: Program
prog1 = [
  "x"   := 0,
  "acc" := 0,
  While (Not (Eq "x" 20)) [
      "acc" := "acc" + 10,
      "x"   := "x" + 1
      ]
  ]
-- runProgram prog1 == [("x",Left 20),("acc",Left 200)]

prog2 :: Program
prog2 = [
  "b1" := true,
  "x"  := 10,
  If "b1" ["x" := 100] ["x" := 200]
  ]
-- runProgram prog2 == [("b1",Right True),("x",Left 100)]

fact :: Int -> Program
fact n = [
  "in"  := IntLit n,
  "out" := 1,
  While (Lt 0 "in") [
      "out" := "out" * "in",
      "in"  := "in" - 1
      ]
  ]
-- runProgram (fact 5) == [("in",Left 0),("out",Left 120)]
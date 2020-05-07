import Control.Monad

printNTimes :: Int -> String -> IO ()
printNTimes a s = replicateM_ a (putStrLn s)

f :: IO String
f = 
  getLine >>= \l1 ->
  getLine >>= \l2 ->
  putStrLn "hello world" >>= \_ ->
  putStrLn (l1 ++ l2) >>= \_ ->
  pure l2
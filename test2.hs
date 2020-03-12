
f :: (c -> a -> b) -> (c -> a) -> c -> b
f x y c = x c (y(c))

g :: (c -> a) -> (a -> c -> b) -> c -> b
g x y c = y (x c) c
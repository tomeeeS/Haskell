{-# LANGUAGE FlexibleInstances #-}
module Hf2 where 

import Prelude

data RoseTree a = Node { value    :: a
                       , children :: [RoseTree a]
                       }
  deriving (Eq, Show)

haskellTree :: RoseTree String
haskellTree = Node "Haskell"
            [ Node "is"
                [ Node "a" []
                , Node "purely" []
                ]
            , Node "functional"
                [ Node "programming" []
                , Node "language" []
                ]
            ]

newtype PreOrder  t a = PreOrder  { getPreOrder  :: t a } deriving (Eq, Show)
newtype PostOrder t a = PostOrder { getPostOrder :: t a } deriving (Eq, Show)

instance Foldable (PreOrder  RoseTree) where
    foldMap f = fold . map' f
  
    fold Nil = mempty
    fold (Cons x xs) = x <> fold xs
    
{-#    
instance Foldable (PostOrder RoseTree)
  foldMap f = fold . map' f

  fold Nil = mempty
  fold (Cons x xs) = x <> fold xs

preOrder :: RoseTree a -> [a]
preOrder (Node v []) = [v]
preOrder (Node v c) = [v] ++ (foldl (++) (map preOrder c) [])

-- postOrder :: RoseTree a -> [a]

#-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleInstances, InstanceSigs #-}
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

instance Foldable (PreOrder RoseTree) where
--instance Foldable RoseTree where
    --foldr :: (a -> b -> b) -> b -> (PreOrder RoseTree) a -> b 
    foldr f z (PreOrder node) = 
      case node of 
        (Node v []) -> f v z
        (Node v children) -> foldr f (f v z) (f v (foldList children)) where 
                                foldList :: [RoseTree a] -> b  -- can't be empty
                                foldList c:cs = foldr f z
                                foldList [c] = value

{-  
instance Foldable (PostOrder RoseTree)
  foldMap f = fold . map' f

  fold Nil = mempty
  fold (Cons x xs) = x <> fold xs

preOrder :: RoseTree a -> [a]
preOrder (Node v []) = [v]
preOrder (Node v c) = [v] ++ (foldl (++) (map preOrder c) [])

-- postOrder :: RoseTree a -> [a]

-}
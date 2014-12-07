module TrinaryTree where

import Control.Applicative

data TrinaryTree a = TEmpty | TTree a (TrinaryTree a) [a] (TrinaryTree a)

instance Functor TrinaryTree where
  fmap _ TEmpty = TEmpty
  fmap f (TTree x left center right) = TTree (f x) (f <$> left) (f <$> center) (f <$> right)

instance Show a => Show (TrinaryTree a) where
  show TEmpty = "TEmpty"
  show (TTree x left center right) =
    "TTree " ++ show x ++ "\n" ++
      "|- " ++ (shift $ show left) ++
      "|- " ++ (shift $ show center) ++
      "|- " ++ (shift $ show right)
    where shift s = unlines $ fmap ("  " ++) $ lines s

insert :: Ord a => a -> TrinaryTree a -> TrinaryTree a
insert x TEmpty = TTree x TEmpty [] TEmpty
insert y (TTree x left center right)
  | y <  x = TTree x (work left) center right
  | y == x = TTree x left (y : center) right
  | y >  x = TTree x left center (work right)
  where work = insert y

trinaryTreeToList :: TrinaryTree a -> [a]
trinaryTreeToList TEmpty = []
trinaryTreeToList (TTree x left center right) = (trinaryTreeToList left) ++ [x] ++ center ++ (trinaryTreeToList right)

main = do
  let x = foldl (flip insert) TEmpty [5, 4, 9, 5, 7, 2, 2]
  putStrLn $ show $ trinaryTreeToList x
  putStrLn $ show x

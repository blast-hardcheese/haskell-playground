module Oneliners where

import Control.Applicative
import Data.List (findIndex)

atoi :: String -> Maybe Int
atoi = foldl (\a x -> ((+) . (*10)) <$> a <*> findIndex (x ==) numbers) (Just 0)
  where numbers = "0123456789"

main = do
  putStrLn $ show $ atoi "512321"
  putStrLn $ show $ atoi "not a number"

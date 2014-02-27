module Exercises3 where

import Data.List

myLength :: [a] -> Integer
myLength []     = 0
myLength (x:xs) = 1 + myLength xs

mean :: (Fractional a) => [a] -> a
mean xs = sum xs / (fromIntegral (length xs))

toPalindrome :: [a] -> [a]
toPalindrome xs = xs ++ reverse xs

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []  = True
isPalindrome [x] = True
isPalindrome xs  = let (x, ys) = (head xs, tail xs)
                       (zs, y) = separateLast ys
                   in x == y && isPalindrome zs
  where separateLast [] = error "Can't separateLast an empty list"
        separateLast xs = let (x, ys) = (head (reverse xs), tail (reverse xs))
                          in (reverse ys, x)

sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy (\xs ys -> compare (length xs) (length ys))

myIntersperse :: a -> [[a]] -> [a]
myIntersperse _   []       = []
myIntersperse _   [xs]     = xs
myIntersperse sep (xs:xss) = xs ++ [sep] ++ myIntersperse sep xss

data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

height :: Tree a -> Integer
height Empty        = 0
height (Node _ l r) = 1 + max (height l) (height r)

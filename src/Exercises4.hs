module Exercises4 where

import Control.Monad (foldM, liftM, liftM2)
import Data.Char (digitToInt, isDigit, isSpace)
import Data.List (foldl')
import Data.Maybe (fromMaybe)

interactWith :: (String -> String) -> String -> String -> IO ()
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)
  
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f (x:xs) | f x       = let (fore, aft) = span f (x:xs)
                                 in fore : splitWith f aft
                   | otherwise = splitWith f xs

firstWordOfLine :: String -> String -> IO ()
firstWordOfLine = interactWith firstWord
  where firstWord = unlines . map (fromMaybe "" . safeHead . words) . lines

transposeText :: String -> String -> IO ()
transposeText = interactWith (unlines . transpose . lines)
  where transpose [] = []
        transpose xss | any null xss = []
                      | otherwise    = let (heads, tails) = unzip $ map behead xss
                                       in heads : transpose tails
        behead xs = (head xs, tail xs)

asInt_fold :: String -> Int
asInt_fold ""                         = error "no digits"
asInt_fold (c:s) | c == '-' && null s = error "no digits"
                 | c == '-'           = negate (parseInt_fold s)
                 | otherwise          = parseInt_fold (c:s)
  where parseInt_fold = foldl' adjoinDigit 0
        adjoinDigit n c | isDigit c = 10 * n + (digitToInt c)
                        | otherwise = error (c : " is not a digit")

type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either ""                         = Left "no digits"
asInt_either (c:s) | c == '-' && null s = Left "no digits"
                   | c == '-'           = liftM negate (parseInt_either s)
                   | otherwise          = parseInt_either (c:s)
  where parseInt_either = foldM adjoinDigit 0
        adjoinDigit n c | isDigit c = Right $ 10 * n + (digitToInt c)
                        | otherwise = Left (c : " is not a digit")

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

myTakeWhile_recur :: (a -> Bool) -> [a] -> [a]
myTakeWhile_recur _ [] = []
myTakeWhile_recur f (x:xs) | f x       = x : myTakeWhile_recur f xs
                           | otherwise = myTakeWhile_recur f xs

myTakeWhile_foldr :: (a -> Bool) -> [a] -> [a]
myTakeWhile_foldr f = foldr adjoin []
  where adjoin x xs | f x       = x:xs
                    | otherwise = []

myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy eq = foldr adjoin []
  where adjoin x []                       = [[x]]
        adjoin x ((y:ys):xss) | x `eq` y  = (x:y:ys) : xss
                              | otherwise = [x] : (y:ys) : xss
        adjoin _ _                        = error "empty head can't happen"

any_fold :: (a -> Bool) -> [a] -> Bool
any_fold f = foldr (\x b -> f x || b) False

cycle_fold :: [a] -> [a]
cycle_fold xs = foldr (:) (cycle_fold xs) xs

words_fold :: String -> [String]
words_fold = filter (not . null) . foldr adjoin []
  where adjoin c []      | isSpace c = []
                         | otherwise = [[c]]
        adjoin c ([]:ws) | isSpace c = [] : ws
                         | otherwise = [c] : ws
        adjoin c (w:ws)  | isSpace c = [] : w : ws
                         | otherwise = (c:w) : ws

unlines_fold :: [String] -> String
unlines_fold = foldr (\s acc -> s ++ "\n" ++ acc) ""

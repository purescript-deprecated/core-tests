module Test.Data.Array where

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Data.Array
import Data.Maybe
import Data.Tuple
import Data.Foldable (sum)
import Debug.Trace
import Global (nan, infinity)
import Test.QuickCheck
import Test.Classes

double :: Number -> Number
double x = x * 2

one :: forall a. a -> Number
one _ = 1

doubleAndOrig :: Number -> [Number]
doubleAndOrig x = [x * 2, x]

nil :: [Number]
nil = []

odd :: Number -> Boolean
odd n = n % 2 /= 0

main = do

  trace "test equality"
  check1 $ \n -> [n] == [n]

  trace "test inequality"
  check1 $ \n -> [n] /= [n + 1]

  trace "(!!) should return Just x when the index is within the bounds of the array"
  assert $ [1, 2, 3] !! 0 == (Just 1)
  assert $ [1, 2, 3] !! 1 == (Just 2)
  assert $ [1, 2, 3] !! 2 == (Just 3)

  trace "(!!) should return Nothing when the index is outside of the bounds of the array"
  assert $ [1, 2, 3] !! 6 == Nothing
  assert $ [1, 2, 3] !! -1 == Nothing

  trace "(!!) should return Nothing when the index not an integer"
  assert $ [1, 2, 3] !! 0.2 == Nothing
  assert $ [1, 2, 3] !! nan == Nothing
  assert $ [1, 2, 3] !! infinity == Nothing
  assert $ [1, 2, 3] !! -infinity == Nothing

  trace "snoc should add an item to the end of an array"
  assert $ [1, 2, 3] `snoc` 4 == [1, 2, 3, 4]
  assert $ nil `snoc` 1 == [1]

  trace "singleton should construct an array with a single value"
  check1 $ \x -> singleton x == [x]

  trace "head should return a Just-wrapped first value of a non-empty array"
  quickCheck $ \xs n -> head (n : xs) == Just (n :: Number)

  trace "head should return Nothing for an empty array"
  assert $ head nil == Nothing

  trace "last should return a Just-wrapped first value of a non-empty array"
  quickCheck $ \xs n -> last (xs `snoc` n) == Just (n :: Number)

  trace "last should return Nothing for an empty array"
  assert $ last nil == Nothing

  trace "tail should return a Just-wrapped array containing all the items in an array apart from the first for a non-empty array"
  quickCheck $ \xs n -> tail (n : xs) == Just (xs :: [Number])

  trace "tail should return Nothing for an empty array"
  assert $ tail nil == Nothing

  trace "init should return a Just-wrapped array containing all the items in an array apart from the first for a non-empty array"
  quickCheck $ \xs n -> init (xs `snoc` n) == Just (xs :: [Number])

  trace "init should return Nothing for an empty array"
  assert $ init nil == Nothing

  trace "null should return false for non-empty arrays"
  checkA1 $ \xs -> case xs of
    [] -> true
    xs -> null xs == false

  trace "null should return true for an empty array"
  assert $ null nil == true

  trace "map should transform every item in an array"
  checkA1 $ \xs -> sum (map double xs) == double (sum xs)

  trace "mapMaybe should transform every item in an array, throwing out Nothing values"
  assert $ mapMaybe (\x -> if x % 2 == 0 then Just x else Nothing) [0, 1, 2, 3, 4, 5] == [0, 2, 4]

  trace "length should return the number of items in an array"
  checkA1 $ \xs -> length xs == sum (map one xs)

  trace "elemIndex should return the index of an item in an array"
  assert $ (elemIndex 1 [1, 2, 1]) == 0
  assert $ (elemIndex 4 [1, 2, 1]) == -1

  trace "elemIndex should return the last index of an item in an array"
  assert $ (elemLastIndex 1 [1, 2, 1]) == 2
  assert $ (elemLastIndex 4 [1, 2, 1]) == -1

  trace "concat should join a list of lists"
  assert $ (concat [[1, 2], [3, 4]]) == [1, 2, 3, 4]
  assert $ (concat [[1], nil]) == [1]
  assert $ (concat [nil, nil]) == nil

  trace "reverse should reverse the order of items in an array"
  assert $ (reverse [1, 2, 3]) == [3, 2, 1]
  assert $ (reverse nil) == nil

  trace "reverse should remove the specified number of items from the front of an array"
  assert $ (drop 1 [1, 2, 3]) == [2, 3]
  assert $ (drop 2 [1, 2, 3]) == [3]
  assert $ (drop 1 nil) == nil

  trace "reverse should keep the specified number of items from the front of an array, discarding the rest"
  assert $ (take 1 [1, 2, 3]) == [1]
  assert $ (take 2 [1, 2, 3]) == [1, 2]
  assert $ (take 1 nil) == nil

  trace "insertAt should add an item at the specified index"
  assert $ (insertAt 0 1 [2, 3]) == [1, 2, 3]
  assert $ (insertAt 1 1 [2, 3]) == [2, 1, 3]

  trace "deleteAt should remove an item at the specified index"
  assert $ (deleteAt 0 1 [1, 2, 3]) == [2, 3]
  assert $ (deleteAt 1 1 [1, 2, 3]) == [1, 3]

  trace "updateAt should replace an item at the specified index"
  assert $ (updateAt 0 9 [1, 2, 3]) == [9, 2, 3]
  assert $ (updateAt 1 9 [1, 2, 3]) == [1, 9, 3]
  assert $ (updateAt 1 9 nil) == nil

  trace "concatMap should be equivalent to concat . map"
  checkA1 $ \xs -> concatMap doubleAndOrig xs == concat (map doubleAndOrig xs)

  trace "filter should remove items that don't match a predicate"
  assert $ filter odd (range 0 10) == [1, 3, 5, 7, 9]

  trace "range should create an inclusive array of integers for the specified start and end"
  assert $ range 0 5 == [0, 1, 2, 3, 4, 5]
  assert $ range 2 (-3) == [2, 1, 0, -1, -2, -3]

  trace "zipWith should use the specified function to zip two lists together"
  assert $ zipWith Tuple [1, 2, 3] ["a", "b", "c"] == [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"]

  trace "nub should remove duplicate items from the list"
  assert $ nub [1, 2, 2, 3, 4, 1] == [1, 2, 3, 4]

  trace "nubBy should remove duplicate items from the list using a supplied predicate"
  let nubPred = \x y -> if odd x then false else x == y
  assert $ nubBy nubPred [1, 2, 2, 3, 3, 4, 4, 1] == [1, 2, 3, 3, 4, 1]

  trace "sort should reorder a list into ascending order based on the result of compare"
  assert $ sort [1, 3, 2, 5, 6, 4] == [1, 2, 3, 4, 5, 6]

  let ty = [0]

  trace "test functor laws"
  checkFunctor ty

  trace "test applicative laws"
  checkApplicative ty ty ty

  trace "test monad laws"
  checkMonad ty


assert :: Boolean -> QC {}
assert = quickCheck' 1

check1 :: (Number -> Boolean) -> QC {}
check1 = quickCheck

checkA1 :: ([Number] -> Boolean) -> QC {}
checkA1 = quickCheck

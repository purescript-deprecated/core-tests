module Test.Data.Tuple where

import Data.Array
import Data.Tuple
import Debug.Trace
import Control.Monad.Eff
import Math
import Test.QuickCheck
import Test.QuickCheck.Tuple
import Test.Classes

type Ty = Tuple [Number] [Number]

main = do

  let ty = Tuple [0] [1]

  trace "test equality"
  check2 $ \x y -> Tuple x y == Tuple x y
  
  trace "test inequality"
  check2 $ \x y -> not $ Tuple x y == Tuple (x + 1) y
  
  trace "test ordering on the first element"
  check3 \a b x -> compare (Tuple a x) (Tuple b x) == compare a b
  
  trace "test ordering on the second element (when first element is equal)"
  check3 \a b x -> compare (Tuple x a) (Tuple x b) == compare a b
 
  {- 
  trace "test functor laws"
  checkFunctor ty

  trace "test applicative laws"
  checkApplicative ty ty ty

  trace "test monad laws"
  checkMonad ty
  -}

  trace "fst should return the first element"
  check2 $ \x y -> fst (Tuple x y) == x
  
  trace "snd should return the second element"
  check2 $ \x y -> snd (Tuple x y) == y
  
  trace "curry should maintain argument order"
  check2 $ \x y -> curry (\(Tuple x y) -> x / y) x y == x / y
  
  trace "uncurry should maintain argument order"
  check2 $ \x y -> uncurry (\x y -> x / y) (Tuple x y) == x / y
  
  trace "zip should produce a list that matches the length of the shorter input"
  let testZipSize :: [Number] -> [Number] -> Boolean
      testZipSize xs ys = min (length xs) (length ys) == length (zip xs ys)
  quickCheck testZipSize
  
  trace "zip should produce a list of tuples maintaining argument order"
  let testZipPairs :: [Number] -> [Number] -> Boolean
      testZipPairs xs ys = compareZip xs ys (zip xs ys)
  quickCheck testZipPairs
  
  trace "unzip should produce a tuple of lists that match the input length"
  let testUnzipSize :: [TestTuple Number Number] -> Boolean
      testUnzipSize ts = case unzip (map runTestTuple ts) of 
        (Tuple xs ys) -> length ts == length xs && length ts == length ys
  quickCheck testUnzipSize
  
  trace "unzip should produce tuple of a lists with elements in the original order"
  let testUnzipPairs :: [TestTuple Number Number] -> Boolean
      testUnzipPairs ts = 
        let ts' = map runTestTuple ts in 
        case unzip ts' of 
          (Tuple xs ys) -> compareZip xs ys ts'
  quickCheck testUnzipPairs
  
  trace "swap should switch the first and second element"
  check2 $ \x y -> swap (Tuple x y) == (Tuple y x)

check2 :: (Number -> Number -> Boolean) -> QC {}
check2 = quickCheck

check3 :: (Number -> Number -> Number -> Boolean) -> QC {}
check3 = quickCheck

instance arbAToTupleBC :: (Arbitrary (a -> b), Arbitrary (a -> c)) => Arbitrary (a -> Tuple b c) where
  arbitrary = do
    f <- arbitrary
    g <- arbitrary
    return $ \x -> Tuple (f x) (g x)

compareZip :: forall a b. (Eq a, Eq b) => [a] -> [b] -> [Tuple a b] -> Boolean
compareZip (x : xs) (y : ys) ((Tuple x' y') : ts) = x == x' && y == y' && compareZip xs ys ts
compareZip [] _  [] = true
compareZip _  [] [] = true
compareZip _  _  _  = false

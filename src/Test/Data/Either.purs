module Test.Data.Either where

import Data.Either
import Debug.Trace
import Control.Monad.Eff
import Test.QuickCheck
import Test.QuickCheck.Either
import Test.Classes

type Ty = Either Number Number

main = do

  trace "test equality"
  check1 $ \n -> Left n  == Left n :: Ty
  check1 $ \n -> Right n == Right n :: Ty

  trace "test inequality"
  check1 $ \n -> Left n  /= Right n :: Ty
  check1 $ \n -> Right n /= Left n :: Ty
  check1 $ \n -> Left n  /= Left (n + 1) :: Ty
  check1 $ \n -> Right n /= Right (n + 1) :: Ty

  trace "test order"
  check2 $ \x y -> compare (Left x :: Ty)  (Left y)  == compare x y
  check2 $ \x y -> compare (Left x :: Ty)  (Right x) == LT
  check2 $ \x y -> compare (Right x :: Ty) (Right y) == compare x y
  check2 $ \x y -> compare (Right x :: Ty) (Left y)  == GT

  trace "either should transform values wrapped in an Either"
  check1 $ \n -> either negate id (Left n :: Ty)  == -n
  check1 $ \n -> either id negate (Right n :: Ty) == -n

  trace "isLeft should return the appropriate value"
  assert $ isLeft (Left {})  == true
  assert $ isLeft (Right {}) == false

  trace "isRight should return the appropriate value"
  assert $ isRight (Right {}) == true
  assert $ isRight (Left {})  == false
  
  let tty = TestEither (Left 0) :: (TestEither Number Number)

  trace "test functor laws"
  checkFunctor tty

  trace "test applicative laws"
  checkApplicative tty tty tty

  trace "test monad laws"
  checkMonad tty

assert :: Boolean -> QC {}
assert = quickCheck' 1

check1 :: (Number -> Boolean) -> QC {}
check1 = quickCheck

check2 :: (Number -> Number -> Boolean) -> QC {}
check2 = quickCheck

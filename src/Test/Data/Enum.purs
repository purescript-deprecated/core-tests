module Test.Data.Enum where

import Data.Maybe
import Data.Enum
import Debug.Trace
import Control.Monad.Eff
import Test.QuickCheck

data TestEnum = A | B | C

instance eqTestEnum :: Eq TestEnum where
  (==) A A = true
  (==) B B = true
  (==) C C = true
  (==) _ _ = false
  (/=) x y = not (x == y)

instance enumTestEnum :: Enum TestEnum where
  toEnum 1 = Just A
  toEnum 2 = Just B
  toEnum 3 = Just C
  toEnum _ = Nothing
  fromEnum A = 1
  fromEnum B = 2
  fromEnum C = 3

main = do

  let ty = Just 0

  trace "succ should return the next enum value"
  assert $ succ A == Just B
  assert $ succ B == Just C
  
  trace "succ should return nothing for the last enum value in a sequence"
  assert $ succ C == Nothing
  
  trace "pred should return the previous enum value"
  assert $ pred B == Just A
  assert $ pred C == Just B
  
  trace "pred should return nothing for the first enum value in a sequence"
  assert $ pred A == Nothing


assert :: Boolean -> QC Unit
assert = quickCheck' 1

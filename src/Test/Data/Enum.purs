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

  (/=) a b = not (a == b)

instance ordTestEnum :: Ord TestEnum where
  compare a b = fromEnum a `compare` fromEnum b

instance enumTestEnum :: Enum TestEnum where
  cardinality = Cardinality 3

  firstEnum = A

  lastEnum = C

  succ A = Just B
  succ B = Just C
  succ C = Nothing

  pred A = Nothing
  pred B = Just A
  pred C = Just B

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

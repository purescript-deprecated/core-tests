module Test.Global where

import Global
import Math
import Debug.Trace
import Control.Monad.Eff
import Test.QuickCheck

main = do

  trace "nan should be NaN"
  assert $ nan /= nan -- NaN is not equal to itself in JS
  
  trace "isNaN should return the appropriate value"
  assert $ isNaN nan
  quickCheck $ \x -> isNaN x == false
  
  trace "infinity should be Infinity"
  assert $ infinity == 1 / 0
  
  trace "isFinite should return the appropriate value"
  assert $ not (isFinite infinity)
  quickCheck $ \x -> isFinite x == (x /= infinity)
  
  trace "readInt should return the appropriate value"
  assert $ readInt 2 "111" == 7
  assert $ readInt 10 "111" == 111
  assert $ readInt 16 "0xFF" == 255
  assert $ readInt 10 "3.6" == 3
  
  trace "readFloat should return the appropriate value"
  assert $ readFloat "0" == 0
  assert $ readFloat "0.2" == 0.2

assert :: Boolean -> QC Unit
assert = quickCheck' 1

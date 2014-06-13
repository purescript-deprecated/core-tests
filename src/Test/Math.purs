module Test.Math where

import Global
import Math
import Debug.Trace
import Control.Monad.Eff
import Test.QuickCheck

infix 4 =~=

-- Approximate equality to overcome precision issues
(=~=) :: Number -> Number -> Boolean
(=~=) x y = (y - x) <= epsilon && (y - x) >= (-epsilon)
  where
  epsilon = 0.00000001

main = do

  trace "abs should return an absolute value"
  assert $ abs (-1) == 1
  assert $ abs 1 == 1
  
  trace "acos should compute arccosine"
  assert $ acos 0 == pi / 2
  assert $ acos 1 == 0
 
  trace "asin should compute arcsine"
  assert $ asin 0 == 0
  assert $ asin 1 == pi / 2
 
  trace "atan should compute arctangent"
  assert $ atan 0 == 0
  assert $ atan 1 == pi / 4
  
  trace "atan2 should compute arctangent (y/x)"
  assert $ atan2 0 1 == 0
  assert $ atan2 1 0 == pi / 2
  
  trace "ceil should round up"
  assert $ ceil 0 == 0
  assert $ ceil 0.1 == 1
  assert $ ceil 0.9  == 1
  assert $ ceil 1  == 1
  
  trace "cos should compute cosine"
  assert $ cos 0 == 1
  assert $ cos (pi / 2) =~= 0
  assert $ cos pi == -1
  
  trace "exp should compute e^x"
  assert $ exp 1 == e
  assert $ exp 2 =~= e * e
  
  trace "floor should round down"
  assert $ floor 0 == 0
  assert $ floor 0.1 == 0
  assert $ floor 0.9  == 0
  assert $ floor 1  == 1
  
  trace "log should compute a natural logarithm"
  assert $ log 0 == (-infinity)
  assert $ log e == 1
  
  trace "max should return the larger input"
  assert $ max 10 20 == 20
  assert $ max (-infinity) infinity == infinity
    
  trace "min should return the larger input"
  assert $ min 10 20 == 10
  assert $ min (-infinity) infinity == (-infinity)
  
  trace "pow should raise a number to the specified power"
  assert $ pow 2 1 == 2
  assert $ pow 2 4 == 2 * 2 * 2 * 2
  assert $ pow 6 3 == 6 * 6 * 6
  
  trace "round should round up or down"
  assert $ round 0 == 0
  assert $ round 0.1 == 0
  assert $ round 0.9  == 1
  assert $ round 1  == 1
  
  trace "sin should compute sine"
  assert $ sin 0 == 0
  assert $ sin (pi / 2) == 1
  assert $ sin pi =~= 0
  
  trace "sqrt should compute square roots"
  assert $ sqrt 1 == 1
  assert $ sqrt 4 == 2
  assert $ sqrt 9 == 3
  assert $ sqrt 0.25 == 0.5
  
  trace "tan should compute tangents"
  assert $ tan 0 == 0
  assert $ tan 1 =~= 1.5574077246549025
  assert $ tan pi =~= 0
  
  trace "constants should be defined"
  assert $ e =~= 2.718281828459045
  assert $ ln2 =~= log 2
  assert $ ln10 =~= log 10
  assert $ log2e =~= 1.4426950408889634
  assert $ log10e =~= 0.4342944819032518
  assert $ pi =~= 3.141592653589793
  assert $ sqrt1_2 =~= sqrt (1 / 2)
  assert $ sqrt2 =~= sqrt 2

assert :: Boolean -> QC Unit
assert = quickCheck' 1

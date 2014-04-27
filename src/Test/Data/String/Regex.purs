module Test.Data.String.Regex where

import Data.String.Regex
import Debug.Trace
import Control.Monad.Eff
import qualified Test.QuickCheck as QC

assert :: Boolean -> QC.QC {}
assert = QC.quickCheck' 1

main = do

  let r = regex "test" "g"

  trace "test 'test' matches correctly"
  assert $ test r "testing"
  trace "test 'test' reports false for a mismatch"
  assert $ not $ test r "foo"

  trace "test 'replaceR' replaces all matches"
  assert $ replaceR r "work" "testing testing 1 2 3" == "working working 1 2 3" 

  trace "test 'search' finds the first match"
  assert $ search r "the string 'test' first appears in this test at index 12" == 12

  trace "test 'match' reports the correct number of matches"
  assert $ match r "test test test" == ["test", "test", "test"]

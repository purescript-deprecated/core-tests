module Test.Data.String.Regex where

import Data.Array (reverse)
import Data.String (joinWith)
import Data.String.Regex
import Debug.Trace
import Control.Monad.Eff
import qualified Test.QuickCheck as QC

assert :: Boolean -> QC.QC Unit
assert = QC.quickCheck' 1

main = do

  let flags = { unicode: false, sticky: false, multiline: false, ignoreCase: false, global: true } 
  let r = regex "test" flags

  trace "test 'test' matches correctly"
  assert $ test r "testing"
  trace "test 'test' reports false for a mismatch"
  assert $ not $ test r "foo"

  trace "test 'replace' replaces all matches"
  assert $ replace r "work" "testing testing 1 2 3" == "working working 1 2 3"

  trace "test 'replace'' replaces matches with a function"
  assert $ replace' (regex "(some) (test)" flags) (\match groups -> joinWith " " (reverse groups)) "here are some test words" == "here are test some words"

  trace "test 'search' finds the first match"
  assert $ search r "the string 'test' first appears in this test at index 12" == 12

  trace "test 'match' reports the correct number of matches"
  assert $ match r "test test test" == ["test", "test", "test"]

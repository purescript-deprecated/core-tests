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


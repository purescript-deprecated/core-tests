module AllTests where

import Control.Monad.Eff
import Debug.Trace

main = do
  trace ""
  trace ":: Data.Maybe ::"
  trace ""
  Test.Data.Maybe.main
  
  trace ""
  trace ":: Data.Either ::"
  trace ""
  Test.Data.Either.main

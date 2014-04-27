module AllTests where

import Control.Monad.Eff
import Debug.Trace

main = do
  
  trace ""
  trace ":: Data.Array ::"
  trace ""
  Test.Data.Array.main
  
  trace ""
  trace ":: Data.Either ::"
  trace ""
  Test.Data.Either.main
    
  trace ""
  trace ":: Data.Enum ::"
  trace ""
  Test.Data.Enum.main
  
  trace ""
  trace ":: Data.Maybe ::"
  trace ""
  Test.Data.Maybe.main
  
  trace ""
  trace ":: Data.Tuple ::"
  trace ""
  Test.Data.Tuple.main
  
  trace ""
  trace ":: Data.Validation ::"
  trace ""
  Test.Data.Validation.main
  
  trace ""
  trace ":: Global ::"
  trace ""
  Test.Global.main
  
  trace ""
  trace ":: Math ::"
  trace ""
  Test.Math.main

  trace ""
  trace ":: Data.String.Regex ::"
  trace ""
  Test.Data.String.Regex.main

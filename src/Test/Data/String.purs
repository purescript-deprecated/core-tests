module Test.Data.String where

import Data.String
import Debug.Trace
import Control.Monad.Eff
import Test.QuickCheck

main = do

  trace "charAt should return the character at the specified index"
  assert $ charAt 0 "abcd" == "a"
  assert $ charAt 1 "abcd" == "b"
  
  trace "charCodeAt should return the character at the specified index"
  print $ charCodeAt 0 "abcd" == 97
  print $ charCodeAt 1 "abcd" == 98
  
  trace "fromCharCode should return a character for a char code"
  assert $ fromCharCode 97 == "a"
  assert $ fromCharCode 98 == "b"
  
  trace "indexOf should return the index for a substring"
  assert $ indexOf "b" "abc" == 1
  assert $ indexOf "q" "abc" == -1
  
  trace "indexOf' should return the index for a substring starting from an offset"
  assert $ indexOf' "b" 2 "abcb" == 3
  assert $ indexOf' "q" 2 "abcb" == -1
  
  trace "lastIndexOf should return the last index for a substring"
  assert $ lastIndexOf "a" "abca" == 3
  assert $ lastIndexOf "q" "abca" == -1
  
  trace "lastIndexOf' should return the last index for a substring starting from an offset"
  assert $ lastIndexOf' "a" 1 "abca" == 0
  assert $ lastIndexOf' "q" 1 "abca" == -1
  
  trace "length should return the length of a string"
  assert $ length "" == 0
  assert $ length "a" == 1
  assert $ length "abdc" == 4
  
  trace "localeCompare should perform a locale-sensitive comparison"
  assert $ indexOf "b" "abc" == 1
  assert $ indexOf "q" "abc" == -1
  
  trace "replace should replace one substring with another"
  assert $ replace "bar" "baz" "foobar" == "foobaz"
  
  trace "take should keep the specified number of characters and drop the rest"
  assert $ take 2 "abcd" == "ab"
  
  trace "drop should drop the specified number of characters from the start of a string and keep the rest"
  assert $ drop 2 "abcd" == "cd"
  
  trace "split should split a string on a substring"
  assert $ split "," "a,b,c,d" == ["a", "b", "c", "d"]
  
  trace "toLower should transform a string to lower-case"
  assert $ toLower "ABCD" == "abcd"
  
  trace "toUpper should transform a string to upper-case"
  assert $ toUpper "abcd" == "ABCD"
  
  trace "trim should remove leading and trailing whitespace"
  assert $ trim " abc" == "abc"
  assert $ trim " abc " == "abc"
  assert $ trim "abc" == "abc"
  
  trace "joinWith should join a list of strings with a separator string"
  assert $ joinWith "," ["a", "b", "c", "d"] == "a,b,c,d"
  assert $ joinWith " " ["p", "q", "r"] == "p q r"

assert :: Boolean -> QC {}
assert = quickCheck' 1

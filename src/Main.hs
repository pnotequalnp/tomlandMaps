module Main (main) where

import Map1 qualified
import Map2 qualified
import Map3 qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain . testGroup "tomland maps" =<< sequenceA
    [ Map1.tests
    , Map2.tests
    , Map3.tests
    ]

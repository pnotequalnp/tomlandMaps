{-# LANGUAGE OverloadedLists #-}

-- | A slightly more complicated map that uses lists of records. It has the same issues but now
-- passes the isomorphism tests, for reasons I don't understand.
module Map2 (tests) where

import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Text (Text)
import Data.Traversable (for)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Toml

type Map2 = Map Text [Entry]

data Entry = Entry
  { field1 :: Text
  , field2 :: Text
  }
  deriving (Eq, Show)

map2Codec :: TomlCodec Map2
map2Codec = tableMap _KeyText (list entryCodec) "key"

entryCodec :: TomlCodec Entry
entryCodec = Entry
  <$> text "field1" .= field1
  <*> text "field2" .= field2

tests :: IO TestTree
tests = do
  parsing <- fmap (testGroup "parsing") . for tomls $ \(filename, expected) -> do
    actual <- decodeFileExact map2Codec ("data/map2/" <> filename <> ".toml")
    pure $ testCase filename (actual @?= Right expected)

  let isomorphism = testGroup "isomorphism" $ tomls <&> \(name, toml) ->
        testCase name (decodeExact map2Codec (encode map2Codec toml) @?= Right toml)

  pure (testGroup "map2" [parsing, isomorphism])

tomls :: [(FilePath, Map2)]
tomls =
  [ ("a", [ ("foo", [ Entry "a" "b"
                    , Entry "c" "d"
                    , Entry "e" "f"
                    ])
          ])
  , ("b", [ ("foo", [ Entry "a" "b"
                    , Entry "c" "d"
                    ])
          , ("bar", [ Entry "e" "f"
                    , Entry "g" "h"
                    ])
          ])
  ]

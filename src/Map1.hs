{-# LANGUAGE OverloadedLists #-}

-- | This is a simple map of records at the top level. The issue is @tableMap@'s insistence on
-- having a key. With that, it will not support the "top-level" table. Interestingly, this also
-- fails the isomorphism tests. That is, decoding the encoded data does not result in the original
-- data.
module Map1 (tests) where

import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Text (Text)
import Data.Traversable (for)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Toml

type Map1 = Map Text Entry

data Entry = Entry
  { field1 :: Text
  , field2 :: Text
  }
  deriving (Eq, Show)

map1Codec :: TomlCodec Map1
map1Codec = tableMap _KeyText (const entryCodec) "key"

entryCodec :: TomlCodec Entry
entryCodec = Entry
  <$> text "field1" .= field1
  <*> text "field2" .= field2

tests :: IO TestTree
tests = do
  parsing <- fmap (testGroup "parsing") . for tomls $ \(filename, expected) -> do
    actual <- decodeFileExact map1Codec ("data/map1/" <> filename <> ".toml")
    pure $ testCase filename (actual @?= Right expected)

  let isomorphism = testGroup "isomorphism" $ tomls <&> \(name, toml) ->
        testCase name (decodeExact map1Codec (encode map1Codec toml) @?= Right toml)

  pure (testGroup "map1" [parsing, isomorphism])


tomls :: [(FilePath, Map1)]
tomls =
  [ ("a", [ ("foo", Entry "bar" "baz")
          ])
  , ("b", [ ("foo", Entry "bar" "baz")
          , ("bar", Entry "baz" "quux")
          ])
  ]

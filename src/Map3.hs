{-# LANGUAGE OverloadedLists #-}

-- | This is the same as Map2 except with different @toml@ files to attempt to accommodate the
-- forced key in the @tableMap@ codec. @a.toml@ and @b.toml@ use the key explicitly but still fail
-- to parse. @c.toml@ is a copy of @b.toml@ with an empty @[key]@ entry at the top, and is the only
-- file to successfully parse, despite being equivalent to @b.toml@.
module Map3 (tests) where

import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Text (Text)
import Data.Traversable (for)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Toml

type Map3 = Map Text [Entry]

data Entry = Entry
  { field1 :: Text
  , field2 :: Text
  }
  deriving (Eq, Show)

map3Codec :: TomlCodec Map3
map3Codec = tableMap _KeyText (list entryCodec) "key"

entryCodec :: TomlCodec Entry
entryCodec = Entry
  <$> text "field1" .= field1
  <*> text "field2" .= field2

tests :: IO TestTree
tests = do
  parsing <- fmap (testGroup "parsing") . for tomls $ \(filename, expected) -> do
    actual <- decodeFileExact map3Codec ("data/map3/" <> filename <> ".toml")
    pure $ testCase filename (actual @?= Right expected)

  let isomorphism = testGroup "isomorphism" $ tomls <&> \(name, toml) ->
        testCase name (decodeExact map3Codec (encode map3Codec toml) @?= Right toml)

  pure (testGroup "map3" [parsing, isomorphism])

tomls :: [(FilePath, Map3)]
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
  , ("c", [ ("foo", [ Entry "a" "b"
                    , Entry "c" "d"
                    ])
          , ("bar", [ Entry "e" "f"
                    , Entry "g" "h"
                    ])
          ])
  ]

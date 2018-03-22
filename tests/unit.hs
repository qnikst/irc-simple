module Main
  where

import Test.Tasty
import qualified Test.Protocol.Parser

main = defaultMain $
  testGroup "unit"
    [ Test.Protocol.Parser.tests
    ]

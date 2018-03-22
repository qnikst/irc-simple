{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Protocol.Parser
  ( tests
  ) where

import Control.Lens
import Control.Applicative
import Data.Attoparsec.Text
import Data.Coerce
import Data.Monoid
import Data.String
import Protocol.Wire (message, toWire)
import Protocol.Types
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as TextL
import qualified Data.Text.Lazy as TextL
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series (Serial(..), decDepth)
import qualified Test.SmallCheck.Series as SC

tests :: TestTree
tests = testGroup "parser"
  [ testGroup "RFC examples" testsRFC
  -- , testGroup "properties" testProp
  ]

testsRFC = map (uncurry mkTest)
   [ ("NICK Wiz",
        mkTextCommand "NICK" & params .~ ["Wiz"])
   --, (":WiZ!jto@tolsun.oulu.fi NICK Kilroy",
   --     mkTextCommand "JOIN" & prefix .~ (Just ":WiZ!jto@tolsun.oulu.fi")
   --                          & params .~ ["Kilroy"])
   , ("JOIN #foobar", Message Nothing "JOIN" ["#foobar"] Nothing)
   , ("JOIN &foo fubar", Message Nothing "JOIN" ["&foo", "fubar"] Nothing)
   , ("JOIN #foo,&bar fubar", Message Nothing "JOIN" ["#foo,&bar", "fubar"] Nothing)
   -- , (":WiZ!jto@tolsun.oulu.fi JOIN #Twilight_zone"
   --   , Message (Just "WiZ!jto@tolsun.oulu.fi") "JOIN" ["#Twilight_zone"] Nothing)
   , ("PART #twilight_zone",
        Message Nothing "PART" ["#twilight_zone"] Nothing)
   , (":WiZ PART #playzone :I lost",
        mkTextCommand "PART" & prefix .~ (Just "WiZ")
                             & params .~ ["#playzone"]
                             & trailing .~ Just "I lost")
   , (":Angel PRIVMSG Wiz :Are you receiving this message ?",
        mkTextCommand "PRIVMSG" & prefix .~ (Just "Angel")
                                & params .~ ["Wiz"]
                                & trailing .~ Just "Are you receiving this message ?")
   , ("PRIVMSG Angel :yes I'm receiving it !",
        Message Nothing "PRIVMSG" ["Angel"] (Just "yes I'm receiving it !"))
   ]
  where
    mkTest msg pattern = testCase msg $ parseOnly message (fromString msg <> "\r\n") @?= Right pattern

{-

testProp =
  [ SC.testProperty "parse . toWire == id" $
      \msg ->
         let s = TextL.toStrict $ TextL.toLazyText $ toWire msg
             a = parseOnly message s
             b = Right msg :: Either String Message
         in if a == b then Right ("OK"::String)
                      else Left $ unlines [ "Request: " <> show msg
                                          , "Line:    " <> show (Text.unpack s)
                                          , "Result:  " <> show a
                                          ]
  ]

instance Monad m => Serial m Channel where
  series = coerce . Text.pack <$> series
instance Monad m => Serial m Nickname where
  series = coerce . Text.pack <$> series
instance Monad m => Serial m Command where
  series = TextCommand . Text.pack . SC.getNonEmpty <$> series
        <|> IntCommand <$> series
instance Monad m => Serial m Code
instance Monad m => Serial m Message where
  series = decDepth $ Message <$> series
                              <*> series
                              <*> series
                              <*> fmap (fmap Text.pack) series
instance Monad m => Serial m Prefix where
  series = coerce . Text.pack <$> series
instance Monad m => Serial m Param where
  series = coerce . Text.pack <$> series
-}

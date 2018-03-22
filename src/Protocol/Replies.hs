{-# LANGUAGE OverloadedStrings #-}
module Protocol.Replies
  ( ReplyTopic(..)
  , NamReply(..)
  ) where

import Control.Lens
import Data.Coerce
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Set (Set)
import qualified Data.Set as Set
import Protocol.Types

data ReplyTopic = ReplyTopic !Channel !Text

instance IsMessage ReplyTopic where
  toMessage (ReplyTopic chan topic) = mkCodeCommand (Code 3 3 2)
    & params .~ [toParam chan]
    & trailing ?~ topic

data NamReply = NamReply !Channel !(Set Nickname)

instance IsMessage NamReply where
  toMessage (NamReply chan nicks)  = mkCodeCommand (Code 3 5 3)
    & params .~ [toParam chan]
    & trailing ?~ Text.intercalate " " (coerce Set.toAscList nicks)

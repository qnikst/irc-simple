-- После того как я написал код, я начал с этого, я понял
-- что мне очень не нравится это файл и я бы все переделал.
--
-- Отображать все сообщения для ADT та ещё задача тем более,
-- что их будет много. Даже если это и нормально, то парсер
-- написан не самым эффективным образом. Поэтому лучше как-то
-- объединить парсер с case split в коде Server.hs.
--
-- Впрочем это работает и можно обосновать.
{-# LANGUAGE OverloadedStrings #-}
module Protocol
  ( Message(..)
  , T.Nickname(..)
  , T.Channel(..)
  , T.MsgTo(..)
  , protocol
  ) where

import           Control.Lens
import           Control.Monad.Catch
import qualified Protocol.Wire as Wire
import           Protocol.Types (Protocol(..), params, trailing, mkTextCommand)
import qualified Protocol.Types as T
import           Data.Conduit
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Foldable
import qualified Data.Conduit.List as CL

-- | Типизированное сообщение, поддерживаемое сервером. Если
-- у нас какие-то проблемы с контентом сообщения то мы это
-- увидим.
data Message
  = -- | присоединиться к каналу
    Join T.Channel
    -- | Уйти с канала
  | Part T.Channel (Maybe Text)
    -- | Послать сообщение
  | PrivMsg T.MsgTarget Text
    -- | Сменить ник
  | Nick T.Nickname
  deriving (Show)

-- | Конвертация в нетипизированное сообщение
instance Wire.IsMessage Message where
  toMessage (Join chan)  =
    mkTextCommand "JOIN" & params .~ [T.toParam chan]
  toMessage (Part chan reason)  =
    mkTextCommand "PART" & params .~ [T.toParam chan]
                         & trailing .~ reason
  toMessage (PrivMsg trgt text) =
    mkTextCommand "PRIVMSG" & params .~ [T.toParam trgt]
                            & trailing .~ Just text
  toMessage (Nick nick)         =
    mkTextCommand "NICK" & params .~ [T.toParam nick]

-- Протокол, см. Protocol.Ty[es
protocol :: MonadThrow m => Protocol m Wire.Message Message ByteString
protocol = Protocol
  { encodeStream = encodeStream Wire.protocol
  , decodeStream = decodeStream Wire.protocol
                .| CL.mapMaybe parseCommand
  }

-- | Разбор сообщений
--
-- XXX: тут можно использовать either и накапливать и выводить ошибки, так
--      же поддерживать warnings, но мне лень
parseCommand :: Wire.Message -> Maybe Message
parseCommand msg = asum
  [ do Wire.Message Nothing (Wire.TextCommand "JOIN") [mchannel] _ <- pure msg
       chan <- Wire.parseParam Wire.channel mchannel
       pure $ Join chan
  , do Wire.Message Nothing (Wire.TextCommand "PART") [mchannel] reason <- pure msg
       chan <- Wire.parseParam Wire.channel mchannel
       pure $ Part chan reason
  , do Wire.Message Nothing (Wire.TextCommand "PRIVMSG") [mrecipient] (Just text) <- pure msg
       target <- Wire.parseParam Wire.msgtarget mrecipient
       pure $ PrivMsg target text
  , do Wire.Message Nothing (Wire.TextCommand "NICK") [muser] Nothing <- pure msg
       user <- Wire.parseParam Wire.nickname muser
       pure $ Nick user
  ]

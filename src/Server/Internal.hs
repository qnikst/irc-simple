{-# LANGUAGE FlexibleInstances #-}   -- Смешные расширения, компилятор говорит сам,
{-# LANGUAGE FlexibleContexts #-}    -- когда они нужны.

-- |
-- Внутренняя часть сервера и используемые структуры данных
-- Данный уроверь может предоставлять доступ к внутреннему
-- интерфейсу и отвечает за отправку сообщений между пользователями.
module Server.Internal
  ( ServerState
  , withServerState
    -- * API
  , HasServerState(..)
  , askServerChannels
  , updateServerChannels
  , askServerUsers
  , updateServerUsers
  , send
    -- * Types
  , Channel(..)
  , ChannelState(..)
  , UserInfo(..)
  , Message(..)
  ) where

import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.Base
import           Control.Monad.Reader
import           Data.Foldable (for_)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import           Data.Text (Text)
import           Protocol  (Nickname, Channel)
import qualified Protocol
import qualified Protocol.Wire as Wire


-- | безопасное использование ресурса, аналог @try {} finally {}@
withServerState :: (ServerState -> IO a) -> IO a
withServerState =
  bracket (ServerState <$> newTVarIO Map.empty
                       <*> newTVarIO Map.empty)
          (\_ -> pure ())

-- | Сообщения, которыми общается сервер, отличается тем, что нём
-- используется префикс реального пользователя.
data Message = Message
  { _smTarget  :: Nickname
  , _smMessage :: Protocol.Message
  }

-- Такое сообщенгие может быть конвертировано в нетипизированное
instance Wire.IsMessage Message where
  toMessage (Message nm p) = case Wire.toMessage p of
    Wire.Message _ a b c -> Wire.Message (Just $ Wire.Prefix nm) a b c

-- | Состояние сервера.
data ServerState = ServerState
  { serverUsers :: TVar (Map Nickname UserInfo)
  , serverChannels :: TVar (Map Channel ChannelState)
  }

-- | Состояние канала.
--
-- XXX: сильно упрощено, т.к. не хранится информация о модах, банах и
-- привелегиях на канале.
data ChannelState = ChannelState
  { channelUsers :: Set Nickname
  , channelTopic :: Text
  , channelName  :: Channel
  }

-- | Информация о пользователе, канал, который используется для
-- отправки сообщений.
newtype UserInfo = UserInfo
  { userQueue :: Message -> STM ()
  }

-- | Интерфейс описывающий то, что возможна работа с состоянием
class HasServerState m where
  askServerState :: m ServerState

instance Monad m => HasServerState (ReaderT ServerState m) where
  askServerState = ask

-- Куча вспомогательных функций

askServerUsers :: (MonadBase STM m, HasServerState m) => m (Map Nickname UserInfo)
askServerUsers =
  liftBase . readTVar =<< fmap serverUsers askServerState

updateServerUsers :: (MonadBase STM m, HasServerState m)
                  => (Map Nickname UserInfo -> Map Nickname UserInfo)
                  -> m ()
updateServerUsers f = do
  users <- fmap serverUsers askServerState
  liftBase $ modifyTVar users f

askServerChannels :: (MonadBase STM m, HasServerState m) => m (Map Channel ChannelState)
askServerChannels =
  liftBase . readTVar =<< fmap serverChannels askServerState

updateServerChannels :: (MonadBase STM m, HasServerState m) => Channel -> ChannelState -> m ()
updateServerChannels ch st = do
  tv <- fmap serverChannels askServerState
  liftBase $ modifyTVar tv $ Map.insert ch st

-- | Отправить сообщение пользователю.
send :: (MonadBase STM m, HasServerState m) => Nickname -> Message -> m ()
send to (Message nickname msg) = do
  users <- askServerUsers
  for_ (Map.lookup to users) $ \ustate ->
    liftBase $ userQueue ustate $ Message nickname msg

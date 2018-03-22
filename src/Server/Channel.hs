-- | Работа с каналами.
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Server.Channel
  ( with_
  , with'_
  , addUser
  , partUser
  , get
  , member
  , Server.Channel.send
  ) where

import           Control.Concurrent.STM
import           Control.Monad.Base
import           Control.Monad.Except
import           Data.Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Protocol
import           Protocol.Errors
import           Protocol.Types (Nickname, Channel)
import           Server.Internal as Internal
import           Uni

-- | Работа с каналом. Если канала нету, то он создается.
--
-- Тут у нас есть следующие контексты:
--    MonadBase STM - ура мы можем делать транзакции, и не можем делать
--                    произвольно еIO
--    HasServerState - у нас есть доступ к состоянию сервера
--
-- У нас есть 2 кандидата для контекста где мы можем это выполнять
-- 
--    ReaderT ServerState
--    Handler e
--
-- Чем мы и пользуемся
with_ :: (MonadBase STM m, HasServerState m)
      => Channel -> (ChannelState -> m ChannelState) -> m ()
with_ channel = withInternal (pure $ ChannelState Set.empty "" channel) channel

-- | Работа с каналом. Если канала нету, то он исключение
--
-- Тут появляется ещё и новый контекст, что мы можем вернуть ошибку e,
-- и сделать инъекцию из NoSuchChannel в e.
with'_ :: (Inj e NoSuchChannel, MonadBase STM m, HasServerState m, MonadError e m)
      => Channel -> (ChannelState -> m ChannelState) -> m ()
with'_ channel = withInternal (throwError $ inj $ NoSuchChannel channel) channel

-- Функция для написания двух выше, вообще так любят писать в ФП
-- что много разных вещей можно выразить через общий набор примитивов
withInternal
  :: (MonadBase STM m, HasServerState m)
  => m ChannelState    -- что делать если канала нету
  -> Channel           -- имя канала
  -> (ChannelState -> m ChannelState)  -- операция над состоянием
  -> m ()
withInternal onNothing channel f
    = askServerChannels >>=
        maybe onNothing pure . Map.lookup channel >>=
        f >>= updateServerChannels channel

-- получить состояние канала, и ошбику если нету
get :: (Inj e NoSuchChannel, MonadBase STM m, HasServerState m, MonadError e m)
    => Channel
    -> m ChannelState
get chan = do
  channels <- askServerChannels
  case Map.lookup chan channels of
    Nothing -> throwError $ inj $ NoSuchChannel chan
    Just x  -> pure x

-- | Добавить пользователя на канал
addUser :: (Inj e UserOnChannel, MonadBase STM m, HasServerState m, MonadError e m)
        => Internal.ChannelState
        -> Nickname
        -> m ChannelState
addUser state nick = do
  let channel = channelName state
  when (Set.member nick (channelUsers state)) $                 -- Проверили, что пользователь не на канале
    throwError $ inj $ UserOnChannel nick channel               -- вернули ошибку
  let users' = Set.insert nick (channelUsers state)             -- добавили пользователя
      state' = state{channelUsers = users'}
  Server.Channel.send state' $ Message nick (Protocol.Join channel)      -- анонсировали вход на канал
  pure state'

-- | Пользователь выходит с канала.
partUser :: (Inj e NotOnChannel, MonadBase STM m, HasServerState m, MonadError e m)
         => ChannelState
         -> Nickname
         -> Maybe Text
         -> m ChannelState
partUser state nick reason = do
  let channel = channelName state
  unless (Set.member nick (channelUsers state))
    $ throwError $ inj $ NotOnChannel channel
  let state' = state {channelUsers = Set.delete nick (channelUsers state)}
  Server.Channel.send state' $ Message nick (Protocol.Part channel reason)
  pure state'

send :: (MonadBase STM m, HasServerState m)
     => ChannelState
     -> Message
     -> m ()
send state msg = do
  for_ (channelUsers state) $ \nick -> Internal.send nick msg

member :: Nickname -> ChannelState -> Bool
member n state = Set.member n (channelUsers state)

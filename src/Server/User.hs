-- Не особо интересный файл, который  предоставляет API для работы с
-- пользователем
{-# LANGUAGE FlexibleContexts #-}
module Server.User
  ( UserInfo(..)
  , get
  , Server.User.send
  ) where

import Control.Monad.Base
import Control.Concurrent.STM
import Control.Monad.Except
import Protocol.Types hiding (Message)
import Protocol.Errors
import qualified Data.Map as Map
import Server.Internal
import Uni

get :: (Inj e NoSuchNick, HasServerState m, MonadError e m, MonadBase STM m)
    => Nickname
    -> m UserInfo
get nickname = do
  users <- askServerUsers
  case Map.lookup nickname users of
    Nothing -> throwError $ inj $ NoSuchNick nickname
    Just x  -> pure x

send :: (MonadBase STM m) => UserInfo -> Message -> m ()
send (UserInfo run) msg = liftBase $ run msg


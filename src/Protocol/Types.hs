{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Protocol.Types
  ( Protocol(..)
  , Protocol'
    -- * Message
  , Message(..)
  , IsMessage(..)
  , IsParam(..)
  , mkCodeCommand
  , mkTextCommand
  , prefix
  , command
  , params
  , trailing
  , Command(..)
  , Code(..)
  , Nickname(..)
  , Channel(..)
  , Param(..)
  , Prefix(..)
  , MsgTarget(..)
  , MsgTo(..)
  ) where

import Conduit
import Control.Lens
import Data.Coerce
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Monoid
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics

type Protocol' m t o = Protocol m t t o

-- протокол который описывает как преобразуются сообщения
--  m - контекст в котором работаем
--  s - внутренний тип ответов (клиенту)
--  t - внутренний тип запросов (от клиента)
--  o - тип общения по проводу Text/ByteString
data Protocol m s t o = Protocol
  { encodeStream :: ConduitT s o m ()
  , decodeStream :: ConduitT o t m ()
  }

-- | Обобщенный тип сообщения с соотвествии с RFC.
data Message = Message
  { _prefix   :: !(Maybe Prefix)
  , _command  :: !Command
  , _params   :: ![Param]
  , _trailing :: !(Maybe Text)
  } deriving (Show, Eq, Generic)


mkCodeCommand :: Code -> Message
mkCodeCommand code = Message Nothing (IntCommand code) [] Nothing

mkTextCommand :: Text -> Message
mkTextCommand cmd = Message Nothing (TextCommand cmd) [] Nothing

-- | Код команды
data Code = Code !Int !Int !Int deriving (Eq, Show, Generic)

-- | Команда
data Command
  = TextCommand {-# UNPACK #-} !Text
  | IntCommand {-# UNPACK #-} !Code
  deriving (Show, Eq, Generic)

-- | Имя пользователя
newtype Nickname = Nickname Text deriving (Show, Eq, Ord, IsString)

-- | Имя канала.
newtype Channel = Channel Text deriving (Show, Eq, Ord, IsString)

-- | Источник сообщения
newtype Prefix = Prefix Nickname deriving (Show, Eq, IsString, Generic)


instance IsString Command where
  fromString = TextCommand . Text.pack

-- | Параметр команды
newtype Param   = Param Text deriving (Show, Eq, IsString, Generic)


-- | Список адресатов
newtype MsgTarget = MsgTarget (NonEmpty MsgTo) deriving (Show, Eq)

-- | Адресат сообщения
data MsgTo
  = MsgToChannel !Channel
  | MsgToUser    !Nickname
  deriving (Eq, Show)

-- | Интерфейс описывающий, что данный тип может быть
-- преобразован в сообщение
class IsMessage a where toMessage :: a -> Message

instance IsMessage Message where toMessage = id

instance (IsMessage a, IsMessage b) => IsMessage (Either a b) where
  toMessage (Left a) = toMessage a
  toMessage (Right b) = toMessage b


class IsParam a where
  toParam :: a -> Param

instance IsParam Param where
  toParam = id

instance IsParam Nickname where
  toParam = coerce

instance IsParam a => IsParam [a] where
  toParam = coerce . Text.intercalate "," . coerce . map toParam

instance IsParam MsgTarget where
  toParam (MsgTarget ne) = toParam $ NE.toList ne

instance IsParam MsgTo where
  toParam (MsgToChannel chan) = toParam chan
  toParam (MsgToUser    user) = toParam user

instance IsParam Channel where
  toParam (Channel chan) = Param $ "#" <> coerce chan




makeLenses ''Message


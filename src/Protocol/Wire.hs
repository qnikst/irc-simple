{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
-- | Обработка сырого протокола.
module Protocol.Wire
  ( -- * Типы
    Message(..)
  , MsgTarget(..)
  , MsgTo(..)
  , Command(..)
  , Nickname(..)
  , Channel(..)
  , Param(..)
  , Prefix(..)
  , Code(..)
  , IsMessage(..)
  , IsParam(..)
    -- * Конвертация в текстовое сообщение
  , protocol
  , toWire
  -- $bnf
  , message
  , msgtarget
  , nickname
  , channel
  , parseParam
  , trailing
  , prefix
  , command
  , middle
  ) where

import Control.Applicative
import Control.Monad.Catch
import Data.ByteString (ByteString)
import Data.Char (digitToInt, isLetter, isDigit)
import Data.Text (Text)
import qualified Data.List.NonEmpty as NE
import Data.Monoid
import Data.Text.Internal.Builder as Builder
import Data.Text.Internal.Builder.Functions (i2d)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import Data.Attoparsec.Text as Parser
import Data.Foldable
import Data.Coerce
import Protocol.Types hiding (prefix, command, trailing)
import Data.Conduit
import Data.Conduit.Attoparsec
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT

-- Создание из сообщения его представление в проводе, аналогично Builder
-- в привычных языках. А builder ~ Monoid
toWire :: Message -> Builder
toWire (Message prefix' cmd params' trail) = mconcat
  [ case prefix' of
      Nothing -> mempty
      Just (Prefix (Nickname nick)) ->
        Builder.singleton ':' <> Builder.fromText nick <> Builder.singleton ' '
  , case cmd of
      TextCommand t -> Builder.fromText t
      IntCommand  (Code a b c) ->
        Builder.singleton (i2d a)
        <> Builder.singleton (i2d b)
        <> Builder.singleton (i2d c)
  , case params' of
      [] -> mempty
      _  -> singleton ' ' <> (Builder.fromText $ Text.intercalate " " $ coerce params')
  , case trail of
      Nothing -> mempty
      Just s  -> Builder.singleton ' '
              <> Builder.singleton ':'
              <> Builder.fromText s
  , singleton '\r'
  , singleton '\n'
  ]

-- | разбор параметров
parseParam :: Parser a -> Param -> Maybe a
parseParam p (Param t) = case parseOnly p t of
  Left _err -> Nothing
  Right x  -> pure x

-- / Протокол
protocol :: MonadThrow m => Protocol' m Message ByteString
protocol = Protocol
  { decodeStream = CT.decode CT.utf8             --- декодируем текст из байтов
                .| conduitParser message         -- запускаем парсер
                .| CL.map snd                    -- убираем информацию о позициях в потоке
  , encodeStream = CL.map (Text.Lazy.toChunks . Builder.toLazyText . toWire) -- получаем текст из билдера
                .| CC.concat                     -- отправляем все чанками хорошего размера
                .| CT.encode CT.utf8             -- кодируем текст в байты
  }

-- а дальше используем парсер он маппится в функции 1 в 1, буков там много
-- но если каждый раз возвращаться к BNF то все должно быть ясно.
--
-- $bnf
--
-- BNF протокола:
--
-- Упрощения:
--   1. часть протокола упрощена (отмечено (!))
--   2. не делается проверка на наличие NULL в сообщениях
--   3. не введено ограничение на размеры параметров и строк, которые
--      присутсвуют в протоколе
-- @
-- message  ::= [ ":" prefix SPACE ] command [params] clrf
-- prefix   ::= nickname                                                  -- (!)
-- command  ::= 1*letter | 3 digit
-- params   ::= *(SPACE middle) [SPACE ":" trailing]
-- middle   ::= nospcrlf * (":" nospcrlfcl)
-- trailing ::= *(":" | " " | nospcrlcfl)
-- space    ::= ' '
-- crlf     ::= '\r\n'
--
-- nickname   ::= (1*letter | special) *(letter | digit | special | "-")
-- msgtarget  ::= msgto *("," msgto)
-- msgto      ::= channel | user                                            -- (!)
-- channel    ::= ( "#" ) chanstring                                        -- (!)
-- chanstring ::=  *  ; Any except NUL, BELL, CR, LF, " ", ",", ":"
-- special    ::=  *  ; "[", "]", "\", "`", "_", "^", "{", "|", "}"
-- @

-- | Парсер сообщения
message :: Parser Message
message = convert
       <$> optional (char ':' *> prefix <* char ' ')
       <*> command
       <*> (optional ((,) <$> (many1 (char ' ' *> (Param <$> middle)))
                          <*> optional (space *> char ':' *> trailing)))
       <*  char '\r'
       <*  char '\n'
  where
    convert mprefix cmd Nothing = Message mprefix cmd [] Nothing
    convert mprefix cmd (Just (x,mt)) = Message mprefix cmd x mt

-- | Парсер префикса
prefix :: Parser Prefix
prefix = Prefix <$> nickname

-- | Парсер ника
nickname :: Parser Nickname
nickname = fmap Nickname $
  Text.cons <$> (special <|> letter)
            <*> Parser.takeWhile (getAny . foldMap (fmap Any) [isLetter, isDigit, isSpecial])

-- | Парсер канала
channel :: Parser Channel
channel = Channel <$> (char '#' *> Parser.takeWhile chanstring)

-- | Команда
command :: Parser Command
command = asum
  [ fmap TextCommand $ Text.cons <$> letter <*> Parser.takeWhile isLetter
  , fmap IntCommand  $ Code <$> (digitToInt <$> digit)
                            <*> (digitToInt <$> digit)
                            <*> (digitToInt <$> digit)
  ]

-- | Собираем все выражения для, в которых нету контрольных символов.
middle :: Parser Text
middle = takeWhile1 nospcrlf

special :: Parser Char
special = satisfy isSpecial

-- | Собираем все сообщение до конца.
trailing :: Parser Text
trailing = Parser.takeWhile (/= '\r')

msgtarget :: Parser MsgTarget
msgtarget = fmap MsgTarget $ (NE.:|) <$> msgto <*> (sepBy msgto ",")

msgto :: Parser MsgTo
msgto = asum [ MsgToChannel <$> channel
             , MsgToUser    <$> nickname
             ]

-- | Допустим ли символ при кодировании параметра
nospcrlf :: Char -> Bool
nospcrlf '\r' = False
nospcrlf '\n' = False
nospcrlf ' '  = False
nospcrlf ':'  = False
nospcrlf '\0' = False
nospcrlf _    = True

-- | Допустим ли символ при кодировании канала
chanstring :: Char -> Bool
chanstring '\0'   = False
chanstring '\BEL' = False
chanstring '\r'   = False
chanstring '\n'   = False
chanstring ' '    = False
chanstring ','    = False
chanstring ':'    = False
chanstring _      = True

isSpecial :: Char -> Bool
isSpecial '['  = True
isSpecial ']'  = True
isSpecial '\\' = True
isSpecial '_'  = True
isSpecial '`'  = True
isSpecial '^'  = True
isSpecial '{'  = True
isSpecial '}'  = True
isSpecial '|'  = True
isSpecial _    = False

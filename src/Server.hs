-- Это уже интересная часть сервера, в этом файле мы описываем
-- сам сервер и как он работает с внешним миром, эта часть примерно
-- постоянная не зависимо от того, что же за реализацию мы засунем
-- в сервер. Поехали!
--
-- Сначала подключим расширения (их компилятор скажет когда подключить)
-- вообще надо помнить, что Haskell это implementation defined язык и
-- жизни вне GHC нету. Так что расширения это не какие-то плагины (которые есть)
-- это вполне нормально
{-# LANGUAGE FlexibleContexts #-}  -- игнорируйте
{-# LANGUAGE TypeApplications #-}  -- применение типов, например, у вас есть
                                   -- функция @foo :: forall a . b -> a -> b@
                                   -- и компилятор не может вывести тип @b@
                                   -- из контекста, а мы можем сказать, 'foo @ A'
                                   -- и компилятор уже знает что это foo :: b -> A b
                                   -- тут это чисто для интереса, можно и обойтись
{-# LANGUAGE DataKinds #-}         -- тоже для интереса позволяет использовать конструкторы типов
                                   -- гетерогенного множества см Uni
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}    -- позволяет писать @foo :: forall a . a@ (вот это forall)
{-# LANGUAGE LambdaCase #-}        -- позволяет писать @foo >>= \case@ вместо @foo >>= \x -> case x of@
module Server
  ( server
    -- * Internal
  , asyncClient
  , clientConduit
  ) where

import           Conduit                        -- фреймворк позволяющий итеративную обработку данных
import           Control.Concurrent.Async       -- библиотека для безопасной работы с потоками
                                                -- весьма неплохая и рекомендуется новичкам
import           Control.Concurrent.STM         -- software transactional memory
import           Control.Monad (unless, join)
import           Control.Monad.Trans.Reader     -- возможность таскать свой контекст
import           Control.Monad.Trans.Resource (ResourceT) -- контекст в котором можно безопасно
                                                          -- создавать ресурсы, которые гарантировано
                                                          -- освободятся несмотря на исключения и прочее
                                                          -- нету проблем с вложенностью, которое есть
                                                          -- у @try { } finally { }@
import           Data.ByteString (ByteString)
import           Data.Conduit.Combinators (repeatM)
import           Data.Conduit.Network                     -- работа с сетью
-- INFO: for_
-- достаточно интересная штука позволяет итерироваться по структуре
-- игнориря результат, и нам важны только эффекты. Аналог
--
-- @
-- for (x : structure.iterator() {
-- @
--
-- Может использоваться для списков, множеств, но например для @Maybe@ или @Either@,
-- которые делают действие только если значение есть.
import           Data.Foldable (for_)
import           Data.List.NonEmpty (NonEmpty(..))      -- Непустой список
import qualified Data.Set as Set
import qualified Protocol as Protocol
import           Protocol.Errors
import           Protocol.Replies
import           Protocol.Types
import qualified Protocol.Wire as Wire
import           Server.Handler as Handler
import           Server.Internal (ServerState, ChannelState(..), withServerState)
import qualified Server.Channel as Channel
import qualified Server.User    as User
import           Uni

-- | Функция нашего сервера. Которую будет выполнять executabe
server :: ServerSettings -> IO ()
server settings = withServerState $ \s ->               -- мы работаем с состоянием сервера
                                                        -- к которому у нас нету прямого доступа
                                                        -- но которое передается в обработчик
  runTCPServer settings $                               -- запускаем TCP сервер
    asyncClient Protocol.protocol (clientConduit s)     -- для пользователей мы используем
                                                        -- асинхронный вариант, определенный
                                                        -- протокол. и 'clientConduit' вместо
                                                        -- логики обработчиков.

-- | Вспомогательная функция, которая отправляет ответы, пользователю.
-- Данная функция гарантирует, что все выделенные ресурсы будут освобождены,
-- по завершеню функции, будь то нормальный выход или исключение.
--
-- INFO RTS:
--  Про рантайм систему в Haskell (да и прочих ЯП с зелеными потоками) можно думать, как об
--  высокоуровневым интерфейсом над epoll, каждый поток создает свой контекст, который использует
--  неблокирующее IO и добавляет callback в epoll, поэтому когда хэндл становится доступен,
--  то поток будет пробужден и работать. В итоге если мы хотим асинхронно читать и писать для
--  каждого клиента, то мы просто делаем 2 потока, один пишет и обрабатывает сообщения, а другой
--  пишет ответы (возможно асинхронно) и все радостны. что мы тут и делаем
asyncClient
  :: forall s t .
     Protocol (ResourceT IO) s t ByteString               -- описание протокола, подробно в Protocol.Types
     -- тут важно, что @s@ тип который отдает протокол, @t@ входящий тип, @ByteString@ - тип в проводе.
  -> ((s -> STM ()) -> ConduitT t Void (ResourceT IO) ()) -- пользовательский протокол обработки сообщений
     -- INFO: тут можно поговорить подольше об абстракции и всем таком, но мне лень. Суть в том,
     -- что логике внутри совершенно нету разници до того, как огранизовано общение. Ей только
     -- достаточно знать как послать ответ пользователю, поэтому мы передаем callback @(s -> STM ()@
     -- т.е. функция, которая получает ответ, и вызывает STM транзакцию. При реализации логики
     -- мы в праве выбирать любую, например подставлять тестовую.
  -> AppData                                              -- структура хранящая информацию о приложении
  -> IO ()
asyncClient protocol f ad = do
  q <- newTQueueIO                                 -- создали очередь, тут можно делать хитро
                                                   -- и можно использовать разные очереди, например
                                                   -- буфферизованную, чтобы ограничить использование
                                                   -- памяти и т.п.
  race_ (replier q)                                -- Запускаем 2 легких потока для чтения и записи.
        (receiver q)                                -- Если один завершается, то другой тоже.
  where
    -- Вспомогательная функция - бесконечно читаем из очереди
    waitForMsg q = repeatM $ liftIO $ atomically $ readTQueue q
    replier :: TQueue s -> IO ()
    replier q = runResourceT $ runConduit
              $ waitForMsg q                       -- Получаем сообщения
             .| encodeStream protocol              -- Кодируем сообщение в байты
             .| appSink ad                         -- Отдаем клиенту
    receiver :: TQueue s -> IO ()
    receiver q = runResourceT                      -- Контролируем ресурсы
               $ runConduit $ appSource ad         -- Читаем данные из сокета
              .| decodeStream protocol             -- Декодируем их в текст (utf8)
              .| f (writeTQueue q)                 -- Обрабатываем
-- INFO: conduit
-- при построении control flow такими блоками можно делать аналог unix pipes,
-- когда мы пишем простые блоки и комбинируем из них результат. При этом они
-- хорошо оптимизируются и fuse-ятся (т.е. происходит операция инлайна, специализации
-- и оптимизации) таким образом по скорости код не отличается от того, если
-- написать все в куче и проделать оптимизации самим (например на си).
-- А так вроде понятнее


-- В общем на этом сам сервер закончился, все :).
-- Теперь уже сама реализация.

-- Сам поток клиента
clientConduit
  :: ServerState                                            -- Состояние сервера
  -> (Wire.Message -> STM ())                               -- callback переданный от tcp сервера
  -> ConduitT Protocol.Message a (ResourceT IO) ()          -- ой!
clientConduit ss sendBack = do
  hdl <- greeting                                          -- привествие, мы должны получить
                                                           -- информацию о том, как запускать
                                                           -- транзацкии

  awaitForever $ \case                                     -- мы бесконечно ждем сообщения
                                                           -- и обрабатываем их
    Protocol.Join channel ->                               -- команда JOIN
      -- Вот тут интересно, мы явно описываем все возможные исключения, точнее на
      -- самом деле компилятор сам нам их рассказывает, так что программист видит,
      -- что может индти не так. Эта магия, которую наверное делать не надо, но 
      -- раз можно то почему бы и да.
      --
      -- run запускает транзацию. см. Handler
      runHandler hdl @ '[UserOnChannel] $
        Channel.with_ channel $ \state -> do               -- работаем с каналом, создать если нету
          nick   <- askNickname                            -- получили текущий ник
          state' <- Channel.addUser state nick             -- добавить юзера на канал
          reply $ ReplyTopic channel (channelTopic state') -- анонисировали пользоватею топик
          reply $ NamReply  channel  (channelUsers state') -- анонсировали список юзеров
          pure state'
    Protocol.Part channel reason ->                        -- команда PART
      runHandler hdl @'[NotOnChannel, NoSuchChannel] $ do
        nick   <- askNickname
        Channel.with'_ channel $ \state ->                 -- работаем с каналом, исключение если нету
          Channel.partUser state nick reason               -- убрать пользователя.
        updateUserChannels $ Set.delete channel            -- убрать информацию о канале
    Protocol.PrivMsg (Wire.MsgTarget target) msg  ->       -- отправить сообщение
      for_ target $ \to ->                                 -- для каждого получателя из списка
                                                           -- отдельная транзакция
        runHandler hdl @ '[NoSuchNick, NoSuchChannel, NotOnChannel] $
          case to of
            MsgToChannel channel -> do                       -- сообщение на канал
              nick <- askNickname
              state <- Channel.get channel                   -- получить канал
              unless (nick `Channel.member` state)           -- проверить, что пользователь там есть
                $ throwHandler $ inj $ NotOnChannel channel
              -- отправить сообщение, тут бы не помеша комбинатор какой. PR welcome, все дела.
              let cmd' = Protocol.PrivMsg (Wire.MsgTarget $ MsgToChannel channel :| []) msg
              -- отправить сообщение, в канал.
              Channel.send state $ Handler.Message nick cmd'
            MsgToUser to' -> do
              nick <- askNickname
              state <- User.get to'
              let cmd' = Protocol.PrivMsg (Wire.MsgTarget $ MsgToUser to' :| []) msg
              User.send state $ Handler.Message nick cmd'
    Protocol.Nick _newnick -> pure ()                      -- лень, сами добавляейте.
  where
    -- не стоит тут пытаться писать тип, если что я предупреждаел
    greeting = await >>= \case
      Nothing -> error "user exit before handshake"         -- Ахахаха!
      Just (Protocol.Nick nick) -> do                       -- пользователь регистрируется
        -- Ух,
        -- @join :: m (m a) -> m a@ - "сворачивает контекст". В данном примере используется
        -- для того, чтобы выполнить действия после STM транзакции, поскольку в транзакции
        -- мы не можем делать эффекты, которые нельзя повторять или прервать в середине или
        -- которые создают эффекты в реальном мире.
        -- поэтому мы далаем @join $ atomically $ trasaction >> return action@ что происходит
        -- мы выполняем транзацию и возвращаем действие как результат, получается:
        -- @join (action:: IO (IO a))@ которое выполняет это действие @action@.
        -- Можно просто запомнить этот трюк.
        join $ liftIO $ atomically $ do
          eresult <- register ss nick sendBack                   -- регистрируем пользователя
          case eresult of
            Left e -> sendBack (errorToWire e) >> pure greeting  -- не удалось, отправили ошибку и заново
            Right reg -> pure (lift reg)                         -- удалось, регистрируем пользователя
                                                                 -- теперь если этот тред умрёт, то
                                                                 -- пользователь не забудет выйти с
                                                                 -- каналов.
      _ -> greeting                                              -- можно и тут ошибку послать, но мне лень.

-- Вроде все немногословно и достаточно просто, и можно ещё почище сделать
-- главное, что это уже можно тестировать и изменять.

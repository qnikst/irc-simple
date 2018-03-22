{-# LANGUAGE OverloadedStrings #-}
module Client
  ( client
  ) where


import Control.Monad.IO.Class
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import Conduit
import Data.Conduit.Combinators
import qualified Data.Conduit.Text as CT
import Data.Conduit.Network

client :: ClientSettings -> IO ()
client settings = do
   q <- newTQueueIO
   _ <- async $ runTCPClient settings $ \ad ->
          race_ (runConduit
                   $ repeatM (atomically $ readTQueue q)
                  .| CT.encode CT.utf8
                  .| appSink ad)
                (runConduit
                   $ appSource ad
                  .| CT.decode CT.utf8
                  .| Data.Conduit.Combinators.print)
   loop q
   where
      loop :: TQueue Text -> IO ()
      loop q = do
        input <- getLine
        case input of
            "" -> return ()
            "quit" -> return ()
            s -> do
              let t = Text.pack s <> "\r\n"
              liftIO $ Prelude.print t
              liftIO $ atomically $ writeTQueue q t
              loop q

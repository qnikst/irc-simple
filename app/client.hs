{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Client
import Data.ByteString.Char8 (ByteString)
import Data.Conduit.Network
import Data.Monoid ((<>))
import Options.Applicative

-- | Server configuration
data Config = Config
  { cfgPort :: !Int
  , cfgHost :: !ByteString
  }

-- | Command line parser
irc :: Parser Config
irc = Config <$> option auto
                   ( long "port"
                   <> metavar "PORT"
                   <> short 'p'
                   <> value 9999
                   <> help "Application port")
             <*> option auto
                   ( long "host"
                   <> short 'h'
                   <> metavar "HOST"
                   <> value "localhost"
                   <> help "Application host")

main :: IO ()
main = execParser opts >>= run where
  run :: Config -> IO ()
  run (Config port host) = client $ clientSettings port host
  opts = info (irc <**> helper)
              (fullDesc
               <> progDesc "simple irc server"
               <> header "irc - is a nice thing")

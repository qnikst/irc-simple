{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Protocol.Errors
  ( NickNameInUse(..)
  , NoSuchNick(..)
  , NoSuchChannel(..)
  , NotOnChannel(..)
  , UserOnChannel(..)
  , IsCodeError(..)
  , errorToWire
  ) where

import Control.Lens
import Protocol.Types
import Uni

class IsCodeError a where
  code :: a -> Code
  populate :: a -> Message -> Message

errorToWire :: IsCodeError a => a -> Message
errorToWire a = populate a $ mkCodeCommand (code a)

instance {-# OVERLAPS #-} (IsCodeError e) => IsCodeError (Uni '[e]) where
  code (This e) = code e
  code (That v) = case v of {}
  populate (This e) = populate e
  populate (That v) = case v of {}

instance {-# OVERLAPPABLE #-} (IsCodeError e, IsCodeError (Uni es)) => IsCodeError (Uni (e ': es)) where
  code (This e) = code e
  code (That es) = code es
  populate (This e) = populate e
  populate (That es) = populate es

instance (IsCodeError a, IsCodeError b) => IsCodeError (Either a b) where
  code (Left a) = code a
  code (Right b) = code b
  populate (Left a) = populate a
  populate (Right b) = populate b

-- | @401 ERR_NOSUCHNICK "<server name> :No such server"@
data NoSuchNick = NoSuchNick Nickname
instance IsCodeError NoSuchNick where
  code _ = Code 4 0 1
  populate (NoSuchNick nick) msg
    = msg & params .~ [toParam nick]
          & trailing .~ Just "No such nick/channel"

-- 402 ERR_NOSUCHSERVER "<server name> :No such server"

-- | @403 ERR_NOSUCHCHANNEL"<channel name> :No such channel"@
data NoSuchChannel = NoSuchChannel Channel
instance IsCodeError NoSuchChannel where
  code _ = Code 4 0 3
  populate (NoSuchChannel chan) msg
    = msg & params .~ [toParam chan]
          & trailing .~ Just "No such channel"

-- | @404 ERR_CANNOTSENDTOCHAN"<channel name> :Cannot send to channel"@
data CanNotSendToChan = CanNotSendToChan Channel
instance IsCodeError CanNotSendToChan where
  code _ = Code 4 0 4
  populate (CanNotSendToChan chan) msg
    = msg & params .~ [toParam chan]
          & trailing .~ Just "Cannot send to channel"

-- 405 ERR_TOOMANYCHANNELS"<channel name> :You have joined too many channels"
-- 406 ERR_WASNOSUCHNICK"<nickname> :There was no such nickname"
-- 407 ERR_TOOMANYTARGETS"<target> :<error code> recipients. <abort message>"
-- 408 ERR_NOSUCHSERVICE"<service name> :No such service"
-- 409 ERR_NOORIGIN":No origin specified"
-- 411 ERR_NORECIPIENT":No recipient given (<command>)"
-- 412 ERR_NOTEXTTOSEND":No text to send"
-- 413 ERR_NOTOPLEVEL"<mask> :No toplevel domain specified"
-- 414 ERR_WILDTOPLEVEL"<mask> :Wildcard in toplevel domain"
-- 415 ERR_BADMASK"<mask> :Bad Server/host mask"
-- 421 ERR_UNKNOWNCOMMAND"<command> :Unknown command"
-- 422 ERR_NOMOTD":MOTD File is missing"
-- 423 ERR_NOADMININFO"<server> :No administrative info available"
-- 424 ERR_FILEERROR":File error doing <file op> on <file>"
-- 431 ERR_NONICKNAMEGIVEN":No nickname given"
-- 432 ERR_ERRONEUSNICKNAME"<nick> :Erroneous nickname"

-- | @433 ERR_NICKNAMEINUSE"<nick> :Nickname is already in use"@
data NickNameInUse = NickNameInUse Nickname
instance IsCodeError NickNameInUse where
  code _ = Code 4 3 3
  populate (NickNameInUse nick) msg
    = msg & params .~ [toParam nick]
          & trailing .~ Just "Nickname already is use"
-- 436 ERR_NICKCOLLISION"<nick> :Nickname collision KILL from <user>@<host>"
-- 437 ERR_UNAVAILRESOURCE"<nick/channel> :Nick/channel is temporarily unavailable"
-- 441 ERR_USERNOTINCHANNEL"<nick> <channel> :They aren't on that channel"

-- | @442 ERR_NOTONCHANNEL"<channel> :You're not on that channel"@
data NotOnChannel = NotOnChannel !Channel
instance IsCodeError NotOnChannel where
  code _ = Code 4 4 2
  populate (NotOnChannel chan) msg
    = msg & params .~ [toParam chan]
          & trailing .~ Just "YYou're not on that channel"

-- | @443 ERR_USERONCHANNEL"<user> <channel> :is already on channel"@
data UserOnChannel = UserOnChannel !Nickname !Channel
instance IsCodeError UserOnChannel where
  code _ = Code 4 4 3
  populate (UserOnChannel nick chan) msg
    = msg & params .~ [toParam nick, toParam chan]
          & trailing .~ Just "is already on channel"

-- 444 ERR_NOLOGIN"<user> :User not logged in"
-- 445 ERR_SUMMONDISABLED":SUMMON has been disabled"
-- 446 ERR_USERSDISABLED":USERS has been disabled"
-- 451 ERR_NOTREGISTERED":You have not registered"
-- 461 ERR_NEEDMOREPARAMS"<command> :Not enough parameters"
-- 462 ERR_ALREADYREGISTRED":Unauthorized command (already registered)"
-- 463 ERR_NOPERMFORHOST":Your host isn't among the privileged"
-- 464 ERR_PASSWDMISMATCH":Password incorrect"
-- 465 ERR_YOUREBANNEDCREEP":You are banned from this server"
-- 466 ERR_YOUWILLBEBANNED
-- 467 ERR_KEYSET"<channel> :Channel key already set"
-- 471 ERR_CHANNELISFULL"<channel> :Cannot join channel (+l)"
-- 472 ERR_UNKNOWNMODE"<char> :is unknown mode char to me for <channel>"
-- 473 ERR_INVITEONLYCHAN"<channel> :Cannot join channel (+i)"
-- 474 ERR_BANNEDFROMCHAN"<channel> :Cannot join channel (+b)"
-- 475 ERR_BADCHANNELKEY"<channel> :Cannot join channel (+k)"
-- 476 ERR_BADCHANMASK"<channel> :Bad Channel Mask"
-- 477 ERR_NOCHANMODES"<channel> :Channel doesn't support modes"
-- 478 ERR_BANLISTFULL"<channel> <char> :Channel list is full"
-- 481 ERR_NOPRIVILEGES":Permission Denied- You're not an IRC operator"
-- 482 ERR_CHANOPRIVSNEEDED"<channel> :You're not channel operator"
-- 483 ERR_CANTKILLSERVER":You can't kill a server!"
-- 484 ERR_RESTRICTED":Your connection is restricted!"
-- 485 ERR_UNIQOPPRIVSNEEDED":You're not the original channel operator"
-- 491 ERR_NOOPERHOST":No O-lines for your host"
-- 501 ERR_UMODEUNKNOWNFLAG":Unknown MODE flag"
-- 502 ERR_USERSDONTMATCH":Cannot change mode for other users"

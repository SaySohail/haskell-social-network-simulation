{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-unused-matches -Wno-name-shadowing -Wno-unused-imports -Wno-compat-unqualified-imports -Wno-dodgy-imports -Wno-orphans -Wno-unused-local-binds  #-}
{-|
Module      : Types
Description : Data Types and Instance variables for User and Messages
Maintainer  : Sayed Sohail Peerzade - 220541549
-}

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-unused-matches -Wno-name-shadowing -Wno-unused-imports -Wno-compat-unqualified-imports -Wno-dodgy-imports -Wno-orphans -Wno-unused-local-binds  #-}


module Types (
    User (..),
    Message (..)
) where

import qualified GHC.Generics as Generics
import qualified Data.Text as DataText 
import qualified Control.Parallel.Strategies as ParallelStrategies
import Control.Concurrent.MVar
import Data.Time.Clock
import Data.Time.Format



{-|
   Message Data type, to store the messages sent by the users
-}
data Message = Message {
    from :: Int, -- ^ Stores the user_id of the sender
    to :: Int, -- ^ Stores the user_id of the recipient
    content :: DataText.Text, -- ^ Stores the message body
    time   :: String, -- ^ Time when the message was recieved
    date   :: String -- ^ Date When the message was recieved
} deriving (Generics.Generic)

{-|
   Instances of parMap function Message DataType to map a set of fuctions parallelly
-} 
instance ParallelStrategies.NFData Message


{-|
   Instances of show to convert values of type Message into a string representation, so that it can be displayed to the user.
-} 
instance Show Message where
    show Message{to=to, from=from, content=message, date=d, time=t} = "-------------------\n" ++ d ++ " " ++ t ++ " " ++ "User" ++ show from ++ " " ++ "--->" ++ " " ++"User"++ show to ++ ": " ++ show message ++ "\n\n"

{-|
    User Data Type, to store information about the user
-}
data User = User {
    user_id :: Int, -- ^ Unique Identifier to identify the user
    messages_received :: Int, -- ^ Stores the Number of Messages received by the user
    received_messages :: [Message], -- ^ Stores the Messages received by the user
    no_of_messages_sent :: Int, -- ^ Stores the Number of Messages sent by the user
    sent_messages :: [Message] -- ^ Stores the messages sent by the user
} deriving (Generics.Generic)

instance Show User where
    show (User id messages_recieved messages_received no_of_messages_sent sent_messages) = 
        "User " ++ show id ++ " has received " ++ show messages_recieved ++ 
        " messages and has the following messages: " ++ show messages_received

{-|
   Instances of parMap function for User DataType to map a set of fuctions parallelly
-} 
instance ParallelStrategies.NFData User

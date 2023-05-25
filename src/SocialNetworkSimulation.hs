{-|
Module      : Social Network Simulation
Description : To simulate a social network by creating 10 Users and sending 100 messages randomly on 10 threads
Maintainer  : Sayed Sohail Peerzade - 220541549
-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-unused-matches -Wno-name-shadowing -Wno-unused-imports -Wno-compat-unqualified-imports -Wno-dodgy-imports -Wno-orphans -Wno-unused-local-binds  #-}

module SocialNetworkSimulation (simulate,
                                displayInboxes,
                                displayUserMessages
                                 ) where
    
import Control.Concurrent
import qualified Control.Parallel.Strategies as ParallelStrategies 
import qualified Control.DeepSeq

import qualified Data.Sequence as DataSequence
import qualified Data.Foldable as DF

import MessageFunctions -- | > Importing functions from MessagesFunctions module which performs messaging and user creation operations 
import Types -- | > Importing User and Messages data types 
import Data.Data
import Control.Monad.Cont
import Text.Read

{-|
function performs the social network simulation by creating 10 Users and sending 100 messages randomly on 10 threads
-}
simulate :: IO ()
simulate = do
    let total_users = 10 -- ^ setting the number of users

    let total_messages =  100 -- ^ setting number of messages

    atomicPutStrLn $ "\n\n"++"Starting the simulation: " ++ "Simulating " ++ show total_users ++ " users sending " ++ show total_messages ++ " messages amongst eachother." ++ "\n\n"

    -- ^ Initialising user sequence by invoking createUsers function from MessageFunctions Module
    let user_sequence = DataSequence.fromList $ ParallelStrategies.parMap ParallelStrategies.rdeepseq createUser [1..total_users]
    -- ^ Initialising MVars for synchronizing access to shared state between multiple threads
    total <- newMVar 0
    wait <- newEmptyMVar
    users <- newMVar user_sequence

    -- ^ Invoiking mapFork Fuction of MessageFnctions module which generates threads for each user, and sends random messages to other users and also displays messages on consle as they are being sent.
    mapFork users wait total [1..total_users] total_messages total_users

    -- ^ The wait MVar will not be cleared until all messages have been sent, thus ensuring that threads are not terminated prematurely.
    w <- takeMVar wait
    
    -- Displaying the total Messages received by each user
    u <- takeMVar users
    atomicPutStrLn "Total Messages Received by users are: "
    let output = map outputTotalMessagesPar (DF.toList u) 
    mapM_ (atomicPutStrLn . show) output
    displayUserMessages u
   
{-|
   Function to display a User's Messages
-}
displayUserMessages :: DataSequence.Seq User -- ^ Sequence of Users
                     -> IO () -- ^ Return type of IO ()
displayUserMessages users = do
    putStrLn "\n---------------------------------\n"
    putStrLn "(1) Show Messages for a User"
    putStrLn "(2) Exit"
    putStrLn "Choose an option > "
    maybeInt <- fmap readMaybe getLine :: IO (Maybe Int) -- Exception handling for Int type inputs
    case maybeInt of
        Nothing -> do
            putStrLn "Only Int input allowed"
            threadDelay 1000000
            displayUserMessages users
        Just option -> do
            case option of
                1 -> do
                    putStrLn "\nEnter User ID (1-10) > "
                    userID <- fmap readMaybe getLine :: IO (Maybe Int) -- Exception handling for Int type inputs
                    case userID of
                        Nothing -> do
                            putStrLn "Only Int input allowed"
                            threadDelay 1000000
                            displayUserMessages users
                        Just userId -> do
                            displayInboxes users userId
                            displayUserMessages users
                2 -> do
                    putStrLn "\nEnd of program\n"
                _ -> do
                    putStrLn "Please enter a valid number" -- Exception handling to make sure we enter a valid option number
                    threadDelay 100000
                    displayUserMessages users

                

{-|
   Function to display a User's Inbox Or Outbox
-}
displayInboxes :: DataSequence.Seq User -- ^ Sequence of Users
                -> Int -- ^ User id 
                -> IO () -- ^ Return type of IO ()
displayInboxes users userid = do 
    putStrLn "\n---------------------------------\n"
    let option1 = "(1) Show Inbox For User " ++ show userid
    let option2 = "(2) Show Outbox For User " ++ show userid
    putStrLn option1
    putStrLn option2 
    putStrLn "(3) Back"
    putStrLn "Choose an option > "
    maybeInt <- fmap readMaybe getLine :: IO (Maybe Int) -- Exception handling for Int type inputs
    case maybeInt of
         Nothing -> do
            putStrLn "Only Int input allowed"
            threadDelay 1000000
            displayInboxes users userid
         Just option -> do
            case option of
                1 -> do

                    displayUserInbox userid users
                    threadDelay 3000000
                2 -> do
                    displayUserOutbox userid users
                    threadDelay 3000000
                3 -> do
                    return()
                _ -> do
                    putStrLn "Please enter a valid number" -- Exception handling to make sure we enter a valid option number
                    threadDelay 100000
                    displayInboxes users userid



        


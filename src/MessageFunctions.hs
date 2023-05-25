{-|
Module      : MessageFunctions
Description : This Module inculdes helper functions for creating Users, Sending Messages and displaying the sent messages 
Maintainer  : Sayed Sohail Peerzade - 220541549
-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-unused-matches -Wno-name-shadowing -Wno-unused-imports -Wno-compat-unqualified-imports -Wno-dodgy-imports -Wno-orphans -Wno-unused-local-binds  #-}

module MessageFunctions
    ( 
      isNumeric,
      createUser,
      outputTotalMessagesPar,
      randomUser,
      mapFork,
      process,
      atomicPutStrLn,
      generateRandomMessage,
      stdoutLock,
      getCurrentDateTime,
      generateString,
      findUserById,
      displayUserInbox,
      getId,
      displayUserOutbox,
      displayInboxMessages,
      displayOutboxMessages
    ) where
import Control.Concurrent 
import qualified Text.StringRandom as StringRandom
import qualified Data.Text as DataText
import qualified Data.Sequence as DataSequence
import qualified System.Random as SystemRandom
import qualified Data.Char as DataChar
import Types
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Clock
import Data.Time.Format
import Control.Exception (SomeException)
import Control.Exception.Base (catch)
-- import qualified GHC.TypeLits as reached

{-| 
Checks if a string contains only numbers and outputs a Boolean value based on the results.
-}
isNumeric :: [Char] -- ^ String to test
            -> Bool -- ^ Result
isNumeric [] = True
isNumeric (x:xs) = DataChar.isDigit x && isNumeric xs




{-| 
Creates a user by taking id as input, intialises total messages to 0  and sets the user_messages to empty array
-}
createUser :: Int -- ^ id of user to be created
            -> User -- ^ Returns created user
createUser id = User {user_id = id, messages_received = 0, received_messages = [], no_of_messages_sent = 0, sent_messages = []}

{-| 
Gets the number of messages received by the user. Used with parMap rdeepseq to generate a list of message counts which is then displayed with mapM_
-}
outputTotalMessagesPar :: User -- ^ Input user
                    -> String -- ^ Output
outputTotalMessagesPar user = "User " ++ show (user_id user) ++ " has received " ++ show (messages_received  user) ++ " messages"


{-| 
This function randomly chooses an index associated with a user different from the one provided in the exclude_index argument, allowing a user to send a message to another user.
-}
randomUser :: Int -- ^ RandomIndex will not choose an index that is the same as the number it was given.
            -> Int -- ^ Number of Users  
            -> IO Int -- ^ Returns id of randomly selected user
randomUser exclude_index list_length = do
    x <- SystemRandom.randomRIO (1, list_length)

    -- To prevent a user from sending a message to themselves
    if x == exclude_index then
        do
            if list_length == 0 then
                error "List Length must be greater than 0"
            else
                randomUser exclude_index list_length
    else do
        return x


{-| 
This function allows for convenient generation of a large amount of threads
-}
mapFork :: MVar (DataSequence.Seq User) -- ^ A sequence of users
         -> MVar Int -- ^ Wait time MVar, to stop execution 
         -> MVar Int -- ^ Empty mVar to store number of messages sent
         -> [Int] -- ^ No of user threads to be created
         -> Int -- ^ No of Messages to be sent
         -> Int  -- ^ Max Number of Users
         -> IO ()
mapFork users wait total user_threads max_messages max_user = 
  catch (do
    mapM_ (\n -> forkIO (process n users wait total max_messages max_user)) user_threads
  )
  (\e -> do
    let err = show (e :: SomeException)
    putStrLn $ "Error occurred while creating threads: " ++ err
    return ()
  )




{-|
For each simulated user the following function will check if the macimum number of messages has been reached. 
If maximum number of messages has been reached then the user will exit the thread and initialize MVar to indicate the exit. If not then the following tasks will be performed   
-}

-- |
-- * Choose a user randomly from the users list by using Data.Random.randomIO, which will have a different user_id from the thread_user_id.
-- * Generate a random message to be sent by using Text.StringRandom.stringRandomIO
-- * Message is then sent to the user and both senders and recipeint user inbox is then updated
-- * wait for a random period of time determined by Data.Random.randomRIO.
-- * The function can call itself repeatedly with modified values of the MVars.
process :: Int -- ^ User id of the random user
            -> MVar (DataSequence.Seq User) -- ^ MVar containing sequence of users
            -> MVar Int -- ^ Wait time MVar, to stop execution when the maximum sent message limit is reached
            -> MVar Int -- ^ MVar to store the number all the sent messages
            -> Int -- ^ Total Number of messages to send
            -> Int  -- ^ Number of users
            -> IO () -- ^ Required output by forkIO (used to implement concurrency)
process user_id users wait total max_messages max_user = do
    -- Initialize MVars to check if maximum message limit is reached
    t <- takeMVar total
    u <- takeMVar users
    if t == max_messages then do
        -- Update MVars so to stop the simulation
        putMVar users u
        putMVar wait user_id
        
    else do
        --Generating random message
        to <- randomUser user_id max_user
        randStr <- generateString
        (date,time) <- getCurrentDateTime
        message <- generateRandomMessage to user_id randStr date time

        -- Update the senders user inbox 
        let sender = DataSequence.index u (user_id - 1)
        let sender' = sender {sent_messages = message : sent_messages sender, no_of_messages_sent = no_of_messages_sent sender + 1}

        --Update the receivers user inbox
        let receiver = DataSequence.index u (to - 1)
        let receiver' = receiver {messages_received = messages_received receiver + 1, received_messages = message : received_messages receiver} 

        --Update the uesrs
        let u' = DataSequence.update (user_id - 1) sender' u
        let u'' = DataSequence.update (to - 1) receiver' u'

        -- Update MVars for total number of messages
        let t' = t + 1
        putMVar total t'
        putMVar users u''
        -- Display the sent message
        (atomicPutStrLn . show) message
        -- Wait random time
        x <- (*10^6) <$> SystemRandom.randomRIO (1, 5)
        threadDelay x
        process user_id users wait total max_messages max_user


{-| 
Generates a message in the specified format to be displayed on screen 
-}
generateRandomMessage :: Int -- ^ User Id of the recipient
                         -> Int -- ^ User Id of the Sender
                         -> DataText.Text -- ^ Message contentin 
                         -> String  -- ^ Date when the message was sent
                         -> String -- ^ Time when the message was sent
                         -> IO Message -- ^ Return type of IO Message
generateRandomMessage to from msgstring date time = do
    return $ Message{to=to, from=from, content=msgstring,date=date, time=time}

{-| 
Thread safe version of putStrLn, It uses takeMVar and putMVar functions from the Control.Concurrent.MVar module to acquire and release a lock on the standard output stream
-}
atomicPutStrLn :: String -- ^ String to be displayed on the screen
                 -> IO () -- ^ Return type of IO ()
atomicPutStrLn s = takeMVar stdoutLock >> putStrLn s >> putMVar stdoutLock ()

{-| 
To allow two threads to write over stdout
-}
stdoutLock :: MVar ()
stdoutLock = unsafePerformIO $ newMVar ()

{-| 
Function to return tuple of strings containing the current date and current time.
-}
getCurrentDateTime :: IO (String,String) -- ^ A tuple of String containing current date and current time.
getCurrentDateTime = do
    currentTime <- getCurrentTime
    let formattedTime = formatTime defaultTimeLocale "%T" currentTime
    let formattedDate = formatTime defaultTimeLocale "%F" currentTime
    return (formattedDate,formattedTime)

{-| 
Function to generate a random string of characters that matches a given regular expression pattern
-}
generateString :: IO DataText.Text
generateString = do
  let regex = DataText.pack "[a-z]{1,20}$"
  randStr <- StringRandom.stringRandomIO regex
  return randStr

{-| 
Function to find a user given a userid from a list of users
-}
findUserById :: Int -- ^ UserId Of the recipient
                -> [User] -- ^ List of users 
                -> Maybe User -- ^ Returns a user or error
findUserById userId users = 
    let matchingUsers = filter (\user -> user_id user == userId) users
    in
        case matchingUsers of
            [] -> Nothing
            (user:_) -> Just user

{-| 
Function to display the user's inbox
-}
displayUserInbox :: Int -- ^ userId of the recipient
                  -> DataSequence.Seq User  -- ^ Sequence of Users
                  -> IO () -- ^ Returns IO to display the user's Inbox
displayUserInbox userId users = do
    let user = DataSequence.findIndexL (\u -> (userId == getId u)) users
    case user of
        Just u -> displayInboxMessages (DataSequence.index users u)
        Nothing -> putStrLn "No user found"

{-| 
Function to display the user's outbox
-}
displayUserOutbox :: Int -- ^ userId of the recipient
                   -> DataSequence.Seq User -- ^ Sequence of Users
                   -> IO () -- ^ Returns IO to display the user's Outbox
displayUserOutbox userId users = do
    let user = DataSequence.findIndexL (\u -> (userId == getId u)) users
    case user of
        Just u -> displayOutboxMessages (DataSequence.index users u)
        Nothing -> putStrLn "No user found"

{-| 
Function to get userId from User
-}
getId :: User -- ^ User object
       -> Int -- ^ Returns userId
getId user = user_id user

{-| 
Function to display the Inbox Messages
-}
displayInboxMessages :: User -- ^ User Object 
                    -> IO () -- ^ Return type IO
displayInboxMessages user = do
  putStrLn $ "*******************************\n"
  putStrLn $ "Inbox Of User " ++ (show $ user_id user) ++ "\n"
  putStrLn $ "Number of messages received: " ++ (show $ messages_received user) ++ "\n"
  putStrLn $ "Received Messages: "
  mapM_ (putStrLn . show) (received_messages user)

{-| 
Function to display the Inbox Messages
-}
displayOutboxMessages :: User -- ^ User Object 
                      -> IO () -- ^ Return type IO
displayOutboxMessages user = do
  putStrLn $ "*******************************\n"
  putStrLn $ "Outbox Of User " ++ (show $ user_id user) ++ "\n"
  putStrLn $ "Number of messages sent: " ++ (show $ no_of_messages_sent user) ++ "\n"
  putStrLn $ "Sent Messages: "
  mapM_ (putStrLn . show) (sent_messages user)
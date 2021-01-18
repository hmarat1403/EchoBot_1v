{-# LANGUAGE OverloadedStrings #-}

module Config
       ( readToken
       , telegramOffset
       , telegramLimit
       , telegramTimeout
       , telegramAllowUpdates
       , telegramUsers
       , defaultNumberOfMessages
       , helpMessage
       , startMessage
       , defaultHelpMessage
       , defaultStartMessage
       , defaultKeyboard
       )  where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC 
import qualified Data.Map as Map
import System.Directory ( doesFileExist )
import TelegramAPI (InlineKeyboardButton (..), InlineKeyboardMarkUp (..))
import Users (readMapFromFile)

readToken :: IO BC.ByteString
readToken = do
    existFile <- doesFileExist "Data.txt"
    if existFile
        then BC.readFile "Data.txt" 
        else inputToken 
   

inputToken :: IO BC.ByteString 
inputToken = do 
    putStrLn "Input yuor telegram-token:"    
    token <- BC.getLine  
    BC.writeFile "Data.txt" token
    return token

telegramOffset :: Int
telegramOffset = 0 

telegramLimit :: Int
telegramLimit = 10

telegramTimeout :: Int
telegramTimeout = 25

telegramAllowUpdates :: [T.Text]
telegramAllowUpdates = ["message", "channel_post", "callback_query"]

telegramUsers :: IO (Map.Map Int Int)
telegramUsers = do 
    existFile <- doesFileExist "Users.txt"
    if existFile
    then readMapFromFile "Users.txt"
    else return Map.empty

defaultNumberOfMessages :: Int
defaultNumberOfMessages = 1      

helpMessage :: IO BC.ByteString
helpMessage = do 
    existFile <- doesFileExist "Help.txt"
    if existFile
    then BC.readFile "Help.txt"
    else return defaultHelpMessage

startMessage :: IO BC.ByteString
startMessage = do 
    existFile <- doesFileExist "Start.txt"
    if existFile
    then BC.readFile "Start.txt"
    else return defaultStartMessage    

defaultHelpMessage :: BC.ByteString
defaultHelpMessage = "I am echo-bot. I can send back the received messages\n\
                     \I accept commands /help, /repeat, /start and /getMyCommands \n\
                     \/help - displays information about me\n\
                     \/start - starting chat\n\
                     \/repeat - displays information about the number of\n\
                     \repeating messages and give you the opportunity\n\
                     \to change this number in the range from up to 5" 

defaultStartMessage :: BC.ByteString 
defaultStartMessage = "Hello!\nI'm echo-bot. For more info send me command /help"                  

defaultKeyboard :: InlineKeyboardMarkUp
defaultKeyboard = InlineKeyboardMarkUp { inline_keyboard = 
                                      [ [oneButton]
                                      , [twoButton]
                                      , [threeButton]
                                      , [forButton]
                                      , [fiveButton]
                                      ]}
oneButton :: InlineKeyboardButton
oneButton = InlineKeyboardButton { text = "1", callback_data = Just "1" }
twoButton :: InlineKeyboardButton
twoButton = InlineKeyboardButton { text = "2", callback_data = Just "2" }
threeButton :: InlineKeyboardButton
threeButton = InlineKeyboardButton { text = "3", callback_data = Just "3" }
forButton :: InlineKeyboardButton
forButton = InlineKeyboardButton { text = "4", callback_data = Just "4" }
fiveButton :: InlineKeyboardButton
fiveButton = InlineKeyboardButton { text = "5", callback_data = Just "5" }

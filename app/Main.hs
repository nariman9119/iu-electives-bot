{-# LANGUAGE OverloadedStrings #-}


module Main where

import qualified Data.Text                        as Text
import          TelegramBot                       as TelegramBot
import qualified Telegram.Bot.API                 as Telegram
--import BotDatabase

main :: IO ()
main = do
  putStrLn "Please, enter Telegram bot's API token:"
  token <- Telegram.Token . Text.pack <$> getLine
    
  TelegramBot.run token

--  conn <- BotDatabase.initDb
--  inputData <- getLine
--  BotDatabase.insertToDb conn inputData
--  readValue <- BotDatabase.readAllFromDb conn
--  print readValue
--  BotDatabase.disconnectFromDb conn
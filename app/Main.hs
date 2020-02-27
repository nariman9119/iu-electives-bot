{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import           Data.Text                 (Text)
import qualified Data.Text                        as Text
import qualified Telegram.Bot.API          as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser


import           System.Environment          (getEnv)

--translation imports
import Control.Monad
import           Control.Monad.Reader
import qualified Data.Text.IO as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Translate

-- | Bot conversation state model.
data Model = Model
  {
    todoItems :: [TodoItem]
  }
  deriving (Show)

data TodoItem = TodoItem
  { todoItemTitle :: Text  -- ^ Item title.
  } deriving (Show)

inititalModel :: Model
inititalModel = Model { todoItems = [] }

mkTodoItem :: Text -> TodoItem
mkTodoItem title = TodoItem { todoItemTitle = title }

-- | Add a new item to the model.
addItem :: TodoItem -> Model -> Model
addItem item model = model { todoItems = item : todoItems model }

-- | Actions bot can perform.
data Action
  = NoAction    -- ^ Perform no action.
  | Reply Text  -- ^ Reply some text.
  | AddItem Text
  deriving (Show)

-- | Bot application.
bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = Model []
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }


returnText :: Text -> BotM ()
returnText message = do
  let new_message = translateText message
  reply (toReplyMessage message) 


translateText:: Text -> IO [Text]
translateText message = do
  
  Right TranslationResponse { translations = xs } <-
    newManager tlsManagerSettings >>= \mgr ->
    translate (mgr) (Key "") (Just (Source English)) (Target Russian) (Body message)
  forM xs $ \Translation { translatedText = TranslatedText txt } ->
    return txt
    --writeFile "temp.txt" (convertToString txt)

    

convertToText:: String -> Text
convertToText string = Text.pack(string)

convertToString:: Text -> String
convertToString text = Text.unpack(text)

  
-- | How to process incoming 'Telegram.Update's
-- and turn them into 'Action's.
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _  = parseUpdate (AddItem <$> text)

-- | How to handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction action model = 
  case action of
    NoAction -> pure model

    AddItem message -> model <# do
      
      returnText "Noted"
      replyText (message)   
      pure NoAction
  
    --
  --let new_message = returnText message
  

  

-- | Run bot with a given 'Telegram.Token'.
run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (traceBotDefault bot) env
  

-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
-- main :: IO ()
-- main = getEnvToken "token" >>= run
main :: IO ()
main = do
  
  
  
  
  temp <- translateText "please work"
  T.putStrLn (Text.concat(temp))
  putStrLn "Please, enter Telegram bot's API token:" 
  token <- Telegram.Token . Text.pack <$> getLine
    
  run token
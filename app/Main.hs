{-# LANGUAGE OverloadedStrings #-}
-- | This todo bot can add items, remove them
-- and show a current list of things to do.
module Main where

import           Control.Applicative              ((<|>))
import           Control.Concurrent               (threadDelay)
import           Control.Monad.Trans              (liftIO)
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Time
import qualified Telegram.Bot.API                 as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser

-- | Bot conversation state model.
data Model = Model
  { electiveCourses :: [ElectiveCourse]
  , myElectiveCourses :: [ElectiveCourse]
  , currentTime :: UTCTime      -- ^ A list of todo items.
  } deriving (Show)

data ElectiveCourse = ElectiveCourse { electiveCourseTitle :: Text 
                                     , electiveCourseReminder :: Maybe UTCTime
                             } deriving (Show)  

initialModel :: IO Model
initialModel = do

  now <- getCurrentTime
  pure Model { electiveCourses = [
                      ElectiveCourse { electiveCourseTitle = "elective2" , electiveCourseReminder = Nothing }
                    , ElectiveCourse { electiveCourseTitle = "elective1" , electiveCourseReminder = Nothing}]
                    , myElectiveCourses = [],currentTime = now}

-- | Create a new todo item with just a title.
mkCourse :: Text -> ElectiveCourse
mkCourse title = ElectiveCourse { electiveCourseTitle = title, electiveCourseReminder = Nothing }

-- | Ade new course to elective course list.
addCourse :: ElectiveCourse -> Model -> Model
addCourse course model = model { myElectiveCourses = course : myElectiveCourses model }

-- | Remove an item from todo list
removeCourse :: Text -> Model -> Model
removeCourse title model = model { myElectiveCourses = filter p (myElectiveCourses model) }
  where
    p item = electiveCourseTitle item /= title

-- | Pretty print a single todo item.
ppCourse :: ElectiveCourse -> Text
ppCourse course = "- " <> electiveCourseTitle course <> "\n"

-- | Pretty print a todo list.
ppCourses :: Model -> Text
ppCourses model =
  case foldMap ppCourse (electiveCourses model) of
    ""    -> "Would like to add an elective course? :)"
    items -> "Elective courses selected:\n" <> items

-- | Pretty print a todo list.
ppMyCourses :: Model -> Text
ppMyCourses model =
  case foldMap ppCourse (myElectiveCourses model) of
    ""    -> "Would like to add an elective course? :)"
    items -> "Elective courses selected:\n" <> items

-- | Actions bot can perform.
data Action
  = NoAction                -- ^ Perform no action.
  | SetTime UTCTime         -- ^ Update current time.
  | AddItem Text            -- ^ Add a new todo item.
  | RemoveItem Text         -- ^ Remove an item by its title.
  | ShowItems               -- ^ Display all items (either with a new message or by updating existing one).
  | ShowAllCourses
  | Start                   -- ^ Display start message.
  | RevealItemActions Text  -- ^ Update list of items to display item actions.
  | SetReminderIn Int Text  -- ^ Set a reminder for an item in a given amount of minutes from now.
  deriving (Show, Read)

-- | Bot application.
initBot :: IO (BotApp Model Action)
initBot = do
  model <- initialModel
  pure BotApp
    { botInitialModel = model
    , botAction = flip handleUpdate
    , botHandler = handleAction
    , botJobs =
      [ BotJob
        { botJobSchedule = "* * * * *"  -- every minute
        , botJobTask = courseReminder
        }
      ]
    }

setReminderIn :: Int -> Text -> Model -> Model
setReminderIn minutes title model = setReminder title alarmTime model
  where
    now = currentTime model
    alarmTime = addUTCTime (fromIntegral (60 * minutes)) now

-- | Set an absolute alarm time for an item with a given title.
setReminder :: Text -> UTCTime -> Model -> Model
setReminder title datetime model = model
  { electiveCourses = map updateReminder (electiveCourses model) }
    where
      updateReminder item
        | title /= electiveCourseTitle item = item
        | otherwise = item { electiveCourseReminder = Just datetime }
        
courseReminder :: Model -> Eff Action Model
courseReminder model = do
  newItems <- mapM electiveReminder (electiveCourses model)
  pure model { electiveCourses = newItems }
  where
    electiveReminder item =
      case electiveCourseReminder item of
        Just alarmTime | alarmTime <= currentTime model -> do
          eff $ do
            replyText ("Reminder: " <> electiveCourseTitle item)
            return NoAction
          return item { electiveCourseReminder = Nothing }
        _ -> return item

startMessage :: Text
startMessage = Text.unlines
 [ "Introduction text"
 , "Text about functionality of this bot"
 ]

 -- | A start keyboard with some helpful todo suggestions.
startMessageKeyboard :: Telegram.ReplyKeyboardMarkup
startMessageKeyboard = Telegram.ReplyKeyboardMarkup
  { Telegram.replyKeyboardMarkupKeyboard =
      [ [ "Start", "Show" ]
      ]
  , Telegram.replyKeyboardMarkupResizeKeyboard = Just True
  , Telegram.replyKeyboardMarkupOneTimeKeyboard = Just True
  , Telegram.replyKeyboardMarkupSelective = Just True
  }


myCoursesAsInlineKeyboard :: Model -> EditMessage
myCoursesAsInlineKeyboard model =
  case myElectiveCourses model of
    [] -> "No courses selected. Please choose something!!)"
    items -> (toEditMessage "Your list of selected Elective courses")
      { editMessageReplyMarkup = Just $
          Telegram.SomeInlineKeyboardMarkup (myCoursesInlineKeyboard items)
      }
myCoursesInlineKeyboard :: [ElectiveCourse] -> Telegram.InlineKeyboardMarkup
myCoursesInlineKeyboard
  = Telegram.InlineKeyboardMarkup .  map (pure . myCourseInlineKeyboardButton)

myCourseInlineKeyboardButton :: ElectiveCourse -> Telegram.InlineKeyboardButton
myCourseInlineKeyboardButton item = actionButton title (RevealItemActions title)
  where
    title = electiveCourseTitle item


itemsAsInlineKeyboard :: Model -> EditMessage
itemsAsInlineKeyboard model =
  case electiveCourses model of
    [] -> "The list of elective courses is not yet available"
    items -> (toEditMessage "List of available elective courses")
      { editMessageReplyMarkup = Just $
          Telegram.SomeInlineKeyboardMarkup (itemsInlineKeyboard items)
      }

-- | Inline keyboard with every button representing one item.
itemsInlineKeyboard :: [ElectiveCourse] -> Telegram.InlineKeyboardMarkup
itemsInlineKeyboard
  = Telegram.InlineKeyboardMarkup .  map (pure . itemInlineKeyboardButton)

-- | Inline keyboard button for a given todo item.
itemInlineKeyboardButton :: ElectiveCourse -> Telegram.InlineKeyboardButton
itemInlineKeyboardButton item = actionButton title (AddItem title)
  where
    title = electiveCourseTitle item


    
    -- | Actions to do with an item as an inline keyboard message.
itemActionsMessage :: Text -> EditMessage
itemActionsMessage title = (toEditMessage (Text.unlines ["«" <> title <> "»", "Course Instructor: Test Testovich", "Next Lecture: 30.02.2020 09:00"]))
  { editMessageReplyMarkup = Just $
      Telegram.SomeInlineKeyboardMarkup (itemActionsKeyboard title) }

itemActionsKeyboard :: Text -> Telegram.InlineKeyboardMarkup
itemActionsKeyboard title = Telegram.InlineKeyboardMarkup
  [ [ btnRemindIn ]
  , [ btnBack ]
  ]
    where
      btnBack   = actionButton "\x2B05 Back to items list" ShowItems
      btnRemindIn  = actionButton
        ("Set reminder for next lecture")
        (SetReminderIn 5 title)



      
-- | How to process incoming 'Telegram.Update's
-- and turn them into 'Action's.
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ = parseUpdate
    $ ShowItems   <$  command "show"
  <|> RemoveItem  <$> (command "remove" <|> command "done")
  <|> Start       <$  command "start"
  <|> callbackQueryDataRead

  <|> AddItem     <$> text


  
-- | How to handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  NoAction -> pure model
  SetTime t -> model { currentTime = t } <# do
    SetTime <$> liftIO (threadDelay 1000 >> getCurrentTime)
  AddItem title -> addCourse (mkCourse title) model <# do
    replyText "Noted."
    pure NoAction
  RemoveItem title -> removeCourse title model <# do
    replyText ("Removed item: " <> title)
    pure ShowItems
  ShowItems -> model <# do
    replyOrEdit (myCoursesAsInlineKeyboard model)
    pure NoAction
  ShowAllCourses -> model <# do
    replyOrEdit (itemsAsInlineKeyboard model)
    pure NoAction
  Start -> do
    eff $ do
      reply (toReplyMessage startMessage)
        { replyMessageReplyMarkup = Just $
            Telegram.SomeReplyKeyboardMarkup startMessageKeyboard
        }
      pure ShowAllCourses
    eff $ SetTime <$> liftIO getCurrentTime
    pure model
  RevealItemActions title -> model <# do
    editUpdateMessage (itemActionsMessage title)
    pure NoAction
  SetReminderIn minutes title -> setReminderIn minutes title model <# do
    replyText "Ok, I will remind you."
    pure NoAction

-- | Run bot with a given 'Telegram.Token'.
run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  bot <- initBot
  startBot_ (useLatestUpdateInJobs (traceBotDefault bot)) env


-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
-- main :: IO ()
-- main = getEnvToken "token" >>= run
main :: IO ()
main = do 
  putStrLn "Please, enter Telegram bot's API token:" 
  token <- Telegram.Token . Text.pack <$> getLine
    
  run token
{-# LANGUAGE OverloadedStrings #-}


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
  { electiveCourses :: [ElectiveCourse] --list of all elective courses
  , myElectiveCourses :: [ElectiveCourse] --elective courses that user will choose
  , currentTime :: UTCTime 
  } deriving (Show)


data ElectiveCourse = ElectiveCourse { electiveCourseTitle :: Text 
                                     , electiveCourseReminder :: Maybe UTCTime
                    --               , electiveCourseLecturer :: Text
                    --               , electiveCourseRoomNumber :: Text
                    --               , electiveCourseSchedule :: [Text] 
                             } deriving (Eq, Show)  

initialModel :: IO Model
initialModel = do

  now <- getCurrentTime
  pure Model { electiveCourses = [
                      ElectiveCourse { electiveCourseTitle = "elective2" , electiveCourseReminder = Nothing
                   -- , electiveCourseLecturer = "Testoslav Testovich",  electiveCourseRoomNumber = "303"
                   -- , electiveCourseSchedule = ["31.03.2020 09:00-10:35", "5.04.2020 09:00-10:35", "06.05.2020 09:00-10:35"]
                     }
                    
                    , ElectiveCourse { electiveCourseTitle = "elective1" , electiveCourseReminder = Nothing
                   -- , electiveCourseLecturer = "Testoveta Testovna",  electiveCourseRoomNumber = "303"
                   -- , electiveCourseSchedule = ["31.03.2020 09:00-10:35", "5.04.2020 09:00-10:35", "06.05.2020 09:00-10:35"]
                     }
                    ]
                    , myElectiveCourses = [],currentTime = now}


-- Do not provide possibility to add course manually to the list of all courses
mkCourse :: Text -> ElectiveCourse
mkCourse title = ElectiveCourse { electiveCourseTitle = title, electiveCourseReminder = Nothing }


-- | Add course to user`s list from list of all courses.
isMember :: ElectiveCourse -> [ElectiveCourse] -> Bool
isMember n [] = False
isMember n (x:xs)
    | n == x = True
    | otherwise = isMember n xs

addCourse :: ElectiveCourse -> Model -> Model
addCourse course model = do
    if (isMember course (myElectiveCourses model))
      then model
      else model { myElectiveCourses = course : myElectiveCourses model } 
-- | Ability to remove course from user`s list
removeCourse :: Text -> Model -> Model
removeCourse title model = model { myElectiveCourses = filter p (myElectiveCourses model) }
  where
    p item = electiveCourseTitle item /= title

--TODO restrict possibility to add same course many times.

-- | Actions bot can perform.
data Action
  = NoAction                -- ^ Perform no action.
  | AddItem Text            -- ^ Add a new todo item.
  | RemoveItem Text         -- ^ Remove an item by its title.
  | ShowItems               -- ^ Display all items (either with a new message or by updating existing one).
  | ShowAllCourses
  | Start                   -- ^ Display start message.
  | SetTime UTCTime         -- ^ Update current time.
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


--TODO reimplement reminder
setReminderIn :: Int -> Text -> Model -> Model
setReminderIn minutes title model = setReminder title alarmTime model
  where
    now = currentTime model
    alarmTime = addUTCTime (fromIntegral (60 * minutes)) now
--TODO reimplement reminder
-- | Set an absolute alarm time for an item with a given title.
setReminder :: Text -> UTCTime -> Model -> Model
setReminder title datetime model = model
  { electiveCourses = map updateReminder (electiveCourses model) }
    where
      updateReminder item
        | title /= electiveCourseTitle item = item
        | otherwise = item { electiveCourseReminder = Just datetime }

--TODO reimplement reminder        
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
 [ "Welcome to Elective course schedule"
 ]


 -- | A start keyboard with commands to start with.
startMessageKeyboard :: Telegram.ReplyKeyboardMarkup
startMessageKeyboard = Telegram.ReplyKeyboardMarkup
  { Telegram.replyKeyboardMarkupKeyboard =
      [ [ "/start", "/show" ]
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


coursesAsInlineKeyboard :: Model -> EditMessage
coursesAsInlineKeyboard model =
  case electiveCourses model of
    [] -> "The list of elective courses is not yet available"
    items -> (toEditMessage "List of available elective courses")
      { editMessageReplyMarkup = Just $
          Telegram.SomeInlineKeyboardMarkup (coursesInlineKeyboard items)
      }
coursesInlineKeyboard :: [ElectiveCourse] -> Telegram.InlineKeyboardMarkup
coursesInlineKeyboard
  = Telegram.InlineKeyboardMarkup .  map (pure . courseInlineKeyboardButton)

courseInlineKeyboardButton :: ElectiveCourse -> Telegram.InlineKeyboardButton
courseInlineKeyboardButton item = actionButton title (AddItem title)
  where
    title = electiveCourseTitle item


    
myCourseActionsMessage :: Text -> EditMessage
myCourseActionsMessage title = (toEditMessage (Text.unlines ["«" <> title <> "»", "Course Instructor: Test Testovich", "Next Lecture: 30.02.2020 09:00"]))
  { editMessageReplyMarkup = Just $
      Telegram.SomeInlineKeyboardMarkup (myCourseActionsKeyboard title) }

myCourseActionsKeyboard :: Text -> Telegram.InlineKeyboardMarkup
myCourseActionsKeyboard title = Telegram.InlineKeyboardMarkup
  [ [ btnRemindIn ]
  , [ btnBack ]
  ]
    where
      btnBack   = actionButton "\x2B05 Back to course list" ShowItems
      btnRemindIn  = actionButton
        ("Set reminder for next lecture")
        (SetReminderIn 5 title)



      
-- | How to process incoming 'Telegram.Update's
-- and turn them into 'Action's.
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ = parseUpdate
    $ ShowItems   <$  command "show"
  <|> RemoveItem  <$> command "remove" 
  <|> Start       <$  command "start"
  <|> callbackQueryDataRead
-- <|> AddItem     <$> text   no need to handle this action


  
-- | How to handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  NoAction -> pure model
  -- TODO reimplement reminder
  SetTime t -> model { currentTime = t } <# do
    SetTime <$> liftIO (threadDelay 1000 >> getCurrentTime)
  -- add course by creating new course from selected one
  AddItem title -> addCourse (mkCourse title) model <# do
    replyText "Course in your list"
    pure NoAction
  -- remove course from list of user`s courses
  RemoveItem title -> removeCourse title model <# do
    replyText ("Course " <> title <> "removed from your list") 
    pure ShowItems
  -- show list of your courses
  ShowItems -> model <# do
    replyOrEdit (myCoursesAsInlineKeyboard model)
    pure NoAction
  -- show list of all courses
  ShowAllCourses -> model <# do
    replyOrEdit (coursesAsInlineKeyboard model)
    pure NoAction
  -- start telegram bot 
  Start -> do
    eff $ do
      reply (toReplyMessage startMessage)
        { replyMessageReplyMarkup = Just $
            Telegram.SomeReplyKeyboardMarkup startMessageKeyboard
        }
      pure ShowAllCourses
    eff $ SetTime <$> liftIO getCurrentTime
    pure model
-- show actions from course that was selected on user`s list
  RevealItemActions title -> model <# do
    editUpdateMessage (myCourseActionsMessage title)
    pure NoAction
--TODO reimplement reminder
  SetReminderIn minutes title -> setReminderIn minutes title model <# do
    replyText "Ok, I will remind you."
    pure NoAction

run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  bot <- initBot
  startBot_ (useLatestUpdateInJobs (traceBotDefault bot)) env


main :: IO ()
main = do 
  putStrLn "Please, enter Telegram bot's API token:" 
  token <- Telegram.Token . Text.pack <$> getLine
    
  run token
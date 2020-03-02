{-# LANGUAGE OverloadedStrings #-}

module TelegramBot
  ( run
  ) where

import           Control.Applicative              ((<|>))
import           Control.Concurrent               (threadDelay)
import           Control.Monad.Trans              (liftIO)
import           Course
import           Data.Text                        (Text)
import           Data.Text                        as T hiding (concat, filter,
                                                        length, map, null, zip,
                                                        zipWith)
import qualified Data.Text                        as Text
import           Data.Time
import qualified Telegram.Bot.API                 as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser

data Reminder =
  Reminder
    { reminderTitle :: Text
    , reminderTime  :: UTCTime
    }
  deriving (Show)

-- | Bot conversation state model.
data Model =
  Model
    { electiveCourses   :: [Course] --list of all elective courses
    , myElectiveCourses :: [Course] --elective courses that user will choose
    , currentTime       :: UTCTime
    , reminders         :: [Reminder]
    }
  deriving (Show)

initialModel :: IO Model
initialModel = do
  now <- getCurrentTime
  pure Model {electiveCourses = loadCourses, myElectiveCourses = [], reminders = [], currentTime = now}

compareCourses :: Text -> Course -> Bool
compareCourses title course = (T.pack $ name course) == title

-- | Copy course from list of all courses and add it to user`s list
copyCourse :: Model -> Text -> Course
copyCourse model title = (filter (compareCourses title) (electiveCourses model)) !! 0

-- | Check if course is already in your course list
isMember :: Course -> [Course] -> Bool
isMember n [] = False
isMember n (x:xs)
  | name n == name x = True
  | otherwise = isMember n xs

-- | Add course to user`s list from list of all courses.
addCourse :: Course -> Model -> Model
addCourse course model = do
  if (isMember course (myElectiveCourses model))
    then model
    else model {myElectiveCourses = course : myElectiveCourses model}

-- | Ability to remove course from user`s list
removeCourse :: Text -> Model -> Model
removeCourse title model = model {myElectiveCourses = filter p (myElectiveCourses model)}
  where
    p item = (T.pack $ name item) /= title

-- | Actions bot can perform.
data Action
  = NoAction -- ^ Perform no action.
  | AddItem Text -- ^ Add a new todo item.
  | RemoveItem Text -- ^ Remove an item by its title.
  | ShowItems -- ^ Display all items (either with a new message or by updating existing one).
  | ShowAllCourses
  | Start -- ^ Display start message.
  | SetTime UTCTime -- ^ Update current time.
  | RevealItemActions Text -- ^ Update list of items to display item actions.
  | SetReminderIn Text -- ^ Set a reminder for an item in a given amount of minutes from now.
  | ShowReminder Text
  | WeekCourses
  deriving (Show, Read)

-- | Bot application.
initBot :: IO (BotApp Model Action)
initBot = do
  model <- initialModel
  pure BotApp {botInitialModel = model, botAction = flip handleUpdate, botHandler = handleAction, botJobs = []}

findCourse :: Text -> Model -> Course
findCourse title model = (filter equalsItem (myElectiveCourses model)) !! 0
  where
    equalsItem item = (T.pack $ name item) == title

extractTime :: Course -> [UTCTime]
extractTime course = map (\lecture -> startTime $ lecTime lecture) (lectures course)

createReminder :: [UTCTime] -> Text -> Model -> Model
createReminder times title model = model {reminders = (reminders model) ++ newReminders}
  where
    newReminders = map (\timeStart -> Reminder {reminderTitle = title, reminderTime = timeStart}) times

setReminderIn :: Text -> Model -> Model
setReminderIn title model = createReminder startTimes title model
  where
    course = findCourse title model
    startTimes = extractTime course

startMessage :: Text
startMessage =
  Text.unlines
    [ "Welcome to Elective course schedule"
    , "/start - show list of all possible courses"
    , "/show - show list of selected courses"
    , "/remove - remove course from list of selected courses"
    , "/show_week - show courses on this week"
    ]

myCoursesAsInlineKeyboard :: Model -> EditMessage
myCoursesAsInlineKeyboard model =
  case myElectiveCourses model of
    [] -> "No courses selected. Please choose something!!)"
    items ->
      (toEditMessage "Your list of selected Elective courses")
        {editMessageReplyMarkup = Just $ Telegram.SomeInlineKeyboardMarkup (myCoursesInlineKeyboard items)}

myCoursesInlineKeyboard :: [Course] -> Telegram.InlineKeyboardMarkup
myCoursesInlineKeyboard = Telegram.InlineKeyboardMarkup . map (pure . myCourseInlineKeyboardButton)

myCourseInlineKeyboardButton :: Course -> Telegram.InlineKeyboardButton
myCourseInlineKeyboardButton item = actionButton (T.pack $ title) (RevealItemActions (T.pack title))
  where
    title = name item

coursesAsInlineKeyboard :: Model -> EditMessage
coursesAsInlineKeyboard model =
  case electiveCourses model of
    [] -> "The list of elective courses is not yet available"
    items ->
      (toEditMessage "List of available elective courses")
        {editMessageReplyMarkup = Just $ Telegram.SomeInlineKeyboardMarkup (coursesInlineKeyboard items)}

coursesInlineKeyboard :: [Course] -> Telegram.InlineKeyboardMarkup
coursesInlineKeyboard = Telegram.InlineKeyboardMarkup . map (pure . courseInlineKeyboardButton)

courseInlineKeyboardButton :: Course -> Telegram.InlineKeyboardButton
courseInlineKeyboardButton item = actionButton (T.pack title) (AddItem (T.pack title))
  where
    title = name item

myCourseActionsMessage :: Model -> Text -> EditMessage
myCourseActionsMessage model title = do
  let course = copyCourse model title
   in (toEditMessage (T.pack $ showCourse course))
        {editMessageReplyMarkup = Just $ Telegram.SomeInlineKeyboardMarkup (myCourseActionsKeyboard title)}

myCourseActionsKeyboard :: Text -> Telegram.InlineKeyboardMarkup
myCourseActionsKeyboard title = Telegram.InlineKeyboardMarkup [[btnRemindIn], [btnBack], [btnReminders]]
  where
    btnReminders = actionButton ("Show all reminders") (ShowReminder title)
    btnBack = actionButton "\x2B05 Back to course list" ShowItems
    btnRemindIn = actionButton ("Set reminder") (SetReminderIn title)

-- | How to process incoming 'Telegram.Update's
-- and turn them into 'Action's.
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ =
  parseUpdate $
  ShowItems <$ command "show" <|> 
  RemoveItem <$> command "remove" <|> 
  Start <$ command "start" <|>
  WeekCourses <$ command "show_week" <|>
  callbackQueryDataRead

remindersAsInlineKeyboard :: Model -> Text -> EditMessage
remindersAsInlineKeyboard model course =
  case reminders model of
    [] -> "The list of reminders is not yet available"
    items ->
      (toEditMessage "List of reminders")
        { editMessageReplyMarkup =
            Just $
            Telegram.SomeInlineKeyboardMarkup (remindersInlineKeyboard (filter (\i -> reminderTitle i == course) items))
        }

weekLecturesAsInlineKeyboard :: Model -> EditMessage
weekLecturesAsInlineKeyboard model =
  case courses of
    [] -> "You don't have lectures on this week"
    items ->
      (toEditMessage "List of courses")
        {editMessageReplyMarkup = Just $ Telegram.SomeInlineKeyboardMarkup (weekLecturesInlineKeyboard items)}
  where
    courses = thisWeekSchedule day (myElectiveCourses model)
    day = utctDay (currentTime model)

weekLecturesInlineKeyboard :: [Course] -> Telegram.InlineKeyboardMarkup
weekLecturesInlineKeyboard = Telegram.InlineKeyboardMarkup . map (pure . weekLecturesInlineKeyboardButton)

weekLecturesInlineKeyboardButton :: Course -> Telegram.InlineKeyboardButton
weekLecturesInlineKeyboardButton item = actionButton course (AddItem course)
  where
    course = T.pack $ name item

remindersInlineKeyboard :: [Reminder] -> Telegram.InlineKeyboardMarkup
remindersInlineKeyboard = Telegram.InlineKeyboardMarkup . map (pure . reminderInlineKeyboardButton)

reminderInlineKeyboardButton :: Reminder -> Telegram.InlineKeyboardButton
reminderInlineKeyboardButton item = actionButton time (AddItem time)
  where
    time = T.pack $ timeToStr $ reminderTime item

-- | How to handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction action model =
  case action of
    NoAction -> pure model
    SetTime t -> model {currentTime = t} <# do SetTime <$> liftIO (threadDelay 1000 >> getCurrentTime)
  -- add course by creating new course from selected one
    AddItem title ->
      addCourse (copyCourse model title) model <# do
        replyText "Course in your list"
        pure NoAction
  -- remove course from list of user`s courses
    RemoveItem title ->
      removeCourse title model <# do
        replyText ("Course " <> title <> " removed from your list")
        pure ShowItems
  -- show list of your courses
    ShowItems ->
      model <# do
        replyOrEdit (myCoursesAsInlineKeyboard model)
        pure NoAction
  -- show list of all courses
    ShowAllCourses ->
      model <# do
        replyOrEdit (coursesAsInlineKeyboard model)
        pure NoAction
    WeekCourses ->
      model <# do
        replyOrEdit (weekLecturesAsInlineKeyboard model)
        pure NoAction
  -- start telegram bot
    Start -> do
      eff $ do
        reply (toReplyMessage startMessage)
        pure ShowAllCourses
      eff $ SetTime <$> liftIO getCurrentTime
      pure model
    RevealItemActions title ->
      model <# do
        editUpdateMessage (myCourseActionsMessage model title)
        pure NoAction
    SetReminderIn title ->
      setReminderIn title model <# do
        replyText "Ok, I will remind you."
        pure NoAction
    ShowReminder title ->
      model <# do
        replyOrEdit (remindersAsInlineKeyboard model title)
        pure NoAction

-- show actions from course that was selected on user`s list
run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  bot <- initBot
  startBot_ (useLatestUpdateInJobs (traceBotDefault bot)) env

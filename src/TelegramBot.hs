{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module TelegramBot
  ( run
  ) where

import           Data.Bifunctor
import           Data.Hashable              (Hashable)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap


import           Control.Applicative              ((<|>))
import           Control.Concurrent               (threadDelay)
import           Control.Monad.Trans              (liftIO)
import           Course
import           Data.Text                        (Text)
import           Data.Text                        as T hiding (concat, filter,
                                                        length, map, null, zip,
                                                        zipWith, take)

import Data.Maybe
import qualified Data.Text                        as Text
import           Data.Time
import qualified Telegram.Bot.API                 as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser
import           Telegram.Bot.Simple.Conversation
import           Control.Lens

-- | Bot conversation state model.
data Model =
  Model
    { electiveCourses   :: [Course] --list of all elective courses
    , myElectiveCourses :: [Course] --elective courses that user will choose
    , currentTime       :: UTCTime
    , timeZone          :: TimeZone
    , remindLectures    :: [ToRemindLecture]
    }
  deriving (Show)

  -- | An item in a todo list.
data ToRemindLecture = ToRemindLecture
  { toRemindTitle    :: Text            -- ^ Item title.
  , lecReminder :: Maybe UTCTime   -- ^ Optional notification time.
  } deriving (Show)


-- | Create a new todo item with just a title.
mkToRemindLecture :: Text -> ToRemindLecture
mkToRemindLecture title = ToRemindLecture
  { toRemindTitle    = title
  , lecReminder = Nothing
  }

-- | Add a new reminder lecture to todo list.
addReminder :: ToRemindLecture-> Model -> Model
addReminder item model = model { remindLectures = item : remindLectures model }

-- | Remove an item from todo list
removeLecReminder:: Text -> Model -> Model
removeLecReminder title model = model { remindLectures = filter p (remindLectures model) }
  where
    p item = toRemindTitle item /= title

setLecReminderIn :: Text -> Model -> Integer -> Lecture -> Model
setLecReminderIn courseName model lec_id lecture = setReminder title (startTime $ lecTime lecture) model
    where
        title = T.intercalate " " [courseName, T.pack (show lec_id), T.pack (showLecture lecture (timeZone model))]


setLecturesReminder :: Text -> Model -> [(Lecture, Integer)] -> Model
setLecturesReminder courseName model [] = model
setLecturesReminder courseName model (enumLecture: enumLectures) = setLecturesReminder courseName new_model enumLectures
    where
            new_model = setLecReminderIn courseName model lec_id lecture
            lec_id = snd enumLecture
            lecture = fst enumLecture

-- | Set alarm time for an item with a given title
setCourseReminderIn :: Maybe Course -> Model -> Model
setCourseReminderIn Nothing model = model
setCourseReminderIn (Just course) model = setLecturesReminder (T.pack $ name course) model (zip (lectures course) [1..])


-- | Set an absolute alarm time for an item with a given title.
setReminder :: Text -> UTCTime -> Model -> Model
setReminder title datetime model = addReminder  (ToRemindLecture {toRemindTitle = title, lecReminder = Just datetime}) model

-- | Remind user of things to do (if there are any).
todoReminder :: Model -> Eff Action Model
todoReminder model = do
  newItems <- mapM itemReminder (remindLectures model)
  pure model { remindLectures = newItems }
  where
    itemReminder item =
      case lecReminder item of
        Just alarmTime | alarmTime <= currentTime model -> do
          eff $ do
            replyText ("Reminder: " <> toRemindTitle item)
            return NoAction
          return item { lecReminder = Nothing }
        _ -> return item

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
  | ShowTime Text Text
  deriving (Show, Read)


initialModel :: IO Model
initialModel = do
  now <- getCurrentTime
  tz  <-  getCurrentTimeZone
  allCourses <- loadCourses
  pure Model {electiveCourses = catMaybes allCourses, myElectiveCourses = [], currentTime = now, timeZone = tz, remindLectures = []}

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


-- TODO make it to Maybe Course
copyCourse :: Model -> Text -> Course
copyCourse model title = (filter (compareCourses title) (electiveCourses model)) !! 0

findCourse :: Text -> Model -> Maybe Course
findCourse title model = (filter equalsItem (myElectiveCourses model))  ^? element 0
  where
    equalsItem item = (T.pack $ name item) == title


-- | Bot application with different conversations
initBot :: IO  (BotApp
                  (HashMap (Maybe Telegram.ChatId) Model)
                  (Maybe Telegram.ChatId, Action))
initBot = do
  model <- initialModel
  let botjobs = [BotJob {botJobSchedule =  "* * * * *" -- every minute
                     ,  botJobTask = todoReminder
                     }
                 ]
  let someBot  = BotApp {botInitialModel = model, botAction = flip handleUpdate, botHandler = handleAction, botJobs = botjobs}
  pure (conversationBot Telegram.updateChatId  someBot)





setReminderIn :: Text -> Model -> Model
setReminderIn title model = setCourseReminderIn course model
  where
    course = findCourse title model

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
   in (toEditMessage (T.pack $ showCourse course (timeZone model)))
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

-- TODO make it not as buttons
remindersAsInlineKeyboard :: Model -> Text -> EditMessage
remindersAsInlineKeyboard model course =
  case remindLectures model of
    [] -> "The list of reminders is not yet available"
    items ->
      (toEditMessage "List of reminders")
        { editMessageReplyMarkup =
            Just $
            Telegram.SomeInlineKeyboardMarkup (remindersInlineKeyboard (filter (\i -> True) items)) -- (splitOn " " ( toRemindTitle i)) !! 0  == course
        }

weekLecturesAsInlineKeyboard :: Model -> EditMessage
weekLecturesAsInlineKeyboard model =
  case courses of
    [] -> "You don't have lectures on this week"
    items ->
      (toEditMessage "List of courses")
        {editMessageReplyMarkup = Just $ Telegram.SomeInlineKeyboardMarkup (weekLecturesInlineKeyboard day tz items)}
  where
    courses = thisWeekSchedule (day, tz) (myElectiveCourses model)
    tz = timeZone model
    day = localTimeDayFromUTC (currentTime model) tz

weekLecturesInlineKeyboard :: Day-> TimeZone -> [Course] -> Telegram.InlineKeyboardMarkup
weekLecturesInlineKeyboard day tz = Telegram.InlineKeyboardMarkup . map (pure . weekLecturesInlineKeyboardButtonTZ)
    where
        weekLecturesInlineKeyboardButtonTZ course = weekLecturesInlineKeyboardButton day tz course

weekLecturesInlineKeyboardButton :: Day -> TimeZone -> Course -> Telegram.InlineKeyboardButton
weekLecturesInlineKeyboardButton day tz item = actionButton courseName (ShowTime courseName lectureStr) 
  where
    courseName = T.pack $ name item
    lecs = (thisWeekCourseLectures (day, tz) item)
    lectureStr = T.pack ("\n" ++ concat( map showLectureInTimeZone $ take 1 lecs))
    showLectureInTimeZone l = showLecture l tz

remindersInlineKeyboard :: [ToRemindLecture] -> Telegram.InlineKeyboardMarkup
remindersInlineKeyboard = Telegram.InlineKeyboardMarkup . map (pure . reminderInlineKeyboardButton)

reminderInlineKeyboardButton :: ToRemindLecture -> Telegram.InlineKeyboardButton
reminderInlineKeyboardButton item = actionButton title (AddItem title)
  where
    title = toRemindTitle item

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
    ShowTime title time -> model <# do
      replyText (append(append title (T.pack " - "))time)
      pure NoAction
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

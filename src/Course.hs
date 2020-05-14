{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Course where
import Data.Text.Lazy.IO as I hiding (putStrLn)
import qualified Data.ByteString.Lazy as BS
import GHC.Generics
import Data.Time.Clock
import Data.Aeson
import Data.Time.Calendar
import Data.Text as T hiding (concat, map, zip, filter, length, zipWith, null, intercalate, drop)
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List

--import Control.Monad.Trans.Maybe
import GHC.Exts

import Data.Time
import Data.Time.Calendar.OrdinalDate

-- | Parses string to UTCTime
strToTime:: String ->UTCTime
strToTime = understandTime
    where
        timeFormat = "%Y-%m-%d:%H:%M:%S"  --  time "2020-12-10:10:30:20"
        understandTime = parseTimeOrError True defaultTimeLocale timeFormat

-- | Exports time to UTCTime
timeToStr:: UTCTime -> String
timeToStr = formatTime defaultTimeLocale "%Y-%m-%d:%H:%M:%S"

toObject :: ToJSON a => a -> Object
toObject a = case toJSON a of
  Object o -> o
  _        -> error "toObject: value isn't an Object"

-- | Data LactureTime presents Lecture start time and end time
data LectureTime = LectureTime {startTime::UTCTime, endTime::UTCTime} deriving (Show)

-- | Is used to serialize LectureTime to JSON and from JSON
instance FromJSON LectureTime where
  parseJSON = withObject "LectureTime" $ \o -> do
    startTimestr <- o .: "startTime"
    endTimestr <- o .: "endTime"
    let startTime = strToTime $ T.unpack startTimestr
    let endTime = strToTime $ T.unpack endTimestr
    return LectureTime{..}

instance ToJSON LectureTime where   -- decode (encode lect):: Maybe LectureTime
  toJSON LectureTime{..} = object [
    "startTime" .= T.pack (timeToStr startTime),
    "endTime" .= T.pack (timeToStr endTime)]

type Room = String -- room number

{-|
  Data Lecture consists of lecture time and room where lecture will be present
-}
data Lecture = Lecture {lecTime::LectureTime, room::Room} deriving (Show, Generic)
instance FromJSON Lecture where


instance ToJSON Lecture where

{-|
  Data type course consists of course name and lectures - [Lectures]
  Is serializable
-}
data Course = Course {name::String, lectures::[Lecture]} deriving (Show, Generic)
instance FromJSON Course where

instance ToJSON Course where

-- | Extracts day from local time given utc time and local timezone
localTimeDayFromUTC:: UTCTime -> TimeZone -> Day
localTimeDayFromUTC utct tz = localDay $ utcToLocalTime tz utct

-- | Shows LectureTime in local time
showLectureTime::LectureTime-> TimeZone->String
showLectureTime lecTime tz = (drop 5 $ show $ localDay $ utcToLocalTime tz $ startTime lecTime) ++ " " ++ (intercalate "-" $ map showTime  [startTime lecTime, endTime lecTime])
    where
        localTime time = localTimeOfDay $ utcToLocalTime tz time
        showTime time = intercalate ":" $ map show  [todHour $ localTime time, todMin $ localTime time]

-- | Shows Lecture in local time
showLecture :: Lecture -> TimeZone -> String
showLecture Lecture {lecTime=lecTime, room=room} tz  = "\x1F55B " ++ (showLectureTime lecTime tz)
    ++   "\n\x1F6AA" ++ show room ++ "\n"

-- | Shows Course in local time
showCourse :: Course -> TimeZone -> String
showCourse Course {name=name, lectures=lectures} tz = name ++ "\n" ++
    concat( map showLectureInTimeZone lectures)
    where
        showLectureInTimeZone lec = showLecture lec tz

-- | Extracts today lectures from course
thisDayCourseLectures:: Day -> Course-> [Lecture]  -- local day
thisDayCourseLectures localday Course {name=name, lectures=lectures} =
    filter ( \lec-> utctDay (startTime $ lecTime lec) == localday) lectures

{-|
  Creates today schedule by
  - extracting courses that have lectures today from list of all courses
  - extracting today lectures from these courses
  - returns list of today courses = courses that contain only today lectures
-}
thisDaySchedule:: Day -> [Course] ->[Course]
thisDaySchedule day courses = filter (not . null . lectures) todayCourses
    where
        todayCourses =  map (\course ->
            Course {name =name course, lectures = thisDayCourseLectures day course}) courses

-- | Tells if two days are on the same week
daysOnSameWeek :: Day -> Day -> Bool
daysOnSameWeek day1 day2 = diff >= 0 && diff < 7
  where
    diff = diffDays day2 firstDayOfWeek1
    firstDayOfWeek1 = addDays diffBetweenWeekStart day1
    diffBetweenWeekStart = -(toInteger week_day - 1)
    (_, week_day) = mondayStartWeek day1

-- | extracts list of lectures that will be present on this week
thisWeekCourseLectures:: (Day, TimeZone) -> Course -> [Lecture]
thisWeekCourseLectures (day, tz) Course {name=name, lectures=lectures} =
    filter ( \lec-> daysOnSameWeek (localTimeDayFromUTC (startTime $ lecTime lec) tz) day) lectures


{-|
  Creates week schedule by
  - extracting courses that have lectures on this week from list of all courses
  - extracting this week lectures from these courses
  - returns list of week courses = courses that contain only this week lectures
-}
thisWeekSchedule:: (Day, TimeZone) -> [Course] -> [Course]
thisWeekSchedule timezonedDay courses = filter (not . null . lectures) thisWeekCourses
    where
        thisWeekCourses =  map (\course ->
            Course {name =name course, lectures = thisWeekCourseLectures timezonedDay course}) courses


-- | if course has this title
compareCourses :: Text -> Course -> Bool
compareCourses title course = (T.pack $ name course) == title

-- | Check if course is already in your course list
isMember :: Course -> [Course] -> Bool
isMember n [] = False
isMember n (x:xs)
  | name n == name x = True
  | otherwise = isMember n xs

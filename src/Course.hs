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
import Data.Text as T hiding (concat, map, zip, filter, length, zipWith, null)
import Control.Applicative
import Control.Monad
import Data.Maybe
--import Control.Monad.Trans.Maybe
import GHC.Exts

import Data.Time
import Data.Time.Calendar.OrdinalDate


strToTime:: String ->UTCTime
strToTime = understandTime
    where
        timeFormat = "%Y-%m-%d:%H:%M:%S"  --  time "2020-12-10:10:30:20"
        understandTime = parseTimeOrError True defaultTimeLocale timeFormat

timeToStr:: UTCTime -> String
timeToStr = formatTime defaultTimeLocale "%Y-%m-%d:%H:%M:%S"

toObject :: ToJSON a => a -> Object
toObject a = case toJSON a of
  Object o -> o
  _        -> error "toObject: value isn't an Object"


data LectureTime = LectureTime {startTime::UTCTime, endTime::UTCTime} deriving (Show)

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

type Room = Int -- room number

data Lecture = Lecture {lecTime::LectureTime, room::Room} deriving (Show, Generic)
instance FromJSON Lecture where


instance ToJSON Lecture where


data Course = Course {name::String, lectures::[Lecture]} deriving (Show, Generic)
instance FromJSON Course where


instance ToJSON Course where


showLecture :: (Int, Lecture) -> String
showLecture (i, Lecture {lecTime=lecTime, room=room}) = show i ++ " Lecture:\nSTART: " ++
    show (startTime lecTime) ++ "\nEND: " ++ show (endTime lecTime) ++   "\nROOM:" ++ show room ++ "\n"

showCourse :: Course -> String
showCourse Course {name=name, lectures=lectures}= name ++ "\n" ++
    concat(zipWith (curry showLecture) [1 ..] lectures)

thisDayCourseLectures:: Day -> Course-> [Lecture]
thisDayCourseLectures day Course {name=name, lectures=lectures} =
    filter ( \lec-> utctDay (startTime $ lecTime lec) == day) lectures

thisDaySchedule:: Day -> [Course] ->[Course]
thisDaySchedule day courses = filter (not . null . lectures) todayCourses
    where
        todayCourses =  map (\course ->
            Course {name =name course, lectures = thisDayCourseLectures day course}) courses

daysOnSameWeek :: Day -> Day -> Bool
daysOnSameWeek day1 day2 = diff >= 0 && diff < 7
  where
    diff = diffDays day2 firstDayOfWeek1
    firstDayOfWeek1 = addDays diffBetweenWeekStart day1
    diffBetweenWeekStart = -(toInteger week_day - 1)
    (_, week_day) = mondayStartWeek day1

thisWeekCourseLectures:: Day -> Course -> [Lecture]
thisWeekCourseLectures day Course {name=name, lectures=lectures} =
    filter ( \lec-> daysOnSameWeek (utctDay (startTime $ lecTime lec)) day) lectures

thisWeekSchedule:: Day -> [Course] -> [Course]
thisWeekSchedule day courses = filter (not . null . lectures) thisWeekCourses
    where
        thisWeekCourses =  map (\course ->
            Course {name =name course, lectures = thisWeekCourseLectures day course}) courses


loadCourse:: String-> IO (Maybe Course)
loadCourse filename = do
    filecontent <- BS.readFile filename
    return (decode (filecontent) :: Maybe Course)


decodeCourse :: BS.ByteString -> Maybe Course
decodeCourse s = decode s :: Maybe Course

loadCourses::[Course]
loadCourses = catMaybes $ map decodeCourse coursesStr
    where
    coursesStr = [cv, virt, devops, haskell]
    cv = "{\n  \"lectures\": [\n    {\n      \"room\": 100,\n      \"lecTime\": {\n        \"startTime\": \"2020-03-02:10:35:00\",\n        \"endTime\": \"2020-03-02:12:05:00\"\n      }\n    },\n    {\n      \"room\": 100,\n      \"lecTime\": {\n        \"startTime\": \"2020-01-10:13:40:00\",\n        \"endTime\": \"2020-01-10:14:10:00\"\n      }\n    },\n    {\n      \"room\": 312,\n      \"lecTime\": {\n        \"startTime\": \"2020-02-20:10:35:00\",\n        \"endTime\": \"2020-02-20:12:05:00\"\n      }\n    },\n    {\n      \"room\": 312,\n      \"lecTime\": {\n        \"startTime\": \"2020-02-20:12:15:00\",\n        \"endTime\": \"2020-02-20:13:45:00\"\n      }\n    },\n    {\n      \"room\": 100,\n      \"lecTime\": {\n        \"startTime\": \"2020-02-23:10:35:00\",\n        \"endTime\": \"2020-02-23:12:05:20\"\n      }\n    },\n    {\n      \"room\": 321,\n      \"lecTime\": {\n        \"startTime\": \"2020-02-23:17:10:00\",\n        \"endTime\": \"2020-02-23:18:40:00\"\n      }\n    },\n    {\n      \"room\": 321,\n      \"lecTime\": {\n        \"startTime\": \"2020-02-23:18:50:00\",\n        \"endTime\": \"2020-02-23:20:10:00\"\n      }\n    }\n  ],\n  \"name\": \"CV\"\n}\n\n"
    virt = "{\n  \"lectures\": [\n    {\n      \"room\": 108,\n      \"lecTime\": {\n        \"startTime\": \"2020-02-27:17:10:00\",\n        \"endTime\": \"2020-02-27:18:40:00\"\n      }\n    },\n    {\n      \"room\": 108,\n      \"lecTime\": {\n        \"startTime\": \"2020-01-20:13:40:00\",\n        \"endTime\": \"2020-01-20:14:10:00\"\n      }\n    },\n    {\n      \"room\": 312,\n      \"lecTime\": {\n        \"startTime\": \"2020-01-21:10:35:00\",\n        \"endTime\": \"2020-01-21:12:05:00\"\n      }\n    },\n    {\n      \"room\": 108,\n      \"lecTime\": {\n        \"startTime\": \"2020-01-22:12:15:00\",\n        \"endTime\": \"2020-01-22:13:45:00\"\n      }\n    },\n    {\n      \"room\": 100,\n      \"lecTime\": {\n        \"startTime\": \"2020-02-06:10:35:00\",\n        \"endTime\": \"2020-02-06:12:05:20\"\n      }\n    },\n    {\n      \"room\": 318,\n      \"lecTime\": {\n        \"startTime\": \"2020-02-11:17:10:00\",\n        \"endTime\": \"2020-02-11:18:40:00\"\n      }\n    },\n    {\n      \"room\": 104,\n      \"lecTime\": {\n        \"startTime\": \"2020-02-11:18:50:00\",\n        \"endTime\": \"2020-02-11:20:10:00\"\n      }\n    },\n    {\n      \"room\": 104,\n      \"lecTime\": {\n        \"startTime\": \"2020-02-18:18:50:00\",\n        \"endTime\": \"2020-02-18:20:10:00\"\n      }\n    }\n  ],\n  \"name\": \"Virtualization\"\n}"
    devops = "{\n  \"lectures\": [\n    {\n      \"room\": 106,\n      \"lecTime\": {\n        \"startTime\": \"2020-03-03:17:10:00\",\n        \"endTime\": \"2020-03-03:18:40:00\"\n      }\n    },\n    {\n      \"room\": 106,\n      \"lecTime\": {\n        \"startTime\": \"2020-01-20:13:40:00\",\n        \"endTime\": \"2020-01-20:14:10:00\"\n      }\n    },\n    {\n      \"room\": 101,\n      \"lecTime\": {\n        \"startTime\": \"2020-01-21:10:35:00\",\n        \"endTime\": \"2020-01-21:12:05:00\"\n      }\n    },\n    {\n      \"room\": 101,\n      \"lecTime\": {\n        \"startTime\": \"2020-01-21:12:15:00\",\n        \"endTime\": \"2020-01-21:13:45:00\"\n      }\n    },\n    {\n      \"room\": 101,\n      \"lecTime\": {\n        \"startTime\": \"2020-02-06:10:35:00\",\n        \"endTime\": \"2020-02-06:12:05:20\"\n      }\n    },\n    {\n      \"room\": 320,\n      \"lecTime\": {\n        \"startTime\": \"2020-02-11:17:10:00\",\n        \"endTime\": \"2020-02-11:18:40:00\"\n      }\n    },\n    {\n      \"room\": 104,\n      \"lecTime\": {\n        \"startTime\": \"2020-02-09:10:30:00\",\n        \"endTime\": \"2020-02-09:12:10:00\"\n      }\n    },\n    {\n      \"room\": 320,\n      \"lecTime\": {\n        \"startTime\": \"2020-02-18:18:50:00\",\n        \"endTime\": \"2020-02-18:20:10:00\"\n      }\n    }\n  ],\n  \"name\": \"Devops\"\n}"
    haskell = "{\n  \"lectures\": [\n    {\n      \"room\": 100,\n      \"lecTime\": {\n        \"startTime\": \"2020-03-09:10:35:00\",\n        \"endTime\": \"2020-03-09:12:05:00\"\n      }\n    },\n    {\n      \"room\": 100,\n      \"lecTime\": {\n        \"startTime\": \"2020-01-20:13:40:00\",\n        \"endTime\": \"2020-01-20:14:10:00\"\n      }\n    },\n    {\n      \"room\": 312,\n      \"lecTime\": {\n        \"startTime\": \"2020-01-21:10:35:00\",\n        \"endTime\": \"2020-01-21:12:05:00\"\n      }\n    },\n    {\n      \"room\": 312,\n      \"lecTime\": {\n        \"startTime\": \"2020-01-28:12:15:00\",\n        \"endTime\": \"2020-01-28:13:45:00\"\n      }\n    },\n    {\n      \"room\": 100,\n      \"lecTime\": {\n        \"startTime\": \"2020-02-04:10:35:00\",\n        \"endTime\": \"2020-02-04:12:05:20\"\n      }\n    },\n    {\n      \"room\": 312,\n      \"lecTime\": {\n        \"startTime\": \"2020-02-11:17:10:00\",\n        \"endTime\": \"2020-02-11:18:40:00\"\n      }\n    },\n    {\n      \"room\": 312,\n      \"lecTime\": {\n        \"startTime\": \"2020-02-11:18:50:00\",\n        \"endTime\": \"2020-02-11:20:10:00\"\n      }\n    },\n    {\n      \"room\": 312,\n      \"lecTime\": {\n        \"startTime\": \"2020-02-18:18:50:00\",\n        \"endTime\": \"2020-02-18:20:10:00\"\n      }\n    }\n  ],\n  \"name\": \"Haskell\"\n}"


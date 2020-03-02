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

loadCourses::IO [(Maybe Course)]
loadCourses = mapM loadCourse coursesStr
    where
        coursesStr = ["CV_Course.json", "Devops_Course.json", "Haskell_Course.json", "Virtualization_Course.json"]

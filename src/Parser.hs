module Parser (runParser) where

import qualified Data.Array
import qualified Data.List
import qualified Data.Maybe
import Codec.Xlsx
import qualified Data.ByteString.Lazy as L
import Control.Lens
import           Data.Text                        as T hiding (concat, filter,
                                                        length, map, null, zip,
                                                        zipWith, take)

data Lecture = Lecture {name :: String, teacher :: String, room :: String, time :: String, day :: String} deriving (Show)

-- 37, 816

-- time !! ((y + 1) `mod` 7)

timeArray :: [(String, String)]
timeArray = [
  ("09:00:00", "10:30:00"),
  ("10:35:00", "12:05:00"),
  ("12:10:00", "13:40:00"),
  ("14:10:00", "15:40:00"),
  ("17:20:00", "18:50:00"),
  ("18:55:00", "20:25:00"),
  ("20:30:00", "22:00:00")]

-- (y - 2) / 8
-- (y - 2) / 8

getDay :: Int -> Int
getDay y = ((y - 2) `div` 8)

getTime y = fst timePair ++ "-" ++ snd timePair
  where
    timePair = timeArray !! ((y - 1) `mod` 7)

--getContent :: Int -> Int -> IO(Maybe CellText)
getContent x y = do
  file <- L.readFile "report.xlsx"
  let value = toXlsx file ^? ixSheet (T.pack $ "Main") . ixCell (x,y) . cellValue . _Just
  return value

foo :: [Int] -> Int -> [(Int, String)] -> IO([(Int, Lecture)])
foo (y:ys) x date = do
  word <- getContent y x
  words <- foo ys x date

  let objStr = case word of
       Just (CellText a) -> do
        let splitted = (splitOn (T.pack $ "\n") a)
        let name = (splitOn (T.pack $ "\"") (splitted !! 0)) !! 0
        let day = snd (date !! (getDay y))
        Lecture {name = T.unpack $ name, teacher = T.unpack $ (splitted !! 1), room = T.unpack $ (splitted !! 2), time = (getTime x), day = day}
--        (replacing) ++ (getTime x)
       Just _ -> Lecture {}
       Nothing -> Lecture {}

  if (Data.Maybe.isJust word) then
    return ((y, objStr) : words)
  else
    return words
foo [] _ _ = do return []

goColumns :: [Int] -> [(Int, String)] -> IO([(Int, [(Int, Lecture)])])
goColumns (column:columns) date = do
  word <- foo (Data.Array.range (2, 10)) column date
  words <- goColumns columns date

  return ((column, word) : words)
goColumns [] _ = do return []

getDate :: [Int] -> IO([(Int, String)])
getDate (y:ys) = do
  word <- getContent y 1
  words <- getDate ys

  let objStr = case word of
         Just (CellText a) -> T.unpack $ a
         Just _ -> ""
         Nothing -> ""

  let splittedDay = splitOn (T.pack $ "/") (T.pack $ objStr)
  let parsedDay = (T.unpack $ splittedDay !! 0) ++ "-" ++ (T.unpack $ splittedDay !! 1) ++ "-" ++ (T.unpack $ splittedDay !! 2)

  if (Data.Maybe.isJust word) then
    return ((getDay y, parsedDay) : words)
  else
    return words
getDate [] = do return []

runParser :: IO ()
runParser = do
  date <- getDate (Data.Array.range (2, 60))
  print date
  dates <- goColumns (Data.Array.range (31, 37)) date
  print dates
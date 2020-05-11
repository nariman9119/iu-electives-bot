module Parser (runParser) where

import qualified Data.Array
import Codec.Xlsx
import qualified Data.ByteString.Lazy as L
import Control.Lens
import           Data.Text                        as T hiding (concat, filter,
                                                        length, map, null, zip,
                                                        zipWith, take)

getContent :: Int -> Int -> IO(String)
getContent x y = do
  file <- L.readFile "report.xlsx"
  let value = toXlsx file ^? ixSheet (T.pack $ "Main") . ixCell (x,y) . cellValue . _Just
  return (show value)

foo :: [Int] -> IO([(Int, String)])
foo (x:xs) = do
  word <- getContent x 4
  words <- foo xs
  return ((x, word) : words)
foo [] = do return []

runParser :: IO ()
runParser = do
  dates <- foo (Data.Array.range (2, 10))
  print value
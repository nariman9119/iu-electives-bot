module BotDatabase (initDb, insertToDb, readFromDb, readAllFromDb, disconnectFromDb) where

import Control.Lens
import Control.Monad
import Database.HDBC
import Database.HDBC.Sqlite3

tableName :: String
tableName = "bot"

dbName :: String
dbName = "db.db"

checkHasTable :: [[SqlValue]] -> Bool
checkHasTable value =
  case (value ^? element 0) of
    Just innerValue ->
      case (innerValue ^? element 0) of
        Just result ->
          if (fromSql result == tableName)
            then True
          else False
        Nothing -> False
    Nothing -> False

initDb :: IO Connection
initDb = do
  conn <- connectSqlite3 dbName
  let query = "SELECT name FROM sqlite_master WHERE type='table' AND name='" ++ tableName ++ "'"
  hasTable <- quickQuery conn query []
  when (not $ checkHasTable hasTable) $ createTable conn
  return conn

createTable :: Connection -> IO()
createTable conn = do
  let query = "CREATE TABLE " ++ tableName ++ " (id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, desc VARCHAR(80))"
  _ <- run conn query [];
  commit conn

disconnectFromDb :: Connection -> IO()
disconnectFromDb conn = do
  disconnect conn

readFromDb :: Connection -> Int -> IO([String])
readFromDb conn identifier = do
  let query = "SELECT id, desc from " ++ tableName ++ " where id = ?"
  result <- quickQuery conn query [toSql identifier]
  let stringRows = map convertRow result
  return stringRows

readAllFromDb :: Connection -> IO([String])
readAllFromDb conn = do
  let query = "SELECT id, desc from " ++ tableName
  result <- quickQuery conn query []
  let stringRows = map convertRow result
  return stringRows

insertToDb :: Connection -> String -> IO()
insertToDb conn value = do
  let query = "INSERT INTO " ++ tableName ++ " (desc) VALUES (?)"
  updatedColumns <- run conn query [toSql value]
  when (updatedColumns /= 1) $ print "Something went wrong"
  commit conn

-- Function for converting table to human-read format
convertRow :: [SqlValue] -> String
convertRow [sqlId, sqlDesc] =
  show identifier ++ ": " ++ desc
  where
    identifier = (fromSql sqlId)::Integer
    desc = case fromSql sqlDesc of
           Just x -> x
           Nothing -> "NULL"
convertRow x = fail $ "Unexpected result: " ++ show x
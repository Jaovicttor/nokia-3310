{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}
module DB.Models.Event where
import Database.PostgreSQL.Simple
import DB.Connection
import GHC.Generics (Generic)
import DB.Models.Chip
import Data.Time
import Data.Time (UTCTime)

-- title, event_day, comments, chip_id 

data Event = Event {
    event_id :: Int,
    title :: String,
    comment :: String,
    event_day :: UTCTime, 
    chip_id :: Int
} deriving (Generic, FromRow, Show, Read, Eq)

createEvents :: IO()
createEvents = do
    conn <- connectCloud
    execute_ conn "CREATE TABLE IF NOT EXISTS events (\
                    \id SERIAL PRIMARY KEY,\
                    \title VARCHAR(255) NOT NULL,\
                    \event_day date NOT NULL,\
                    \comments VARCHAR(255),\
                    \chip_id int,\
                    \FOREIGN KEY(chip_id) REFERENCES chips(id));"

    return ()

insertEvent :: String -> String -> String -> IO ()
insertEvent title event_day comments = do
 let q = "insert into events (title, comments, event_day, chip_id ) values (?,?,?,?)"
 conn <- connectCloud
 execute conn q (title, comments, event_day,(idChip myChip))
 return ()

getNextEvent:: IO [Event]
getNextEvent= do
    conn <- connectCloud
    query conn "select  id,title,comments,event_day, chip_id from events \
                \where event_day >= current_timestamp and chip_id =?" (Only (idChip myChip))

getPreviusEvent:: IO [Event]
getPreviusEvent= do
    conn <- connectCloud
    query conn "select id,title,comments,event_day, chip_id from events \
                \where event_day <= current_timestamp and chip_id =?" (Only (idChip myChip))

getEvents :: IO [Event]
getEvents = do
    conn <- connectCloud
    query conn "select id,title,comments,event_day,chip_id from events \
                \where chip_id =?" (Only (idChip myChip))

findEvent :: String -> IO [Event]
findEvent title = do
 let q = "select id,title,comments,event_day,chip_id from events \
                \where id=? and chip_id =?" 
 conn <- connectCloud
 query conn q (title,(idChip myChip)) :: IO [Event]


deleteEventDB :: Int -> IO ()
deleteEventDB x = do
  let q = "delete from events where id = ?"
  conn <- connectCloud
  execute conn q (Only x)
  return ()
  
eventsToString:: [Event] -> Int -> String
eventsToString [] _ = []
eventsToString (x:xs) n = show(n + 1) ++ " - " ++ (singleEventToString x) ++ "\n" ++ eventsToString xs (n+1)

singleEventToString :: Event -> String
singleEventToString event = (formatTime defaultTimeLocale "%Y-%m-%d" (event_day event)) ++ " - " ++  "T: "++ (title event) 
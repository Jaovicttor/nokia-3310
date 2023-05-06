{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}
module DB.Models.Event where
import Database.PostgreSQL.Simple
import DB.Connection
import GHC.Generics (Generic)
import DB.Models.Chip

-- title, event_day, comments, chip_id 

data Event = Event {
    idEvent:: Int,
    title:: String,
    --event_day:: String, --todo, ver como haskell lida com utc
    comment:: String,
    chip_id:: Int
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

insertEvent :: String -> String -> String -> Int -> IO ()
insertEvent title event_day comments chip_id = do
 let q = "insert into events (title, event_day, comments, chip_id ) values (?,?,?,?)"
 conn <- connectCloud
 execute conn q (title, event_day, comments, chip_id)
 return ()

getNextEvent:: IO [Event]
getNextEvent= do
    conn <- connectCloud
    query conn "select id,title,comments,chip_id from events \
                \where event_day >= current_timestamp and chip_id =?" (Only (idChip myChip))

getPreviusEvent:: IO [Event]
getPreviusEvent= do
    conn <- connectCloud
    query conn "select id,title,comments,chip_id from events \
                \where event_day < current_timestamp and chip_id =?" (Only (idChip myChip))

findEvent:: String -> IO [Event]
findEvent title = do
    conn <- connectCloud
    query conn "select id,title,comments,chip_id from events \
                \where title=? and chip_id =?" (Only title (idChip myChip)) --todo event_day=? and

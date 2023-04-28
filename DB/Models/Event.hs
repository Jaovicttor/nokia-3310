{-# LANGUAGE OverloadedStrings #-}
module DB.Models.Event where
import Database.PostgreSQL.Simple
import DB.Connection

createEvents :: IO()
createEvents = do
    conn <- connectionMyDB
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
 conn <- connectionMyDB
 execute conn q (title, event_day, comments, chip_id)
 return ()
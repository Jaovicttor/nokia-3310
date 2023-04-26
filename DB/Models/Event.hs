{-# LANGUAGE OverloadedStrings #-}
module DB.Models.Event where
import Database.PostgreSQL.Simple

createEvents :: Connection -> IO()
createEvents conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS events (\
                    \id SERIAL PRIMARY KEY,\
                    \title VARCHAR(255) NOT NULL,\
                    \event_day date NOT NULL,\
                    \comments VARCHAR(255),\
                    \chip_id int,\
                    \FOREIGN KEY(chip_id) REFERENCES chips(id));"

    return ()

insertEvent ::  Connection -> String -> String -> String -> Int -> IO ()
insertEvent conn title event_day comments chip_id = do
 let q = "insert into events (title, event_day, comments, chip_id ) values (?,?,?,?)"
 execute conn q (title, event_day, comments, chip_id)
 return ()
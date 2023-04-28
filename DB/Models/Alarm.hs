{-# LANGUAGE OverloadedStrings #-}
module DB.Models.Alarm where
import Database.PostgreSQL.Simple
import DB.Connection

createAlarms :: IO()
createAlarms = do
    conn <- connectionMyDB
    execute_ conn "CREATE TABLE IF NOT EXISTS alarms (\
                    \id SERIAL PRIMARY KEY,\
                    \deadline time NOT NULL,\
                    \title VARCHAR(255) NOT NULL unique,\
                    \active boolean NOT NULL);"
    return ()

insertAlarm ::  String -> String -> IO ()
insertAlarm time title = do
 let q = "insert into alarms (deadline,title,active) values (?,?,?)"
 conn <- connectionMyDB
 execute conn q (time, title, True)
 return ()
{-# LANGUAGE OverloadedStrings #-}
module DB.Models.Chip where
import Database.PostgreSQL.Simple
import DB.Connection

createChips :: IO()
createChips = do
    conn <- connectionMyDB
    execute_ conn "CREATE TABLE IF NOT EXISTS chips (\
                    \id SERIAL PRIMARY KEY,\
                    \owner VARCHAR(255) NOT NULL,\
                    \number VARCHAR(11) NOT NULL unique,\
                    \isOn boolean NOT NULL);"
    return ()

insertChip ::  String -> String -> Bool -> IO ()
insertChip owner number isOn = do
 let q = "insert into chips (owner,number,ison) values (?,?,?)"
 conn <- connectionMyDB
 execute conn q (owner, number, isOn)
 return ()
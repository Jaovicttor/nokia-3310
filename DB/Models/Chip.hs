{-# LANGUAGE OverloadedStrings #-}
module DB.Models.Chip where
import Database.PostgreSQL.Simple

createChips :: Connection -> IO()
createChips conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS chips (\
                    \id SERIAL PRIMARY KEY,\
                    \owner VARCHAR(255) NOT NULL,\
                    \number VARCHAR(11) NOT NULL unique,\
                    \isOn boolean NOT NULL);"
    return ()

insertChip ::  Connection -> String -> String -> Bool -> IO ()
insertChip conn owner number isOn = do
 let q = "insert into chips (owner,number,ison) values (?,?,?)"
 execute conn q (owner, number, isOn)
 return ()
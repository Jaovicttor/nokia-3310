{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module DB.Models.Contact where
import Database.PostgreSQL.Simple
import DB.Connection
import GHC.Generics (Generic)

data Contact = Contact { name :: String } deriving (Generic, Show, FromRow, Read, Eq)

createContacts :: IO()
createContacts = do
    conn <- connectionMyDB
    execute_ conn "CREATE TABLE IF NOT EXISTS contacts (\
                    \id SERIAL PRIMARY KEY,\
                    \name VARCHAR(255) NOT NULL,\
                    \phone VARCHAR(11) NOT NULL,\
                    \birthday date,\
                    \speed_dial int,\
                    \chip_id int,\
                    \FOREIGN KEY(chip_id) REFERENCES chips(id));"

    return ()

insertContact :: String -> String -> String -> Int -> Int -> IO ()
insertContact name phone birthday speed_dial chip_id = do
 let q = "insert into contacts (name, phone, birthday, speed_dial, chip_id ) values (?,?,?,?,?)"
 conn <- connectionMyDB
 execute conn q (name, phone, birthday, speed_dial, chip_id)
 return ()

findByPhone::Int -> String -> IO (Maybe String)
findByPhone chip_id phone = do
    let q = "select name from contacts where chip_id = ? and phone = ?"
    conn <- connectionMyDB 
    result <- query conn q (chip_id, phone)
    case result of
        [Contact name] -> return (Just name)
        _              -> return Nothing
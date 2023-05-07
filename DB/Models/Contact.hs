{-# LANGUAGE OverloadedStrings #-}
module DB.Models.Contact where
import Database.PostgreSQL.Simple
import DB.Connection

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


getContactNames :: IO [String]
getContactNames = do
  let q = "select name from contacts"
  conn <- connectionMyDB
  result <- query_ conn q :: IO [Only String]
  return $ map fromOnly result

getSpeedDial :: IO [String]
getSpeedDial = do
  let q = "select name from contacts where speed_dial < 10 and speed_dial > 0"
  conn <- connectionMyDB
  result <- query_ conn q :: IO [Only String]
  return $ map fromOnly result

deleteContact :: String -> IO ()
deleteContact name = do
  let q = "delete from contacts where name = ?"
  conn <- connectionMyDB
  execute conn q [name]
  return ()



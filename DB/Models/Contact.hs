{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module DB.Models.Contact where
import Database.PostgreSQL.Simple
import DB.Connection
import GHC.Generics (Generic)
import DB.Models.Chip
import Data.Maybe (fromMaybe)

data Contact = Contact {
  idContact:: Int,
  name::  String,
  phone:: String,
  speed_dial:: Int
} deriving (Generic, FromRow,Show, Read, Eq)

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
  let q = "select id, name, phone, speed_dial from contacts where chip_id = ? and phone = ?"
  conn <- connectionMyDB 
  result <- query conn q (chip_id, phone) :: IO [Contact]
  if length result > 0 then return (Just (name (head result)))
  else return Nothing

getContacts :: IO [Contact]
getContacts = do
  conn <- connectionMyDB
  query conn "select id, name, phone, speed_dial from contacts where chip_id = ?"  (Only (idChip myChip))

getContactById :: Int -> IO [Contact]
getContactById id = do
  conn <- connectionMyDB
  query conn "select id, name, phone, speed_dial from contacts where id = ? and chip_id = ?" (Only (idChip myChip))

getSpeedDial :: IO [(String, Int)]
getSpeedDial = do
  let q = "select name, speed_dial from contacts where speed_dial < 10 and speed_dial > 0"
  conn <- connectionMyDB
  result <- query_ conn q :: IO [(Maybe String, Maybe Int)]
  return $ map (\(n, sd) -> (fromMaybe "" n, fromMaybe 0 sd)) result

contactToString :: [Contact] -> Int -> String
contactToString [] _ = [] 
contactToString (x:xs) n = show(n + 1) ++ " - " ++ (name x) ++ "\n" ++ contactToString xs (n+1)

deleteContact :: Int -> IO ()
deleteContact contact_id = do
  let q = "delete from contacts where id = ? and chip_id = ?"
  conn <- connectionMyDB
  execute conn q [contact_id, (idChip myChip)]
  return ()

updateContact :: Int -> String -> String -> String -> Int -> IO ()
updateContact contactId name phone birthday speedDial = do
  let q = "UPDATE contacts SET name = ?, phone = ?, birthday = ?, speed_dial = ? WHERE id = ?"
  conn <- connectionMyDB
  execute conn q (name, phone, birthday, speedDial, contactId)
  return ()

streams :: [Contact] -> [String]
streams [] = []
streams (x:xs) = phone x : streams xs

fromTuple :: (a, b) -> (Maybe a, Maybe b)
fromTuple (x, y) = (Just x, Just y)
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module DB.Models.Chip where
import Database.PostgreSQL.Simple
import DB.Connection
import GHC.Generics (Generic)

data Chip = Chip {
    idChip:: Int,
    owner:: String,
    number:: String,
    isOn :: Bool
} deriving (Generic, FromRow,Show, Read, Eq)

myChip :: Chip
myChip = Chip{idChip = 1, owner = "Joao Victor", number = "12345678910", isOn = True }

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

findByNumber:: String -> IO(Maybe Chip)
findByNumber number = do
    let q = "select * from chips where number = ?"
    conn <- connectionMyDB
    result <- query conn q (Only number):: IO[Chip]
    if length result == 0 then return Nothing
    else return (Just (head result))
    
getChips:: IO [Chip]
getChips = do
    conn <- connectionMyDB
    query_ conn "SELECT * FROM chips" :: IO [Chip]

getChipByNumber :: String -> IO [Chip]
getChipByNumber number = do
    conn <- connectionMyDB
    query conn "SELECT * FROM chips WHERE number = ?" (Only number)

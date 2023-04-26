{-# LANGUAGE OverloadedStrings #-}
module DB.Models.Contact where
import Database.PostgreSQL.Simple

createContacts :: Connection -> IO()
createContacts conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS contacts (\
                    \id SERIAL PRIMARY KEY,\
                    \name VARCHAR(255) NOT NULL,\
                    \phone VARCHAR(11) NOT NULL,\
                    \birthday date,\
                    \speed_dial int,\
                    \chip_id int,\
                    \FOREIGN KEY(chip_id) REFERENCES chips(id));"

    return ()

insertContact ::  Connection -> String -> String -> String -> Int -> Int -> IO ()
insertContact conn name phone birthday speed_dial chip_id = do
 let q = "insert into contacts (name, phone, birthday, speed_dial, chip_id ) values (?,?,?,?,?)"
 execute conn q (name, phone, birthday, speed_dial, chip_id)
 return ()
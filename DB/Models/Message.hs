{-# LANGUAGE OverloadedStrings #-}
module DB.Models.Message where
import Database.PostgreSQL.Simple

createMessages :: Connection -> IO()
createMessages conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS messages (\
                    \id SERIAL PRIMARY KEY,\
                    \message VARCHAR(255) NOT NULL,\
                    \message_date timestamp NOT NULL,\
                    \sented_by int,\
                    \received_by int,\
                    \FOREIGN KEY(sented_by) REFERENCES chips(id),\
                    \FOREIGN KEY(received_by) REFERENCES chips(id));"
    return ()

insertMessage ::  Connection -> String -> String -> Int -> Int -> IO ()
insertMessage conn message message_date sented_by received_by = do
 let q = "insert into messages (message, message_date, sented_by, received_by ) values (?,?,?,?)"
 execute conn q (message, message_date, sented_by, received_by)
 return ()
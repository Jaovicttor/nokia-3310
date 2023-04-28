{-# LANGUAGE OverloadedStrings #-}
module DB.Models.Call where
import Database.PostgreSQL.Simple
import DB.Connection

createCalls :: IO()
createCalls = do
    conn <- connectionMyDB
    execute_  conn "CREATE TABLE IF NOT EXISTS calls (\
                    \id SERIAL PRIMARY KEY,\
                    \started_at timestamp NOT NULL,\
                    \finish_at timestamp NOT NULL,\
                    \answered boolean NOT NULL,\
                    \sented_by int,\
                    \received_by int,\
                    \FOREIGN KEY(sented_by) REFERENCES chips(id),\
                    \FOREIGN KEY(received_by) REFERENCES chips(id));"
    return ()

insertCall :: String -> String -> Bool -> Int -> Int -> IO ()
insertCall  started_at finish_at answered sented_by received_by  = do
 let q = "insert into calls (started_at, finish_at, answered, sented_by, received_by ) values (?,?,?,?,?)"
 conn <- connectionMyDB
 execute conn q (started_at, finish_at, answered, sented_by, received_by )
 return ()
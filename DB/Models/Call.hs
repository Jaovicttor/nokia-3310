{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module DB.Models.Call where

import Database.PostgreSQL.Simple
import DB.Connection
import Data.Time.Clock.POSIX
import GHC.Generics (Generic)
import Data.Time.Clock

data Call = Call {
    cid::Int,
    started_at::UTCTime,
    finished_at::UTCTime,
    answered::Bool,
    sented_by::Int,
    received_by::Int,
    has_sender_deleted::Bool,
    has_receiver_deleted::Bool,
    sender_number:: Maybe String,
    receiver_number::Maybe String
} deriving (Generic, Show, FromRow, Read, Eq)

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

insertCall :: UTCTime -> UTCTime -> Bool -> Int -> Int -> Bool -> Bool -> IO ()
insertCall  started_at finish_at answered sented_by received_by has_sender_deleted has_receiver_deleted = do
 let q = "insert into calls (started_at, finish_at, answered, sented_by, received_by, has_sender_deleted, has_receiver_deleted ) values (?,?,?,?,?,?,?)"
 conn <- connectionMyDB
 execute conn q (started_at, finish_at, answered, sented_by, received_by, has_sender_deleted, has_receiver_deleted)
 return ()

save:: Call -> IO()
save call = do
    let q = "update calls set started_at = ?, finish_at = ?, answered = ?, sented_by = ?, received_by = ?, has_sender_deleted = ?, has_receiver_deleted = ? where id = ?"
    conn <- connectionMyDB
    execute conn q (started_at call, finished_at call, answered call, sented_by call, received_by call, has_sender_deleted call, has_receiver_deleted call, cid call)
    return ()

findAll::Int -> IO [Call]
findAll chip_id = do
    conn <- connectionMyDB
    query conn "select c.*, s.number, r.number from calls c join chips s on c.sented_by = s.id join chips r on c.received_by = r.id where (sented_by = ? and has_sender_deleted = false) or (received_by = ? and has_receiver_deleted = false) order by started_at desc" (chip_id, chip_id):: IO [Call]
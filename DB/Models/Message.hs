{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module DB.Models.Message where
import Database.PostgreSQL.Simple
import DB.Connection
import GHC.Generics (Generic)

data Message = Message {
    message:: String,
    sented_by:: Int,
    received_by:: Int
} deriving (Generic, FromRow,Show, Read, Eq)

data Conversation = Conversation {
    name:: Maybe String,
    number:: String
} deriving (Generic, FromRow,Show, Read, Eq)

createMessages :: IO()
createMessages = do
    conn <- connectionMyDB
    execute_ conn "CREATE TABLE IF NOT EXISTS messages (\
                    \id SERIAL PRIMARY KEY,\
                    \message VARCHAR(255) NOT NULL,\
                    \message_date timestamp NOT NULL,\
                    \sented_by int,\
                    \received_by int,\
                    \FOREIGN KEY(sented_by) REFERENCES chips(id),\
                    \FOREIGN KEY(received_by) REFERENCES chips(id));"
    return ()

insertMessage :: String -> String -> Int -> Int -> IO ()
insertMessage message message_date sented_by received_by = do
 let q = "insert into messages (message, message_date, sented_by, received_by ) values (?,?,?,?)"
 conn <- connectionMyDB
 execute  conn q (message, message_date, sented_by, received_by)
 return ()

getConversations:: IO String
getConversations = do
    conn <- connectionMyDB
    result <- query_ conn "select c2.name, c.number from messages m \ 
                \join chips c \
                \on (c.id = m.sented_by or c.id  = m.received_by) \
                \left join contacts c2 \
                \on c2.phone = c.number \
                \where (m.received_by = 1 or\
                \    m.sented_by = 1) and \
                \    (c2.chip_id = 1 or c2.chip_id is null)\
                \    and c.id != 1\
                \group by name,number" :: IO [Conversation]

    return (conversationsToString result)


conversationsToString:: [Conversation] -> String
conversationsToString [] = []
conversationsToString (x:xs) = (toStringConversation x) ++ "\n" ++ conversationsToString xs

toStringConversation :: Conversation -> String
toStringConversation conv =  maybe (number conv) id (name conv)
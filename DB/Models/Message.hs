{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module DB.Models.Message where
import Database.PostgreSQL.Simple
import DB.Connection
import DB.Models.Chip
import Data.Time.Clock
import GHC.Generics (Generic)

data Message = Message {
    message:: String,
    sented_by:: Int,
    received_by:: Int
} deriving (Generic, FromRow,Show, Read, Eq)

data Conversation = Conversation {
    chip_id :: Int,
    name:: Maybe String,
    phone:: String
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
                    \available_sented_by boolean DEFAULT True,\
                    \available_received_by  boolean DEFAULT True,\
                    \FOREIGN KEY(sented_by) REFERENCES chips(id),\
                    \FOREIGN KEY(received_by) REFERENCES chips(id));"
    return ()

insertMessage :: String -> Int -> IO ()
insertMessage message received_by = do
 let q = "insert into messages (message, message_date, sented_by, received_by ) values (?,?,?,?)"
 conn <- connectionMyDB
 currentTime <- getCurrentTime
 execute  conn q (message, currentTime, (idChip myChip), received_by)
 return ()

getConversations:: IO [Conversation]
getConversations = do
    let q = "select c.id, c2.name, c.number from messages m \ 
                \join chips c \
                \on (c.id = m.sented_by or c.id  = m.received_by) \
                \left join contacts c2 \
                \on c2.phone = c.number \
                \where ((m.received_by = ? and  available_received_by = true)  or\
                \    (m.sented_by = ? and available_sented_by = true) ) and \
                \    (c2.chip_id = ? or c2.chip_id is null)\
                \    and c.id != ?\
                \group by name,number,c.id" 
    conn <- connectionMyDB
    query conn q ((idChip myChip), (idChip myChip), (idChip myChip), (idChip myChip)) :: IO [Conversation]


conversationsToString:: [Conversation] -> Int -> String
conversationsToString [] _ = []
conversationsToString (x:xs) n = show(n + 1) ++ " - " ++ (toStringConversation x) ++ "\n" ++ conversationsToString xs (n+1)

toStringConversation :: Conversation -> String
toStringConversation conv =  maybe (phone conv) id (name conv)


findConversation :: Int ->  IO [Message]
findConversation receivedBy = do
    let q = "select m.message, m.sented_by, m.received_by from messages m where (m.sented_by = ? and m.received_by = ? and available_sented_by = true ) or (m.sented_by = ? and m.received_by = ? and available_received_by = true) order by m.message_date" 
    conn <- connectionMyDB
    query conn q ((idChip myChip), receivedBy, receivedBy, (idChip myChip)) :: IO [Message]

messageToString :: String -> [Message] -> String
messageToString _ [] = []
messageToString name (x:xs) | (sented_by x) == (idChip myChip) = ("Você: "++ (message x) ++ "\n"  ++ (messageToString name xs))
                            | otherwise =  (name ++ ": "++ (message x) ++ "\n"  ++ (messageToString name xs))

deleteConversation :: Int -> IO()
deleteConversation chipId = do
    deleteSentedMessage chipId
    deleteReceivedMessage chipId
    
deleteSentedMessage :: Int -> IO()
deleteSentedMessage received_by = do
    let q = "update messages set available_sented_by = false where sented_by = ? and received_by = ? and available_sented_by = true;" 
    conn <- connectionMyDB
    execute conn q ((idChip myChip), received_by)
    return ()

deleteReceivedMessage :: Int -> IO()
deleteReceivedMessage sented_by = do
    let q = "update messages set available_received_by = false where sented_by = ? and received_by = ? and available_received_by = true;" 
    conn <- connectionMyDB
    execute conn q (sented_by, (idChip myChip))
    return ()


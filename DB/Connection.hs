{-# LANGUAGE OverloadedStrings #-}
module DB.Connection where
import Database.PostgreSQL.Simple
import DB.Models.Chip
import DB.Models.Call
import DB.Models.Contact
import DB.Models.Event
import DB.Models.Message


localDB:: ConnectInfo
localDB = defaultConnectInfo {
    connectHost = "localhost",
    connectDatabase = "nokia-3310",
    connectUser = "postgres",
    connectPassword = "postgres",
    connectPort = 5432
}

connectionMyDB :: IO Connection
connectionMyDB = connect localDB


startDatabase:: IO Connection
startDatabase = do
 c <- connectionMyDB
 createChips c
 createCalls c
 createMessages c
 createEvents c
 createContacts c
 return c
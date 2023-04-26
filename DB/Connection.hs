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

connectCloud :: IO Connection
connectCloud = connectPostgreSQL "host=ep-autumn-smoke-494458.us-east-2.aws.neon.tech port=5432 user=rodrigo.rodrigues password=RrAOk4gU9QED dbname=nokia-3310 sslmode=require options=project=ep-autumn-smoke-494458"

connectionMyDB :: IO Connection
connectionMyDB = connect localDB


startDatabase:: IO Connection
startDatabase = do
 c <- connectionMyDB
--  c <- connectCloud  conectar com o bd em nuvem
 createChips c
 createCalls c
 createMessages c
 createEvents c
 createContacts c
 return c
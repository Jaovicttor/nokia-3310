{-# LANGUAGE OverloadedStrings #-}
module DB.Connection where
import Database.PostgreSQL.Simple

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
<<<<<<< HEAD
connectionMyDB = connectCloud
=======
connectionMyDB = connect localDB
>>>>>>> ab31347673602a826e04493910d476b7ca7f4a12

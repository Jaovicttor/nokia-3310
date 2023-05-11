{-# LANGUAGE OverloadedStrings #-}
module DB.Connection where
import Database.PostgreSQL.Simple

localDB:: ConnectInfo
localDB = defaultConnectInfo {
    connectHost = "localhost",
    connectDatabase = "nokia-3310",
    connectUser = "postgres",
    connectPassword = "thalles77",
    connectPort = 5432
}

connectCloud :: IO Connection
connectCloud = connectPostgreSQL "host=ep-autumn-smoke-494458.us-east-2.aws.neon.tech port=5432 user=rodrigo.rodrigues password=RrAOk4gU9QED dbname=nokia-3310 sslmode=require options=project=ep-autumn-smoke-494458"

connectionMyDB :: IO Connection
connectionMyDB = connectCloud

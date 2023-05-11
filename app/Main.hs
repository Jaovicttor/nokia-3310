{-# LANGUAGE OverloadedStrings #-} 
module Main where
import Database.PostgreSQL.Simple
import DB.Connection
import App.Modules.Alarm.Index
import qualified App.Modules.Alarm.MainAlarm as Alarm
import qualified App.Modules.Alarm.MainAlarm as Cron
import qualified App.Modules.Message.Index as Message
import Control.Concurrent 


main :: IO()  
main = do
    Message.mainMessage  

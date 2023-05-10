{-# LANGUAGE OverloadedStrings #-} 
module Main where
import Database.PostgreSQL.Simple
import DB.Connection
import App.Modules.Calendar.Index
import qualified App.Modules.Message.Index as Message
import qualified App.Modules.Calendar.Index as Calendar



main :: IO()
main = do 
 Message.mainMessage           
  

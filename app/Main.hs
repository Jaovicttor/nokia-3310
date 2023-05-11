{-# LANGUAGE OverloadedStrings #-}
module Main where
import Database.PostgreSQL.Simple
import DB.Connection
import DB.Models.Chip
import DB.Models.Contact
import DB.Models.Call
import DB.Models.Event
import DB.Models.Message
import DB.Models.Alarm

import App.Modules.Call.Index (menu)

main :: IO()
main = do
    menu
{-# LANGUAGE OverloadedStrings #-} 
module Main where
import Database.PostgreSQL.Simple
import qualified App.Modules.Message.Index as Message

main :: IO()
main = do 
 Message.mainMessage           

{-# LANGUAGE OverloadedStrings #-} 
module Main where
import Database.PostgreSQL.Simple
import qualified App.Modules.Contacts.Index as Contact

main :: IO()
main = do 
 Contact.main     
      
                
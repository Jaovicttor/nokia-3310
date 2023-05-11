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
import App.Modules.Alarm.Index
import App.Modules.Alarm.MainAlarm
import Control.Concurrent 
main :: IO() 
main = do
    _ <- forkIO alarm        
    mainAlarm  
   -- putStrLn "Criando base de dados..."
   --startDatabase  
--putStrLn "Base de dados criada"
  
--  insertChip "Joao Victor" "79988686084" False
--  insertChip "Rodrigo Correia" "83954478512" False
--  insertContact "Rodrigo Monstrinho" "83954478512" "2023-01-01" (-1) 1
--  insertMessage "Eae corno" "2023-04-26 16:09:12" 1 2
--  insertCall "2023-04-26 16:09:12" "2023-04-26 16:12:12" True 2 1
--  insertEvent "Aniversario do monstrinho" "2023-04-26" "" 1
--  insertAlarm "15:50" "Teste alarms"
  
startDatabase:: IO()
startDatabase = do  
 createChips 
 createCalls  
 createMessages 
 createEvents 
 createContacts  
  
 createAlarms
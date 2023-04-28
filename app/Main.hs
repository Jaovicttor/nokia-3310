{-# LANGUAGE OverloadedStrings #-}
module Main where
import Database.PostgreSQL.Simple
import DB.Connection
import DB.Models.Chip
import DB.Models.Contact
import DB.Models.Call
import DB.Models.Event
import DB.Models.Message

main :: IO()
main = do
 putStrLn "Criando base de dados..."
 startDatabase 
 putStrLn "Base de dados criada"
 insertChip "Joao Victor" "79988686084" False
 insertChip "Rodrigo Correia" "83954478512" False
 insertContact "Rodrigo Monstrinho" "83954478512" "2023-01-01" (-1) 1
 insertMessage "Eae corno" "2023-04-26 16:09:12" 1 2
 insertCall "2023-04-26 16:09:12" "2023-04-26 16:12:12" True 2 1
 insertEvent "Aniversario do monstrinho" "2023-04-26" "" 1

startDatabase:: IO()
startDatabase = do
 createChips 
 createCalls 
 createMessages 
 createEvents 
 createContacts 
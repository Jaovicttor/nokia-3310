{-# LANGUAGE OverloadedStrings #-}
module Main where
import Database.PostgreSQL.Simple
import DB.Connection
import App.Modules.Calendar.Index

main :: IO()
main = do
 putStrLn "Criando base de dados..."
 putStrLn "Base de dados criada"
 menuCalendar
 
--  insertChip "Joao Victor" "79988686084" False
--  insertChip "Rodrigo Correia" "83954478512" False
--  insertContact "Rodrigo Monstrinho" "83954478512" "2023-01-01" (-1) 1
--  insertMessage "Eae corno" "2023-04-26 16:09:12" 1 2
--  insertCall "2023-04-26 16:09:12" "2023-04-26 16:12:12" True 2 1
--  insertEvent "Aniversario do monstrinho" "2023-04-26" "" 1
--  insertAlarm "15:50" "Teste alarms"

-- encapsular main no DB 




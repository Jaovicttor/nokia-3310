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
 conn <- startDatabase 
 putStrLn "Base de dados criada"
 insertChip conn "Joao Victor" "79988686084" False
 insertChip conn "Rodrigo Correia" "83954478512" False
 insertContact conn "Rodrigo Monstrinho" "83954478512" "2023-01-01" (-1) 1
 insertMessage conn "Eae corno" "2023-04-26 16:09:12" 1 2
 insertCall conn "2023-04-26 16:09:12" "2023-04-26 16:12:12" True 2 1
 insertEvent conn "Aniversario do monstrinho" "2023-04-26" "" 1
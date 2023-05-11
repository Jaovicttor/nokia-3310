{-# LANGUAGE OverloadedStrings #-} 
module Main where
import qualified App.Modules.Alarm.MainAlarm as Cron
import qualified App.Modules.Alarm.Index as Alarm
import qualified App.Modules.Calendar.Index as Calendar
import qualified App.Modules.Contacts.Index as Contacts
import qualified App.Modules.Message.Index as Message
import qualified App.Shared.Main.Helper.Display as Display
 
import Control.Concurrent   

main :: IO ()
main = do  
    Display.printeHeader "--------"
    putStrLn "1 - Contatos"   
    putStrLn "2 - Ligações"
    putStrLn "3 - Mensagens" 
    putStrLn "4 - Calendário"
    putStrLn "5 - Alarmes"
    putStrLn "0 - Sair"
    Display.printeBottom
    choice <- getLine 
      
    if choice == "0" then return()
    else do
        case choice of
            "1" -> Contacts.main  
            "2" -> print "To Do" 
            "3" -> Message.mainMessage
            "4" -> Calendar.menuCalendar
            "5" -> Alarm.mainAlarm
            _   -> putStrLn "Opção inválida! Tente novamente."
        main   

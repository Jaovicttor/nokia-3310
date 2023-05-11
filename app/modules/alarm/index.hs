module App.Modules.Alarm.Index where
import Data.Time.Format
import Data.Time.LocalTime
import Data.Bool (bool)
import App.Shared.Main.Helper.Timer
import DB.Models.Alarm
import DB.Models.Chip
import App.Modules.Alarm.MainAlarm
import qualified App.Shared.Main.Helper.Display as Display

mainAlarm::IO()
mainAlarm = do
    Display.printeHeader "Alarmes--"
    putStrLn "1 - Adicionar alarme"
    putStrLn "2 - Listar alarmes"
    putStrLn "0 - Sair"
    Display.printeBottom
    choice <- getLine
    case choice of
        "0" -> return ()
        "1" -> addAlarm >>mainAlarm
        "2" -> listAlarms >>mainAlarm
        "3" -> deleteAlarm >>mainAlarm
        "-"-> return ()
        _ -> do
            putStrLn "Opção inválida!"
            mainAlarm        
    

addAlarm :: IO ()
addAlarm = do
    Display.printeHeader "Alarmes--"
    putStrLn "Digite a hora do alarme (no formato hh:mm):"
    time <- getLine
    case parseTimeM True defaultTimeLocale "%H:%M" time :: Maybe TimeOfDay of
        Just t -> do
            var <- checkAlarm t
            if var == True
                then do 
                    putStrLn "Digite o título do alarme:"
                    title <- getLine
                    if title == "-" 
                        then do 
                            mainAlarm
                            putStrLn ""
                    else do  
                        insertAlarm t title (idChip myChip)
                        putStrLn "Alarme adicionado com sucesso!"
                        Display.printeBottom
                        mainAlarm
            else do
                putStrLn "Alarme já existe"
                Display.printeBottom
                addAlarm
        Nothing -> do
            if time == "-"
                then do
                    putStrLn ""
                    mainAlarm
            else 
                do
                putStrLn "Formato de hora inválido. Por favor, use o formato hh:mm."
                Display.printeBottom
                addAlarm
    

listAlarms:: IO ()
listAlarms = do
    Display.printeHeader "Alarmes--"
    resultados <- getAlarms 
    putStrLn "\nAlarmes:"
    putStrLn "Hora\tAtivo\tTitulo"
    mapM_ (\(Alarm id time title active chip_id) -> putStrLn $ formatTime defaultTimeLocale "%H:%M" time ++ "\t" ++ bool "" "X" active ++"\t"++title ++ "\t") resultados
    putStrLn "\n1 - Excluir alarme"
    putStrLn "2 - Editar alarme"
    putStrLn "3 - Ativar/Desativar alarme"
    putStrLn "0 - Sair"
    Display.printeBottom
    choice <- getLine
    case choice of
        "0" -> do
            mainAlarm
        "1" -> deleteAlarm
        "2" -> updateAlarm 
        "3" -> activeAlarm
        "-" -> do
            putStrLn " "
            mainAlarm
        _ -> do
            putStrLn "Opção inválida!"
            listAlarms 

        

deleteAlarm::IO()
deleteAlarm = do
    Display.printeHeader "Alarmes--"
    putStrLn "Digite a hora do alarme que deseja deletar (no formato hh:mm):"
    time <- getLine
    case parseTimeM True defaultTimeLocale "%H:%M" time :: Maybe TimeOfDay of
        Just t -> do
            var <- checkAlarm t
            if var == False
                then do 
                    deleteAlarms t
                    putStrLn "Alarme apagado com sucesso!"
                    Display.printeBottom
                    listAlarms
            else do
                putStrLn "Alarme inexistente, tente novamente"
                Display.printeBottom
                deleteAlarm
        Nothing -> do
            if time == "-"
                then do
                    putStrLn ""
                    listAlarms
            else 
                do
                putStrLn "Formato de hora inválido. Por favor, use o formato hh:mm."
                Display.printeBottom
                deleteAlarm
    
activeAlarm::IO()
activeAlarm = do
    Display.printeHeader "Alarmes--"
    putStrLn "Digite a hora do alarme que deseja Ativar/Desativar (no formato hh:mm):"
    time <- getLine
    case parseTimeM True defaultTimeLocale "%H:%M" time :: Maybe TimeOfDay of
        Just t -> do
            var <- checkAlarm t
            if var == False
                then do 
                    activeAlarms t
                    putStrLn "Alarme alterado com sucesso!"
                    Display.printeBottom
                    listAlarms
            else do
                putStrLn "Alarme inexistente, tente novamente"
                Display.printeBottom
                activeAlarm
        Nothing -> do
            if time == "-"
                then do
                    putStrLn ""
                    mainAlarm
            else 
                do
                putStrLn "Formato de hora inválido. Por favor, use o formato hh:mm."
                Display.printeBottom
                activeAlarm
    
updateAlarm::IO()
updateAlarm = do
    Display.printeHeader "Alarmes--"
    putStrLn "Digite a hora do alarme que deseja alterar (no formato hh:mm):"
    time <- getLine
    case parseTimeM True defaultTimeLocale "%H:%M" time :: Maybe TimeOfDay of
        Just t -> do
            var <- checkAlarm t
            if var == False then do
                titleAtual <- getTitleAlarms t
                putStrLn $ concat [
                    "Atual:",
                    time,
                    "\n",
                    "Digite o novo horário do alarme (no formato hh:mm):   "]
                newTime <- getLine
                case parseTimeM True defaultTimeLocale "%H:%M" newTime :: Maybe TimeOfDay of
                    Just nt -> do
                        var <- checkAlarm nt
                        if var == True then do 
                            putStrLn $ "Atual: " ++ titleAtual
                            putStrLn "Digite o novo título do alarme:"
                            title <- getLine
                            if title == "-" 
                                then do 
                                    listAlarms
                                    putStrLn ""
                            else do  
                                updateAlarms t nt title
                                putStrLn "Alarme alterado com sucesso!"
                                Display.printeBottom
                                listAlarms
                        else do
                            putStrLn "Alarme já existe, tente outro horário"
                            Display.printeBottom
                            updateAlarm
                    Nothing -> do
                        if time == "-" then do
                            putStrLn ""
                            listAlarms
                        else  do
                            putStrLn "Formato de hora inválido. Por favor, use o formato hh:mm."
                            Display.printeBottom
                            updateAlarm
                        else do
                putStrLn "Alarme inexistente, tente novamente"
                Display.printeBottom
                updateAlarm
        Nothing -> do
            if time == "-"
                then do
                    putStrLn ""
                    listAlarms
            else 
                do
                putStrLn "Formato de hora inválido. Por favor, use o formato hh:mm."
                Display.printeBottom
                updateAlarm
   
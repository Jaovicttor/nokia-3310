module App.Modules.Alarm.Index where
import Data.Time.Format
import Data.Time.LocalTime
import Data.Bool (bool)
import App.Shared.Main.Helper.Timer
import DB.Models.Alarm
import DB.Models.Chip
import App.Modules.Alarm.MainAlarm

mainAlarm::IO()
mainAlarm = do
    time <- getCurrentTimeString
    putStrLn $ concat [
        "\n-----------------------------------------\n",
        "------------------",
        time,
        "------------------",
        "\n-----------------------------------------\n",
        "1 - Adicionar alarme\n",
        "2 - Listar alarmes\n",
        "0 - Sair\n",
        "-----------------------------------------\n",
        "Escolha sua ação:"]
    choice <- getLine
    if choice == "0" then return()
    else do
        case choice of
            "1" -> addAlarm
            "2" -> listAlarms
            "3" -> deleteAlarm
            _ -> do
                putStrLn "Opção inválida!"
        mainAlarm

addAlarm :: IO ()
addAlarm = do
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
                        mainAlarm
            else do
                putStrLn "Alarme já existe"
                addAlarm
        Nothing -> do
            if time == "-"
                then do
                    putStrLn ""
                    mainAlarm
            else 
                do
                putStrLn "Formato de hora inválido. Por favor, use o formato hh:mm."
                addAlarm

listAlarms:: IO ()
listAlarms = do
    resultados <- getAlarms 
    putStrLn "\nAlarmes:"
    putStrLn "Hora\tAtivo\tTitulo"
    mapM_ (\(Alarm id time title active chip_id) -> putStrLn $ formatTime defaultTimeLocale "%H:%M" time ++ "\t" ++ bool "" "X" active ++"\t"++title ++ "\t") resultados
    putStrLn $ concat [
        "\n-----------------------------------------\n",
        "1 - Excluir alarme\n",
        "2 - Editar alarme\n",
        "3 - Ativar/Desativar alarme\n",
        "0 - Sair\n",
        "-----------------------------------------\n",
        "Escolha sua ação:"]
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
    putStrLn "Digite a hora do alarme que deseja deletar (no formato hh:mm):"
    time <- getLine
    case parseTimeM True defaultTimeLocale "%H:%M" time :: Maybe TimeOfDay of
        Just t -> do
            var <- checkAlarm t
            if var == False
                then do 
                    deleteAlarms t
                    putStrLn "Alarme apagado com sucesso!"
                    listAlarms
            else do
                putStrLn "Alarme inexistente, tente novamente"
                deleteAlarm
        Nothing -> do
            if time == "-"
                then do
                    putStrLn ""
                    listAlarms
            else 
                do
                putStrLn "Formato de hora inválido. Por favor, use o formato hh:mm."
                deleteAlarm
activeAlarm::IO()
activeAlarm = do
    putStrLn "Digite a hora do alarme que deseja Ativar/Desativar (no formato hh:mm):"
    time <- getLine
    case parseTimeM True defaultTimeLocale "%H:%M" time :: Maybe TimeOfDay of
        Just t -> do
            var <- checkAlarm t
            if var == False
                then do 
                    activeAlarms t
                    putStrLn "Alarme alterado com sucesso!"
                    listAlarms
            else do
                putStrLn "Alarme inexistente, tente novamente"
                activeAlarm
        Nothing -> do
            if time == "-"
                then do
                    putStrLn ""
                    mainAlarm
            else 
                do
                putStrLn "Formato de hora inválido. Por favor, use o formato hh:mm."
                activeAlarm

updateAlarm::IO()
updateAlarm = do
    putStrLn "Digite a hora do alarme que deseja alterar (no formato hh:mm):"
    time <- getLine
    case parseTimeM True defaultTimeLocale "%H:%M" time :: Maybe TimeOfDay of
        Just t -> do
            var <- checkAlarm t
            titleAtual <- getTitleAlarms t
            if var == False then do
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
                                listAlarms
                        else do
                            putStrLn "Alarme já existe, tente outro horário"
                            updateAlarm
                    Nothing -> do
                        if time == "-" then do
                            putStrLn ""
                            listAlarms
                        else  do
                            putStrLn "Formato de hora inválido. Por favor, use o formato hh:mm."
                            updateAlarm
                        else do
                putStrLn "Alarme inexistente, tente novamente"
                updateAlarm
        Nothing -> do
            if time == "-"
                then do
                    putStrLn ""
                    listAlarms
            else 
                do
                putStrLn "Formato de hora inválido. Por favor, use o formato hh:mm."
                updateAlarm

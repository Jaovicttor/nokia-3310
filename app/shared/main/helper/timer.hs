module App.Shared.Main.Helper.Timer where
import Data.Time
import Data.Time.LocalTime

-- retorna o horário atual em formato de String
getCurrentTimeString :: IO String            
getCurrentTimeString = do
    currentTime <- getCurrentTime         -- recebe o horário atual
    timeZone <- getTimeZone currentTime   -- recebe o fuso horário 
    let localTime = utcToLocalTime timeZone currentTime   -- converte a hora atual para o horário local
        timeFormatted = formatTime defaultTimeLocale "%H:%M" localTime -- formata para o padrão HH:MM
    return $ timeFormatted

-- recebe uma String "HH:MM" e verifica se é o horario atual, retornando um boolean como resposta
isCurrentTime :: String -> IO Bool  
isCurrentTime timeString = do
    current <- getCurrentTimeString
    return (current == timeString)


module App.Modules.Alarm.MainAlarm where
import Data.Time.Format
import Data.Time.LocalTime
import App.Shared.Main.Helper.Timer
import DB.Models.Alarm
import DB.Models.Chip
import Data.Time
import Control.Concurrent
import Control.Monad

alarm :: IO ()
alarm = forever $ do 
        verificationAlarm
        threadDelay 60000000

verificationAlarm :: IO ()
verificationAlarm = do
        timeString <- getCurrentTimeString
        let parsedTime = parseTimeM True defaultTimeLocale "%H:%M" timeString :: Maybe TimeOfDay
        alarmes <- case parsedTime of
              Just time -> do
                verificationAlarms time
              Nothing -> return []
        mapM_(\(Alarm id time title active chip_id) -> putStrLn $ "\n" ++ "Alarme disparado: " ++ show time ++ "\t" ++ title) alarmes
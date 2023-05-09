{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module DB.Models.Alarm where
import Database.PostgreSQL.Simple
import DB.Connection
import GHC.Generics (Generic)
import DB.Models.Chip
import Data.Time.LocalTime
import Data.Time.Format

data Alarm = Alarm { id :: Int, time :: TimeOfDay, title :: String, active :: Bool,chip_id::Int }
  deriving (Show, Generic,FromRow)

createAlarms :: IO()
createAlarms = do
    conn <- connectionMyDB
    execute_ conn "CREATE TABLE IF NOT EXISTS alarms (\
                   \id SERIAL PRIMARY KEY,\
                    \deadline time NOT NULL,\
                    \title VARCHAR(255) NOT NULL,\
                    \active boolean NOT NULL,\
                    \chip_id int NOT NULL,\
                    \FOREIGN KEY(chip_id) REFERENCES chips(id));"
    return ()

insertAlarm ::  TimeOfDay -> String -> Int -> IO ()
insertAlarm time title chip_id = do
 let q = "insert into alarms (deadline,title,active,chip_id) values (?,?,?,?)"
 conn <- connectionMyDB
 execute conn q (time, title, True,chip_id)
 return ()
     
getAlarms::IO[Alarm]
getAlarms = do
      conn <- connectionMyDB
      let chip_id = (idChip chipAtual)
      query conn "SELECT * FROM alarms WHERE chip_id = ? order by deadline " (Only chip_id)

checkAlarm :: TimeOfDay -> IO Bool 
checkAlarm time = do
  conn <- connectionMyDB
  let chip_id = idChip chipAtual
  result <- query conn "SELECT * FROM alarms WHERE chip_id = ? AND deadline = ?" (chip_id, time) :: IO [Alarm]
  if null result
    then return True
    else return False 

deleteAlarms::TimeOfDay-> IO()
deleteAlarms timer = do
    conn <- connectionMyDB
    let chip_id = (idChip chipAtual)
    let q =  "DELETE FROM alarms WHERE chip_id = ? AND deadline = ?"
    execute conn q (chip_id,timer)
    return ()

updateAlarms:: TimeOfDay -> TimeOfDay -> String -> IO()
updateAlarms oldTimer currentTimer title = do 
    conn <- connectionMyDB
    let chip_id = (idChip chipAtual)
    let q = "UPDATE alarms SET deadline = ?,title = ? WHERE chip_id = ? AND deadline = ?"
    execute conn q (currentTimer,title,chip_id,oldTimer)
    return ()

updateTimerAlarms:: TimeOfDay -> TimeOfDay -> IO()
updateTimerAlarms oldTimer currentTimer  = do 
    conn <- connectionMyDB
    let chip_id = (idChip chipAtual)
    let q = "UPDATE alarms SET deadline = ? WHERE chip_id = ? AND deadline = ?"
    execute conn q (currentTimer,chip_id,oldTimer)
    return ()
    
updateTitleAlarms::TimeOfDay -> String -> IO()
updateTitleAlarms timer title = do 
    conn <- connectionMyDB
    let chip_id = (idChip chipAtual)
    let q = "UPDATE alarms SET title = ? WHERE chip_id = ? AND deadline = ?"
    execute conn q (title,chip_id,timer)
    return ()

activeAlarms::TimeOfDay-> IO()
activeAlarms timer = do
    conn <- connectionMyDB
    let chip_id = (idChip chipAtual)
    let q =  "SELECT active from alarms WHERE chip_id =? AND deadline = ?" 
    result <- query conn q (chip_id, timer)
    let active = if head result == [True] then "UPDATE alarms SET active = False WHERE chip_id = ? AND deadline = ?"
                 else "UPDATE alarms SET active = True WHERE chip_id = ? AND deadline = ?"
    execute conn active (chip_id, timer)
    return ()



verificationAlarms :: TimeOfDay -> IO[Alarm]
verificationAlarms timer = do
    conn <- connectionMyDB
    let chip_id = idChip chipAtual
    result <- query conn "SELECT * from alarms WHERE chip_id = ? AND deadline = ? AND active = True" (chip_id, timer)
    close conn
    return result
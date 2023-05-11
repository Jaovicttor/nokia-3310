module App.Modules.Call.ExecuteCall where

import Data.List (elemIndex)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Text.Printf (printf)
import System.Random
import qualified DB.Models.Contact as Contact
import DB.Models.Chip
import DB.Models.Call
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import System.Console.ANSI
import System.IO
import System.Timeout

randomTimestamp:: Int -> IO UTCTime
randomTimestamp duration = do
    now <- getCurrentTime
    let futureTime = addUTCTime (fromIntegral duration) now
    return futureTime

randomCallData:: Int -> IO(Bool, UTCTime, UTCTime)
randomCallData duration = do
    randomValue <- randomIO:: IO Int
    currentTime <- getCurrentTime
    future_time <- randomTimestamp duration
    if even randomValue then return (True, currentTime, future_time)
    else return (False, currentTime, currentTime)

printCallTimeCounter:: Int -> IO()
printCallTimeCounter seconds = do
    let delay = 1000000
    let formatTime s = let (m, s') = s `divMod` 60 in printf "%02d:%02d" m s'
    forM_ [0..seconds] $ \s -> do
        let time = formatTime s
        setCursorColumn 0
        putStr time
        hFlush stdout
        clearLine
        threadDelay delay

execute:: Chip -> String -> IO()
execute my_chip receiver_number = do
    receiver_chip <- findByNumber receiver_number
    case receiver_chip of
        Just c -> do
            if (isOn c) then do
                print "[+] Chamando..."
                randSecs <- randomRIO (0, 10) :: IO Int
                (answered, startTime, endTime) <- randomCallData randSecs
                let chip = myChip
                if answered then do
                    print "[+] Em andamento..."
                    printCallTimeCounter randSecs
                    putStrLn "[!] Ligacao encerrada"
                else print "[!] Ligacao recusada"
                insertCall startTime endTime answered (idChip my_chip) (idChip c) False False
            else print "[!] Numero não pode receber chamadas nesse momento"
        Nothing -> print "[!] Chip nao encontrado para numero informado"

doCall::String -> IO()
doCall "" = print "[!] Numero Invalido"
doCall number
    | length number == 1 = do
        let nInt = read number :: Int
        if nInt >= 0 && nInt <= 9 then do
            let chip = myChip
            contact <- Contact.findByPhone (idChip myChip) number
            case contact of
                Just c -> do
                    print (c ++ "...")
                    execute chip number
                Nothing -> do
                    print "[!] Speed dial nao encontrado"
        else print "[!] Numero Invalido" 
    | (length number == 3 || length number == 9 || length number == 11) = do
        let chip = myChip
        contact <- Contact.findByPhone (idChip myChip) number
        case contact of
            Just c -> do
                print (c ++ "...")
                execute chip number
            Nothing -> do
                print (number ++ "...")
                execute chip number
    | otherwise = print "[!] Numero Invalido"
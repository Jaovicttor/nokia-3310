module App.Modules.Call.Index where

import Data.List (elemIndex)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Text.Printf (printf)
import App.Modules.Call.ExecuteCall
import App.Modules.Call.ListCalls
import App.Modules.Call.DeleteCall
import DB.Models.Call

proxyDeleteCall:: [Call] -> IO()
proxyDeleteCall calls = do
    putStrLn "-> Qual chamada deseja apagar do histórico ?"
    id <- readLn::IO Int
    let (valid_calls, call_to_save) = deleteCall id calls
    save call_to_save
    printCalls valid_calls
    menu

proxyOptions::Int -> IO()
proxyOptions 0 = putStrLn "[!] Saindo..."
proxyOptions 1 = do
    putStrLn "-> Digite o número que deseja ligar:"
    number <- getLine
    doCall number
    menu
proxyOptions 2 = do
    putStrLn "-> Seu Histórico"
    calls <- listCalls
    printCalls calls
    putStrLn "[x] - apagar chamada"
    putStrLn "[m] - menu"
    option <- getLine
    case option of
        "x" -> proxyDeleteCall calls
        "m" -> menu
        _ -> do
            print "[!] Operacao Invalida"
            menu
proxyOptions _ = print "[!] Operacao Invalida"

menu::IO()
menu = do
    putStrLn "[1] - Fazer Ligação"
    putStrLn "[2] - Listar Histórico"
    putStrLn "[0] - Sair"
    option <- readLn::IO Int
    proxyOptions option
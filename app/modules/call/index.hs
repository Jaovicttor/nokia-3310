module App.Modules.Call.Index where

import Data.List (elemIndex)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Text.Printf (printf)
import App.Modules.Call.ExecuteCall
import App.Modules.Call.ListCalls
import App.Modules.Call.DeleteCall
import DB.Models.Call

printeHeader :: IO()
printeHeader = do
  putStrLn "\n----------------------------"
  putStrLn "---------NOKIA-3310---------"
  putStrLn "----------------------------"
  putStrLn "----------chamadas----------"
  putStrLn "----------------------------"
  putStrLn "     Selecione uma opção    "
  putStrLn "----------------------------\n"

printeBottom :: IO()
printeBottom = do
  putStrLn "\n----------------------------\n"
  putStrLn  "    .---.  .---.  .---."
  putStrLn  "    / 1 /  / 2 /  / 3 /"
  putStrLn  "    `---'  `---'  `---'"
  putStrLn  "    .---.  .---.  .---."
  putStrLn  "    / 4 /  / 5 /  / 6 /"
  putStrLn  "    `---'  `---'  `---'"
  putStrLn  "    .---.  .---.  .---."
  putStrLn  "    / 7 /  / 8 /  / 9 /"
  putStrLn  "    `---'  `---'  `---'"
  putStrLn "----------------------------\n"

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
    putStrLn "-> Seu Historico"
    calls <- listCalls
    printCalls calls
    putStrLn "[x] - apagar chamada"
    putStrLn "[0] - menu"
    option <- getLine
    case option of
        "x" -> proxyDeleteCall calls
        "0" -> menu
        _ -> do
            print "[!] Operacao Invalida"
            menu
proxyOptions _ = print "[!] Operacao Invalida"

menu::IO()
menu = do
    printeHeader
    putStrLn "[1] - Fazer Ligacao"
    putStrLn "[2] - Listar Historico"
    putStrLn "[0] - Sair"
    printeBottom
    option <- readLn::IO Int
    proxyOptions option
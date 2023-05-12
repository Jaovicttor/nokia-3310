module App.Modules.Call.ListCalls where

import DB.Models.Call
import DB.Models.Chip
import DB.Models.Contact

formatCallPrint::Int -> Int -> Call -> IO String
formatCallPrint index chip_id call_data = do
    let is_receiver = if (sented_by call_data) == (idChip myChip) then False else True
    if is_receiver then do
        contact <- findByPhone chip_id (maybe "" id (sender_number call_data))
        case contact of
            Just c -> pure (show index ++ " - " ++ c)
            Nothing -> pure (show index ++ " - " ++ maybe "" id (sender_number call_data))
    else do
        contact <- findByPhone chip_id (maybe "" id (receiver_number call_data))
        case contact of
            Just c -> pure (show index ++ " - " ++ c)
            Nothing -> pure (show index ++ " - " ++ maybe "" id (receiver_number call_data))

printCalls:: [Call] -> IO()
printCalls [] = putStrLn "[+] Sem chamadas no histórico"
printCalls (x:xs) = do
    let indexed = zip [0..] (x:xs)
    let chip = myChip
    mapped <- mapM (\(i, c) -> formatCallPrint i (idChip myChip) c) indexed
    mapM_ putStrLn mapped

listCalls:: IO [Call]
listCalls = do
    let chip = myChip
    findAll (idChip myChip)
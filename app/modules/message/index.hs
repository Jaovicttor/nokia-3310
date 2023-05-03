module App.Modules.Message.Index  where
import DB.Models.Message
import DB.Models.Chip

mainMessage :: IO()
mainMessage = do
 showConversations
 putStrLn "x - Enviar mensagem"
 putStrLn "0 - Sair"
 op <- getLine

 if(op == "0") then 
    putStrLn "saindo..."
 else do
    if(op == "x") then sendMessage
    else putStrLn("Erro")
    mainMessage
    

sendMessage :: IO()
sendMessage = do
 putStrLn "-------------------------------------------"
 putStrLn "Número:"
 number <- getLine
 if(number == "-") then putStrLn""
 else do
    putStrLn "Mensagem:"
    message <- getLine
    if(message == "-") then putStrLn""
    else do
        putStrLn "-------------------------------------------"
        if(message == "") then putStrLn "A mensagem não pode ser vazia."
        else do
            chip <- getChipByNumber number
            if length chip == 0
                then putStrLn  "Número de telefone inválido"
            else do
                let c = head chip
                let received_by = (idChip c)
                let sented_by = (idChip chipAtual) 
                if(received_by == sented_by ) then putStrLn "Não é possivel enviar uma mensagem para o próprio número"
                else do
                    insertMessage message "2023-05-02 14:28:12" sented_by received_by
                    putStrLn "Mensagem enviada com sucesso"

showConversations :: IO()
showConversations = do
    putStrLn "-------------------------------------------"
    putStrLn "------------------Mensagens----------------"
    putStrLn "-------------------------------------------"
    conversations <- getConversations
    putStrLn conversations
    putStrLn "-------------------------------------------"
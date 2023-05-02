module App.Modules.Message.Index  where
import DB.Models.Message
import DB.Models.Chip

mainMessage :: IO()
mainMessage = do
 print "x - Enviar mensagem"
 op <- getLine
 if(op == "x") 
    then do
     sendMessage
 else print("Erro")
 mainMessage


sendMessage :: IO()
sendMessage = do
 print "-------------------------------------------"
 print "Numero:"
 number <- getLine
 print "Mensagem:"
 message <- getLine
 print "-------------------------------------------"
 if(message == "") then print "A mensagem nao pode ser vazia."
 else do
    chip <- getChip number
    if length chip == 0
        then print "Numero de telefone invalido"
    else do
        let c = head chip
        let received_by = (idChip c)
        let sented_by = (idChip chipAtual) 
        if(received_by == sented_by ) then print "Nao e possivel enviar uma mensagem para o proprio numero"
        else do
            insertMessage message "2023-05-02 14:28:12" sented_by received_by
            print "Mensagem enviada com sucesso"
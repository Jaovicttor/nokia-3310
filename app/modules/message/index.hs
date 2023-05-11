module App.Modules.Message.Index  where
import qualified DB.Models.Message as Message
import DB.Models.Chip

mainMessage :: IO()
mainMessage = do
 showConversations
 putStrLn "x - Iniciar conversa"
 putStrLn "1 - Ler conversa"
 putStrLn "2 - Apagar conversa"
 putStrLn "0 - Sair"
 choice <- getLine
 if choice == "0" then putStrLn "saindo..."
 else do
    case choice of
        "x" -> startConversation
        "1" -> formConversation 
        "2" -> formDeleteConversation 
        _   -> do
            putStrLn "Opção inválida! Tente novamente."
    mainMessage
    
startConversation :: IO()
startConversation = do
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
                let sented_by = (idChip myChip) 
                if(received_by == sented_by ) then putStrLn "Não é possivel enviar uma mensagem para o próprio número"
                else do
                    Message.insertMessage message received_by
                    putStrLn "Mensagem enviada com sucesso"

showConversations :: IO()
showConversations = do
    putStrLn "-------------------------------------------"
    putStrLn "------------------Mensagens----------------"
    putStrLn "-------------------------------------------"
    conversations <- Message.getConversations
    let conversations_string = Message.conversationsToString conversations 0
    putStrLn conversations_string
    putStrLn "-------------------------------------------"

formConversation :: IO()
formConversation = do 
    putStrLn "-------------------------------------------"
    putStrLn "Informe o nº da conversa:"
    conversation_number <- getLine
    putStrLn "-------------------------------------------"
    findConversation (read conversation_number)


findConversation :: Int -> IO()
findConversation number = do
    conversations <- Message.getConversations
    if(number <= 0 || number > length conversations ) then do
        putStrLn "Conversa não encontrada"
        putStrLn "-------------------------------------------"
    else do
        let conversation = (conversations !! (number-1))
        showConversation (Message.toStringConversation conversation ) conversation
        mainMessage

showConversation :: String -> Message.Conversation -> IO()
showConversation name conversation = do
    putStrLn "-------------------------------------------"
    messages <- Message.findConversation (Message.chip_id conversation)
    let messages_string = Message.messageToString name messages
    putStrLn(messages_string)
    putStrLn "-------------------------------------------"
    putStrLn "x - Enviar mensagem"
    putStrLn "0 - Sair"
    choice <- getLine
    putStrLn "-------------------------------------------"
    case choice of
        "x" -> sendMessage (Message.chip_id conversation) >> showConversation name conversation
        "0" -> putStrLn "saindo..."
        _   -> do
            putStrLn "Opção inválida! Tente novamente." >> showConversation name conversation
    

sendMessage:: Int -> IO()
sendMessage received_by = do
    putStrLn "Mensagem:"
    message <- getLine
    if(message == "-") then putStrLn""
    else do
        putStrLn "-------------------------------------------"
        if(message == "") then putStrLn "A mensagem não pode ser vazia."
        else
            Message.insertMessage message received_by

formDeleteConversation :: IO()
formDeleteConversation = do 
    putStrLn "-------------------------------------------"
    putStrLn "Informe o nº da conversa:"
    conversation_number <- getLine
    putStrLn "-------------------------------------------"
    deleteConversation (read conversation_number)

deleteConversation :: Int -> IO()
deleteConversation number = do
    conversations <- Message.getConversations
    if(number <= 0 || number > length conversations ) then do
        putStrLn "Conversa não encontrada"
        putStrLn "-------------------------------------------"
    else do
        let conversation = (conversations !! (number-1))
        Message.deleteConversation (Message.chip_id conversation)
        putStrLn "Conversa deletada."
        putStrLn "-------------------------------------------"
        mainMessage
module App.Modules.Message.Index  where
import qualified DB.Models.Message as Message
import DB.Models.Chip
import qualified App.Shared.Main.Helper.Display as Display

mainMessage :: IO()
mainMessage = do
 showConversations
 putStrLn "x - Iniciar conversa"
 putStrLn "1 - Ler conversa"
 putStrLn "2 - Apagar conversa"
 putStrLn "0 - Sair"
 Display.printeBottom
 choice <- getLine
 case choice of
    "0" -> return ()
    "x" -> startConversation >> mainMessage
    "1" -> formConversation >> mainMessage
    "2" -> formDeleteConversation >> mainMessage
    _  -> putStrLn "Opção inválida! Tente novamente." >> mainMessage
 
    
startConversation :: IO()
startConversation = do
 Display.printeHeader "mensagens"
 putStrLn "Número:"
 number <- getLine
 if(number == "-") then putStrLn""
 else do
    putStrLn "Mensagem:"
    message <- getLine
    if(message == "-") then putStrLn""
    else do
        putStrLn "----------------------------"
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
    Display.printeBottom
showConversations :: IO()
showConversations = do
    Display.printeHeader "mensagens"
    conversations <- Message.getConversations
    let conversations_string = Message.conversationsToString conversations 0
    putStrLn conversations_string
    putStrLn "----------------------------"

formConversation :: IO()
formConversation = do 
    Display.printeHeader "mensagens"
    conversations <- Message.getConversations
    let conversations_string = Message.conversationsToString conversations 0
    putStrLn conversations_string
    putStrLn "----------------------------"
    putStrLn "Informe o nº da conversa:"
    Display.printeBottom
    conversation_number <- getLine
    putStrLn "----------------------------"
    findConversation (read conversation_number)


findConversation :: Int -> IO()
findConversation number = do
    conversations <- Message.getConversations
    if(number <= 0 || number > length conversations ) then do
        putStrLn "Conversa não encontrada"
        putStrLn "----------------------------"
    else do
        let conversation = (conversations !! (number-1))
        showConversation (Message.toStringConversation conversation ) conversation
        mainMessage

showConversation :: String -> Message.Conversation -> IO()
showConversation name conversation = do
    Display.printeHeader "mensagens"
    messages <- Message.findConversation (Message.chip_id conversation)
    let messages_string = Message.messageToString name messages
    putStrLn(messages_string)
    putStrLn "----------------------------"
    putStrLn "x - Enviar mensagem"
    putStrLn "0 - Sair"
    Display.printeBottom
    choice <- getLine
    putStrLn "----------------------------"
    case choice of
        "x" -> sendMessage (Message.chip_id conversation) >> showConversation name conversation
        "0" -> return ()
        _   -> do
            putStrLn "Opção inválida! Tente novamente." >> showConversation name conversation
    

sendMessage:: Int -> IO()
sendMessage received_by = do
    Display.printeHeader "mensagens"
    putStrLn "Mensagem:"
    Display.printeBottom
    message <- getLine
    if(message == "-") then putStrLn""
    else do
        putStrLn "----------------------------"
        if(message == "") then putStrLn "A mensagem não pode ser vazia."
        else
            Message.insertMessage message received_by

formDeleteConversation :: IO()
formDeleteConversation = do 
    Display.printeHeader "mensagens"
    conversations <- Message.getConversations
    let conversations_string = Message.conversationsToString conversations 0
    putStrLn conversations_string
    putStrLn "Informe o nº da conversa:"
    Display.printeBottom
    conversation_number <- getLine
    putStrLn "----------------------------"
    deleteConversation (read conversation_number)

deleteConversation :: Int -> IO()
deleteConversation number = do
    conversations <- Message.getConversations
    if(number <= 0 || number > length conversations ) then do
        putStrLn "Conversa não encontrada"
        putStrLn "----------------------------"
    else do
        let conversation = (conversations !! (number-1))
        Message.deleteConversation (Message.chip_id conversation)
        putStrLn "Conversa deletada."
        putStrLn "----------------------------"
        mainMessage
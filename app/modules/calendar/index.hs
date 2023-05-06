module App.Modules.Calendar.Index where
import DB.Models.Event
import DB.Models.Chip
import Data.Time.Clock
import Data.Time (UTCTime(utctDay))


--pq o import n ta funcionado
--como funciona o negocio de conexao do banco
-- title, event_day, comments, chip_id 
--O sistema não deverá cadastrar um evento sem identificação e 
--em data, muito menos um evento com uma data menor do que a atual.

addEvento :: IO ()
addEvento = do
  putStrLn "Adicionar Evento\n"
  putStrLn "Titulo: "
  titleInput <- getLine
  putStrLn "Data (YYYY-MM-DD):  "
  dataInput <- getLine
  putStrLn "Comentários: "
  comentario <- getLine
  if (validData dataInput) && (validTitle titleInput)
    then do
        insertEvent titleInput dataInput comentario (idChip myChip)
        putStrLn "Cadastrou com sucesso\n"
        menuLoop
    else do
        putStrLn ("O nome não deve ser vazio e a data deve ser maior que o dia de hoje") 
        --to-do
        --display da data atual
        -- try again ou volte
        addEvento


validData :: String -> Bool
validData x = True
--validaData x = maiorAtual x && formatoValido 

formatoValido :: String -> Bool
formatoValido x = True

maiorAtual :: String -> Bool
maiorAtual x = True
-- maiorAtual
--     |formataData input == show utctDay = True
--     |otherwise = False   

validTitle :: String -> Bool
--- dar um trim espeaco vazio e comparar so
validTitle x = True

nextEvents :: IO()
nextEvents = do
    events <- getNextEvent
    print (events)

existOnDb :: IO()
existOnDb = do
  putStrLn "Titulo: "
  title <- getLine
  print (findEvent title)

-- editEvent :: IO()
-- editEvent = do
--   putStrLn "Editar Evento\n"
--   putStrLn "Titulo: "
--   oldTitleInput <- getLine
--   if (validTitle oldTitleInput) && (existOnDb oldTitleInput)
--     then do
--         newInformation       
--     else do
--         putStrLn ("Tente novamente!!\nO nome não deve ser vazio e a data deve ser maior que" show utctDay)
--         mainCalendar


-- newInformation :: IO()
-- newInformation= do
--     putStrLn "Novo Titulo: "
--     newTitleInput <- getLine
--     putStrLn "Data (DD/MM/YYYY):  "
--     newTitleInput <- getLine
--     if validaData (read newDataInput) && validTitle (read newTitleInput) 
--         then do
--             overWriteDb oldTitleInput newTitleInput newTitleInput
--         else do
--             putStrLn "Falhaaa!!!"


-- removeEvent :: IO()
-- removeEvent = do
--     putStrLn "Remover Evento\n"
--     putStrLn "Titulo: "
--     titleInput <- getLine
--     if (validTitle titleInput)  && (existOnDb Titile)
--         then do
--             menuLoop
--         else do
--             putStrLn "Titulo invalido"
--             menuLoop

-- showAllEvent :: IO()
-- showAllEvent = do
--     displayFromDb callAllEvents

-- previusEvents :: IO()
-- previusEvents = do
--     displayAllFromDb (callPreviusEvents utctDay)

menuLoop :: IO ()
menuLoop = do
    putStrLn "\n"
    putStrLn "Selecione uma opção:"
    putStrLn "1 - Adicionar Evento"
    putStrLn "2 - ExisteDB"
    putStrLn "3 - Listar próximos Eventos"
    putStrLn "4 - Listar Eventos passados"
    putStrLn "5 - Editar Evento"
    putStrLn "6 - Excluir Evento"
    putStrLn "0 - Voltar"
    choice <- getLine
    case choice of
        "1" -> addEvento 
        "2" -> existOnDb
        -- "3" -> removeEvent 
        -- "4" -> showAllEvent 
        "5" -> nextEvents
        -- "6" -> callPreviusEvents
        "0" -> putStrLn "saindo"
        _   -> do
            putStrLn "Opção inválida! Tente novamente."
            menuLoop

menuCalendar :: IO ()
menuCalendar = do
    putStrLn "Bem-vindo(a) ao Calendário!"
    menuLoop
module App.Modules.Calendar.Index where
import DB.Models.Event
import DB.Models.Chip
import Data.Time
import App.Shared.Main.Helper.Timer
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Char

-- todo remove pelo Banco

addEvento :: Int ->IO ()
addEvento 0 = do
  putStrLn "Titulo: "
  titleInput <- getLine
  validBack titleInput
  putStrLn "Data (YYYY-MM-DD): "
  dataInput <- getLine
  validBack dataInput
  putStrLn "Comentários: "
  comentario <- getLine
  validBack comentario
  if (validData dataInput) && (validTitle titleInput)
    then do
        insertEvent titleInput dataInput comentario 
        header "Cadastrou com sucesso"
        menuLoop
    else do
        putStrLn $ "O nome não deve ser vazio (YYYY-MM-DD)" ++ "\n" ++
          "a data deve ser no formato" ++ "\n" ++
          "maior que: " 
        currentDayMonth
        addEvento 0
addEvento 1 = do
  putStrLn "Titulo: "
  titleInput <- getLine
  validBack titleInput
  putStrLn "Data (YYYY-MM-DD): "
  dataInput <- getLine
  validBack dataInput
  putStrLn "Comentários: "
  comentario <- getLine
  validBack comentario
  if (validData dataInput) && (validTitle titleInput)
    then do
        insertEvent titleInput dataInput comentario
    else do
        putStrLn $ "O nome não deve ser vazio (YYYY-MM-DD)" ++ "\n" ++
          "a data deve ser no formato" ++ "\n" ++
          "maior que: " 
        currentDayMonth
        addEvento 1



currentDayMonth :: IO() 
currentDayMonth = do 
  currentTime <- getCurrentTime
  timeZone <- getTimeZone currentTime 
  let localTime = utcToLocalTime timeZone currentTime 
  let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d" localTime
  putStrLn formattedTime

utctimeToString :: UTCTime -> String
utctimeToString time = formatTime defaultTimeLocale "%Y-%m-%d" time

validData :: String -> Bool
validData x = (validEmpty x) && (isValidDate x)

isValidDate :: String -> Bool
isValidDate dateStr =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr :: Maybe Day of
    Just _  -> True
    Nothing -> False

validEmpty :: String -> Bool
validEmpty title = length title > 0

validTitle :: String -> Bool
validTitle title = validEmpty title

nextEvents :: IO()
nextEvents = do
    events <- getNextEvent
    let str = "Próximos Eventos"
    header str
    putStrLn $ displayEvents2 events (length str)
    menuCalendar


previusEvents :: IO()
previusEvents = do
    events<- getPreviusEvent
    let str = "Eventos Anteriores"
    header str
    putStrLn $ displayEvents2 events (length str)
    menuCalendar

showAllEvents :: IO() 
showAllEvents = do 
    events <- getEvents
    let str = "Todos os Eventos2"
    header str
    putStrLn $ displayEvents2 events (length str)
    menuCalendar

existOnDb :: IO()
existOnDb = do
  header "Existe"
  putStrLn "Titulo: "
  title <- getLine
  validBack title
  existe <- findEvent title
  putStrLn $ "\n" ++ displayEvents existe 
  menuCalendar

existDbBool :: [Event] -> Bool
existDbBool [] = False
existDbBool x = True

displayEvents :: [Event] -> String 
displayEvents [] = "Não há eventos"
displayEvents events =
  let separator = replicate 31 '-'
      displayEvent event = 
        separator ++ "\n" ++
        "Titulo: " ++ title event ++ "\n" ++ --formatacao nokia quebra
        "Comentário: " ++ comment event ++ "\n" ++
        "Data: " ++ utctimeToString (event_day event) ++ "\n" ++ --parse utc
        separator 
  in unlines $ map displayEvent events

displayEvents2 :: [Event] -> Int -> String 
displayEvents2 [] x  = "Não há eventos"
displayEvents2 events num =
  let separator = replicate (21 + num) '-'
      displayEvent event = 
        separator ++ "\n" ++
        "Titulo: " ++ title event ++ "\n" ++ --formatacao nokia quebra
        "Comentário: " ++ comment event ++ "\n" ++
        "Data: " ++ utctimeToString (event_day event) ++ "\n" ++ --parse utc
        separator 
  in unlines $ map displayEvent events

displaySingleEvent :: Event -> String
displaySingleEvent event = 
          padrao ++ "\n" ++
          "Titulo: " ++ title event ++ "\n" ++
          "Comentário: " ++ comment event ++ "\n" ++
          "Data: " ++ utctimeToString (event_day event) ++ "\n" ++
          padrao

padrao :: String
padrao = replicate 31 '-'

validBack :: String -> IO ()
validBack "_" = menuCalendar
validBack _ = putStrLn ""

formDeleteEvent :: IO()
formDeleteEvent = do 
    events <- getEvents
    let events_display = eventsToString events 0
    putStrLn events_display
    putStrLn padrao
    putStrLn "Informe o nº do evento:"
    event_number <- getLine
    validBack event_number
    putStrLn padrao
    deleteEvent 0 (read event_number)

deleteEvent :: Int -> Int-> IO()
deleteEvent 0 number = do
    events <- getEvents
    if(number <= 0 || number > length events) then do -- verificação se valido no index
        putStrLn "Evento não encontrada"
        putStrLn padrao
    else do
        let event = (events !! (number-1)) -- acessa na linsta
        deleteEventDB (chip_id event)
        putStrLn "Evento deletado."
        putStrLn padrao
        menuCalendar
deleteEvent 1 number = do
    events <- getEvents
    if(number <= 0 || number > length events) then do -- verificação se valido no index
        putStrLn "Evento não encontrada"
        putStrLn padrao
    else do
        let event = (events !! (number-1)) -- acessa na linsta
        deleteEventDB (chip_id event)
    

editEvent :: IO()
editEvent = do 
    events <- getEvents
    let events_display = eventsToString events 0
    putStrLn events_display
    putStrLn padrao
    putStrLn "Informe o nº do evento:"
    event_number <- getLine
    validBack event_number
    putStrLn padrao
    deleteEvent 1 (read event_number)
    addEvento 1
    putStrLn $ padrao ++ "\n" ++ "Editou com sucesso"

menuLoop :: IO ()
menuLoop = do
    putStrLn $ replicate 31 '-'        ++ "\n" ++
     "     " ++ "Selecione uma opção:" ++ "\n" ++
     replicate 31 '-'                  ++ "\n" ++
     "| 1 - Adicionar Evento        |" ++ "\n" ++
     "| 2 - Listar todos os Eventos |" ++ "\n" ++
     "| 3 - Listar próximos Eventos |" ++ "\n" ++
     "| 4 - Listar Eventos passados |" ++ "\n" ++
     "| 5 - Editar Evento           |" ++ "\n" ++
     "| 6 - Excluir Evento          |" ++ "\n" ++
     "| 7 - Existe o Evento         |" ++ "\n" ++
     "| 0 - Voltar                  |" ++ "\n" ++
     replicate 31 '-' 
    numberPad
    putStrLn "Digite: "

    choice <- getLine
    case choice of
        "1" -> header "Adicionar Evento" >> addEvento 0
        "2" -> showAllEvents  >> menuCalendar
        "3" -> nextEvents >> menuCalendar
        "4" -> previusEvents >> menuCalendar
        "5" -> editEvent >> menuCalendar
        "6" -> formDeleteEvent >> menuCalendar
        "7" -> existOnDb >> menuCalendar
        "0" -> putStrLn "saindo"
        _   -> do
            putStrLn "Opção inválida! Tente novamente."
            menuLoop

menuCalendar :: IO ()
menuCalendar = do
    header "Calendário"
    currentTime
    --todo lista 5 eventos
    menuLoop

header :: String -> IO()
header str = putStrLn  $ 
            replicate (21 + length str) '-' ++ "\n" ++
            replicate 10 '-' ++ "nokia-3310" ++ replicate (length str + 1) '-' ++ "\n" ++
            replicate (21 + length str) '-' ++ "\n" ++
            replicate 10 '-' ++ str ++ replicate 11 '-' ++ "\n" ++
            replicate (21 + length str) '-' 

numberPad :: IO()
numberPad = putStrLn $ 
              "     | 1  |  2  |  3  |\n" ++
              "     | 4  |  5  |  6  |\n" ++
              "     | 7  |  8  |  9  |\n" ++
              "     |    |  0  |     |\n"
  
-- podemos colocar essas duas funcoes no helper de timer
currentTime :: IO()
currentTime = do
  currentTime <- getCurrentTime
  timeZone <- getTimeZone currentTime 
  let localTime = utcToLocalTime timeZone currentTime 
  let timeString = formatTime defaultTimeLocale "%c" localTime
  putStrLn $ "   " ++ timeString 
  putStrLn  $ replicate 31 '-'
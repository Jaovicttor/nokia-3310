module App.Modules.Calendar.Index where
import DB.Models.Event
import DB.Models.Chip
import Data.Time
import App.Shared.Main.Helper.Timer
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Char
import Data.List (sortBy)
import Data.Ord (comparing)

addEvento :: Int ->IO ()
addEvento 0 = do
  putStrLn "Titulo: "
  titleInput <- getLine
  validBack titleInput
  putStrLn "Data (DD-MM-AAAA): "
  dataInput <- getLine
  validBack dataInput
  putStrLn "Comentários: "
  comentario <- getLine
  validBack comentario
  if (validData dataInput) && (validTitle titleInput)
    then do
        insertEvent titleInput (swapData(dataInput)) comentario 
        header "Cadastrou com sucesso"
        menuLoop
    else do
        putStrLn $ "O nome não deve ser vazio (DD-MM-AAAA)" ++ "\n" ++
          "a data deve ser no formato" ++ "\n" ++
          "maior que: " 
        currentDayMonth
        addEvento 0
addEvento 1 = do
  putStrLn "Titulo: "
  titleInput <- getLine
  validBack titleInput
  putStrLn "Data (DD-MM-AAAA): "
  dataInput <- getLine
  validBack dataInput
  putStrLn "Comentários: "
  comentario <- getLine
  validBack comentario
  if (validData dataInput) && (validTitle titleInput)
    then do
        insertEvent titleInput dataInput comentario
    else do
        putStrLn $ "O nome não deve ser vazio (DD-MM-AAAA):" ++ "\n" ++
          "a data deve ser no formato" ++ "\n" ++
          "maior que: " 
        currentDayMonth
        addEvento 1

addAniversary :: String -> String -> IO()
addAniversary name date = do 
 insertEvent ("Aniversário de " ++ name) date ("Aniversário de " ++ name ++ "criado automaticamente pelo sistema")

    
showAllEvents :: IO() 
showAllEvents = do
  events <- getEvents 
  let sortedEvents = sortBy (comparing event_day) events
      str = "Todos os Eventos"
  header str
  putStrLn $ displayEvents sortedEvents (length str)
  waitForKey   

nextEvents :: IO ()
nextEvents = do
  events <- getNextEvent
  let sortedEvents = sortBy (comparing event_day) events
      str = "Próximos Eventos"
  header str
  putStrLn $ displayEvents sortedEvents (length str)
  waitForKey   

previusEvents :: IO()
previusEvents = do
  events<- getPreviusEvent
  let sortedEvents = sortBy (comparing event_day) events
      str = "Eventos Anteriores"
  header str
  putStrLn $ displayEvents sortedEvents (length str)
  waitForKey  

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
        putStrLn "Evento não encontrado"
        putStrLn padrao
    else do
        let event = (events !! (number-1)) -- acessa na lista
        deleteEventDB (event_id event)
        putStrLn "Evento deletado."
        putStrLn padrao
        menuCalendar
deleteEvent 1 number = do
    events <- getEvents
    if(number <= 0 || number > length events) then do -- verificação se valido no index
        putStrLn "Evento não encontrado"
        putStrLn padrao
    else do
        let event = (events !! (number-1)) -- acessa na lista
        deleteEventDB (event_id event)

menuLoop :: IO ()
menuLoop = do
    putStrLn "Digite: "
    choice <- getLine
    case choice of
        "1" -> header "Adicionar Evento" >> addEvento 0
        "2" -> showAllEvents  >> menuCalendar
        "3" -> nextEvents >> menuCalendar
        "4" -> previusEvents >> menuCalendar
        "5" -> editEvent >> menuCalendar
        "6" -> formDeleteEvent >> menuCalendar
        "0" -> putStrLn "saindo"
        _   -> do
            putStrLn "Opção inválida! Tente novamente."
            menuLoop

waitForKey :: IO ()
waitForKey = do
  putStrLn "Precione qualquer tecla para continuar"
  _ <- getLine
  putStrLn "Continuando..."

menuCalendar :: IO ()
menuCalendar = do
    strTime <- currentTime
    putStrLn $ nokia strTime
    -- header "Calendário"
    -- currentTime
    menuLoop

-- helper interface
header :: String -> IO()
header str = putStrLn  $ 
            replicate (21 + length str) block ++ "\n" ++
            replicate 10 block ++ "nokia-3310" ++ replicate (length str + 1) block ++ "\n" ++
            replicate (21 + length str) block ++ "\n" ++
            replicate 10 block ++ str ++ replicate 11 block ++ "\n" ++
            replicate (21 + length str) block

block :: Char
--block = '\x2588'
block = '-'
  
--todo helper time
currentTime :: IO String 
currentTime = do
  currentTime <- getCurrentTime
  timeZone <- getTimeZone currentTime 
  let localTime = utcToLocalTime timeZone currentTime 
  let timeString = formatTime defaultTimeLocale "%c"localTime
  return $ timeString 
  

currentDayMonth :: IO() 
currentDayMonth = do 
  currentTime <- getCurrentTime
  timeZone <- getTimeZone currentTime 
  let localTime = utcToLocalTime timeZone currentTime 
  let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d" localTime
  putStrLn formattedTime

utctimeToString :: UTCTime -> String
utctimeToString time = formatTime defaultTimeLocale "%d-%m-%Y" time

swapData :: String -> String
swapData strData = formatTime defaultTimeLocale "%Y-%m-%d" dataObj
    where
        dataObj = parseTimeOrError True defaultTimeLocale "%d-%m-%Y" strData :: Day

-- Metodos Auxiliares exclusivos de Eventos
-- Validação 
isDateAfter :: Day -> Day -> Bool
isDateAfter d1 d2 = compare d1 d2 == GT

validData :: String -> Bool
validData x = (validEmpty x) && (isValidDate x)

isValidDate :: String -> Bool
isValidDate dateStr =
  case parseTimeM True defaultTimeLocale "%d-%m-%Y" dateStr :: Maybe Day of
    Just _  -> True
    Nothing -> False

validEmpty :: String -> Bool
validEmpty title = length title > 0

validTitle :: String -> Bool
validTitle title = validEmpty title

existDbBool :: [Event] -> Bool
existDbBool [] = False
existDbBool x = True

-- Listagem 
displayEvents :: [Event] -> Int -> String 
displayEvents [] x  = "Não há eventos"
displayEvents events num =
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

-- helper interface
padrao :: String
padrao = replicate 110 '-'

validBack :: String -> IO ()
validBack "-" = menuCalendar
validBack _ = putStrLn ""

nokia :: String -> String
nokia hora =
  replicate 110 '-' ++  "\n " ++  
  "    -╤▓▓▓▓▓▓▓▓██████████████▓▓╦       " ++     "\n" ++                
  "  .╠╬╣█▓▓▓█████████████████████╣▒     " ++     "\n" ++                  
  "  ╩╬╩█▓▓█▓█████████████████████╣╣▒    " ++     "\n" ++       
  "  ║╬╣█▓█████████████████████████╣╬    " ++     "\n" ++                
  " ]╣╣█▓██████████████████████████╣╬⌐   " ++     "\n" ++                
  " ║╬╣█▓███████████████████████████▓▌   " ++     replicate 10 ' ' ++  hora ++ replicate 10 ' ' ++ "\n" ++       
  " ║╬▓█████████████████████████████║╬   " ++     "\n" ++                                                    
  " ║╬██████████▌╬Γ╝╫Γ▀▌▀╚██████████▌╬   " ++      "        ██████████████Opções█████████████\n"     ++                              
  " ║▌█▓█████▓███████████████████████▌⌐  " ++      "         | 1 - Adicionar Evento        |\n" ++
  " ║╣███▓▓▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▓███Å▒  " ++      "         | 2 - Listar todos os Eventos |\n" ++
  " ║╬███░---░░░░░░░░░░░░░░░░░░-░╣███▓▒  " ++      "         | 3 - Listar próximos Eventos |\n" ++
  " ║╣███▒░,░░░░░░░░░░░░░≥░░░░░░░╠████▌  " ++      "         | 4 - Listar Eventos passados |\n" ++
  " ║╣██▓▒░»░░░░░░░░░░░░░░░░░░░░░│████▌  " ++      "         | 5 - Editar Evento           |\n" ++
  " ║╬███▒░░░░░░░░░░░░░░░░░≥░░░░░│███▓▌  " ++      "         | 6 - Excluir Evento          |\n" ++
  " ║▌▓██▒░░░░░░░░░░░░░░░░░░░░░∩░║███▓▌  " ++      "         | 0 - Voltar                  |\n" ++
  " ║╣███▌░░░░░░░░░░░░░░░░░░░░░⌐\"╟███▌  " ++      "         █████████████████████████████████\n"     ++             
  " ╠╬▓██▌░--░░░░░░░░░░░░░░░░░░~:╟██▌▓▌  " ++     "\n" ++               
  " ╠╬╣███▄▄▄▄▄▄▄▄▄▄▄░▄▄▄▄▄▄▄▄▄▄▄███╬▓▌  " ++     "\n" ++               
  " ╠╬╬╬▓█▓████████████████████████▒╣╬▌  " ++     "\n" ++               
  " ╠╬╬╬╬▓█▌╣╬╣████▓▓▓█████╣▓██▓▓█▒╣╣▓▒  " ++     "\n" ++               
  " ╠╬╬╣╬╣███▄╠╚╚▀╠╠║╦╣╬╬▒╚╩╠╣▓██╣╣╣╣╬▒  " ++     "\n" ++               
  " ║╬╬╬╬╝╬╬▓▀███▓▓█▓▓▓█▓▓███▀▓▄█╠╣╣╣╬▒  " ++     "\n" ++               
  " ║╬╬╬╬╪╣╬╠▀█████▀▓▓██▀███▓╬╣╣▀║╣╣╣╬░  " ++     "\n" ++                                                        
  " ]╬╬╬╬╬╣╬╠╠╚╠╬╣╬╠╬╠║╩╬╬▒╬╠╠╠╣╣╬╣╬╣╬   " ++     "\n" ++                                    
  "  ╠╬╬╣╣╣▓▄╩╣╬╬╬╬╬╣▒╣╣╬╬╬╣╝╬╣▓██▌╬╣╬   " ++     "\n" ++                                    
  "  ╠╬╬╬╩╩╚╩▀█▓▒╬▒╣▀▓▓███▓╬╬╩╩╠╠╬╣╬╬╬   " ++     "\n" ++                                    
  "  ╠╬╬╬╬▒╦╦╬╣╣╣╣╬╠╦╦╧╠╠╬╣╬╬▒▒▒╣╣╣╬╬▒   " ++     "\n" ++                                    
  "  ╠╬╬Φ╩║▓██▄╬╠╠╠╠╠╬╬╠╠╠╠╠╫╣▓▓▌█▌╣╣▒   " ++     "\n" ++                                    
  "  ║╠╬╣╬╩ü╠╚╩╬╠╝╠╠╚╩╚║▀▀╬╬╠≥≡≡╠╬╣╣╬░   " ++     "\n" ++                                    
  "  ]╠╠╬╩╠╠╠╩╬╬╬╬╣╬╦╣╬╬╬╬╬╬╣╬╠▄▄▄╣╣╬    " ++     "\n" ++                                    
  "   ╠╬╠╚▒╝╝▀██╣╣╣╣╣▓███▓╬╬▀╬▀▀▒╩╣╬╬    " ++     "\n" ++                                    
  "   ╠╠╬╬╦▒▒╠║╬╣╬╬╠╦░╩╠╠╬╬▒╦╗╣╦╣╬╬╬▒    " ++     "\n" ++                                    
  "   ╚╠╬╬╟╝╣╣▄╠╣╬╬╩╝╬╬╩╣╬╬╬╫▓█▀█║╣╬▒    " ++     "\n" ++                                    
  "    ╠╠╬╣▒╚░╚╚▒╣Φ╩╚╝╝▀▀╠╪╬╠≥╠╠╣╬╬╬⌐    " ++     "\n" ++                                    
  "    ╩╠╠╬╬╠╬╬╬╬╬╠▒╦╗╦╬╣╬╬╬╬╬╬╬╬╣╬╬     " ++     "\n" ++ 
  replicate 110 '-'
 
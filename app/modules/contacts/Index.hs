module App.Modules.Contacts.Index where 
import DB.Connection
import Data.Time.Format
import Data.Time.Calendar
import Control.Monad (when)
import Data.List (null)
import Data.List (sortOn)
import qualified DB.Models.Contact as Contact

main :: IO ()
main = do
    menuLoop

menuLoop :: IO ()
menuLoop = do
    printeHeader
    putStrLn "1 - Listar Contatos"
    putStrLn "2 - Adicionar Contato"
    putStrLn "3 - Contatos de Emergência"
    putStrLn "0 - Voltar ao Menu"
    printeBottom
    choice <- getLine
    if choice == "0" then return()
    else do
      case choice of
          "1" -> listContact 
          "2" -> addContact 
          "3" -> speedDial
          _   -> putStrLn "Opção inválida! Tente novamente." >> menuLoop
      
 
subMenu :: IO()
subMenu = do
  printeHeader
  putStrLn "1 - Editar Contato"
  putStrLn "2 - Excluir Contato"
  putStrLn "3 - Adicionar Contato"
  putStrLn "0 - Voltar"
  printeBottom
  choice <- getLine
  case choice of
        "1" -> editContact
        "2" -> formDeleteContact
        "3" -> addContact
        "0" -> main
        
        _   -> do
            putStrLn "Opção inválida! Tente novamente."
            subMenu

listContact :: IO()
listContact = do
  contact <- Contact.getContacts
  let contact_string = Contact.contactToString contact 0 
  putStrLn "\n---Contato(s)---\n"
  putStrLn contact_string
  subMenu
  
formDeleteContact :: IO()
formDeleteContact = do
  putStrLn "\n---Excluir Contato---\n"
  putStrLn (" Pressione '-' para retornar \n")
  contact <- Contact.getContacts
  let contact_string = Contact.contactToString contact 0 
  putStrLn contact_string
  putStrLn "Digite o  número do contato que deseja excluir"
  n <- getLine
  when (n == "-") $ menuLoop
  deleteContact (read n)
  listContact

deleteContact :: Int -> IO()
deleteContact n = do
  contacts <- Contact.getContacts
  if(n <= 0 || n > length contacts ) then do
        putStrLn "Contato não encontrado"
  else do
    let contact = (contacts !! (n-1))
    Contact.deleteContact (Contact.idContact contact)
    


speedDial :: IO ()
speedDial = do
  contacts <- Contact.getSpeedDial
  let sortedContacts = sortOn snd contacts -- ordena a lista pelo segundo elemento (speed_dial)
  putStrLn "\n---Contatos de Emergencia---\n"
  mapM_ (\(name, speed_dial) -> putStrLn (show speed_dial ++ " " ++ name)) sortedContacts
  menuLoop


addContact :: IO ()
addContact = do
  putStrLn (" Pressione '-' para retornar \n")
  putStrLn ("Nome: ")
  name <- getLine
  when (name == "-") $ menuLoop
  putStrLn ("Telefone: ")
  phone <- getLine
  when (phone == "-") $ menuLoop
  check phone 
  if length phone /= 11 && length phone /= 3
    then do
      putStrLn ("\nNúmero informado é invalido siga o padrão:  XX999999999\n")
      addContact
    else do
      putStrLn ("Aniversário (Opcional - DD-MM-AAAA): ")
      birthday <- getLine
      when (birthday == "-") $ menuLoop
      let birthdayCheck = if null birthday then "31-12-9999" else birthday
      let birthdayStr = swapAndRemoveHyphen birthdayCheck
      putStrLn ("Adicione uma posição para discagem Rápida (0-9) (Opcional): ")
      speedDialStr <- getLine
      when (speedDialStr == "-") $ menuLoop
      let speedDial = if null speedDialStr then -1 else read speedDialStr
      let nameStr = if null name then phone else name
      Contact.insertContact nameStr phone birthdayStr speedDial 1
      putStrLn "\nContato cadastrado com sucesso"
      listContact

editContact :: IO ()
editContact = do
  putStrLn "\n---Editar Contato---\n"
  contact <- Contact.getContacts
  let contact_string = Contact.contactToString contact 0 
  putStrLn contact_string
  putStrLn (" Pressione '-' para retornar \n")
  putStrLn "\nDigite o  número do contato que deseja editar:"
  n <- getLine
  when (n == "-") $ subMenu
  putStrLn "\n"
  editAdd
  deleteContact (read n)
  listContact


removeHyphen :: String -> String
removeHyphen date = concat $ words date

swapAndRemoveHyphen :: String -> String
swapAndRemoveHyphen date = formatTime defaultTimeLocale "%Y%m%d" dataObj
  where
    dateWithoutHyphen = removeHyphen date
    dataObj = parseTimeOrError True defaultTimeLocale "%d-%m-%Y" dateWithoutHyphen :: Day


check :: String -> IO ()
check n = do
  contacts <- Contact.getContacts
  let phones = Contact.streams contacts
  if n `elem` phones
    then do
      putStrLn "O número já existe na lista de contatos."
      menuLoop
  else 
    return()


editAdd :: IO ()
editAdd = do
  putStrLn ("Pressione '-' para retornar \n")
  putStrLn ("Nome: ")
  name <- getLine
  when (name == "-") $ menuLoop
  putStrLn ("Telefone: ")
  phone <- getLine
  when (phone == "-") $ menuLoop
  check phone 
  if length phone /= 11 && length phone /= 3
    then do
      putStrLn ("\nNúmero informado é invalido siga o padrão:  XX999999999\n")
      addContact
    else do
      putStrLn ("Aniversário (Opcional - DD-MM-AAAA): ")
      birthday <- getLine
      when (birthday == "-") $ menuLoop
      let birthdayCheck = if null birthday then "31-12-9999" else birthday
      let birthdayStr = swapAndRemoveHyphen birthdayCheck
      putStrLn ("Adicione uma posição para discagem Rápida (0-9) (Opcional): ")
      speedDialStr <- getLine
      when (speedDialStr == "-") $ menuLoop
      let speedDial = if null speedDialStr then -1 else read speedDialStr
      let nameStr = if null name then phone else name
      Contact.insertContact nameStr phone birthdayStr speedDial 1
      putStrLn "\nContato atualizado com sucesso"



printeHeader :: IO()
printeHeader = do
  putStrLn "\n----------------------------"
  putStrLn "---------NOKIA-3310---------"
  putStrLn "----------------------------"
  putStrLn "-----------agenda-----------"
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
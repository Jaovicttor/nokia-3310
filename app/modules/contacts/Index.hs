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
    putStrLn "\n-----Agenda-----\n"
    putStrLn "1 - Listar Contatos"
    putStrLn "2 - Adicionar Contato"
    putStrLn "3 - Contatos de Emergência"
    putStrLn "0 - Voltar ao Menu"
    choice <- getLine
    case choice of
        "1" -> listContact 
        "2" -> addContact >> menuLoop
        "3" -> speedDial >> menuLoop
        "0" -> putStrLn "TODOOOO"
        _   -> do
            putStrLn "Opção inválida! Tente novamente."
            menuLoop
 
subMenu :: IO()
subMenu = do
  putStrLn "-----Opções-----"
  putStrLn "1 - Editar Contato"
  putStrLn "2 - Excluir Contato"
  putStrLn "3 - Adicionar Contato"
  putStrLn "0 - Voltar"
  choice <- getLine
  case choice of
        "1" -> editContact >> listContact
        "2" -> formDeleteContact >> listContact
        "3" -> addContact >> listContact
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
        subMenu
  else do
    let contact = (contacts !! (n-1))
    Contact.deleteContact (Contact.idContact contact)
    


speedDial :: IO ()
speedDial = do
  contacts <- Contact.getSpeedDial
  let sortedContacts = sortOn snd contacts -- ordena a lista pelo segundo elemento (speed_dial)
  putStrLn "\n---Contatos de Emergencia---\n"
  mapM_ (\(name, speed_dial) -> putStrLn (show speed_dial ++ " " ++ name)) sortedContacts


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
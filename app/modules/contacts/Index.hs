module App.Modules.Contacts.Index where 
import DB.Connection
import Data.Time.Format
import Data.Time.Calendar
import qualified DB.Models.Contact as Contact

import Data.Time.Format
import Data.Time.Calendar

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
        "0" -> putStrLn "TODO"
        _   -> do
            putStrLn "Opção inválida! Tente novamente."
            menuLoop
 
subMenu :: IO()
subMenu = do
  putStrLn "-----Opções-----"
  putStrLn "1 - Editar Contato"
  putStrLn "2 - Excluir Contato"
  putStrLn "0 - Voltar"
  choice <- getLine
  case choice of
        "1" -> editContact >> listContact
        "2" -> formDeleteContact >> listContact
        "0" -> main
        _   -> do
            putStrLn "Opção inválida! Tente novamente."
            listContact

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
  putStrLn "Digite o  número do contato que deseja excluir"
  n <- getLine
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
speedDial  = do
  contact <- Contact.getSpeedDial
  putStrLn "\n---Contatos de Emergencia---\n"
  mapM_ (\c -> putStrLn ("- " ++ c)) contact

addContact :: IO ()
addContact = do
  putStrLn (" Pressione '-' para retornar \n")
  putStrLn ("Nome: ")
  name <- getLine
  if name == "-"
    then do
      menuLoop
    else do
      putStrLn ("Telefone: ")
      phone <- getLine
      if length phone /= 11 && length phone /= 3
        then do
          putStrLn ("\nNúmero informado é invalido siga o padrão:  XX999999999\n")
          addContact
        else do
          putStrLn ("Aniversário (Opcional - DDMMYYYY): ")
          birthday <- getLine
          let birthdayStr = swapData birthday
          putStrLn ("Adicione uma posição para discagem Rápida (0-9) (Opcional): ")
          speedDialStr <- getLine
          let speedDial = if null speedDialStr then -1 else read speedDialStr
          let nameStr = if null name then phone else name
          Contact.insertContact nameStr phone birthdayStr speedDial 1

editContact :: IO ()
editContact = do
  putStrLn "\n---Editar Contato---\n"
  contact <- Contact.getContacts
  let contact_string = Contact.contactToString contact 0 
  putStrLn contact_string
  putStrLn "\nDigite o  número do contato que deseja editar:"
  n <- getLine
  deleteContact (read n)
  putStrLn "\n"
  addContact


swapData :: String -> String
swapData strData = formatTime defaultTimeLocale "%Y%m%d" dataObj
    where
        dataObj = parseTimeOrError True defaultTimeLocale "%d%m%Y" strData :: Day

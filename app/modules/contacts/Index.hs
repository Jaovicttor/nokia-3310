module App.Modules.Contacts.Index where 
import DB.Connection
import Data.Time.Format
import Data.Time.Calendar
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
        "1" -> listarContato 
        "2" -> addContato >> menuLoop
        "3" -> discagemRapida >> menuLoop
        "0" -> putStrLn "TODO"
        _   -> do
            putStrLn "Opção inválida! Tente novamente."
            menuLoop
 
subMenu :: IO()
subMenu = do
  putStrLn "-----Opções-----\n"
  putStrLn "1 - Editar Contato"
  putStrLn "2 - Excluir Contato"
  putStrLn "0 - Voltar"
  choice <- getLine
  case choice of
        "1" -> editarContato >> listarContato
        "2" -> formExcluirContato >> listarContato
        "0" -> main
        _   -> do
            putStrLn "Opção inválida! Tente novamente."
            listarContato

listarContato :: IO()
listarContato = do
  contatos <- Contact.getContacts
  let contatos_string = Contact.contactToString contatos 0 
  putStrLn "\n---Contato(s)---\n"
  putStrLn contatos_string
  subMenu
  
formExcluirContato :: IO()
formExcluirContato = do
  putStrLn "\n---Excluir Contato---\n"
  putStrLn "Digite o  número do contato que deseja excluir"
  n <- getLine
  deleteContact (read n)
  listarContato

deleteContact :: Int -> IO()
deleteContact n = do
  contacts <- Contact.getContacts
  if(n <= 0 || n > length contacts ) then do
        putStrLn "Contato não encontrado"
  else do
    let contact = (contacts !! (n-1))
    Contact.deleteContact (Contact.idContact contact)
    
discagemRapida :: IO ()
discagemRapida  = do
  contatos <- Contact.getSpeedDial
  putStrLn "\n---Contatos de Emergencia---\n"
  mapM_ (\c -> putStrLn ("- " ++ c)) contatos

addContato :: IO ()
addContato = do
  putStrLn (" Pressione '-' para retornar \n")
  putStrLn ("Nome: ")
  nome <- getLine
  if nome == "-"
    then do
      menuLoop
    else do
      putStrLn ("Telefone: ")
      telefone <- getLine
      if length telefone /= 11 && length telefone /= 3
        then do
          putStrLn ("\nNúmero informado é invalido siga o padrão:  XX999999999\n")
          addContato
        else do
          putStrLn ("Aniversário (Opcional - YYYY/MM/DD): ")
          aniversario <- getLine
          putStrLn ("Adicione uma posição para discagem Rápida (0-9) (Opcional): ")
          discagemStr <- getLine
          let discagem = if null discagemStr then -1 else read discagemStr
          let nomeStr = if null nome then telefone else nome
          if discagem > 9 || discagem < 0
            then do
              putStrLn ("\nValor inválido digite um número de 0 a 9\n")
              addContato
            else
              Contact.insertContact nomeStr telefone aniversario discagem 1

editarContato :: IO ()
editarContato = do
  putStrLn "\n---Editar Contato---\n"
  contatos <- Contact.getContacts
  let contatos_string = Contact.contactToString contatos 0 
  putStrLn contatos_string
  putStrLn "\nDigite o  número do contato que deseja editar:"
  n <- getLine
  deleteContact (read n)
  putStrLn "\n"
  addContato
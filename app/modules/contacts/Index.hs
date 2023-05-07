module App.Modules.Contacts.Index where 
import DB.Connection
import DB.Models.Contact
import Data.Time.Format
import Data.Time.Calendar


main :: IO ()
main = do
    putStrLn "\n---Bem-vindo à agenda!---\n"
    menuLoop

menuLoop :: IO ()
menuLoop = do
    putStrLn "\n"
    putStrLn "Selecione uma opção:"
    putStrLn "1 - Listar Contatos"
    putStrLn "2 - Adicionar Contato"
    putStrLn "3 - Discagem Rapida"
    putStrLn "4 - Editar Contato"
    putStrLn "5 - Excluir Contato"
    putStrLn "0 - Voltar ao Menu"
    choice <- getLine
    case choice of
        "1" -> listarContato >> menuLoop
        "2" -> addContato >> menuLoop
        "3" -> discagemRapida >> menuLoop
        "4" -> editarContato >> menuLoop
        "5" -> deletarContato >> menuLoop
        "0" -> putStrLn "TODO"
        _   -> do
            putStrLn "Opção inválida! Tente novamente."
            menuLoop
 
editarContato :: IO ()
editarContato = do
  putStrLn "\n---Editar Contato---\n"
  contatos <- getContactNames
  mapM_ (\c -> putStrLn ("- " ++ c)) contatos
  putStrLn "\nDigite o nome do contato que deseja editar:"
  nome <- getLine
  deleteContact nome
  putStrLn "\n"
  coletarInformacoesContato

discagemRapida :: IO ()
discagemRapida  = do
  contatos <- getSpeedDial
  putStrLn "\n---Discagem Rápida---\n"
  mapM_ (\c -> putStrLn ("- " ++ c)) contatos

listarContato :: IO ()
listarContato = do
  contatos <- getContactNames
  putStrLn "\n---Lista de Contatos---\n"
  mapM_ (\c -> putStrLn ("- " ++ c)) contatos

deletarContato :: IO ()
deletarContato = do
  putStrLn "---Excluir Contato---"
  contatos <- getContactNames
  mapM_ (\c -> putStrLn ("- " ++ c)) contatos
  putStrLn "\nDigite o nome do contato que deseja excluir:"
  nome <- getLine
  if nome `elem` contatos
    then do
      deleteContact nome
      putStrLn $ "\nContato " ++ nome ++ " excluído com sucesso!"
    else do
      putStrLn $ "\nNome de contato inválido: " ++ nome
      deletarContato

addContato :: IO ()
addContato = do
  putStrLn ("\n----Adicionar contato----\n")
  coletarInformacoesContato

coletarInformacoesContato :: IO ()
coletarInformacoesContato = do
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
          coletarInformacoesContato
        else do
          putStrLn ("Aniversário (Opcional - YYYY/MM/DD): ")
          aniversarioStr <- getLine
          let aniversario = if null aniversarioStr then "9999/12/31" else read aniversarioStr
          putStrLn ("Adicione uma posição para discagem Rápida (0-9) (Opcional): ")
          discagemStr <- getLine
          let discagem = if null discagemStr then 0 else read discagemStr
          if discagem > 9 || discagem < 0
            then do
              putStrLn ("\nValor inválido digite um número de 0 a 9\n")
              coletarInformacoesContato
            else
              insertContact nome telefone aniversario discagem 1





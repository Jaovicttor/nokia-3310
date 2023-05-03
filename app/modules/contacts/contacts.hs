data Contato = Contato {identificador :: String, telefone :: String}

addContato :: IO ()
addContato = do
  putStrLn ("\n-Adicionar contato-\n")
  putStrLn ( "Nome: ")
  nome <- getLine
  if nome == "-"
    then
      menuLoop
  else do
    putStrLn ("Telefone: ")
    telefone <- getLine
    if nome == "-"
        then
        menuLoop
    else 
      if length telefone /= 11 && length telefone /= 3
        then do
          putStrLn ("\nNúmero informado é invalido siga o padrão:  XX999999999\n")
          addContato
      else do
        let identificador = case nome of
                          "" -> telefone
                          otherwise -> nome
        putStrLn ("\n" ++ "Contato adicionado: " ++ identificador ++ " " ++ " - " ++ telefone ++ "\n")

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
        "1" -> putStrLn "TODO"
        "2" -> addContato >> menuLoop
        "3" -> putStrLn "TODO"
        "4" -> putStrLn "TODO"
        "5" -> putStrLn "TODO"
        "0" -> putStrLn "TODO"
        _   -> do
            putStrLn "Opção inválida! Tente novamente."
            menuLoop
 
main :: IO ()
main = do
    putStrLn "Bem-vindo à agenda!"
    menuLoop


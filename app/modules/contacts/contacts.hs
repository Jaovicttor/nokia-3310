data Contato = Contato {identificador :: String, telefone :: String}

addContato :: IO ()
addContato = do
  putStrLn ("\n-Adicionar contato-\n")
  putStrLn ( "Nome: ")
  nome <- getLine
  putStrLn ("Telefone: ")
  telefone <- getLine
  if length telefone /= 13
    then do
      putStrLn ("\nNúmero informado é invalido siga o padrão: xx xxxx xxxxx\n")
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
    putStrLn "0 - Voltar"
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


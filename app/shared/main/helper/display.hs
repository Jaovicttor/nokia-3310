module App.Shared.Main.Helper.Display where
    
printeHeader :: String -> IO()
printeHeader name = do
  putStrLn "\n----------------------------"
  putStrLn "---------NOKIA-3310---------"
  putStrLn "----------------------------"
  putStrLn ("---------"++name++"----------")
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
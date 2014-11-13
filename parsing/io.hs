import Data.Char  
  
main = do  
    putStrLn "What's your first name?"  
    firstName <- getLine    
    firstName2 <- getLine    
    firstName <- getLine  
    putStrLn $ "hey " ++ firstName ++ " " ++ firstName2 ++ ", how are you?" 
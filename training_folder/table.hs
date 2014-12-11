import Data.Char  
	 
-- | Poisci element v memory
getGroup :: (Eq a) => [(a,b)] -> a -> Maybe b
getGroup [] _ = Nothing
getGroup ((k, v):xs) element = 
	if k == element then  Just v else getGroup xs element
	
-- | Vstavi elemente v memory
addGroup :: (Eq a) => [(a,b)] -> (a,b) ->  [(a,b)]
addGroup memory element = element:memory 

main = do  
    putStrLn "What's your first name?"  
    firstName <- getLine    
    firstName2 <- getLine    
    firstName <- getLine  
    putStrLn $ "hey " ++ firstName ++ " " ++ firstName2 ++ ", how are you?" 
	

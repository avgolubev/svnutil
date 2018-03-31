module Dialogs where

import System.IO  
import System.Environment
import Utils (trim, parseDelimStrL)

-- выбор значения; при anyValue == True проверяется отсутствие введённого значения в cmds  
selectSomething :: [(String, String)] -> String -> Bool -> IO (Maybe String)
selectSomething cmds description anyValue = do  
    mapM (\(k, v) -> putStrLn $ "[" ++ k ++ "] " ++ v ) cmds
    putStrLn "[q] quit the program"
    doSelect cmds
  where 
    doSelect cmds = do
      putStrLn description
      someNum <- trim <$> getLine 
      case someNum of
        "" -> doSelect cmds
        _  -> case (lookup someNum cmds) of
                Just action -> return $ Just action
                Nothing     -> 
                  if someNum == "q" then 
                    return Nothing 
                  else 
                    if anyValue then
                      if (foldl (\a (_, v) -> if v == someNum then True else a) False cmds) then
                        do
                          putStrLn "The issue exists. Enter another issue."
                          doSelect cmds
                      else
                        return $ Just someNum 
                    else
                      doSelect cmds


addFoldersAndFiles :: IO (Maybe [String])
addFoldersAndFiles = do
  putStrLn "Enter modules (folder1=m1,..,mn folder2... ) or [q] quit: "
  modules <- trim <$> getLine 
  case modules of
    "" -> addFoldersAndFiles
    _  -> 
      if modules == "q" then 
        return Nothing 
      else 
        return $ Just $ filter (/= "") (parseDelimStrL ' ' modules)
        
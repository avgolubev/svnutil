module Utils where

import Data.List as L
import Data.Char
import System.IO

-------------------------------------------
trim :: String -> String
trim = f . f
   where f = L.reverse . L.dropWhile isSpace
   
--------------------------------------------
toLeftSlash :: String -> String
toLeftSlash = (conv <$>) 
  where 
    conv '/' = '\\'
    conv c   = c

-------------------------------------------------
-- загрузка конфигурации в ассоциативный список
-- знак # перед текстом в файле означает комментарий
loadProperties :: FilePath  -> IO [(String, String)]
loadProperties file = do
    props <- L.lines <$> readFile file 
    return $  paramValToAList props 

-------------------------------------------------------------------------------------------
-- преобразование списка, состоящего из строк параметр = значение, в ассоциативный массив
--
paramValToAList :: [String] -> [(String, String)]
paramValToAList paramVal = L.filter (\(_, v) -> v /= "" ) $ conv <$> rmComment paramVal
  where
    rmComment = L.filter (\s -> (not $ L.isPrefixOf "#" (trim s)) && (not $ (trim s) == "")) -- убрать пустые строки и комментарии (строки, начинающиеся с символа #)
    conv =  (\(n, v) -> (trim n, trimV v)) . L.break (\c -> c == '=') -- разобрать строку по символу = , убрать пробелы
    trimV = (\v -> if v == "" then "" else  trim $ L.tail v) . trim   -- убрать символ = в значении и пробелы до и после символа =


-----------------------------------------------------
-- разбор строки с разделителем	delim

parseDelimStrL :: Char -> String -> [String]
parseDelimStrL delim [] = []
parseDelimStrL delim str | dlmS == str = ["",""]
						 | afterDelim == dlmS = beforDelim : [""]
						 | otherwise = beforDelim : (parseDelimStrL delim $ if (elem delim str) then L.tail afterDelim else [])
				where 
					dlmS = delim : []
					beforDelim = L.takeWhile (/= delim) str
					afterDelim = L.dropWhile (/= delim) str
				
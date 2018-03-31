module Main where

import System.IO
import System.Environment
import System.Process
import System.Directory
import System.FilePath.Windows ((</>))
import Data.List
import Data.Time
import Data.Char (isSpace)
import Utils (parseDelimStrL, trim, toLeftSlash, loadProperties, paramValToAList)
import Dialogs (selectSomething, addFoldersAndFiles)

type Folder     = String
type File       = String
type Extension  = String
type Path       = String
type URL        = String
type Issue      = String
type Database   = String
type User       = String
type Pass       = String
type ConfigData = [String] -- database, user, pass, svnBranches, svnTrunk, checkoutPath, postfix
type ExtAList   = [(String, String)] -- ассоциативный  массив каталог=расширение (procedures=.prc ...)

{-- |
      svn_utility cmd issue packages=names,.. procedures=names,.. sql triggers=names,..
    cmd: create, create_runme, save, merge, install, undo, puttorc
--}
main :: IO ()
main = start

start :: IO()
start = do
    config <- propsToList <$> loadProperties "conf\\config.properties"
    args <- getArgs
    case args of
      []     -> -- опции не заданы при запуске
        do
          resAction <- selectAction
          case resAction of
            Nothing -> putStrLn "Exit."
            Just action  ->
              do -- при создании задачи вводить наименование, при остальных действиях - выбирать
                newIssues <- if action == "create" then return [] else issues config
                let
                  descr = if action == "create" then "Enter an issue: " else "Select an issue: "
                  any   = if action == "create" then True else False
                resIssue <- selectIssue descr newIssues any
                case resIssue of
                  Nothing     -> putStrLn "Exit."
                  Just issue  ->
                    if action == "create" then
                      do
                        resParams <- addFoldersAndFiles
                        let options = case resParams of
                                    Nothing     -> [action, issue]
                                    Just params -> action : issue : params
                        runUtil options config
                    else
                      runUtil [action, issue] config
      (x:[]) -> putStrLn "Not enough options: svn_utility cmd issue [args]"
      _      -> runUtil args config
  where
      selectAction = selectSomething actions "Select an action: " False
      selectIssue d n a = selectSomething n d a

actions = [("1", "create")
         , ("2", "create_runme")
         , ("3", "save")
         , ("4", "puttorc")
         , ("5", "merge")
         , ("6", "install")
         , ("7", "undo")]

issues :: [String] -> IO [(String, String)]
issues [_, _, _, _, _, _, _, _, checkoutPath, postfix, _, _] = do
    dirs <- listDirectory checkoutPath
    filterDirs dirs 1
  where
    filterDirs :: [String] -> Int -> IO [(String, String)]
    filterDirs [] _ = return []
    filterDirs (d:ds) i = do
      isDir <- doesDirectoryExist $ checkoutPath </> d
      let
        diff  = (length d) - (length postfix)
        flag1 = if diff > 0 then (drop diff d) == postfix else False

      if isDir && flag1 then
        ((show i, take diff d):) <$> filterDirs ds (i +1)
      else
        filterDirs ds i


runUtil :: [String] -> [String] -> IO()
runUtil options config= do
  let (action:issue:args) = options
      [database, user, pass, hostURL, undoURL, svnBranches, svnTrunk, svnRC, checkoutPath, postfix, extByFolder, foldersToMerge] = config
      pathIssue   = checkoutPath </> issue ++ postfix
      pathHOSTPRP = pathIssue </> (toLeftSlash hostURL)
      authDB      = mconcat [user, "/", pass, "@", database]
      extAList    = paramValToAList $ parseDelimStrL ',' extByFolder
      svnIssueRC  = svnRC ++ "/" ++ issue
      newArgs = if args == [] then [" "] else args
  case action of
    "create"       -> create         issue svnBranches svnTrunk pathIssue newArgs hostURL undoURL extAList
    "create_runme" -> createRunMeSQL issue  pathIssue hostURL undoURL
    "save"         -> save           issue  pathIssue hostURL undoURL
    "puttorc"      -> save           issue  pathIssue hostURL undoURL >> putToRC pathIssue svnIssueRC
    "merge"        -> rmTemp >>
                       save          issue  pathIssue hostURL undoURL >> merge svnTrunk hostURL pathHOSTPRP foldersToMerge
    "install"      -> commitDB       authDB pathIssue (\s -> isPrefixOf "@" (trim s)) (drop 2)             "install.sql"
    "undo"         -> commitDB       authDB pathIssue (\s -> isInfixOf "@./UNDO" s)   (dropWhile (/= '/')) "undo.sql"
    _              -> (putStrLn $ "Undefined command " ++ action) >>
                       putStrLn "Use create, create_runme, save, install, undo, puttorc, merge or ..."


commitDB :: String -> Path -> (String -> Bool) -> (String -> String) -> File -> IO()
commitDB authDB pathIssue condition rmSome sqlFile = do
    createTempIfNotExist
    executeURLs <- getRelationURLs (pathIssue </> "sysbee_runme.sql") >>= prefixToRelationURLs
    writeFile toSqlFile (addExitAndUnlines executeURLs)
    execSQL [authDB, pathSqlFile]
  where
    pathSqlFile          = "@" ++ temp </> sqlFile
    getRelationURLs      = (\x -> filter condition . lines <$> readFile x)
    prefixToRelationURLs = (\x -> return $ (("@" ++ pathIssue) ++) . toLeftSlash . rmSome <$> x)
    addExitAndUnlines    = (\urls -> unlines (urls ++ ["exit", "exit"]))
    toSqlFile            = temp </> sqlFile
    createTempIfNotExist = doesDirectoryExist temp >>= (\x -> if x then return () else createDirectory temp)
    temp                 = "temp"


putToRC :: Path -> URL -> IO()
putToRC pathIssue svnIssueRC = do
    (_, Just hOut, _, _) <- createProcess (proc "svn" ["info", pathIssue]){ std_out = CreatePipe }
    branchURL            <- cutBranchURL <$> hGetContents hOut            -- найти строку с branch URL
    execSVN ["copy", branchURL, svnIssueRC, "-m \"\""]
    hClose hOut
  where
    cutBranchURL = dropWhile (\c -> c /= 'h') . concat . filter (\s -> take 4 (trim s) == "URL:") . lines


merge :: URL -> URL -> Path -> String -> IO()
merge svnTrunk hostURL pathHOSTPRP foldersToMerge = do
    getAvailableToMerge pathHOSTPRP foldersToMerge >>= mergeAvailableDir
  where
    svnTrunkHost       = svnTrunk ++ "/" ++ hostURL ++ "/"
    mergeAvailableDir  = mapM_ (mergeDir svnTrunkHost pathHOSTPRP)
    getAvailableToMerge =
        (\path folders  -> filter (`elem` foldersToMergeToList folders) <$> getDirectoryContents path)
      where
        foldersToMergeToList = (trim <$>) . parseDelimStrL ','


mergeDir :: URL -> Path -> String -> IO()
mergeDir svnTrunkHost pathHOSTPRP dirInHOSTPRP = do
        execSVN ["checkout", svnTrunkHost ++ dirInHOSTPRP, tempPath, "--depth", "empty"]
        filesToMerge <- getFilesToMerge
        downloadSvnTrunkFiles filesToMerge
        copyFilesFromHostToTemp filesToMerge
        execSVN ["add",    tempPath </> "*", "--force"]
        execSVN ["commit", tempPath,         "-m \"\""]
    where
        pathTodirInHOSTPRP      = pathHOSTPRP </> dirInHOSTPRP
        getFilesToMerge         = listDirectory pathTodirInHOSTPRP
        downloadSvnTrunkFiles   = mapM_ (\file -> execSVN ["update", tempPath </> file])
        copyFilesFromHostToTemp = mapM_ (\file -> copyFile (pathTodirInHOSTPRP </> file) (tempPath </> file))
        tempPath                = "temp\\temp_" ++ dirInHOSTPRP

save :: Issue -> Path -> URL -> URL -> IO()
save issue pathIssue hostURL undoURL = do
      execSVN ["add",    pathIssue </> "sysbee_runme.sql"]
      execSVN ["add",    allHostPath, "--force"]
      execSVN ["add",    allUndoPath, "--force"]
      execSVN ["commit", pathIssue,   "-m \"\""]
  where
      allHostPath = pathIssue </> (toLeftSlash hostURL) </> "*"
      allUndoPath = pathIssue </> (toLeftSlash undoURL) </> "*"

create :: Issue -> String -> String -> Path -> [String] -> URL -> URL -> ExtAList -> IO()
create issue svnBranches svnTrunk pathIssue args hostURL undoURL extAList = do
    svnIssueBranch <- date_issue >>= composeSvnIssueBranch                         -- сформировать путь к новой ветке
    execSVN ["mkdir",    svnIssueBranch, "-m \"" ++ issue ++ "\""]                    -- создать ветку в branch
    crIssueBranch        svnTrunk svnIssueBranch hostURL undoURL args extAList            -- создать структуру в новой ветке и скопировать нужные файлы
    execSVN ["checkout", svnIssueBranch, pathIssue]                                 -- checkout (выгрузка из SVN в рабочий каталог)
    createRunMeSQL       issue pathIssue hostURL undoURL                                 -- создать файл sysbee_runme.sql
  where
    date_issue            = formatTime defaultTimeLocale ("%Y%m%d_" ++ issue) <$> getZonedTime -- имя ветки в branch - yyyymmdd_issue
    composeSvnIssueBranch = (\dt -> return $ svnBranches ++ "/" ++  dt)

propsToList :: [(String, String)] -> [String]
propsToList al = conv <$> propsListTemplate
  where
    conv = (\n ->
              case (lookup n al) of
                Just v -> v
                _      -> ""
            )


createRunMeSQL :: Issue -> Path -> URL -> URL -> IO()
createRunMeSQL issue pathIssue hostURL undoURL = do
      (_, Just hOut, _, _) <- createProcess (proc "svn" ["info", pathIssue]){ std_out = CreatePipe }
      res <- concat . filter (\s -> take 19 s == "Last Changed Author") . lines <$> hGetContents hOut -- найти строку с автором
      dirsInHOSTPRP <- listDirectory pathHOSTPRP
      dirsInUNDO    <- listDirectory pathUNDO
      let
        author = if res == "" then "uknown" else drop 21 res                      -- если найдена, то «вырезать» автора
        changeINI str = case str of
                          "-- TaskName:"    -> return $ str ++ " " ++ issue
                          "-- Created by:"  -> createdBy str author
                          "files1"          -> unlines <$> prefix1 <$> relURLFiles pathHOSTPRP dirsInHOSTPRP
                          "files2"          -> unlines <$> prefix2 <$> relURLFiles pathHOSTPRP dirsInHOSTPRP
                          "files3"          -> unlines <$> prefix3 <$> relURLFiles pathUNDO    dirsInUNDO
                          _                 -> return str
      content      <- readFile "conf\\sysbee_runme.sql"
      runmeContent <- unlines <$> (mapM changeINI $ lines content)
      writeFile (pathIssue </> "sysbee_runme.sql") runmeContent
      hClose hOut
  where
      mCrDate = formatTime defaultTimeLocale "%d.%m.%Y %T" <$> getZonedTime -- дата создания ветки в branch (monad)
      pathHOSTPRP = pathIssue </> (toLeftSlash hostURL)
      pathUNDO    = pathIssue </> (toLeftSlash undoURL)
      createdBy   = (\str author -> ((mconcat [str, " ", author, " on "]) ++) <$> mCrDate)
      prefix1     = ((("-- ./"  ++ hostURL)    ++)  <$>)
      prefix2     = ((("@./"    ++ hostURL)    ++)  <$>)
      prefix3     = ((("-- @./" ++ undoURL)    ++)  <$>)



relURLFiles :: Path -> [Folder] -> IO [String]
relURLFiles path [] = return []
relURLFiles path (dir:dirRest) = do
      filesURL     <- runPrefix <$> listDirectory (path </> dir)
      restFilesURL <- relURLFiles path dirRest
      return $ filesURL ++ restFilesURL
  where
      runPrefix = ((("/" ++ dir ++ "/") ++) <$>)


crIssueBranch :: String -> String -> URL -> URL -> [Folder] -> ExtAList -> IO()
crIssueBranch svnTrunk svnIssueBranch hostURL undoURL args extAList =
  do
    putStrLn "Begin of creating issue branch"
    mapM_ create args
    putStrLn "End of creating issue branch"
  where
    create :: String -> IO()
    create arg =
      do
        putStrLn $ "Create folder " ++ folder ++ " ::"
        mkURL hostURL
        mkURL undoURL
        copyModules svnTrunk svnIssueBranch hostURL undoURL folder modules extAList
      where
        (folder:modules) = concat $ parseDelimStrL ',' <$> parseDelimStrL '=' arg
        mkURL            = (\rPath -> execSVN ["mkdir"
                                             , trim $ mconcat [svnIssueBranch, "/", rPath, "/", folder], "--parents", "-m \"\""])

-- копирование файлов-модулей (пакеты, триггеры, процедуры...) из транка в новую ветку в branches
copyModules :: String -> String -> URL -> URL -> Folder -> [File] -> ExtAList -> IO()
copyModules _ _ _ _ _ [] _ = putStrLn "End of copying modules from trunk to issue branch."
copyModules svnTrunk svnIssueBranch hostURL undoURL folder (file:rest) extAList =
  do
    execSVN $ argsForCopySvn hostURL
    execSVN $ argsForCopySvn undoURL
    copyModules svnTrunk svnIssueBranch hostURL undoURL folder rest  extAList
  where
    argsForCopySvn = (\rPath -> ["copy"
                                 , mconcat [svnTrunk,  "/", hostURL, "/", folder, "/", file, ext]
                                 , mconcat [svnIssueBranch, "/", rPath, "/", folder]
                                 , "-m \"\""])
    ext            = findExtByFolder folder extAList


findExtByFolder :: Folder -> ExtAList -> Extension
findExtByFolder f al =
  case lookup f al of
    Just ext -> ext
    _         -> ""


execSVN :: [String] -> IO()
execSVN args = execCmd "svn" args

execSQL :: [String] -> IO()
execSQL args = execCmd "sqlplus" args

execCmd :: String -> [String] -> IO()
execCmd cmd args = do
                (_, Just hOut, _, _) <- createProcess (proc cmd args){ std_out = CreatePipe }
                hGetContents hOut  >>=  putStrLn
                hClose hOut


rmTemp :: IO()
rmTemp = removePathForcibly "temp"


propsListTemplate = ["database"
                   , "user"
                   , "pass"
                   , "hostURL"
                   , "undoURL"
                   , "svnBranches"
                   , "svnTrunk"
                   , "svnRC"
                   , "checkoutPath"
                   , "postfix"
                   , "extByFolder"
                   , "foldersToMerge"]

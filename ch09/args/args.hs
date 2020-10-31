import Data.List
import System.Directory
import System.Environment
import System.IO

-- main = do
--   args <- getArgs
--   progName <- getProgName
--   putStrLn "The arguments are:"
--   mapM putStrLn args
--   putStrLn "The program name is:"
--   putStrLn progName

dispatch :: [(String, [String] -> IO ())]
dispatch =
  [ ("add", add),
    ("view", view),
    ("remove", remove)
  ]

main = do
  (command : args) <- getArgs
  let (Just action) = lookup command dispatch
  action args

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
  putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  handle <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let number = read numberString
      todoTasks = lines contents
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName
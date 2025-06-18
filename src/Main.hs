module Main where
import Data.Time 
import Text.Read (readMaybe)
import System.IO (hFlush, stdout)
import Data.List.Split
import Control.Exception (try, SomeException)


main :: IO ()
main = do
  putStrLn "Welcome to my TODO List Manager!"
  taskList <- readTasks savedTasks
  loop taskList

loop :: [Task] -> IO ()
loop taskList = do
  putStr "Enter command: "
  hFlush stdout
  input <- getLine
  isLooping <- handleInput input taskList
  if (fst isLooping)
    then loop (snd isLooping)
    else return ()

handleInput :: String->[Task]-> IO (Bool,[Task])

handleInput "exit" taskList = do
  writeTasks taskList savedTasks
  putStrLn "Goodbye!"
  pure (False,taskList)

handleInput "--help" taskList = do
  putStrLn  "Commands Available: "
  putStrLn  "add"
  putStrLn  "delete"
  putStrLn  "view"
  putStrLn  "update"
  putStrLn  "exit"
  pure (True,taskList)

handleInput "add" taskList = do
  newTask <- getTask
  case newTask of 
    Left message -> do
                    putStrLn message
                    pure (True,taskList)
    Right task ->  do
                   putStrLn "Added new Task: " 
                   putStrLn (show task)
                   pure (True,task : taskList)

handleInput "view" taskList = do
  mapM_ (putStrLn . printTask) taskList
  pure (True,taskList)

handleInput "delete" taskList = do
  putStrLn "Enter description of the task to delete."
  desc <- getLine
  let newList = filter (\task -> (/=) (getDesc task) desc) taskList
  putStrLn "Task removed"
  pure (True,newList)

handleInput "update" taskList = do
  putStrLn "Enter 'modify' to change task description ,enter 'priority' to change task priority,enter 'status' to mark a task as completed."
  cmd <- getLine
  case cmd of 
    "modify" -> do
      putStrLn "Enter previous task description"
      prev <- getLine
      putStrLn "Enter new description"
      new <- getLine
      let newList = modifyList prev new taskList
      putStrLn "Description Updated"
      pure (True,newList)
    "priority" -> do 
      putStrLn "Enter task description"
      desc <- getLine
      putStrLn "Enter new priority - High/Medium/Low"
      priority <- getLine
      case readMaybe priority ::Maybe Priority of
          Nothing -> do
            putStrLn "Invalid priority entered"
            pure (True,taskList)
          Just newPriority -> do
            putStrLn $ ("Updated task priority to " ++ priority)
            pure (True,updatePriority desc newPriority taskList)
    "status" -> do 
      putStrLn "Enter task description"
      desc <- getLine
      putStrLn "Task Marked Complete."
      pure (True,updateStatus desc taskList)
    
    _ -> do
      putStrLn "Invalid update command"
      pure (True,taskList)

handleInput _ taskList = 
  do
    putStrLn "Invalid Command , enter '--help' to see valid options"
    pure (True,taskList)

------------------------------------------------------------------------------
data Priority =   High
                | Medium
                | Low
                deriving (Show,Read)
data Status = Complete 
            | Pending
            deriving (Show,Read)

type DueDate = Day 

data Task = NewTask String DueDate Status Priority
            deriving (Show,Read)

printTask :: Task -> String
printTask (NewTask desc date status priority) =
  ("Description: " <> desc <> " | ") <>
  ("Due date: " <> show date <> " | ") <>
  ("Status: " <> show status <> " | ") <>
  ("Priority: " <> show priority <> "\n")



getDesc::Task -> String 
getDesc (NewTask desc _ _ _) = desc 

modifyList::String -> String -> [Task] -> [Task]
modifyList prevDesc newDesc [] = []
modifyList prevDesc newDesc ((NewTask desc d s p):xs) =
  if (==) prevDesc desc 
    then (NewTask newDesc d s p):xs
  else
    modifyList prevDesc newDesc xs 
--------------------------------------------------------------------------
updateStatus::String -> [Task] -> [Task]
updateStatus desc [] = []
updateStatus desc ((NewTask description d s p):xs) =
   if (==) description desc 
    then (NewTask desc d Complete p):xs
  else
    updateStatus desc xs
--------------------------------------------------------------------------
updatePriority::String -> Priority -> [Task] -> [Task]
updatePriority prevDesc newPriority [] = []
updatePriority prevDesc newPriority ((NewTask desc d s p):xs) =
   if (==) prevDesc desc 
    then (NewTask desc d s newPriority):xs
  else
    updatePriority prevDesc newPriority xs
--------------------------------------------------------------------------


buildTask :: String -> DueDate -> Priority -> IO Task
buildTask description date priority = do
    currTime <- getCurrentTime
    let today = utctDay currTime
    if date < today
      then error "Please enter a valid date"
      else return (NewTask description date Pending priority)
getTask :: IO (Either String Task)
getTask = do
  putStrLn ("Enter Task Description")
  description <- getLine
  putStrLn ("Enter Task DueDate in format yyyy-mm-dd")
  d <- getLine
  case readMaybe d :: Maybe Day of
    Nothing -> return $ Left "Invalid date format. Please use yyyy-mm-dd."
    Just date -> do
      currTime <- getCurrentTime
      let today = utctDay currTime
      if date < today
        then return $ Left "Due Date cannot be in the past. Please enter a valid date"
      else do 
        putStrLn "Enter priority - High,Medium or Low"
        p <- getLine
        case readMaybe p ::Maybe Priority of
          Nothing -> return $ Left "Invalid priority entered."
          Just priority -> return $ Right (NewTask description date Pending priority)
      
todoList::[Task]
todoList = []

readFromFile:: FilePath -> IO String 
readFromFile path = do
                    x <- readFile path 
                    return x
savedTasks = "saved_tasks"

writeToFile:: FilePath -> String -> IO ()
writeToFile path task = do 
                         a <- writeFile path task
                         return a

--a = show (NewTask "abc" (fromGregorian 2021 12 12) Pending High)
--b = read a :: Task  
-- Write a function to convert newline seperated Task representation to [Task]
readTasks :: FilePath -> IO [Task]
readTasks file = do
   ts <- try $ readFile file ::IO (Either SomeException String) 
   case ts of 
    Left err -> return []
    Right val ->
      let taskListString = splitOn "\n" val
      in 
      return $ map read taskListString

writeTasks :: [Task] -> FilePath -> IO ()
writeTasks [] path = return ()
writeTasks tasks path = do
                         writeFile path $ init (unlines (map show tasks))
tasks = [(NewTask "grocery" (fromGregorian 2021 12 12) Pending High ) ,
         (NewTask "laundry" (fromGregorian 2021 12 6) Pending High )]
module Main where
import Data.Time 
import Text.Read (readMaybe)
import System.IO (hFlush, stdout)
import Data.List.Split
import Control.Exception (try, SomeException)
import Data.Ord
import Data.List 



main :: IO ()
main = do
  putStrLn "Welcome to TODO List Manager!"
  putStrLn "Enter '--help' to see commands available"
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
  putStrLn  "view (shows all tasks)"
  putStrLn  "view-priority (shows incomplete tasks sorted by date and priority)"
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
                   putStrLn (printTask task)
                   pure (True,task : taskList)

handleInput "view" taskList = do
  case taskList of
    [] -> do
      putStrLn "No tasks to show !"
      pure (True,taskList)
    _ -> do
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
      let (changed,newList) = modifyList prev new taskList
      case changed of 
        True -> do
          putStrLn "Description update successfully."
          pure (True,newList)
        False -> do
          putStrLn "No task matches description entered."
          pure (True,taskList)

      
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
           
            let (changed,newList) = updatePriority desc newPriority taskList
            case changed of 
              True -> do
                putStrLn $ "Priority Update successfully to " <> (show newPriority)
                pure (True,newList)
              False -> do
                putStrLn "No task matches description entered."
                pure (True,taskList)

    "status" -> do 
      putStrLn "Enter task description"
      desc <- getLine
      let (changed,output) =  updateStatus desc taskList
      case changed of 
        True -> do
          putStrLn "Task Marked Complete."
          pure (True,output)
        False -> do
          putStrLn "No task matches description entered."
          pure (True,taskList)
    _ -> do
      putStrLn "Invalid update command"
      pure (True,taskList)

handleInput "view-priority" taskList = do
  let pendingList = filter (\task -> getStatus task == Pending) taskList
      sortedList = sortBy compareTasks pendingList
  mapM_ (putStrLn . printTask) sortedList
  pure (True, taskList)


   
handleInput _ taskList = 
  do
    putStrLn "Invalid Command , enter '--help' to see valid options"
    pure (True,taskList)

------------------------------------------------------------------------------
data Priority =   High
                | Medium
                | Low
                deriving (Show,Read,Eq)
data Status = Complete 
            | Pending
            deriving (Show,Read,Eq)

type DueDate = Day 

data Task = NewTask String DueDate Status Priority
            deriving (Show,Read,Eq)

printTask :: Task -> String
printTask (NewTask desc date status priority) =
  ("Description: " <> desc <> " | ") <>
  ("Due date: " <> show date <> " | ") <>
  ("Status: " <> show status <> " | ") <>
  ("Priority: " <> show priority <> "\n")



getDesc::Task -> String 
getDesc (NewTask desc _ _ _) = desc 

modifyList::String -> String -> [Task] -> (Bool,[Task])
modifyList prevDesc newDesc [] = (False,[])
modifyList prevDesc newDesc ((NewTask desc d s p):xs) =
  if (==) prevDesc desc 
    then (True,(NewTask newDesc d s p):xs)
  else
    let (changed,tail_val) = modifyList prevDesc newDesc xs 
    in
     (False || changed,(NewTask desc d s p):tail_val)
--------------------------------------------------------------------------
updateStatus::String -> [Task] -> (Bool,[Task])
updateStatus desc [] = (False,[])
updateStatus desc ((NewTask description d s p):xs) =
   if (==) description desc 
    then (True,(NewTask desc d Complete p):xs)
  else
    let (changed,tail_val) = updateStatus desc xs 
    in
      (False || changed,(NewTask description d s p):tail_val)
--------------------------------------------------------------------------
updatePriority::String -> Priority -> [Task] -> (Bool,[Task])
updatePriority prevDesc newPriority [] = (False,[])
updatePriority prevDesc newPriority ((NewTask desc d s p):xs) =
   if (==) prevDesc desc 
    then (True,(NewTask desc d s newPriority):xs)
  else
    let (changed,tail_list) = updatePriority prevDesc newPriority xs
    in
      (False || changed,(NewTask desc d s p):tail_list)
    
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

-- Show Incomplete - 1 , 2 - Show Incomplete sorted by due date and priority
instance Ord Priority where
  Low <= Medium = True
  Low <= High = True
  Medium <= High = True 
  _ <= _ = False 

compareTasks :: Task -> Task -> Ordering
compareTasks t1 t2 = 
  comparing getDueDate t1 t2 <> comparing (Down . getPriority) t1 t2

getDueDate :: Task -> Day
getDueDate (NewTask _ dueDate _ _) = dueDate

getPriority :: Task -> Priority
getPriority (NewTask _ _ _ priority) = priority 

getStatus :: Task -> Status
getStatus (NewTask _ _ status _) = status  

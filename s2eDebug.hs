import System.Environment
import Text.Regex.Posix
import Data.List

fileWithoutInit :: [String] -> [String]
fileWithoutInit = dropWhile (/= "0 [State 0] Created initial state")

-- Finds which state a current line refers to.
getState :: String -> String -> String
getState line prevState = if ((line =~ "(\\[State [0-9]+\\])"))
  then (init . tail) (line =~ "\\[State [0-9]+\\]" :: String)
  else prevState

-- Filters lines where the forks occour
filterForks :: [String] -> [String]
filterForks = filter $ isInfixOf "Forking state"

-- Grabs hexval addresses from a string
getAddresses :: String -> [String]
getAddresses l = getAllTextMatches $ l =~ "0x[a-f0-9]+" :: [String]

-- Pattern for matching state chunks
stateSplit :: String -> [String]
stateSplit f = getAllTextMatches $ f =~ "[0-9] \\[State [0-9]+\\] .*" :: [String]

toCSV :: String -> String
toCSV s = getState s "" ++ ", " ++ (intercalate ", " (getAddresses s))

createCSVFile :: [String] -> String
createCSVFile = intercalate "\n"

main = do
  args <- getArgs
  let path = args!!0
  file <- readFile path
  let splitFile = stateSplit file
  let forksOnly = createCSVFile $ map toCSV $ (filterForks) splitFile
  writeFile (args!!1) forksOnly 

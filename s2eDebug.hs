import System.Environment
import Text.Regex.PCRE
import Data.List
import Data.Tree
import Data.Maybe
import qualified Data.Map.Strict as M

data State = State {number :: Int,
                    constraints :: String,
                    pluginOutput :: [String]}

type STree = Tree State

-- Used to organise the debug log into statewise chunks
type SMap = M.Map String [String]

-- Stores the fork counts at at an address
type FCMap = M.Map String Int

-- Drops the initialisation sequence
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

-- Filters lines where the forks occour
filterCalls :: [String] -> [String]
filterCalls = filter $ isInfixOf "Called from address"

-- Filters lines where the dead ends are inserted
filterDeadEndInsertions :: [String] -> [String]
filterDeadEndInsertions = filter $ isInfixOf "Inserting dead end at"

-- Filters lines where the dead ends are inserted
findTestCases :: [String] -> [String]
findTestCases = filter $ isInfixOf "(string)"

-- Grabs hexval addresses from a string
getAddresses :: String -> [String]
getAddresses l = getAllTextMatches $ l =~ "0x[a-f0-9]+" :: [String]

-- Pattern for matching state chunks
stateSplit :: String -> [String]
stateSplit f = getAllTextMatches $ f =~ "[0-9] \\[State [0-9]+\\] .*" :: [String]

-- Groups the states. TODO: Modify to cover lines with no embedded state info.
groupStates :: [String] -> SMap -> SMap
groupStates [] m = m
groupStates (x:xs) m = groupStates xs $ M.insertWith (flip (++)) (getState x "") [x] m

-- Converts to comma separated values
toCSV :: String -> String
toCSV s = getState s "" ++ ", " ++ (intercalate ", " (getAddresses s))

-- Horrible hack, to replace
createNubbedCSV :: [String] -> String
createNubbedCSV s = intercalate "\n" $ (nubBy (\a b -> (getAddresses a)!!0 == (getAddresses b)!!0)) s

createCSVFile :: [String] -> String
createCSVFile = intercalate "\n"

-- Counts the number of forks at a particular address.
getForkCount :: [String] -> FCMap -> FCMap
getForkCount [] m = m
getForkCount (x:xs) m = getForkCount xs $ M.insertWith (+) (getAddresses x!!0) 1 m

main = do
  args <- getArgs
  let path = args!!0
  file <- readFile path
  let splitFile = stateSplit file
  let forksOnly = createNubbedCSV (map toCSV $ filterForks splitFile)
  let states = groupStates splitFile M.empty
  writeFile (args!!1) (intercalate "\n" $ findTestCases splitFile) 

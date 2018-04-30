import System.Environment
import Text.Regex.PCRE
import Data.List
import Data.Tree
import Data.Maybe
import System.FilePath.Posix
import Data.Elf
import qualified Data.Map.Strict as M

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import DWARF.Basics
import DWARF.Addr2Line

-- Used to organise the debug log into statewise chunks
type SMap = M.Map String [String]

-- Stores the fork counts at at an address
type FCMap = M.Map String Int

-- Copied from the Test.hs file in dwarf-tools
fromElf :: ByteString -> Sections
fromElf bs = sections end
           $ M.fromList [ (name, elfSectionData s)
                          | s <- elfSections elf
                          , let name = elfSectionName s
                          , ".debug_" `isPrefixOf` name ]
  where
  elf  = parseElf bs
  end  = case elfData elf of
           ELFDATA2LSB -> LittleEndian
           ELFDATA2MSB -> BigEndian

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

-- Filters lines where the function calls occour
filterCalls :: [String] -> [String]
filterCalls = filter $ isInfixOf "Called from address"

-- Filters lines where the dead ends are inserted
filterDeadEndInsertions :: [String] -> [String]
filterDeadEndInsertions = filter $ isInfixOf "Inserting dead end at"

-- Finds the lines where the test cases are
findTestCases :: [String] -> [String]
findTestCases = filter $ isInfixOf "(string)"

-- Grabs hexval addresses from a string
getAddresses :: String -> [String]
getAddresses l = getAllTextMatches $ l =~ "0x[a-f0-9]+" :: [String]

-- Pattern for matching state chunks
stateSplit :: String -> [String]
stateSplit f = getAllTextMatches $ f =~ "[0-9]+ \\[State [0-9]+\\] .*" :: [String]

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

getStatus :: [String] -> [String]
getStatus = filter $ isInfixOf "status"

-- Counts the number of forks at a particular address.
getForkCount :: [String] -> FCMap -> FCMap
getForkCount [] m = m
getForkCount (x:xs) m = getForkCount xs $ M.insertWith (+) (getAddresses x!!0) 1 m

getStatusCount :: [String] ->  FCMap -> FCMap
getStatusCount [] m = m
getStatusCount (x:xs) m = getStatusCount xs $ M.insertWith (+) x 1 m

getStatus2 :: [String] -> [String]
getStatus2 = filter $ isInfixOf "with exit code"

getStatusCount2 :: [String] -> FCMap -> FCMap
getStatusCount2 [] m = m
getStatusCount2 (x:xs) m = getStatusCount2 xs $ M.insertWith (+) (findExitCode x) 1 m

-- Eww
findExitCode :: String -> String
findExitCode x = init $ last $ words x

-- Parses commands. Takes the debug file split into lines, and the binary
-- in the Sections format specified by dwarf-tools
-- TODO: Move the links to a map somewhere and lookup maybe?
parseCommand :: String -> Sections -> String -> String
parseCommand debugFile binary x = case x of
  "getForks" -> getForks (stateSplit debugFile) binary
  "findTestCases" -> intercalate "\n" $ findTestCases (stateSplit debugFile)
  "getDeadEnds" -> getDeadEnds (stateSplit debugFile) binary
  "countForks" -> countForks (stateSplit debugFile) binary
  "getStatus" -> intercalate "\n" $ getStatus (lines debugFile)
  "countStatus" -> countStatus $ getStatus $ lines debugFile
  "countStatus2" -> countStatus2 $ getStatus2 $ lines debugFile
  _ -> "Undefined Command. Check parseCommand in s2eDebug.hs for available options"

countStatus2 :: [String] -> String
countStatus2 debugFile = M.foldlWithKey ppStatus "" $ getStatusCount2 debugFile M.empty

countStatus :: [String] -> String
countStatus debugFile = M.foldlWithKey ppStatus "" $ getStatusCount debugFile M.empty

ppStatus :: String -> String -> Int -> String
ppStatus prev curr count = prev ++ "\n" ++ (dropWhile (== ' ') curr) ++ ": " ++ show count

countForks :: [String] -> Sections -> String
countForks debugFile binary = M.foldlWithKey (ppFork binary) "" forkMap
  where
    forkMap = getForkCount (filterForks $ debugFile) M.empty

ppFork :: Sections -> String -> String -> Int -> String
ppFork binary prev addr count = prev ++ "\n" ++ addr ++ ": In " ++ fcn ++
  " at line " ++ lin ++ ": " ++ show count
  where
    inf = addr2line binary (read addr)
    fcn = show $ fromJust $ function inf
    lin = show $ fromJust $ line inf

getDeadEnds :: [String] -> Sections -> String
getDeadEnds debugFile binary = intercalate "\n" $ map (getDeadEnd binary) (filterDeadEndInsertions debugFile)

getDeadEnd :: Sections -> String -> String
getDeadEnd binary l = (getState l "") ++ ": Dead End " ++ addr ++ " in line "
  ++ lin ++ " in function " ++ fcn
  where
    addr = (getAddresses l)!!0
    inf = addr2line binary (read addr)
    fcn = show $ fromJust $ function inf
    lin = show $ fromJust $ line inf

getForks :: [String] -> Sections -> String
getForks debugFile binary = intercalate "\n" $ map (getFork binary) (filterForks debugFile)

getFork :: Sections -> String -> String
getFork binary l = (getState l "") ++ ": Fork at " ++ addr ++ " in line "
  ++ lin ++ " in function " ++ fcn
  where
    addr = (getAddresses l)!!0
    inf = addr2line binary (read addr)
    fcn = show $ fromJust $ function inf
    lin = show $ fromJust $ line inf

getOptional :: String -> [String] -> (String -> a) -> a -> a
getOptional option args fcn default_ =
    let index = option `elemIndex` args in
    if index == Nothing then
        default_
    else
       fcn (args!!(fromJust index+1))

-- Usage ./s2eDebug /path/to/s2e/project/ command -d s2e-out-x/ -o outfile.txt
-- Defaults to s2e-last if no debug output directory is specified.
-- Writes to standard output if no outfile is specified.
-- Assumes that the project directory name is name of binary
main :: IO ()
main = do
  args <- getArgs
  let projectDir = (args!!0)
      command = (args!!1)
      outDir = getOptional "-d" args id "s2e-last"
      file = projectDir </> ((last . splitDirectories) projectDir)
      writer = getOptional "-o" args writeFile putStrLn
  debugFile <- readFile $ projectDir </> outDir </> "debug.txt"
  binary <- fromElf <$> BS.readFile file
  writer $ parseCommand debugFile binary command

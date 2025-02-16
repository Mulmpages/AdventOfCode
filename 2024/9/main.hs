import Data.Void (Void)
import Text.Megaparsec ( parseMaybe, some, Parsec, optional )
import Text.Megaparsec.Char ( digitChar, newline )
import Text.Megaparsec.Char.Lexer as L ( decimal )
import Data.Char ( digitToInt )

-- Parsing
type Parser = Parsec Void String

type DiskMap = [Int]

parseDiskmap :: Parser DiskMap
parseDiskmap = some (digitToInt <$> digitChar) <* optional newline

getInput :: String -> IO ( DiskMap )
getInput str = do
    file <- readFile str
    return $ case parseMaybe parseDiskmap file of
        Nothing -> undefined
        Just x -> x

-- Solve 1

-- Idea: We will keep working with a compressed representation of the file system
-- However, we will keep slightly more information per file
type ID = Int
data Range = Range
    { start :: Int
    , end   :: Int
    } deriving (Eq, Show)
data File = Space Range | File Range ID deriving (Eq, Show)
type FileSys = [File]

getID :: File -> ID
getID (File _ id) = id

isEmpty :: File -> Bool
isEmpty (Space _) = True
isEmpty _ = False

diskmapToFilesystem :: DiskMap -> FileSys
diskmapToFilesystem diskmap = dTFs False 0 0 diskmap
    where
        dTFs :: Bool -> Int -> Int -> DiskMap -> [File]
        dTFs _ _ _ [] = []
        dTFs free pos id (l:ls) = file : (dTFs (not free) (pos + l) id' ls)
            where
                range = Range pos (pos + l - 1)
                file = if free then Space range else File range id
                id' = if free then id else id + 1

moveBlock :: FileSys -> FileSys
moveBlock []                       = []
moveBlock [x]                      = [x]
moveBlock [Space r1, File r2 id]   = [File r1 id, Space r2]
moveBlock (File r id         : fs) = File r id : moveBlock fs
moveBlock (Space (Range a b) : fs) = newBlock : moveBlock (shrunkFirst ++ fsWithLastDeminishedOrDeletedAndSpaceExpanded)
    where
        lastFile = find (not isEmpty) (reverse fs)
        newBlock = case lastFile of
            Nothing -> undefined
            Just (File _ id) -> File (Range a a) id
        shrunkFirst = if b > a + 1 then [Space $ Range (a + 1) b] else []

        remainder = case reverse fs of
            [] -> []
            ()


        -- remainder = case last fs of
        --     Space (Range x y) -> [Space $ Range (x - 1) y]
        --     File  (Range x y) id -> if y > x + 1
        --         then []
        --         else [File (Range x (y - 1)) id, Space $ Range y y]




-- diskmapToDisk :: [Int] -> [Block]
-- diskmapToDisk = diskmapToDisk' True 0
--     where
--         diskmapToDisk' :: Bool -> Int -> [Int] -> [Block]
--         diskmapToDisk' _ _ [] = []
--         diskmapToDisk' False i (x:xs) = replicate x None ++ diskmapToDisk' True i xs
--         diskmapToDisk' True i (x:xs) = replicate x (Data i) ++ diskmapToDisk' False (i + 1) xs

-- compact :: [Block] -> [Block]
-- compact [] = []
-- compact (Data x : xs) = (Data x) : compact xs
-- compact (None : xs) = last xs : (compact . strip) (init xs)
--     where
--         strip = reverse . dropWhile (==None) . reverse

-- checksum :: [Block] -> Int
-- checksum = checksum' 0
--     where
--         checksum' :: Int -> [Block] -> Int
--         checksum' _ [] = 0
--         checksum' i (None : xs) = checksum' (i + 1) xs
--         checksum' i (Data x : xs) = i * x + checksum' (i + 1) xs

-- solve1 :: [Int] -> Int
-- solve1 = checksum . compact . diskmapToDisk

-- -- Solve 2


-- main :: IO ()
-- main = do
--     start <- getInput "input"
--     putStr "Solution 1: "
--     putStrLn $ show $ solve1 start
-- --     putStr "Solution 2: "
-- --     putStrLn $ show $ solve2 start
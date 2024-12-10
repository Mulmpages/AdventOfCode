import Data.Void (Void)
import Text.Megaparsec ( optional, parseMaybe, some, Parsec )
import Text.Megaparsec.Char ( hspace1, newline )
import Text.Megaparsec.Char.Lexer as L ( decimal )
import Data.List (subsequences)

-- Parsing
type Parser = Parsec Void String
type Level = Int
type Report = [Level]

parseReport :: Parser Report
parseReport = some (L.decimal <* optional hspace1)

parseReports :: Parser [Report]
parseReports = some (parseReport <* optional newline)

getReports :: String -> IO ( [Report] )
getReports str = do
    file <- readFile str
    return $ case parseMaybe parseReports file of
        Nothing -> []
        Just x -> x

-- Solve 1
map2 :: (a -> a -> b) -> [a] -> [b]
map2 _ [] = []
map2 _ [x] = []
map2 f (a:b:cs) = f a b : map2 f (b:cs)

safeInc :: Int -> Bool
safeInc x = x > 0 && x < 4

safeDec :: Int -> Bool
safeDec x = x < 0 && x > -4

isSafe :: Report -> Bool
isSafe rs = (and $ map safeInc diffs) || (and $ map safeDec diffs)
    where
        diffs :: [Int]
        diffs = map2 (\a b -> b - a) rs

countSave :: [Report] -> Int
countSave = length . filter isSafe

-- Solve 2
removeSome :: Int -> [a] -> [[a]]
removeSome len xs = filter (\subs -> length subs == length xs - len) (subsequences xs)

isSaveDampened :: Report -> Bool
isSaveDampened rs = any isSafe (removeSome 1 rs)

countSaveDampened :: [Report] -> Int
countSaveDampened = length . filter isSaveDampened

-- Main
main :: IO ()
main = do
    reports <- getReports "input"
    putStr "Solution 1: "
    putStrLn $ show $ countSave reports
    putStr "Solution 2: "
    putStrLn $ show $ countSaveDampened reports
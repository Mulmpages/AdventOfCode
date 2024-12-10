import Data.Void (Void)
import Text.Megaparsec ( optional, parseMaybe, some, empty, Parsec )
import Text.Megaparsec.Char ( newline, space1 )
import Text.Megaparsec.Char.Lexer as L ( space, decimal )
import Data.List (sort)

-- Parsing
type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

parsePair :: Parser (Int, Int)
parsePair = do
    a <- L.decimal
    sc
    b <- L.decimal
    return (a, b)

parsePairs :: Parser [(Int, Int)]
parsePairs = some (parsePair <* optional newline)

getPairs :: String -> IO ( [(Int, Int)] )
getPairs str = do
    file <- readFile str
    return $ case parseMaybe parsePairs file of
        Nothing -> []
        Just x -> x

-- Solve 1
solve1 :: [Int] -> [Int] -> Int
solve1 as bs = sum $ map distance sortedPairs
    where
        sortedPairs =  zip (sort as) (sort bs)
        distance (a, b) = abs (b - a)

-- Solve 2
solve2 :: [Int] -> [Int] -> Int
solve2 as bs = sum $ map similarity as'
    where
        (as', bs') = (sort as, sort bs)
        similarity a = a * length (filter (==a) bs')

main :: IO ()
main = do
    pairs <- getPairs "input"
    let (as, bs) = unzip pairs
    putStr "Solution 1: "
    putStrLn $ show (solve1 as bs)
    putStr "Solution 2: "
    putStrLn $ show (solve2 as bs)


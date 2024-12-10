import Data.List (findIndex)
import Control.Monad (void)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Parsing
type Rule = (Int, Int)
type Rules = [Rule]
type Update = [Int]
type Updates = [Update]

type Parser = Parsec Void String

parseRule :: Parser Rule
parseRule = do
    x <- L.decimal
    char '|'
    y <- L.decimal
    newline
    return (x, y)

parseRules :: Parser Rules
parseRules = many parseRule

parseUpdate :: Parser Update
parseUpdate = L.decimal `sepBy1` char ',' <* newline

parseUpdates :: Parser Updates
parseUpdates = many parseUpdate

parseInputs :: Parser (Rules, Updates)
parseInputs = do
    os <- parseRules
    newline
    us <- parseUpdates
    return (os, us)

getInputs :: String -> IO ( (Rules, Updates) )
getInputs str = do
    file <- readFile str
    return $ case parseMaybe parseInputs file of
        Nothing -> mempty
        Just x -> x

-- Solve 1
checkHead :: Rules -> Update -> Bool
checkHead rs (u:us) = null $ filter (`elem` neverRight) us
    where
        neverRight = map fst $ filter (\(_,b) -> u == b) rs

checkUpdate :: Rules -> Update -> Bool
checkUpdate _ [] = True
checkUpdate rs (u:us) = checkHead rs (u:us) && checkUpdate rs us

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

middleNums :: Updates -> [Int]
middleNums uss = map middle uss

solve1 :: Rules -> Updates -> Int
solve1 rs uss = sum $ middleNums orderedUpdates
    where
        orderedUpdates = filter (checkUpdate rs) uss

-- Solve 2
findLeftMost :: Rules -> Update -> Int
findLeftMost rs us = case leftMostIndex of
        Nothing -> undefined
        Just i -> us !! i
    where
        relevantRules = filter (\(a,b) -> a `elem` us && b `elem` us) rs

        countRights :: Int -> Int
        countRights x = length $ filter (\r -> x == snd r) relevantRules
        
        countOfRights = map countRights us
        leftMostIndex = findIndex (==0) countOfRights

fixUpdate :: Rules -> Update -> Update
fixUpdate _ [] = []
fixUpdate rs us = leftMost : fixUpdate rs remainder
    where
        leftMost = findLeftMost rs us
        remainder = [u | u <- us, u /= leftMost]

solve2 :: Rules -> Updates -> Int
solve2 rs uss = sum $ middleNums fixedUpdates
    where
        inorderedUpdates = filter (not . checkUpdate rs) uss
        fixedUpdates = map (fixUpdate rs) inorderedUpdates


main :: IO ()
main = do
    (rs, uss) <- getInputs "input"
    putStr "Solution 1: "
    putStrLn $ show $ solve1 rs uss
    putStr "Solution 2: "
    putStrLn $ show $ solve2 rs uss

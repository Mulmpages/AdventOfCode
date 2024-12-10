import Data.Void (Void)
import Text.Megaparsec (parseMaybe, some, empty, Parsec)
import Text.Megaparsec.Char (string, newline, hspace1)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.List (singleton)

-- Parsing
type Parser = Parsec Void String

type Equation = (Int, [Int])

sc :: Parser ()
sc = L.space hspace1 empty empty

parseEquation :: Parser Equation
parseEquation = do
    testValue <- L.decimal
    _ <- string ": "
    nums <- some (L.decimal <* sc)
    return (testValue, nums)

parseEquations :: Parser [Equation]
parseEquations = some (parseEquation <* newline)

getInput :: String -> IO ( [Equation] )
getInput str = do
    file <- readFile str
    return $ case parseMaybe parseEquations file of
        Nothing -> undefined
        Just x -> x

-- Solve 1
type Op = Int -> Int -> Int

ops1 :: [Op]
ops1 = [(+), (*)]

sequences :: [a] -> Int -> [[a]]
sequences _  0 = []
sequences xs 1 = map singleton xs
sequences xs i = concatMap (\s -> map (:s) xs) (sequences xs (i - 1))

reduce :: [Int] -> [Op] -> Int
reduce [x] _ = x
reduce (x:y:zs) (o:os) = reduce (x `o` y : zs) os

possibleResults :: [Op] -> [Int] -> [Int]
possibleResults ops nums = map (reduce nums) possibleOps
    where
        possibleOps = sequences ops (length nums - 1)

achievable :: [Op] -> Equation -> Bool
achievable ops (testValue, nums) = any (==testValue) (possibleResults ops nums)

solve :: [Op] -> [Equation] -> Int
solve ops es = sum $ map fst $ filter (achievable ops) es

solve1 :: [Equation] -> Int
solve1 = solve ops1

-- Solve 2
(|||) :: (Num a, Show a, Read a) => a -> a -> a
a ||| b = read (show a ++ show b)

ops2 :: [Op]
ops2 = (|||) : ops1

solve2 :: [Equation] -> Int
solve2 = solve ops2


main :: IO ()
main = do
    start <- getInput "input"
    putStr "Solution 1: "
    putStrLn $ show $ solve1 start
    putStr "Solution 2: "
    putStrLn $ show $ solve2 start
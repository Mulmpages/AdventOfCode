import Data.Void (Void)
import Text.Megaparsec (optional, parseMaybe, choice, some, Parsec, satisfy)
import Text.Megaparsec.Char (char, newline)
import Data.List (findIndices, nub)

-- Parsing
type Parser = Parsec Void String

data Pos = Pos Int Int deriving (Eq, Ord, Show)
data Content = Clear | Antenna' Char deriving (Show, Eq)
type Map = [[Content]]

isAntenna :: Content -> Bool
isAntenna (Antenna' _) = True
isAntenna _ = False

parseContent :: Parser Content
parseContent = choice
    [ char '.' *> return Clear
    , satisfy (/= '\n') >>= (\c -> return $ Antenna' c)
    ]

parseMap :: Parser Map
parseMap = some (some parseContent <* optional newline)

type Dimension = (Int, Int)
data Antenna = Antenna
    { position  :: Pos
    , name      :: Char
    } deriving (Eq, Ord, Show)
type Antinode = Pos
type Info = (Dimension, [Antenna])

findPos :: (Content -> Bool) -> Map -> [Pos]
findPos cond m = findPos' cond m 0
    where
        findPos' :: (Content -> Bool) -> Map -> Int -> [Pos]
        findPos' _ [] _ = []
        findPos' cond (m:ms) i = map (\c -> Pos c i) (findIndices cond m) ++ findPos' cond ms (i + 1)

mapToInfo :: Map -> Info
mapToInfo m = ((dx, dy), antennas)
    where
        dy = length m
        dx = length $ head m

        antennaPoss = findPos isAntenna m
        antennas = map (\(Pos x y) -> Antenna (Pos x y) (case m !! y !! x of Antenna' c -> c)) antennaPoss

getInput :: String -> IO ( Info )
getInput str = do
    file <- readFile str
    return $ case parseMaybe parseMap file of
        Nothing -> undefined
        Just m -> mapToInfo m

-- Solve 1
isOutside :: Dimension -> Pos -> Bool
isOutside (dx, dy) (Pos x y) = x < 0 || x >= dx || y < 0 || y >= dy

(+^) :: Pos -> Pos -> Pos
Pos x1 y1 +^ Pos x2 y2 = Pos (x1 + x2) (y1 + y2)
(-^) :: Pos -> Pos -> Pos
Pos x1 y1 -^ Pos x2 y2 = Pos (x1 - x2) (y1 - y2)

step1 :: Pos -> Pos -> Antinode -> Antinode
step1 p1 p2 a = a +^ (p1 -^ p2)

step2 :: Pos -> Pos -> Antinode -> Antinode
step2 p1 p2 a = a +^ (p2 -^ p1)

antinodePair :: Dimension -> (Antenna, Antenna) -> [Antinode]
antinodePair dim (Antenna p1 c1, Antenna p2 c2) = 
    if c1 /= c2
        then []
        else p1'' ++ p2''
        where
            p1' = step1 p1 p2 p1
            p2' = step2 p1 p2 p2

            p1'' = if isOutside dim p1' then [] else [p1']
            p2'' = if isOutside dim p2' then [] else [p2']

pairs :: [a] -> [(a, a)]
pairs [x, y] = [(x, y)]
pairs (z:zs) = map ((,) z) zs ++ pairs zs

solve :: Info -> ((Antenna, Antenna) -> [Antinode]) -> Int
solve (dim, antennas) makeAntinodes = length $ nub allAntinodes
    where
        antennaPairs = pairs antennas
        allAntinodes = concatMap makeAntinodes antennaPairs

solve1 :: Info -> Int
solve1 (dim, antennas) = solve (dim, antennas) (antinodePair dim)

-- Solve 2
antinodeRay :: Dimension -> (Antenna, Antenna) -> [Antinode]
antinodeRay dim (Antenna p1 c1, Antenna p2 c2) = 
    if c1 /= c2
        then []
        else ray1' ++ ray2'
        where
            ray1 = iterate (step1 p1 p2) p1
            ray2 = iterate (step2 p1 p2) p2

            ray1' = takeWhile (not . isOutside dim) ray1
            ray2' = takeWhile (not . isOutside dim) ray2

solve2 :: Info -> Int
solve2 (dim, antennas) = solve (dim, antennas) (antinodeRay dim)


main :: IO ()
main = do
    start <- getInput "input"
    putStr "Solution 1: "
    putStrLn $ show $ solve1 start
    putStr "Solution 2: "
    putStrLn $ show $ solve2 start
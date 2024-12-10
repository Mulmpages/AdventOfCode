import Data.Void (Void)
import Text.Megaparsec (optional, parseMaybe, choice, some, Parsec)
import Text.Megaparsec.Char (char, newline)
import Control.Monad.State.Lazy (State, get, put, runState)
import Data.List (nub, findIndices)
import qualified Data.Set as Set

-- Parsing
type Parser = Parsec Void String

type Position = (Int, Int)
data Direction = North | East | South | West deriving (Show, Eq, Enum)
data Content = Clear | Obstacle | Guard Direction deriving (Show, Eq)
type Map = [[Content]]

isGuard :: Content -> Bool
isGuard (Guard _) = True
isGuard _ = False

parseContent :: Parser Content
parseContent = choice
    [ char '.' *> return Clear
    , char '#' *> return Obstacle
    , char '^' *> return (Guard North)
    , char '>' *> return (Guard East)
    , char 'v' *> return (Guard South)
    , char '<' *> return (Guard West)
    ]

parseMap :: Parser Map
parseMap = some (some parseContent <* optional newline)

type Dimension = (Int, Int)
type Obstacles = [Position]
data GuardState = GuardState
    { position  :: Position
    , direction :: Direction
    } deriving (Eq, Show)
type Info = (Dimension, Obstacles, GuardState)

findPos :: (Content -> Bool) -> Map -> [Position]
findPos cond m = findPos' cond m 0
    where
        findPos' :: (Content -> Bool) -> Map -> Int -> [Position]
        findPos' _ [] _ = []
        findPos' cond (m:ms) i = map (\c -> (c, i)) (findIndices cond m) ++ findPos' cond ms (i + 1)

mapToInfo :: Map -> Info
mapToInfo m = ((dx, dy), obstacles, guard)
    where
        dy = length m
        dx = length $ head m

        obstacles = findPos (==Obstacle) m

        (guardX, guardY) = head $ findPos isGuard m
        guardDir = case m !! guardY !! guardX of Guard dir -> dir
        guard = GuardState (guardX, guardY) guardDir

getInfo :: String -> IO ( Info )
getInfo str = do
    file <- readFile str
    return $ case parseMaybe parseMap file of
        Nothing -> undefined
        Just m -> mapToInfo m

-- Solve 1
turn :: Direction -> Direction
turn North = East
turn East  = South
turn South = West
turn West  = North

turnGuard :: GuardState -> GuardState
turnGuard (GuardState pos dir) = GuardState pos (turn dir)

walk :: Direction -> Position -> Position
walk North (x, y) = (x, y - 1)
walk East  (x, y) = (x + 1, y)
walk South (x, y) = (x, y + 1)
walk West  (x, y) = (x - 1, y)

walkGuard :: GuardState -> GuardState
walkGuard (GuardState pos dir) = GuardState (walk dir pos) dir

isOutside :: Dimension -> Position -> Bool
isOutside (dx, dy) (x, y) = x < 0 || x >= dx || y < 0 || y >= dy

-- Iterates a State-Machine until a condition is fulfilled.
-- Returns a history of the states.
-- The initial state and the final state are both included
iterateS :: (s -> a -> Bool) -> State s a -> s -> [s]
iterateS cond machine state = state : if cond nextState result
        then [nextState]
        else iterateS cond machine nextState
    where
        (result, nextState) = runState machine state

step :: State Info Bool
step = do
    (dim, obstacles, guard) <- get
    let posInFront = position $ walkGuard guard
    if isOutside dim posInFront 
        then     put (dim, obstacles, walkGuard guard) >> (return True)     -- Finished
        else if posInFront `elem` obstacles
            then put (dim, obstacles, turnGuard guard) >> (return False)    -- Turn
            else put (dim, obstacles, walkGuard guard) >> (return False)    -- Walk

stepPath :: Info -> [GuardState]
stepPath start = map (\(_,_,gs) -> gs) (iterateS (\_ fin -> fin) step start)

solve1 :: Info -> Int
solve1 start = length $ nub positions
    where
        positions = map position (init $ stepPath start)

-- Solve 2
newObstacles :: Info -> [Position]
newObstacles start = obstacles'
    where
        -- New obstacles need to lie on the path of the guard
        path = map position (stepPath start)
        -- Last position is outside field
        path' = init path
        -- Starting field is blocked
        startPosition = head path'
        obstacles = filter (/=startPosition) path'
        -- Duplicate positions are irrelevant
        obstacles' = nub obstacles

-- To check for duplicates in a possibly infinite list,
-- slice it into large chunks and check the chunks using hashSets.
hasDuplicatesLazy :: Ord a => [a] -> Bool
hasDuplicatesLazy xs = hasDuplicatesLazy' xs (Set.fromList [])
    where
        hasDuplicatesLazy' :: Ord a => [a] -> Set.Set a -> Bool
        hasDuplicatesLazy' [] _ = False
        hasDuplicatesLazy' xs set = if length set + length chunk == length combined
                then hasDuplicatesLazy' remainder combined
                else True
            where
                chunk = take 6000 xs
                remainder = drop 6000 xs
                combined = set `Set.union` Set.fromList chunk

-- GuardStates must be Orderable for HashSets to be build
instance Ord Direction where
    (<=) :: Direction -> Direction -> Bool
    North <= b = b `elem` [North]
    East  <= b = b `elem` [North, East]
    South <= b = b `elem` [North, East, South]
    West  <= b = b `elem` [North, East, South, West]

instance Ord GuardState where
    compare :: GuardState -> GuardState -> Ordering
    a `compare` b = case position a `compare` position b of
        EQ -> direction a `compare` direction b
        x -> x

loopy :: Info -> Bool
loopy start = hasDuplicatesLazy (stepPath start)

solve2 :: Info -> Int
solve2 start = length $ filter loopy starts
    where
        (dim, obstacles, guard) = start
        -- Create new obstacle-lists, one for each new obstacle
        obstacless = map (:obstacles) (newObstacles start)
        -- Create list of starting-infos, one for each obstacle-list
        starts = map (\os -> (dim, os, guard)) obstacless

main :: IO ()
main = do
    start <- getInfo "input"
    putStr "Solution 1: "
    putStrLn $ show $ solve1 start
    putStr "Solution 2: "
    putStrLn $ show $ solve2 start
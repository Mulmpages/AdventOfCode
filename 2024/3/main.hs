import Control.Monad.State.Strict (put, get, State, evalState)
import Control.Monad (void)
import Data.Void (Void)
import Text.Megaparsec
    ( anySingle,
      parseMaybe,
      (<|>),
      Parsec,
      MonadParsec(eof, try),
      ParsecT,
      empty,
      runParserT )
import Text.Megaparsec.Char ( string )
import qualified Text.Megaparsec.Char.Lexer as L


-- Parsing
type Mul = (Int, Int)
type Parser = Parsec Void String

parseMul :: Parser Mul
parseMul = do
    string "mul("
    x <- L.decimal
    string ","
    y <- L.decimal
    string ")"
    return (x, y)

parseMemory1 :: Parser [Mul]
parseMemory1 =  ((:) <$> try parseMul <*> parseMemory1)
            <|> (eof *> pure [])
            <|> (anySingle *> parseMemory1)

getMemory1 :: String -> IO ( [Mul] )
getMemory1 str = do
    file <- readFile str
    return $ case parseMaybe parseMemory1 file of
        Nothing -> []
        Just x -> x


type Parser2 = ParsecT Void String (State Bool)

parseMul2 :: Parser2 Mul
parseMul2 = do
    string "mul("
    x <- L.decimal
    string ","
    y <- L.decimal
    string ")"
    return (x, y)

parseDont :: Parser2 ()
parseDont = do
    string "don't()"
    put False

parseDo :: Parser2 ()
parseDo = do
    string "do()"
    put True

parseMemory2 :: Parser2 [Mul]
parseMemory2 = do
    active <- get
    (if active
        then ((:) <$> try parseMul2 <*> parseMemory2)
        else empty)
    <|> (try parseDont *> parseMemory2)
    <|> (try parseDo *> parseMemory2)
    <|> (eof *> pure [])
    <|> (anySingle *> parseMemory2)

getMemory2 :: String -> IO ( [Mul] )
getMemory2 str = do
    file <- readFile str
    return $ case evalState (runParserT parseMemory2 "" file) True of
        Left _  -> []
        Right x -> x

-- Solve
solve :: [Mul] -> Int
solve = sum . map (\(a,b) -> a*b)

main :: IO ()
main = do
    memory1 <- getMemory1 "input"
    putStr "Solution 1: "
    putStrLn $ show $ solve memory1

    memory2 <- getMemory2 "input"
    putStr "Solution 2: "
    putStrLn $ show $ solve memory2
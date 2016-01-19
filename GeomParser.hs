module GeomParser where

import Control.Applicative ((<*),liftA2)
import Text.Parsec
    ( Parsec, ParseError, parse        -- Types and parser
    , endBy, noneOf, sepBy1, many1    -- Combinators
    , char, spaces, digit, newline, eof     -- Simple parsers
    )
import Geometry (Line(..),Point(..))

--data Point = Point Float Float  deriving (Show)
--data Line  = Line Point Point   deriving (Show)

parseCSV :: String -> Either ParseError [Line]
parseCSV = parse csvp ""

csvp :: Parsec String () [Line]
csvp = parseLine `endBy` newline <* eof

parseLine :: Parsec String () Line
parseLine = liftA2 Line parsePoint (char ',' >> parsePoint)

parsePoint :: Parsec String () Point
parsePoint = liftA2 Point parseFloat (char ',' >> parseFloat)

parseFloat :: Parsec String () Float
parseFloat = fmap read (many1 ( noneOf ",\n")) 


{-

--Point <$> cell <*> (char ',' >> cell)
--[17:11] <marchelzo_> <$> is just an infix version of fmap
--[17:11] <marchelzo_> <*> is from Applicative
--[17:13] <marchelzo_> (char ',' >> cell) is of type Parsec String () Double
--[17:13] <marchelzo_> cell is of type Parsec String () Double
--[17:14] <marchelzo_> Point is Double -> Double -> Point
--[17:15] <marchelzo_> so Point <$> cell has type Parsec String () (Double -> Point)
--[17:15] <marchelzo_> now look at <*>
--[17:15] <marchelzo_> @type (<*>)
--[17:15] <lambdabot> Applicative f => f (a -> b) -> f a -> f b
--[17:15] <marchelzo_> f is Parsec String ()
--[17:15] <marchelzo_> a is Double
--[17:15] <marchelzo_> b is Point
--[17:15] <marchelzo_> We have f (a -> b), and cell is f a

line :: Parsec String () [Double]
line =  cell `sepBy1` char ','

cell :: Parsec String () Double
cell = fmap read (many1 ( noneOf ",\n")) 
-}
loadDB :: FilePath -> IO (Either ParseError [Line])
loadDB filename = do
    db <- readFile filename
    return $ parseCSV db


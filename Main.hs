import Svg_writer (writeSVG, Color(..), AnyShowSVG) 
import Geometry (Line(..),Point(..))
import GeomParser (parseCSV)

--parsing happens within IO, so we can print error messages!
parse_without_error :: FilePath -> IO [Line]
parse_without_error filename = do
  contents <- readFile filename 
  case (parseCSV contents) of
    Left err_msg -> do putStrLn $ show_error err_msg
                       return []
    Right lines -> return lines
    where 
      show_error msg = "\nThere was an error in " ++ filename ++ ":\n" ++ (show msg)

get_start :: Line -> Point
get_start (Line begin_pt end_pt) = begin_pt

{- Can't mix points and lines
-- geometricData? 
find_intersects :: ShowSVG a => [a] -> [a] -> [[a]]
find_intersects lines1 lines2 = [start_points,lines1,lines2] 
  where start_points = map get_start lines2
-}

colorize :: Color-> a -> (Color,a)
colorize color geom = (color, geom)

red   = colorize Red
green = colorize Green
blue  = colorize Blue

main = do

  lines1 <- parse_without_error "./data/lines1.txt" 
  lines2 <- parse_without_error "./data/lines2.txt"
  --let data2plot = find_intersects lines1 lines2
  let anylines1 = map AnyShowSVG lines1
  let anylines2 = map AnyShowSVG lines2
  let redlines  = map red anylines1
  let bluelines = map blue anylines2
  writeSVG (300,300) [redlines,bluelines] "./output/monads.svg"

{-
main = do
  (lines1:lines2) <- mapM parse_without_error ["../data/lines1.txt","../data/lines2.txt"]
  let data2plot = find_intersects lines1 lines2
  writeSVG data2plot "./output/monads.svg"
-}

import Geometry (Line(..), Point(..), ShowSVG, showSVG, lineLinesIntersection)
import GeomParser (parseCSV)
import Text.Parsec (ParseError)


parse_without_error :: FilePath -> IO [Line]
parse_without_error filename =
  contents <- readFile filename 
  case (parseCSV contents) of
    Left _ = []
    Right lines = lines

get_start :: Line -> Point
get_start Line (begin_pt, end_pt) = begin_pt

-- geometricData? 
find_intersects' :: ShowSVG a => [a] -> [a] -> (a,a,a)
find_intersects f1 f2 = do 
  lines1 <- parse_without_error f1
  lines2 <- parse_without_error f2
  let start_points = map get_start lines2
  return (start_points,lines1,lines2) 

find_intersects :: ShowSVG a => FilePath -> FilePath -> IO [[a]]
find_intersects f1 f2 = do 
  lines1 <- parse_without_error f1
  lines2 <- parse_without_error f2
  let start_points = map get_start lines2
  return [start_points,lines1,lines2] 

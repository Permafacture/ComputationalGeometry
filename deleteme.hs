module Svg_writer (writeSVG) where

import Geometry (Line(..), Point(..), AABB(..), ShowSVG, showSVG, bbox, addAABBs)
import GeomParser (parseCSV)
import Text.Parsec (ParseError)

{-
--deprecated
csvToSVG :: String -> String
csvToSVG csvString = 
    case possibly_lines of
      Left _ ->  "there was an Error!"
      Right lines ->  accumulateLinesIntoSVG lines 
    where
      accumulateLinesIntoSVG = foldr ((++).redSVG) ""
      possibly_lines = parseCSV csvString :: Either ParseError [Line]
      redSVG = showSVG "rgb(255,0,0)"
-}

overallAABB :: [ShowSVG] -> AABB
overallAABB [acc:others] = foldr addAABBs (bbox acc) (map bbox others)
overallAABB [onlyone] = bbox onlyone

mapGeometries :: [ShowSVG] -> AABB -> [ShowSVG]
--mapGeometries origAABB targetAABB geomList = map (stretch center magnitude) (map (translate lowerleft) geomList)
mapGeometries geomlist (AABB (Point targetLLx targetLLy) (Point targetURx targetURy)) = 
  map (stretch origin magnitude) (map (translate transvector) geomList)
  where
    (AABB (Point origLLx origLLy) (Point origURx origURy)) = overallAABB geomlist  -- !!! NOT GOING TO WORK
    origin = Point origLLx origLLy 
    transvector = Point (origLLx - targetLLx) (origLLy - targetLLy)
    magnitude = ((targetURx-targetLLx)/(origURx-origLLx),(targetURy-targetLLy)/(origURy-origLLy))

writeSVG :: ShowSVG b => (Int,Int) -> [[b]] -> FilePath -> IO()
writeSVG (canvasx, canvasy) geometries filename = do
    writeFile filename $ header ++ (accumulateGeomsToSVG normalized_geometries) ++ footer
    where
      redSVG = showSVG "rgb(255,0,0)"
      accumulateGeomsToSVG = foldr ((++).redSVG) "" 
      normalized_geometries = mapGeometries geometries (AABB (Point 0 0) (Point canvasx canvasy))
      header = "<html><body><svg height=\""++canvasy++"\" version=\"1.1\" width=\""++canvasx++"\" xmlns=\"http://www.w3.org/2000/svg\">"
      footer = "</svg></body></html>"

{-
test :: FilePath -> IO()
    contents <- readFile filename
    writeFile "./temp.svg" $ header ++ (csvToSVG contents) ++ footer
    where
      header = "<html><body><svg height=\"360\" version=\"1.1\" width=\"480\" xmlns=\"http://www.w3.org/2000/svg\">"
      footer = "</svg></body></html>"
-}

{-# LANGUAGE ExistentialQuantification #-}
module Svg_writer (writeSVG ) where

import Geometry (Line(..),Point(..),AABB(..),Geometry,AnyGeometry(..),Color(..),bbox,addAABBs,stretch,translate,showSVG)
import GeomParser (parseCSV)
import Text.Parsec (ParseError)
import Control.Arrow (second)


{- TODO: svg is top to bottom, left to right, so 0,0 is upper left
   Fix naming to corispond to perception? -}

overallAABB :: Geometry a => [a] -> AABB
overallAABB (acc:others) = foldr addAABBs (bbox acc) (map bbox others)
overallAABB [onlyone] = bbox onlyone
overallAABB [] = AABB (Point 0 0) (Point 0 0)

{-
mappingTransform :: source AABB -> target AABB -> transform
transformGeometries :: transform -> [geometry] -> [transformed geometry]
    map transform geometries

writeSVG
  accumulate (map.second transform colorsAndGeoms)
-}


mapGeometries :: [AnyGeometry] -> AABB -> [AnyGeometry]
mapGeometries geomList (AABB (Point targetLLx targetLLy) (Point targetURx targetURy)) = 
  map (stretch origin magnitude . translate transvector) geomList
  where
    (AABB (Point sourceLLx sourceLLy) (Point sourceURx sourceURy)) = overallAABB geomList
    origin = Point targetLLx targetLLy 
    transvector = Point (targetLLx - sourceLLx) (targetLLy - sourceLLy)
    magnitude = ((targetURx-targetLLx)/(sourceURx-sourceLLx),(targetURy-targetLLy)/(sourceURy-sourceLLy))

int2Float x = fromIntegral x :: Float

writeSVG :: (Int,Int) -> [[(Color,AnyGeometry)]] -> FilePath -> IO()-- TODO start here. use show for color
writeSVG (canvasx, canvasy) geometries filename = do
    -- Ugh, come up with better names: make (Color, ShowSVG) a type with a meaningful name?
    writeFile filename $ header ++ (accumulateGeomsToSVG normalizedGeoms) ++ footer
    where
      accumulateGeomsToSVG = foldr ((++).showSVG) ""
      canvas_bbox = AABB (Point 0 0) (Point (int2Float canvasx) (int2Float canvasy)) 

      -- this is a bit ugly
      flattenedGeomList = foldr1 (++) geometries
      (colorsList, geometriesList) = unzip flattenedGeomList
      normalizedGeometries = mapGeometries geometriesList canvas_bbox 
      normalizedGeoms = zip colorsList normalizedGeometries

      header = "<html><body><svg height=\""++show canvasy
               ++"\" version=\"1.1\" width=\""++show canvasx
               ++"\" xmlns=\"http://www.w3.org/2000/svg\">\n"
      footer = "</svg></body></html>"



{-
test :: FilePath -> IO()
    contents <- readFile filename
    writeFile "./temp.svg" $ header ++ (csvToSVG contents) ++ footer
    where
      header = "<html><body><svg height=\"360\" version=\"1.1\" width=\"480\" xmlns=\"http://www.w3.org/2000/svg\">"
      footer = "</svg></body></html>"
-}

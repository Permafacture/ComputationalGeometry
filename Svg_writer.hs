{-# LANGUAGE ExistentialQuantification #-}
module Svg_writer (writeSVG, Color(..), ShowSVG, AnyShowSVG) where

import Geometry (Line(..),Point(..),AABB(..),Geometry,AnyGeometry(..),bbox,addAABBs,stretch,translate)
import GeomParser (parseCSV)
import Text.Parsec (ParseError)
import Control.Arrow (second)

data Color = Red | Green | Blue
instance Show Color where
   show Red   = "rgb(255,0,0)" 
   show Green = "rgb(0,255,0)" 
   show Blue  = "rgb(0,0,255)" 


--data Transform = Transform { origin :: Point

class Geometry a => ShowSVG a where
  showSVG :: (Color, a) -> String

instance ShowSVG Point where  
  showSVG (color, (Point x y)) = "<circle cx=\"" ++ show x 
                           ++ "\" cy=\""      ++ show y 
                           ++ "\" r=\"4\" fill=\"" ++ (show color) ++ "\" />\n"

instance ShowSVG Line where
  showSVG (color, (Line (Point x1 y1) (Point x2 y2))) = "<line x1=\"" ++ show x1 
                                                  ++ "\" y1=\"" ++ show y1 
                                                  ++ "\" x2=\"" ++ show x2 
                                                  ++ "\" y2=\"" ++ show y2 
                                                  ++ "\" style=\"stroke:" ++ (show color) 
                                                  ++ ";stroke-width:2\" />\n"

data AnyShowSVG = forall a. (ShowSVG a) => AnyShowSVG a

instance ShowSVG AnyShowSVG where
  showSVG (color, AnyShowSVG x) = showSVG (color, x)


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

writeSVG :: (Int,Int) -> [[(Color,AnyShowSVG)]] -> FilePath -> IO()-- TODO start here. use show for color
writeSVG (canvasx, canvasy) geometries filename = do
    -- Ugh, come up with better names: make (Color, ShowSVG) a type with a meaningful name?
    writeFile filename $ header ++ (accumulateGeomsToSVG normalizedGeoms) ++ footer
    where
      accumulateGeomsToSVG = foldr ((++).showSVG) ""
      canvas_bbox = AABB (Point 0 0) (Point (int2Float canvasx) (int2Float canvasy)) 

      -- this is a bit ugly
      flattenedGeomList = foldr1 (++) geometries
      (colorsList, geometriesList) = unzip flattenedGeomList
      anyGeometriesList = map (\(AnyShowSVG x) -> AnyGeometry x) geometriesList
      normalizedGeometries = mapGeometries anyGeometriesList canvas_bbox 
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

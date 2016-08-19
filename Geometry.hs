{-# LANGUAGE ExistentialQuantification #-}

module Geometry (Point(..), Line(..), Param(..), AABB(..), Geometry, AnyGeometry(..), 
                 Color(..), linesLinesIntersection, evalIntersects2D, bbox, evalLineParameter, 
                 translate, stretch, addAABBs,showSVG) where

import Data.Maybe (mapMaybe)

data Param  = Param Float        deriving (Show)
data Point  = Point Float Float  deriving (Show)
data Line   = Line Point Point   deriving (Show)
data AABB   = AABB Point Point   deriving (Show) --Lower Left, Upper Right

data Color = Red | Green | Blue
instance Show Color where
   show Red   = "rgb(255,0,0)" 
   show Green = "rgb(0,255,0)" 
   show Blue  = "rgb(0,0,255)" 


class Geometry a where
  bbox    :: a -> AABB
  translate :: Point -> a -> a
  stretch   :: Point -> (Float, Float) -> a -> a --center of stretch -> magnitude -> orig -> stretched
  showSVG :: (Color, a) -> String


instance Geometry Point where
  bbox point = AABB point point
  translate (Point dx dy) (Point x y) = Point (x+dx) (y+dy)
  stretch (Point cx cy) (magx, magy) (Point x y) = Point (cx+(x-cx)*magx) (cy+(y-cy)*magy)
  showSVG (color, (Point x y)) = "<circle cx=\"" ++ show x 
                           ++ "\" cy=\""      ++ show y 
                           ++ "\" r=\"4\" fill=\"" ++ (show color) ++ "\" />\n"

instance Geometry Line where
  bbox (Line (Point x1 y1) (Point x2 y2))  = AABB (Point minx miny) (Point maxx maxy)
    where
      (minx,maxx) = (min x1 x2, max x1 x2)
      (miny,maxy) = (min y1 y2, max y1 y2)
  
  translate trans_vector (Line pt1 pt2) = 
      Line (translate trans_vector pt1) (translate trans_vector pt2) 

  stretch center magnitude (Line pt1 pt2) = 
      Line (stretch center magnitude pt1) (stretch center magnitude pt2)

  showSVG (color, (Line (Point x1 y1) (Point x2 y2))) = "<line x1=\"" ++ show x1 
                                                  ++ "\" y1=\"" ++ show y1 
                                                  ++ "\" x2=\"" ++ show x2 
                                                  ++ "\" y2=\"" ++ show y2 
                                                  ++ "\" style=\"stroke:" ++ (show color) 
                                                  ++ ";stroke-width:2\" />\n"

data AnyGeometry = forall x. (Geometry x) => AnyGeometry x

instance Geometry AnyGeometry where
  bbox (AnyGeometry x) = bbox x
  translate point (AnyGeometry x) = AnyGeometry $ translate point x
  stretch point magnitude (AnyGeometry x) = AnyGeometry $ stretch point magnitude x
  showSVG (color, AnyGeometry x) = showSVG (color, x)

addAABBs :: AABB -> AABB -> AABB
addAABBs (AABB (Point minx1 miny1) (Point maxx1 maxy1)) (AABB (Point minx2 miny2) (Point maxx2 maxy2)) = 
    AABB (Point newminx newminy) (Point newmaxx newmaxy)
    where
        newminx = min minx1 minx2
        newmaxx = max maxx1 maxx2
        newminy = min miny1 miny2
        newmaxy = max maxy1 maxy2


inRange :: Float -> Float -> Float -> Bool
inRange min max val = (val <= max) && (val >= min)

validParam = inRange 0.0 1.0

lineLineIntersection :: Line -> Line -> Maybe (Param, Param)
--get the parametric value of the intersection for each of the two lines
lineLineIntersection (Line (Point x1 y1) (Point x2 y2)) (Line (Point x3 y3) (Point x4 y4))
  | den == 0 =  Nothing
  | (numa /= 0) && (numb /= 0) && (validParam ua) && (validParam ub) = Just (Param ua, Param ub)
  | otherwise = Nothing
       where
         den =  (y4-y3)*(x2-x1) - (x4-x3)*(y2-y1)
         numa = (x4-x3)*(y1-y3) - (y4-y3)*(x1-x3)
         numb = (x2-x1)*(y1-y3) - (y2-y1)*(x1-x3)
         ua = numa/den
         ub = numb/den 

lineLinesIntersection :: Line -> [Line] -> [Maybe (Param, Param)]
lineLinesIntersection l1 l2 = map (lineLineIntersection l1) l2

linesLinesIntersection :: [Line] -> [Line] ->[[Maybe (Param, Param)]]
linesLinesIntersection l1s l2s = map ((flip lineLinesIntersection) l2s) l1s

evalLineParameter :: Line -> Param -> Point
evalLineParameter (Line (Point x1 y1) (Point x2 y2)) (Param u) = Point (x1+u*(x2-x1)) (y1+u*(y2-y1)) 

evalIntersects :: Line -> [Maybe (Param, Param)] -> [Point]
--assumes line corisponds to the first of the two parameters
evalIntersects line list = map (evalLineParameter line) $ map fst (mapMaybe id list)

evalIntersects2D :: [Line] -> [[Maybe (Param, Param)]] -> [[Point]]
evalIntersects2D lines list2d = [f args | args <- zip lines list2d]
    where f = \(l, p) -> evalIntersects l p

--test data
testP1 = Point 1.2 2.4
testP2 = Point (-1.2) (-2.4)
testP3 = Point (-1.2) 2.4
testP4 = Point (1.2) (-2.4)

testL1 = Line testP1 testP2
testL2 = Line testP3 testP4

testL1s = [testL1, testL2]

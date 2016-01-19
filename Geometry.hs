{-# LANGUAGE ExistentialQuantification #-}

module Geometry (Point(..), Line(..), Param(..), AABB(..), Geometry, AnyGeometry(..), 
                 lineLinesIntersection, bbox, translate, stretch, addAABBs) where

data Param  = Param Float        deriving (Show)
data Point  = Point Float Float  deriving (Show)
data Line   = Line Point Point   deriving (Show)
data AABB   = AABB Point Point   deriving (Show) --Lower Left, Upper Right


class Geometry a where
  bbox    :: a -> AABB
  translate :: Point -> a -> a
  stretch   :: Point -> (Float, Float) -> a -> a --center of stretch -> magnitude -> orig -> stretched


instance Geometry Point where
  bbox point = AABB point point
  translate (Point dx dy) (Point x y) = Point (x+dx) (y+dy)
  stretch (Point cx cy) (magx, magy) (Point x y) = Point (cx+(x-cx)*magx) (cy+(y-cy)*magy)

instance Geometry Line where
  bbox (Line (Point x1 y1) (Point x2 y2))  = AABB (Point minx miny) (Point maxx maxy)
    where
      (minx,maxx) = (min x1 x2, max x1 x2)
      (miny,maxy) = (min y1 y2, max y1 y2)
  
  translate trans_vector (Line pt1 pt2) = 
      Line (translate trans_vector pt1) (translate trans_vector pt2) 

  stretch center magnitude (Line pt1 pt2) = 
      Line (stretch center magnitude pt1) (stretch center magnitude pt2)

data AnyGeometry = forall x. (Geometry x) => AnyGeometry x

instance Geometry AnyGeometry where
  bbox (AnyGeometry x) = bbox x
  translate point (AnyGeometry x) = AnyGeometry $ translate point x
  stretch point magnitude (AnyGeometry x) = AnyGeometry $ stretch point magnitude x

addAABBs :: AABB -> AABB -> AABB
addAABBs (AABB (Point minx1 miny1) (Point maxx1 maxy1)) (AABB (Point minx2 miny2) (Point maxx2 maxy2)) = 
    AABB (Point newminx newminy) (Point newmaxx newmaxy)
    where
        newminx = min minx1 minx2
        newmaxx = max maxx1 maxx2
        newminy = min miny1 miny2
        newmaxy = max maxy1 maxy2

lineLineIntersection :: Line -> Line -> Maybe (Param, Param)
lineLineIntersection (Line (Point x1 y1) (Point x2 y2)) (Line (Point x3 y3) (Point x4 y4))
  | den == 0 =  Nothing
  | (numa /= 0) && (numb /= 0) = Just (Param ua, Param ub)
  | otherwise = Nothing
       where
         den =  (y4-y3)*(x2-x1) - (x4-x3)*(y2-y1)
         numa = (x4-x3)*(y1-y3) - (y4-y3)*(x1-x3)
         numb = (x2-x1)*(y1-y3) - (y2-y1)*(x1-x3)
         ua = numa/den
         ub = numb/den 

lineLinesIntersection :: Line -> [Line] -> [Maybe (Param, Param)]
lineLinesIntersection l1 l2 = map (lineLineIntersection l1) l2

evalLineParameter :: Line -> Param -> Point
evalLineParameter (Line (Point x1 y1) (Point x2 y2)) (Param u) = Point (x1+u*x2) (x2+u*y2) 


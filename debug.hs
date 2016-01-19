{-# LANGUAGE ExistentialQuantification #-}

{- Define Geometries -}
data Point  = Point Float Float  deriving (Show)

class Geometry a where
  translate :: Point -> a -> a

instance Geometry Point where
  translate (Point dx dy) (Point x y) = Point (x+dx) (y+dy)


{- Universally quantify geometries for heterogenous lists -} 
data AnyGeometry = forall x. (Geometry x) => AnyGeometry x

instance Geometry AnyGeometry where
  translate point (AnyGeometry x) = AnyGeometry $ translate point x

{- Maybe not every geometry can be represented by SVG, but some can -}
data Color = Color String deriving (Show)

class Geometry a => ShowSVG a where
  showSVG :: (Color, a) -> String

instance ShowSVG Point where  
  showSVG (color, (Point x y)) = "<circle cx=\"" ++ show x 
                           ++ "\" cy=\""      ++ show y 
                           ++ "\" r=\"4\" fill=\"" ++ (show color) ++ "\" />\n"

data AnyShowSVG = forall a. (ShowSVG a) => AnyShowSVG a

instance ShowSVG AnyShowSVG where
  showSVG (color, AnyShowSVG x) = showSVG (color, x)

{- Now do something with these geometries that can be rendered -}

working_ArbitraryTranslation :: Geometry a => [a] -> Point -> [a]
working_ArbitraryTranslation geoms vector = map (translate vector) geoms

working_printSVG :: ShowSVG a => [(Color,a)] -> IO()
working_printSVG showSVGs = do
      putStrLn $ foldr (++) "" (map showSVG translatedShowSVGs)
      where
        --yea, I know. Someone said use a lens and I'll look into that more
        (colorsList, geometriesList) = unzip showSVGs
        translatedGeometries = map (translate (Point 1.0 1.0))  geometriesList 
        translatedShowSVGs = zip colorsList translatedGeometries

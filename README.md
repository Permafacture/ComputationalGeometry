Experimenting with Haskell.  Working on making queries on sets of geometries using GDAL and CGAL like functionalities.  Raytracing?  GIS?  Who knows where it'll go!

I'm using Cabal to build and run this, and the project tree looks like below.  This git repo is in the src directory.  (Obviously, i have no idea about the correct way to package a Haskell project). 


.
├── build-run.sh
├── cabal.sandbox.config
├── data
│   ├── lines1.txt
│   └── lines2.txt
├── geom.cabal
├── LICENSE
├── output
│   └── monads.svg
├── Setup.hs
├── src
│   ├── Geometry.hs
│   ├── GeomParser.hs
│   ├── Main.hs
│   ├── Setup.hs
│   └── Svg_writer.hs
└── TODO.md


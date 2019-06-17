module Ex01 where

-- needed to display the picture in the playground
import Codec.Picture

-- our line graphics programming interface
import ShapeGraphics

-- Part 1
-- picture of a house
housePic :: Picture
housePic = [door, house]
  where
    house :: PictureObject
    house = Path (mapToPoints houseCOs) green Solid
    door :: PictureObject
    door  = Path (mapToPoints doorCOs) red Solid

chimneyHouse :: Picture
chimneyHouse = [door, house, smoke]
  where
    house :: PictureObject
    house = Path (mapToPoints chimneyHouseCOs) green Solid
    door :: PictureObject
    door  = Path (mapToPoints doorCOs) red Solid
    smoke :: PictureObject
    smoke = Path (mapToPoints smokeCOs) grey Solid

-- essentially takes a coordinates tupple array 
-- and converts to an array of points
mapToPoints :: [(Float, Float)] -> [Point]
mapToPoints coordinates = map (\(a, b) -> Point a b) coordinates

houseCOs :: [(Float, Float)]
houseCOs = [(300, 750), (300, 450), (270, 450), (500, 200),
            (730, 450), (700, 450), (700, 750)]

chimneyHouseCOs :: [(Float, Float)]
chimneyHouseCOs = [(300, 750), (300, 450), (270, 450), (500, 200),
                  (615, 325), (615, 250), (650, 250), (650, 363),
                  (730, 450), (700, 450), (700, 750)]
          
doorCOs :: [(Float, Float)]
doorCOs = [(420, 750), (420, 550), (580, 550), (580, 750)]

smokeCOs :: [(Float, Float)]
smokeCOs = [(635, 240), (625, 230), (635, 220), (625, 210)]

grey :: Colour
grey = Colour 255 255 255 128


-- Part 2
movePoint :: Vector -> Point -> Point
movePoint (Vector xv yv) (Point x y) = Point (x + xv) (y + yv)

movePoints ::  Vector -> [Point] -> [Point]
movePoints vec points = map (movePoint vec) points

movePictureObject :: Vector -> PictureObject -> PictureObject
movePictureObject vec (Path pointsPO colour lineStyle)
  = Path (movePoints vec pointsPO) colour lineStyle

movePictureObject vec (Circle centerPO radiusPO colourPO lineStylePO fillStylePO)
  = Circle (movePoint vec centerPO) radiusPO colourPO lineStylePO fillStylePO

movePictureObject vec (Ellipse centerPO widthPO heightPO rotationPO colourPO lineStylePO fillStylePO)
  = Ellipse (movePoint vec centerPO) widthPO heightPO rotationPO colourPO lineStylePO fillStylePO

movePictureObject vec (Polygon pointsPO colourPO lineStylePO fillStylePO)
  = Polygon (movePoints vec pointsPO) colourPO lineStylePO fillStylePO


-- Part 3
-- generate the picture consisting of circles:
-- [Circle (Point 400 400) (400/n) col Solid SolidFill,
--  Circle (Point 400 400) 2 * (400/n) col Solid SolidFill,
--  ....
--  Circle (Point 400 400) 400 col Solid SolidFill]
simpleCirclePic :: Colour -> Float -> Picture
simpleCirclePic col n = map (circlePic col n) [1..n]

circlePic :: Colour -> Float -> Float -> PictureObject
circlePic col n i = Circle (Point 400 400) (i * (400/n)) col Solid SolidFill



-- use 'writeToFile' to write a picture to file "ex01.png" to test your
-- program if you are not using Haskell for Mac
-- e.g., call
-- writeToFile [house, door]

writeToFile pic
  = writePng "ex01.png" (drawPicture 3 pic)

module Art where  

import ShapeGraphics
import Codec.Picture

art :: Picture
art = genArt 10 60 white 5

genArt :: Float -> Float -> Colour -> Int -> Picture
genArt width height colour n
  = genSnow (Point (400 - width / 2) 400) (Vector 0 (-height)) (Vector width 0) colour n
  ++ genSnow (Point (400 - width / 2) 400) (Vector 0 (height)) (Vector width 0) colour n
  ++ genSnow (Point (400 - width / 2) 400) (Vector (-height) 0) (Vector 0 width) colour n
  ++ genSnow (Point (400 - width / 2) 400) (Vector (height) 0) (Vector 0 width) colour n
  where    
    transformColour :: Colour -> Colour
    transformColour (Colour r g b o) = 
      Colour (max 0 (r - n*6)) g (min 255 (b + n*6)) o
    angle = pi/4
    genSnow :: Point -> Vector -> Vector -> Colour -> Int -> Picture
    genSnow pos vec1 vec2 col n
      | n <= 1 = [Polygon [pos, movePoint vec1 pos, 
                                movePoint vec2 $ movePoint vec1 pos, 
                                movePoint vec2 pos]
                          col
                          Solid
                          NoFill]                          
      | otherwise = genSnow pos vec1 vec2 col (n - 1) ++
                    genSnow (movePoint vec1 pos) 
                          (scaleVector 1 $ rotateVector (1 * angle) vec1)
                          (scaleVector 1 $ rotateVector (1 * angle) vec2) 
                          (transformColour col) (n - 1) ++
                    genSnow (movePoint vec1 pos) 
                          (scaleVector 1 $ rotateVector (0 * angle) vec1)
                          (scaleVector 1 $ rotateVector (0 * angle) vec2) 
                          (transformColour col) (n - 1) ++
                    genSnow (movePoint vec1 pos) 
                          (scaleVector 1 $ rotateVector (-1 * angle) vec1)
                          (scaleVector 1 $ rotateVector (-1 * angle) vec2) 
                          (transformColour col) (n - 1) 

      
scaleVector :: Float -> Vector -> Vector
scaleVector fac (Vector x y)
  = Vector (fac * x) (fac * y)                           
 
rotateVector :: Float -> Vector -> Vector
rotateVector alpha (Vector vx vy)
  = Vector (cos alpha * vx - sin alpha * vy)
           (sin alpha * vx + cos alpha * vy)

movePoint :: Vector -> Point -> Point
movePoint (Vector xv yv) (Point xp yp)
  = Point (xv + xp) (yv + yp)

-- use 'writeToFile' to write a picture to file "ex01.png" to test your
-- program if you are not using Haskell for Mac
-- e.g., call
-- writeToFile [house, door]

writeToFile pic
  = writePng "art.png" (drawPicture 3 art)

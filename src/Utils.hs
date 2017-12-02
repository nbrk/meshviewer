module Utils where

import Graphics.UI.GLUT
import Foreign.Ptr
import Foreign.Storable
import System.Random
import Linear

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral


randomColor :: IO (Vertex3 GLfloat)
randomColor = do
  r <- randomRIO (0, 1)
  g <- randomRIO (0, 1)
  b <- randomRIO (0, 1)
  return $ Vertex3 r g b


-- spherical to cartesian coords conversion
sphericalToCartesian :: Float -> Float -> V3 Float
sphericalToCartesian horang vertang =
  V3
  (cos vertang * sin horang)
  (sin vertang)
  (cos vertang * cos horang)

  
  -- "right" vector for camera up calculation
rightVector :: Float -> V3 Float
rightVector horang =
  V3
  (sin (horang - pi/2))
  0
  (cos (horang - pi/2))

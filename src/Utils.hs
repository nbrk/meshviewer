module Utils where

import Graphics.UI.GLUT
import Foreign.Ptr
import Foreign.Storable
import System.Random

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral


randomColor :: IO (Vertex3 GLfloat)
randomColor = do
  r <- randomRIO (0, 1)
  g <- randomRIO (0, 1)
  b <- randomRIO (0, 1)
  return $ Vertex3 r g b

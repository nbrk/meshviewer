module GLUTCallbacks where

import Graphics.UI.GLUT
import Data.IORef
import OpenGL(updateUniforms)
import Data.Time.Clock
import Linear

import Types
import Utils


display :: Descriptor -> DisplayCallback
display d = do
  clearColor $=  Color4 0.2 0.2 0.3 1
  clear [ ColorBuffer, DepthBuffer ]

  updateUniforms d

  polygonMode $= (Line, Line)
--  drawArrays TriangleFan 0 numvs
  drawArrays Triangles 0 (descNVertices d)
--  drawArrays LineLoop 0 numvs
  flush

p m = m >> postRedisplay Nothing
speed = 1
deltaTime = 1

-- keyboard key press
keyboard :: Descriptor -> KeyboardCallback
keyboard d 'w' _ = p $ do
  cdir <- get $ descCameraDir d
  (descCameraPos d) $~ \v -> v ^+^ cdir ^* (deltaTime * speed)
keyboard d 's' _ = p $ do
  cdir <- get $ descCameraDir d
  (descCameraPos d) $~ \v -> v ^-^ cdir ^* (deltaTime * speed)
keyboard d 'd' _ = p $ do
  right <- get $ descCameraRight d
  (descCameraPos d) $~ \v -> v ^+^ right ^* (deltaTime * speed)
keyboard d 'a' _ = p $ do
  right <- get $ descCameraRight d
  (descCameraPos d) $~ \v -> v ^-^ right ^* (deltaTime * speed)
keyboard d 'n' _ = p $ (descA d) $~ \q -> q-1
keyboard d 'N' _ = p $ (descA d) $~ \q -> q+1
keyboard d 'f' _ = p $ (descB d) $~ \q -> q-1
keyboard d 'F' _ = p $ (descB d) $~ \q -> q+1
keyboard d '-' _ = p $ (descFOV d) $~ \q -> q - 5
keyboard d '+' _ = p $ (descFOV d) $~ \q -> q + 5
keyboard _ _ _ = return ()


-- according to freeglut's docs, the wdir is -1/+1
mouseWheel :: Descriptor -> MouseWheelCallback
mouseWheel d _ wdir _ = do
  putStrLn $ "mouseWheel: " ++ show wdir
  p $ (descFOV d) $~ \q -> q - 5 * (fromIntegral wdir)


-- -- special key down
specKeyDown :: Descriptor -> SpecialCallback
specKeyDown d KeyLeft _ = p $ do
--  right <- get $ descCameraRight d
  (descHorizAngle d) $~ \q -> (q + 15)
  horang <- get $ descHorizAngle d
  vertang <- get$ descVertAngle d
  let right = rightVector horang
  let cdir = sphericalToCartesian horang vertang
  descCameraRight d $= right
  descCameraDir d $= cdir
  p $ return ()

--specKeyDown d KeyRight _ = p $ do
--  right <- get $ descCameraRight d
--  (descCameraPos d) $~ \v -> v ^+^ right ^* (deltaTime * speed)

-- specKeyDown d KeyPageDown _ = p $ do
--   right <- get $ descCameraRight d
--   (descCameraPos d) $~ \v -> v ^-^ right ^* (deltaTime * speed)
-- specKeyDown d KeyPageUp _ = p $ do
--   right <- get $ descCameraRight d
--   (descCameraPos d) $~ \v -> v ^+^ right ^* (deltaTime * speed)
specKeyDown _ _ _ = return ()


-- window reposition
position :: Descriptor -> PositionCallback
position d pos =
  descWindowPos d $= pos >>
  resetViewport d


-- window reshape
reshape :: Descriptor -> ReshapeCallback
reshape d size =
  descWindowSize d $= size >>
  resetViewport d


-- reset the viewport to user-provided dimenions and request redisplay
resetViewport :: Descriptor -> IO ()
resetViewport d = do
  pos <- get $ descWindowPos d
  size <- get $ descWindowSize d
  viewport $= (pos, size)
  postRedisplay Nothing


passiveMotion :: Descriptor -> MotionCallback
passiveMotion d (Position posx posy) = do
  let mouseSpeed = 0.0001

  -- get old and current time and reset the ioref
  tlast <- get $ descTimestamp d
  tnow <- getCurrentTime
  descTimestamp d $= tnow

  -- float time diff
  let deltaTime = realToFrac $ diffUTCTime tnow tlast
--  let deltaTime = 0.05

  -- current window dimensions and pos set by the user
  (Size w h) <- get $ descWindowSize d

  -- calculate the centers of the current window
  let horcent = (fromIntegral w) / 2 :: Float
  let vertcent = (fromIntegral h) / 2 :: Float

  -- get old angles
  oldhorang <- get $ descHorizAngle d
  oldvertang <- get $ descVertAngle d

  -- compute and save new horiz and vert angles
  let horang = oldhorang + mouseSpeed * deltaTime * (horcent - (fromIntegral posx))
  let vertang = oldvertang + mouseSpeed * deltaTime * (vertcent - (fromIntegral posy))

  -- "right" vector for camera up calculation
  let right = rightVector horang

  -- cartesian from spherical for the direction
  let cdir = sphericalToCartesian horang vertang

  -- write iorefs
  descVertAngle d $= vertang
  descHorizAngle d $= horang
  descCameraRight d $= right
  descCameraDir d $= cdir

  passiveMotionCallback $= Nothing
  -- set the pointer back to the center
--  pointerPosition $= Position (truncate horcent) (truncate vertcent)
--  putStrLn $ "set pointerPosition to " ++ show (truncate horcent) ++ ", " ++ show (truncate vertcent)

  passiveMotionCallback $= Just (passiveMotion d)
  postRedisplay Nothing


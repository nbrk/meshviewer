module OpenGL where

import Foreign.Marshal.Array(withArray)
import Foreign.Marshal(with)
import Foreign.Ptr
import Foreign.Storable
import Graphics.UI.GLUT hiding (perspective, lookAt)
import qualified Graphics.GL as GLRaw
import Data.IORef
import Control.Monad
import Data.Time.Clock

import Linear


import Types
import Utils
import LoadShaders


setupGL :: Mesh -> IO Descriptor
setupGL m = do
  -- get initial viewport pos and size
  (pos, size) <- get viewport

  -- init iorefs
  wposref <- newIORef pos
  wsizeref <- newIORef size
  vangref <- newIORef (0 :: Float)
  hangref <- newIORef (pi :: Float)
  fovref <- newIORef (45 :: Float)
  cposref <- newIORef (V3 0 0 5)
  rightref <- newIORef (rightVector pi) -- XXX
  cdirref <- newIORef (sphericalToCartesian pi 0) -- XXX
  t <- getCurrentTime
  tstampref <- newIORef t

  -- XXX
  a <- newIORef (0.1 :: Float)
  b <- newIORef (100 :: Float)

  -- compile and link shader program
  program <- loadShaders [
     ShaderInfo VertexShader (FileSource "src/vertex.glsl"),
     ShaderInfo FragmentShader (FileSource "src/fragment.glsl")]
  currentProgram $= Just program

  -- bind the VAO
  vao <- genObjectName
  bindVertexArrayObject $= Just vao

  -- setup the VertexArray buffer
  let poses = meshPositions m
  setupArrayBuffer program poses 3 "vPosition"
--  mapM_ (putStrLn . show) poses

  return
    Descriptor
    { descProgram = program
    , descVAO = vao
    , descNVertices = fromIntegral (length poses)

    , descWindowSize = wsizeref
    , descWindowPos = wposref
    , descTimestamp = tstampref

    , descCameraPos = cposref
    , descCameraDir = cdirref
    , descVertAngle = vangref
    , descHorizAngle = hangref
    , descFOV = fovref
    , descCameraRight = rightref

    , descA = a
    , descB = b
    }


setupArrayBuffer :: Storable a => Program -> [a] -> Int -> String -> IO ()
setupArrayBuffer program dat dim atname =
  -- VBO, copy the position data
  withArray dat $ \ptr -> do
    -- bind new buffer
    buf <- genObjectName
    bindBuffer ArrayBuffer $= Just buf

    -- transfer the data
    let size = fromIntegral (length dat * sizeOf (head dat))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

    -- point shader atname
    atloc <- get $ attribLocation program atname
    vertexAttribPointer atloc  $=
      (ToFloat,
       VertexArrayDescriptor (fromIntegral dim) Float 0 (bufferOffset 0))

    -- enable the location (a block of bytes in the VBO via the above pointer)
    vertexAttribArray atloc $= Enabled


-- update uniforms in the shader program
updateUniforms :: Descriptor -> IO ()
updateUniforms d =
  putStrLn "updateUniforms" >>
  updateUniformMVP d


-- just raw update the uniform
uniformMatrix4fv :: Storable a => Program -> String -> a -> IO ()
uniformMatrix4fv p n m = do
  loc <- get $ uniformLocation p n
  let loc' = (read . head . tail . words . show) loc
  with m $ \ptr -> do
    GLRaw.glUniformMatrix4fv loc' 1 0 (castPtr ptr)


-- get the current iorefs and calc the MVP; update it
updateUniformMVP :: Descriptor -> IO ()
updateUniformMVP d = do
  fov <- get $ descFOV d
  cpos <- get $ descCameraPos d
  horang <- get $ descHorizAngle d
  vertang <- get $ descVertAngle d
  right <- get $ descCameraRight d
  cdir <- get $ descCameraDir d

  -- XXX
  a <- get $ descA d
  b <- get $ descB d

   -- camera up
  let up = cross right cdir

  let modelMatrix = identity :: M44 Float
  let viewMatrix =
        lookAt
        cpos -- eye
        (cpos ^+^ cdir) -- center
        up
  let projectionMatrix =
        perspective
        (fov * (pi / 180)) -- fov (y-dir in rad)
        (1600/900) -- aspect ratio
        a -- near plane
        b -- far plane
  let mvp = projectionMatrix !*! viewMatrix !*! modelMatrix
--  let mvp = identity * viewMatrix * modelMatrix

  -- update the new mvp
  putStrLn $ "fov: " ++ show fov
  uniformMatrix4fv (descProgram d) "MVP" mvp


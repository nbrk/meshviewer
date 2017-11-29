module OpenGL where

import Foreign.Marshal.Array(withArray)
import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Foreign.Storable
import qualified Graphics.Formats.STL as STL

import Types
import Utils
import LoadShaders


setupGL :: Mesh -> IO Descriptor
setupGL m = do
  let poses = meshPositions m

  colors <- forM poses $ \_ -> randomColor

  -- init iorefs
  avref <- newIORef $ Vector3 0 0 (0 :: Float)
  colref <- newIORef $ Vector3 0 0 (1 :: Float)
  scaleref <- newIORef (1 :: Float)

  -- compile and link shader program
  program <- loadShaders [
     ShaderInfo VertexShader (FileSource "src/vertex.glsl"),
     ShaderInfo FragmentShader (FileSource "src/fragment.glsl")]
  currentProgram $= Just program

  -- bind the VAO
  vao <- genObjectName
  bindVertexArrayObject $= Just vao

  setupArrayBuffer program poses 3 "vPosition"
  setupArrayBuffer program colors 3 "vColor"

--  position (Light 1) $= Vertex4 1.0 2.0 2.0 1.0

  return
    Descriptor
    { descAngles = avref
    , descColor = colref
    , descScale = scaleref
    , descProgram = program
    , descVAO = vao
    , descNVertices = fromIntegral (length poses)
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


updateUniforms :: Descriptor -> IO ()
updateUniforms d = do
  -- get settings from IORef
  (Vector3 ax ay az) <- get $ descAngles d
  scale <- get $ descScale d

  -- locate uniforms
  rotAngles <- get $ uniformLocation (descProgram d) "rotAngles"
  scaleFactor <- get $ uniformLocation (descProgram d) "scaleFactor"

  -- write uniforms
  uniform rotAngles $= Vector3 ax ay az
  uniform scaleFactor $= scale

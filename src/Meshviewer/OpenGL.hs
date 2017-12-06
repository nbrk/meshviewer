{-# LANGUAGE TemplateHaskell #-}
-- | Initializations and rendering of OpenGL viewers
module Meshviewer.OpenGL where

import           Control.Lens
import           Data.Foldable
import           Foreign.Marshal.Array            (withArray)
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.Rendering.OpenGL

import           Meshviewer.OpenGL.FragmentShader
import           Meshviewer.OpenGL.LoadShaders
import           Meshviewer.OpenGL.Utils
import           Meshviewer.OpenGL.VertexShader
import           Meshviewer.Types


-- | OpenGL state of the viewer
data OpenGLContext = OpenGLContext
  { -- | VAO for each model in the viewer
    _glctxVAOList :: [VertexArrayObject]
    -- | Compiled shader program
  , _glctxProgram :: Program
  }


makeLenses ''OpenGLContext


-- | Compile the shader program and return ready-to-use gl ctx
glctxInit :: IO OpenGLContext
glctxInit = do
  p <- loadShaders [
    ShaderInfo VertexShader (StringSource  vertexShaderSourceString),
    ShaderInfo FragmentShader (StringSource fragmentShaderSourceString)]
  currentProgram $= Just p

  return $
    OpenGLContext
    { _glctxVAOList = []
    , _glctxProgram = p
    }


-- | Make a VAO context from the model
glctxAddModel :: Model -> OpenGLContext -> IO OpenGLContext
glctxAddModel m o = do
   -- bind the VAO
  vao <- genObjectName
  bindVertexArrayObject $= Just vao

  -- setup the VertexArray buffer
  setupArrayBuffer (o^.glctxProgram) (m^.modelMesh.meshVertices) 3 "vPosition"

  return $
    o & glctxVAOList %~ (\vs -> vao:vs)


-- | Given our ctx, render everything from an abstract viewer
glctxRenderViewer :: Viewer a -> OpenGLContext -> IO ()
glctxRenderViewer v o = do
--  putStrLn $ "glctxRenderViewer: models in viewer: " ++ (v^.viewerModels.to (show.length))

  clearColor $=  v^.viewerSettings.backgroundColor.to color4Of
  clear [ ColorBuffer, DepthBuffer ]


--  updateUniforms d
  _ <- foldlM (\i m -> glctxRenderModel m i o >> return (i + 1)) 0 (v^.viewerModels)

  flush

  return ()

-- | Render a model
glctxRenderModel :: Model -> Int -> OpenGLContext -> IO ()
glctxRenderModel m i o = do
  -- XXX bind VAO
  let vao = (o^.glctxVAOList) !! i
  bindVertexArrayObject $= Just vao

  -- XXX camera, etc...

  -- Draw
  polygonMode $= (Line, Line)
--  drawArrays TriangleFan 0 numvs
  putStrLn "glctxRenderModel"
  drawArrays Triangles 0 (m^.modelMesh.meshVertices.to (fromIntegral . length))
--  drawArrays LineLoop 0 numvs
  return ()


-- | A x-offset pointer into a buffer
bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral


-- | Set up new array buffer
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

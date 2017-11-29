module GLUT where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad

import Types
import OpenGL(updateUniforms)


prepareWindow :: IO ()
prepareWindow = do
  initialDisplayMode $= [ RGBAMode, WithDepthBuffer ]
  initialWindowSize $= Size 512 512
  initialContextVersion $= (4, 2)
  initialContextProfile $= [ CoreProfile ]
  createWindow "Mesh Viewer"
  return ()


consumeArgsAndInit :: IO [String]
consumeArgsAndInit = do
  (_, args) <- getArgsAndInitialize
  return args


setupGLUT :: Descriptor -> IO ()
setupGLUT desc = do
  depthFunc $= Just Less
  displayCallback $= display desc
  specialCallback $= Just (specKeyDown desc)



loopGLUT :: IO ()
loopGLUT = mainLoop

----

display :: Descriptor -> DisplayCallback
display d = do
  clear [ ColorBuffer, DepthBuffer ]

  updateUniforms d

  polygonMode $= (Line, Line)
--  drawArrays TriangleFan 0 numvs
  drawArrays Triangles 0 (descNVertices d)
--  drawArrays LineLoop 0 numvs
  flush


specKeyDown :: Descriptor -> SpecialCallback
specKeyDown d KeyLeft _ = (descAngles d) $~ (\(Vector3 ax ay az) -> Vector3 ax (ay-0.1) az) >> p
specKeyDown d KeyRight _ = (descAngles d) $~ (\(Vector3 ax ay az) -> Vector3 ax (ay+0.1) az) >> p
specKeyDown d KeyUp _ = (descAngles d) $~ (\(Vector3 ax ay az) -> Vector3 (ax+0.1) ay az) >> p
specKeyDown d KeyDown _ = (descAngles d) $~ (\(Vector3 ax ay az) -> Vector3 (ax-0.1) ay az) >> p
specKeyDown d KeyHome _ = (descAngles d) $~ (\(Vector3 ax ay az) -> Vector3 ax ay (az-0.1)) >> p
specKeyDown d KeyEnd _ = (descAngles d) $~ (\(Vector3 ax ay az) -> Vector3 ax ay (az+0.1)) >> p
specKeyDown d KeyPageUp _ = (descScale d) $~ (\scale -> scale + 0.02) >> p
specKeyDown d KeyPageDown _ = (descScale d) $~ (\scale ->
                                                  if scale > 0.01
                                                  then scale - 0.2
                                                  else 0.01) >> p
specKeyDown _ _ _ = return ()

p = postRedisplay Nothing

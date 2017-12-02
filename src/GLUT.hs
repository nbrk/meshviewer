module GLUT where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad

import Types
import GLUTCallbacks


prepareWindow :: IO ()
prepareWindow = do
  initialDisplayMode $= [ RGBAMode, WithDepthBuffer ]
  initialWindowSize $= Size 512 512
  initialContextVersion $= (4, 2)
  initialContextProfile $= [ CoreProfile ]
  createWindow "Mesh Viewer"
--  fullScreen
  return ()


consumeArgsAndInit :: IO [String]
consumeArgsAndInit = do
  (_, args) <- getArgsAndInitialize
  return args


setupGLUT :: Descriptor -> IO ()
setupGLUT desc = do
  depthFunc $= Just Less
  cursor $= None
  displayCallback $= display desc
--  specialCallback $= Just (specKeyDown desc)
  keyboardCallback $= Just (keyboard desc)
  passiveMotionCallback $= Just (passiveMotion desc)
  mouseWheelCallback $= Just (mouseWheel desc)



loopGLUT :: IO ()
loopGLUT = mainLoop



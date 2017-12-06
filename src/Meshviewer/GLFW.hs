{-# LANGUAGE TemplateHaskell #-}
module Meshviewer.GLFW where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Loops
import           Data.Maybe
import qualified Graphics.UI.GLFW    as GLFW

import           Meshviewer.OpenGL
import           Meshviewer.Types
import           Meshviewer.Viewer


-- | The window managed by GLFW
data GLFWContext = GLFWContext
  { _glfwWindow    :: GLFW.Window
  , _glfwOpenGLCtx :: OpenGLContext
  }


makeLenses ''GLFWContext


instance GraphicsContext GLFWContext where
  createViewer = glfwCreateViewer
  loop = glfwLoopViewer
  render = glfwRenderViewer
  bufferModel = glfwBufferModel


glfwCreateViewer :: ViewerSettings -> IO (Viewer GLFWContext)
glfwCreateViewer s = do
  ok <- GLFW.init
  when (not ok) $
    error "Can't init GLFW"

  -- fullscreen
  m <- if (s^.fullscreen)
       then GLFW.getPrimaryMonitor
       else return Nothing

  w <- GLFW.createWindow (s^.size._1) (s^.size._2) "viewer" m Nothing

  when (isNothing w) $ do
    GLFW.terminate
    error "Can't create GLFW window"

  GLFW.makeContextCurrent w

  o <- glctxInit
  let c = GLFWContext
              { _glfwWindow = fromJust w
              , _glfwOpenGLCtx = o
              }

  return $ defaultViewerWith s c


-- XXX
glfwLoopViewer :: Viewer GLFWContext -> IO ()
glfwLoopViewer v = do
  let w = v^.viewerGraphicsContext.glfwWindow
  do
--    glfwRenderViewer v

    GLFW.swapBuffers w
    GLFW.pollEvents
    `untilM_` GLFW.windowShouldClose w

  putStrLn "glfwLoopViewer done"


glfwBufferModel :: Model -> Viewer GLFWContext -> IO (Viewer GLFWContext)
glfwBufferModel m v = do
  -- add the model to the underlying system
  o' <- glctxAddModel m (v^.viewerGraphicsContext.glfwOpenGLCtx)

  return $
    v & (viewerGraphicsContext.glfwOpenGLCtx) .~ o'


glfwRenderViewer :: Viewer GLFWContext -> IO ()
glfwRenderViewer v = glctxRenderViewer v (v^.viewerGraphicsContext.glfwOpenGLCtx)
--glfwRenderViewer v = do
-- glctxRenderModel v^.viewerGraphicsContext.glfwOpenGLCtx

module Main where

import Control.Monad
import Text.Printf

import GLUT
import OpenGL
import LoadMesh
import Types

main :: IO ()
main = do
  -- process GLUT args and return any left
  args <- consumeArgsAndInit

  -- load mesh
  when (null args) $ error "Please specify a .STL or .OBJ mesh filename!"
  mesh <- meshFromFile (head args)

  printf "Loaded %s mesh (%d vertices)\n" (show (meshFileType mesh)) (meshNumVertices mesh)

  -- prepare the window and the OpenGL context
  prepareWindow

  -- setup the OpenGL pipeline
  desc <- setupGL mesh

  -- setup GLUT callbacks and other parameters
  setupGLUT desc

  -- enter main loop
  loopGLUT

module Meshviewer.OpenGL.Utils where

import           Graphics.Rendering.OpenGL
import           Linear


-- | Convert a vector to an opengl color
color4Of :: V4 Float -> Color4 GLfloat
color4Of (V4 x y z w) = Color4 x y z w

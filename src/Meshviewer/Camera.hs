module Meshviewer.Camera where

import Linear

import Meshviewer.Types

-- | Default camera at (0,0,1) looking at the origin
defaultCamera :: Camera
defaultCamera =
  Camera (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

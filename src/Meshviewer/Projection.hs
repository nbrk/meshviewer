module Meshviewer.Projection where

import Meshviewer.Types

-- | Default to orthographic projection
defaultProjection :: Projection
defaultProjection =
  Projection
  { _projOrtho = True
  }

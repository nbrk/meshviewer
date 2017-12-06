module Meshviewer.Viewer where

import Control.Lens

import Meshviewer.Types
import Meshviewer.Camera
import Meshviewer.Projection
import Meshviewer.ViewerSettings


-- | Default viewer holds no models
defaultViewerWith :: ViewerSettings -> a -> Viewer a
defaultViewerWith s a =
  Viewer
  { _viewerModels = []
  , _viewerCamera = defaultCamera
  , _viewerProjection = defaultProjection
  , _viewerSettings = s
  , _viewerGraphicsContext = a
  }


defaultViewer = defaultViewerWith defaultViewerSettings


-- | Add a model to the viewer and its associated graphics context
addModel :: GraphicsContext a => Model -> Viewer a -> IO (Viewer a)
addModel m v = do
  let v' = v & (viewerModels) %~ (\ms -> m:ms)

  bufferModel m v'


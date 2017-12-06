module Meshviewer.ViewerSettings where

import Linear

import Meshviewer.Types


defaultViewerSettings :: ViewerSettings
defaultViewerSettings =
  ViewerSettings
  { _fullscreen = False
  , _backgroundColor = V4 0.2 0.2 0.3 1 -- grey
  , _useMouse = False
  , _speed = 1
  , _size = (640, 480)
  }

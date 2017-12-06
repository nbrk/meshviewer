{-# LANGUAGE TemplateHaskell #-}
module Meshviewer.Types where

import           Control.Lens
import           Linear



-- | Type of primitives the mesh consists of
data MeshPrimitive = MeshTriangles
                   | MeshPoints
                   | MeshLines

-- | Abstract mesh data
data Mesh = Mesh
  { _meshVertices  :: [V3 Float]
  -- Normals, UVs, ...
  , _meshPrimitive :: MeshPrimitive
  }


-- | All rotations are given in radians
type Radian = Float

-- | A mesh with settings
data Model = Model
  { _modelMesh      :: Mesh
  -- | Position in the world
  , _modelPos       :: V3 Float
  , _modelWireframe :: Bool
  , _modelColor     :: V4 Float
  , _modelScale     :: Float
  , _modelRotation  :: V3 Radian
  }

-- | Settings for the viewer's view matrix
data Camera = Camera
  { _cameraPos :: V3 Float
  , _cameraDir :: V3 Float
  , _cameraUp  :: V3 Float
  }

-- | Settings for the viewer's projection matrix
data Projection = Projection
  { _projOrtho :: Bool
  -- fov (zoom), ...
  }


-- | Settings for the viewer itself
data ViewerSettings = ViewerSettings
  { _fullscreen      :: Bool
  , _backgroundColor :: V4 Float
  -- | Whether to use mouse (and to grab the pointer)
  , _useMouse        :: Bool
  -- | Fly-by speed
  , _speed           :: Float
  -- | Current window size (width, height)
  , _size            :: (Int, Int)
  }


-- | The viewer can hold several models (there is a special gfx ctx for each one)
data Viewer g = Viewer
  { _viewerModels          :: [Model]
  , _viewerCamera          :: Camera
  , _viewerProjection      :: Projection
  , _viewerSettings        :: ViewerSettings
  , _viewerGraphicsContext :: g
  }


-- | XXX concrete gfx context necessary and sufficient to render the model
class GraphicsContext a where
  createViewer :: ViewerSettings -> IO (Viewer a)
  loop :: Viewer a -> IO ()
  bufferModel :: Model -> Viewer a -> IO (Viewer a)
  render :: Viewer a -> IO ()

--
-- Generate lenses (must follow the data declarations).
--
makeLenses ''Mesh
makeLenses ''Model
makeLenses ''Camera
makeLenses ''Projection
makeLenses ''ViewerSettings
makeLenses ''Viewer

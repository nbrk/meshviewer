module Main where

import           Linear

import           Meshviewer.GLFW
import           Meshviewer.Types
import           Meshviewer.Viewer
import           Meshviewer.ViewerSettings


mesh1 = Mesh
  { _meshPrimitive = MeshTriangles
  , _meshVertices =
    [ V3 (-0.5) 0 (-1)
    , V3 0 0.5 (-1)
    , V3 0.5 0 (-1)
    ]
  }


model1 = Model
  { _modelMesh = mesh1
  }


main :: IO ()
main = do
  v <- createViewer defaultViewerSettings :: IO (Viewer GLFWContext)

  v' <- addModel model1 v

  render v'
  loop v'


  putStrLn "ok"

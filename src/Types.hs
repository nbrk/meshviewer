module Types where

import Graphics.UI.GLUT
import Data.IORef


data MeshFileType = MeshFileSTL | MeshFileOBJ

instance Show MeshFileType where
  show MeshFileOBJ = "OBJ"
  show MeshFileSTL = "STL"


data Mesh = Mesh
  { meshFileType :: MeshFileType
  , meshNumVertices :: Int
  , meshPositions :: [Vertex3 Float]
  }


data Descriptor = Descriptor
  { descAngles :: IORef (Vector3 Float)
  , descColor :: IORef (Vector3 Float)
  , descScale :: IORef Float
  , descProgram ::Program
  , descVAO :: VertexArrayObject
  , descNVertices :: NumArrayIndices
  }

module Types where

import Graphics.UI.GLUT
import Data.IORef
import Linear
import Data.Time.Clock


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
  { descProgram ::Program
  , descVAO :: VertexArrayObject
  , descNVertices :: NumArrayIndices

  , descWindowSize :: IORef Size
  , descWindowPos :: IORef Position
  , descTimestamp :: IORef UTCTime

  , descCameraPos :: IORef (V3 Float)
  , descCameraDir :: IORef (V3 Float)
  , descVertAngle :: IORef Float
  , descHorizAngle :: IORef Float
  , descFOV :: IORef Float
  , descCameraRight :: IORef (V3 Float)
  , descA :: IORef Float
  , descB :: IORef Float
  }

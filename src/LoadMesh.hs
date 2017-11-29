module LoadMesh where

import Graphics.UI.GLUT
import qualified Data.ByteString as B
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Maybe

import qualified Graphics.Formats.STL as STL
import qualified Codec.Wavefront as OBJ

import Types


meshFromFile :: FilePath -> IO Mesh
meshFromFile fp = do
  mbmesh <- trySTL fp
  if isJust mbmesh
    then return (fromJust mbmesh)
    else do
      mbmesh <- tryOBJ fp
      if isJust mbmesh
        then return (fromJust mbmesh)
        else error "Not a STL or OBJ mesh!"


trySTL :: FilePath -> IO (Maybe Mesh)
trySTL fp = do
  bs <- B.readFile fp
  case (S.decode bs :: Either String STL.STL) of
    Left _ -> return Nothing
    Right stl ->
      let ts = STL.triangles stl
      in
        return $ Just Mesh
        { meshFileType = MeshFileSTL
        , meshNumVertices = (length ts) * 3
        , meshPositions = concatMap verticesOfTriangle ts
        }

tryOBJ :: FilePath -> IO (Maybe Mesh)
tryOBJ fp = do
  e <- OBJ.fromFile fp
  case e of
    Left _ -> return Nothing
    Right obj -> let
      locsv = OBJ.objLocations obj
      in
        return $ Just Mesh
        { meshFileType = MeshFileOBJ
        , meshNumVertices = V.length locsv
        , meshPositions = V.toList $ fmap verticeFromLocation locsv
        }


verticesOfTriangle :: STL.Triangle -> [Vertex3 GLfloat]
verticesOfTriangle (STL.Triangle _normal vs) =
  let (  (v1x, v1y, v1z)
        ,(v2x, v2y, v2z)
        ,(v3x, v3y, v3z)) = vs
  in
    [ Vertex3 v1x v1y v1z
    , Vertex3 v2x v2y v2z
    , Vertex3 v3x v3y v3z
    ]


verticeFromLocation :: OBJ.Location -> Vertex3 GLfloat
verticeFromLocation (OBJ.Location x y z _w) = Vertex3 x y z

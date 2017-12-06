{-# LANGUAGE QuasiQuotes #-}
module Meshviewer.OpenGL.VertexShader where

import           Text.RawString.QQ

vertexShaderSourceString = [r|
#version 130

in vec3 vPosition;

uniform mat4 MVP;


void
main()
{
  gl_Position = MVP * vec4(vPosition, 1);
//  gl_Position = vec4(vPosition, 1);
}
|]

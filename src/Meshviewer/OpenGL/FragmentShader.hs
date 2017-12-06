{-# LANGUAGE QuasiQuotes #-}
module Meshviewer.OpenGL.FragmentShader where

import           Text.RawString.QQ


fragmentShaderSourceString = [r|
#version 130

out vec4 outColor;

void
main()
{
  outColor = vec4(1.0, 0.0, 0.0, 1.0);
}
|]


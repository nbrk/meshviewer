#version 420 core

in vec3 vPosition;

uniform mat4 MVP;


// uniform vec3 rotAngles;
// uniform float scaleFactor;


// mat4 rotationMatrix(vec3 axis, float angle)
// {
//   axis = normalize(axis);
//   float s = sin(angle);
//   float c = cos(angle);
//   float oc = 1.0 - c;


//   return mat4
//     (oc*axis.x*axis.x+c,oc*axis.x*axis.y-axis.z*s,oc*axis.z*axis.x+axis.y*s,0.0,
//      oc*axis.x*axis.y+axis.z*s,oc*axis.y*axis.y+c,oc*axis.y*axis.z-axis.x*s,0.0,
//      oc*axis.z*axis.x-axis.y*s,oc*axis.y*axis.z+axis.x*s,oc*axis.z*axis.z+c,0.0,
//      0.0,0.0,0.0,1.0);
// }


void
main()
{
  // mat4 rot =
  //   rotationMatrix(vec3(1,0,0), rotAngles.x) *
  //   rotationMatrix(vec3(0,1,0), rotAngles.y) *
  //   rotationMatrix(vec3(0,0,1), rotAngles.z);

  // float s = scaleFactor;
  // mat4 scale = mat4(
  //                   s,0,0,0,
  //                   0,s,0,0,
  //                   0,0,s,0,
  //                   0,0,0,1);

  // vec4 pos = scale * rot * vec4(vPosition.xyz, 1);

  // XXX assembly MVP from vectors
  // mat4 MVP = mat4(
  //                 MVP1,
  //                 MVP2,
  //                 MVP3,
  //                 MVP4);

  gl_Position = MVP * vec4(vPosition, 1);
}



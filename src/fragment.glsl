#version 420 core


//in vec2 Texcoord;
in vec3 VColor;

out vec4 outColor;

//uniform sampler2D tex;

void
main()
{
  //outColor = texture(tex, Texcoord);
      outColor = vec4(1.0, 0.0, 0.0, 1.0);
  //  outColor = vec4(VColor, 1);
}

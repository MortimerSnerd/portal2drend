uniform sampler2D texImage1; 
in vec4 vtxColor;
out vec4 outColor;

void main()
{
   outColor = vtxColor * 2.0f*(1.2f - gl_FragCoord.z);
}


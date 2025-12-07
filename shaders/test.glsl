#version 330 core
#if defined(VERTEX)
layout(location = 0) in vec2 a_position;
out vec2 v_texCoord;
void main() {
    gl_Position = vec4(a_position, 0.0, 1.0);
    v_texCoord = vec2(a_position.x, a_position.y) * 0.5 + 0.5;
}
#elif defined(FRAGMENT)
in vec2 v_texCoord;
out vec4 FragColor;
uniform sampler2D rubyTexture;
void main() {
    FragColor = vec4(texture(rubyTexture, v_texCoord).rgb, 1.0);
}
#endif

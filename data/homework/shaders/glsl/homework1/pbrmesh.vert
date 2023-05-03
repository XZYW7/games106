#version 450

layout (location = 0) in vec3 inPos;
layout (location = 1) in vec3 inNormal;
layout (location = 2) in vec2 inUV;
layout (location = 3) in vec3 inColor;
layout (location = 4) in vec4 inTangent;
layout (location = 5) in uint inJointIndex;


layout (set = 0, binding = 0) uniform UBOScene
{
	mat4 projection;
	mat4 view;
	vec4 lightPos;
	vec4 viewPos;
} uboScene;
layout(std430, set = 2, binding = 0) readonly buffer JointMatrices {
	mat4 jointMatrices[];
};
layout(push_constant) uniform PushConsts {
	mat4 model;
} primitive;

layout (location = 0) out vec3 outNormal;
layout (location = 1) out vec3 outColor;
layout (location = 2) out vec2 outUV;
layout (location = 3) out vec3 outworldPos;
layout (location = 4) out vec4 outTangent;
void main() 
{
	outNormal = inNormal;
	outColor = inColor;
	outUV = inUV;
	gl_Position = uboScene.projection * uboScene.view * jointMatrices[inJointIndex] *vec4(inPos.xyz, 1.0);//
	
	vec4 pos = jointMatrices[inJointIndex] * vec4(inPos, 1.0);
	outNormal = (transpose(inverse(jointMatrices[inJointIndex])) * vec4(inNormal,0.0)).xyz;
	outTangent = vec4(mat3(primitive.model) * inTangent.xyz, inTangent.w);
	outworldPos = pos.xyz;
}
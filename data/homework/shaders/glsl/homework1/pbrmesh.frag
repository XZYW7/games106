#version 450
layout (set = 0, binding = 0) uniform UBOScene
{
	mat4 projection;
	mat4 view;
	vec4 lightPos;
	vec4 viewPos;
} uboScene;
layout (set = 1, binding = 0) uniform sampler2D samplerColorMap;
layout (set = 1, binding = 1) uniform sampler2D samplerMRMap;
layout (set = 1, binding = 2) uniform sampler2D samplerNormalMap;
layout (set = 1, binding = 3) uniform sampler2D samplerEmissiveMap;

layout (location = 0) in vec3 inNormal;
layout (location = 1) in vec3 inColor;
layout (location = 2) in vec2 inUV;
layout (location = 3) in vec3 inworldPos;
layout (location = 4) in vec4 inTangent;

layout (location = 0) out vec4 outFragColor;

const float PI = 3.14159265359;


// Normal Distribution function --------------------------------------
float D_GGX(float dotNH, float roughness)
{
	float alpha = roughness * roughness;
	float alpha2 = alpha * alpha;
	float denom = dotNH * dotNH * (alpha2 - 1.0) + 1.0;
	return (alpha2)/(PI * denom*denom); 
}

// Geometric Shadowing function --------------------------------------
float G_SchlicksmithGGX(float dotNL, float dotNV, float roughness)
{
	float r = (roughness + 1.0);
	float k = (r*r) / 8.0;
	float GL = dotNL / (dotNL * (1.0 - k) + k);
	float GV = dotNV / (dotNV * (1.0 - k) + k);
	return GL * GV;
}

// Fresnel function ----------------------------------------------------
vec3 F_Schlick(float cosTheta, vec3 F0)
{
	return F0 + (1.0 - F0) * pow(1.0 - cosTheta, 5.0);
}
vec3 F_SchlickR(float cosTheta, vec3 F0, float roughness)
{
	return F0 + (max(vec3(1.0 - roughness), F0) - F0) * pow(1.0 - cosTheta, 5.0);
}

// Specular BRDF composition --------------------------------------------

vec3 BRDF(vec3 L, vec3 V, vec3 N, float metallic, float roughness, vec3 albedo)
{
	// Precalculate vectors and dot products	
	vec3 H = normalize (V + L);

	float dotNV = clamp(dot(N, V), 0.0, 1.0);

	float dotNL = clamp(dot(N, L), 0.0, 1.0);

	float dotLH = clamp(dot(L, H), 0.0, 1.0);
	float dotNH = clamp(dot(N, H), 0.0, 1.0);

	// Light color fixed
	vec3 lightColor = vec3(1.0);

	vec3 color = vec3(0.0);

	if (dotNL > 0.0)
	{
		float rroughness = max(0.05, roughness);
		// D = Normal distribution (Distribution of the microfacets)
		float D = D_GGX(dotNH, roughness); 
		// G = Geometric shadowing term (Microfacets shadowing)
		float G = G_SchlicksmithGGX(dotNL, dotNV, rroughness);
		// F = Fresnel factor (Reflectance depending on angle of incidence)
		vec3 F0 = vec3(0.04); 
		F0 = mix(F0, albedo, metallic);
		vec3 F = F_SchlickR(dotNV, F0,roughness);

		vec3 spec = D * F * G / (4.0 * dotNL * dotNV+ 0.001);
		vec3 kD = (1.0 - metallic) * (vec3(1.0,1.0,1.0) - F);	
		color +=(kD*albedo+spec )* dotNL * lightColor;
	}

	return color;
}

vec3 calculateNormaleasy()
{
	vec3 tangentNormal = texture(samplerNormalMap, inUV).xyz * 2.0 - 1.0;
	vec3 Q1  = dFdx(inworldPos);
    vec3 Q2  = dFdy(inworldPos);
    vec2 st1 = dFdx(inUV);
    vec2 st2 = dFdy(inUV);

    vec3 N = normalize(inNormal);
    vec3 T = normalize(Q1*st2.t - Q2*st1.t);
    vec3 B = -normalize(cross(N, T));
    mat3 TBN = mat3(T, B, N);
	return normalize(TBN * tangentNormal);
}
vec3 calculateNormal()
{
	vec3 tangentNormal = texture(samplerNormalMap, inUV).xyz * 2.0 - 1.0;

	vec3 N = normalize(inNormal);
	vec3 T = normalize(inTangent.xyz);
	vec3 B = normalize(cross(N, T));
	mat3 TBN = mat3(T, B, N);
	return normalize(TBN * tangentNormal);
}
vec3 Tonemap_ACES(const vec3 c) {
    // Narkowicz 2015, "ACES Filmic Tone Mapping Curve"
    // const float a = 2.51;
    // const float b = 0.03;
    // const float c = 2.43;
    // const float d = 0.59;
    // const float e = 0.14;
    // return saturate((x*(a*x+b))/(x*(c*x+d)+e));

    //ACES RRT/ODT curve fit courtesy of Stephen Hill
	vec3 a = c * (c + 0.0245786) - 0.000090537;
	vec3 b = c * (0.983729 * c + 0.4329510) + 0.238081;
	return a / b;
}
vec3 GammaToLinear(vec3 color)
{
	return pow(color,vec3(2.2f));
}
vec3 LinearToGamma(vec3 color)
{
	return pow(color,vec3(1.0f/2.2f));
}
void main() 
{
	vec3 albedo = GammaToLinear(texture(samplerColorMap, inUV).xyz) * inColor;
	vec3 N = calculateNormal();
	vec3 L = normalize(uboScene.lightPos.xyz - inworldPos);

	vec3 V = normalize((transpose(uboScene.view) * uboScene.viewPos).xyz-inworldPos);

	vec3 ORM = (texture(samplerMRMap, inUV) * vec4(inColor, 1.0)).rgb;
	float metallic = ORM.b;
	float roughness = ORM.g;
	float ao = ORM.r;
	vec3 Lo = BRDF(L, V, N, metallic, roughness, albedo);
	
	// Combine with ambient
	vec3 color = albedo * 0.02 * ao;
	color += Lo;
	vec3 emissive = texture(samplerEmissiveMap, inUV).xyz * uboScene.lightPos.w;
	color += emissive;

	// Tone mapping
	color = Tonemap_ACES(color * 1.0);
	// Gamma correction
	color = LinearToGamma(color);

	outFragColor = vec4(color,1.0);		
}


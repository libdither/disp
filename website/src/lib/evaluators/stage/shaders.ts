// GLSL for the stage. Every instance names a node; the node table (a float texture, two texels a node:
// where it was and how it is changing, where it is going and when) says where that node is now, so a
// whole drawing moves in one tween worked out here, with nothing per frame on the CPU. A node that is
// new grows in after its delay; one that is leaving shrinks where it stands, from when it began to.

export const NODE_GLSL = /* glsl */ `
uniform highp sampler2D uNodes;
uniform float uNodesW;
uniform float uNow;
uniform float uMoveStart;
uniform float uMoveDur;
uniform float uInDur;
uniform float uOutDur;
uniform float uHover;

vec4 nodeTexel(float i) {
  return texelFetch(uNodes, ivec2(int(mod(i, uNodesW)), int(floor(i / uNodesW))), 0);
}
/// The site's glide: a small overshoot, as TreeVis eases.
float easeMove(float t) {
  float c = 0.9;
  float u = t - 1.0;
  return 1.0 + (c + 1.0) * u * u * u + c * u * u;
}
float cubicOut(float t) {
  float f = t - 1.0;
  return f * f * f + 1.0;
}
/// Where node n is now, and how far it has grown in (1) or shrunk away (0).
vec3 nodePos(float n, out float life) {
  vec4 a = nodeTexel(2.0 * n);
  vec4 b = nodeTexel(2.0 * n + 1.0);
  float k = uMoveDur > 0.0 ? clamp((uNow - uMoveStart) / uMoveDur, 0.0, 1.0) : 1.0;
  life = 1.0;
  if (a.w > 0.5 && a.w < 1.5) life = uInDur > 0.0 ? cubicOut(clamp((uNow - uMoveStart - b.w) / uInDur, 0.0, 1.0)) : 1.0;
  else if (a.w > 1.5) life = uOutDur > 0.0 ? 1.0 - cubicOut(clamp((uNow - uMoveStart - b.w) / uOutDur, 0.0, 1.0)) : 0.0;
  return mix(a.xyz, b.xyz, easeMove(k));
}
`

/** Flags an instance can carry. */
export const F = {
  /** Its stroke thickens while its node is under the pointer. */
  hoverGrow: 1,
  /** It pulses: the next move to fire. */
  pulse: 2,
  /** It fades in once its node has landed: a mark that is new. */
  markIn: 4,
  /** It shows only while its node is under the pointer. */
  hoverOnly: 8,
  /** A leaf's vein. */
  vein: 16,
  /** A pod's seam. */
  seam: 32,
  /** It brightens while its node is under the pointer. */
  hoverBright: 64
} as const

const HAS_GLSL = /* glsl */ `
bool has(float flags, float bit) { return mod(floor(flags / bit), 2.0) > 0.5; }
`

const FLAG_GLSL = /* glsl */ `
${HAS_GLSL}
/// An instance's opacity from its flags: pulsing, fading in, shown on hover.
float flagAlpha(float flags, float node) {
  float a = 1.0;
  if (has(flags, ${F.pulse}.0)) a *= 1.0 - 0.45 * (0.5 - 0.5 * cos(6.2831853 * uNow));
  if (has(flags, ${F.markIn}.0)) a *= uMoveDur > 0.0 ? clamp((uNow - uMoveStart - 0.3) / 0.24, 0.0, 1.0) : 1.0;
  if (has(flags, ${F.hoverOnly}.0) && abs(node - uHover) > 0.5) a = 0.0;
  return a;
}
`

/** Shapes a glyph can take. */
export const SHAPE = { disc: 0, leaf: 1 } as const

export const GLYPH_VERT = /* glsl */ `
${NODE_GLSL}
${FLAG_GLSL}
in float aNode;
in vec2 aOff;
in float aR;
in float aShape;
in vec4 aFill;
in vec4 aStroke;
in vec4 aParam;
out vec2 vLocal;
out float vR;
out float vShape;
out vec4 vFill;
out vec4 vStroke;
out vec4 vParam;
out float vAlpha;
void main() {
  float life;
  vec3 p = nodePos(aNode, life);
  float hovered = abs(aNode - uHover) < 0.5 ? 1.0 : 0.0;
  float sw = aParam.x * (has(aParam.w, ${F.hoverGrow}.0) && hovered > 0.5 ? 1.6 : 1.0);
  float ext = aR + sw + 1.0;
  vec4 mv = modelViewMatrix * vec4(p, 1.0);
  mv.xy += (aOff + position.xy * ext) * life;
  gl_Position = projectionMatrix * mv;
  vLocal = position.xy * ext;
  vR = aR;
  vShape = aShape;
  vFill = aFill;
  vStroke = aStroke;
  if (has(aParam.w, ${F.hoverBright}.0) && hovered > 0.5) vFill.rgb = min(vec3(1.0), vFill.rgb * 1.08);
  vParam = vec4(sw, aParam.yzw);
  vAlpha = flagAlpha(aParam.w, aNode) * step(0.001, life);
}
`

export const GLYPH_FRAG = /* glsl */ `
${HAS_GLSL}
in vec2 vLocal;
in float vR;
in float vShape;
in vec4 vFill;
in vec4 vStroke;
in vec4 vParam;
in float vAlpha;
/// A leaf: two arcs meeting at its tips (a vesica), its tips on the y axis.
float sdLeaf(vec2 p, float h) {
  float w = h * 0.483;
  float r = (w * w + h * h) / (2.0 * w);
  float d = r - w;
  p = abs(p);
  return ((p.y - h) * d > p.x * h) ? length(p - vec2(0.0, h)) : length(p - vec2(-d, 0.0)) - r;
}
void main() {
  vec2 q = vLocal;
  float rot = vParam.z;
  if (rot != 0.0) q = mat2(cos(rot), sin(rot), -sin(rot), cos(rot)) * q;
  float d = vShape < 0.5 ? length(q) - vR : sdLeaf(q, vR);
  float aa = max(fwidth(d), 1e-4);
  float inside = 1.0 - smoothstep(-aa, aa, d);
  float sw = vParam.x;
  float ring = sw > 0.0 ? 1.0 - smoothstep(sw * 0.5 - aa, sw * 0.5 + aa, abs(d)) : 0.0;
  float dashes = vParam.y;
  if (dashes > 0.0) {
    float t = fract(atan(q.y, q.x) / 6.2831853 * dashes);
    ring *= step(t, 0.5);
  }
  vec4 fill = vFill;
  float flags = vParam.w;
  if (has(flags, ${F.vein}.0)) {
    float v = 1.0 - smoothstep(0.4 - aa, 0.4 + aa, abs(q.x));
    v *= 1.0 - smoothstep(vR * 0.66 - aa, vR * 0.66 + aa, abs(q.y));
    fill.rgb = mix(fill.rgb, vec3(1.0), 0.55 * v);
  }
  if (has(flags, ${F.seam}.0)) {
    float s = vR / 9.0;
    float seam = abs(length(q - vec2(-12.29 * s, 0.0)) - 13.59 * s);
    float v = (1.0 - smoothstep(0.55 * s - aa, 0.55 * s + aa, seam)) * (1.0 - smoothstep(5.8 * s - aa, 5.8 * s + aa, abs(q.y)));
    fill.rgb = mix(fill.rgb, vec3(1.0), 0.55 * v);
  }
  float af = fill.a * inside;
  float as_ = vStroke.a * ring;
  vec3 c = vStroke.rgb * as_ + fill.rgb * af * (1.0 - as_);
  float a = as_ + af * (1.0 - as_);
  gl_FragColor = vec4(c, a) * vAlpha;
  if (gl_FragColor.a < 0.002) discard;
}
`

export const LABEL_VERT = /* glsl */ `
${NODE_GLSL}
${FLAG_GLSL}
in float aNode;
in vec2 aOff;
in vec2 aSize;
in vec4 aUV;
in vec4 aColor;
in float aFlags;
out vec2 vUV;
out vec4 vColor;
out float vAlpha;
void main() {
  float life;
  vec3 p = nodePos(aNode, life);
  vec4 mv = modelViewMatrix * vec4(p, 1.0);
  // position.xy runs 0..1 over the quad; aOff is its bottom-left corner from the node
  mv.xy += (aOff + position.xy * aSize) * life;
  gl_Position = projectionMatrix * mv;
  vUV = mix(aUV.xy, aUV.zw, position.xy);
  vColor = aColor;
  vAlpha = flagAlpha(aFlags, aNode) * life;
}
`

export const LABEL_FRAG = /* glsl */ `
uniform sampler2D uAtlas;
in vec2 vUV;
in vec4 vColor;
in float vAlpha;
void main() {
  float a = texture(uAtlas, vUV).a * vColor.a * vAlpha;
  if (a < 0.002) discard;
  gl_FragColor = vec4(vColor.rgb * a, a);
}
`

export const STROKE_VERT = /* glsl */ `
${NODE_GLSL}
uniform vec3 uBg;
in float aA;
in float aB;
in vec2 aT;
in float aWidth;
in vec4 aColor;
in vec2 aDash;
in vec2 aBend;
out vec2 vP;
out vec2 vS0;
out vec2 vS1;
out float vW;
out vec3 vColor;
out float vLife;
out float vDashT;
out vec2 vDash;
vec2 bez(vec2 a, vec2 c, vec2 b, float t) {
  float u = 1.0 - t;
  return u * u * a + 2.0 * u * t * c + t * t * b;
}
void main() {
  float la, lb;
  vec3 A = nodePos(aA, la);
  vec3 B = nodePos(aB, lb);
  vec2 C = A.xy + (B.xy - A.xy) * aBend;
  vec2 s0 = bez(A.xy, C, B.xy, aT.x);
  vec2 s1 = bez(A.xy, C, B.xy, aT.y);
  vec2 dir = s1 - s0;
  float len = length(dir);
  dir = len > 1e-5 ? dir / len : vec2(1.0, 0.0);
  vec2 nrm = vec2(-dir.y, dir.x);
  float h = aWidth * 0.5 + 1.0;
  // position.x is 0 or 1 along the segment, position.y -1 or 1 across it; the ends reach past for round caps
  vec2 p = mix(s0 - dir * h, s1 + dir * h, position.x) + nrm * h * position.y;
  gl_Position = projectionMatrix * modelViewMatrix * vec4(p, mix(A.z, B.z, 0.5), 1.0);
  vP = p;
  vS0 = s0;
  vS1 = s1;
  vW = aWidth;
  // a branch's opacity is mixed into its colour over the background, so the joints of its segments never
  // show; it only grows in and shrinks away by real transparency
  vColor = mix(uBg, aColor.rgb, aColor.a);
  vLife = lb;
  vDash = aDash;
  vDashT = mix(aT.x, aT.y, position.x) * length(B.xy - A.xy);
}
`

export const STROKE_FRAG = /* glsl */ `
in vec2 vP;
in vec2 vS0;
in vec2 vS1;
in float vW;
in vec3 vColor;
in float vLife;
in float vDashT;
in vec2 vDash;
void main() {
  vec2 pa = vP - vS0;
  vec2 ba = vS1 - vS0;
  float h = clamp(dot(pa, ba) / max(dot(ba, ba), 1e-6), 0.0, 1.0);
  float d = length(pa - ba * h) - vW * 0.5;
  float aa = max(fwidth(d), 1e-4);
  float a = (1.0 - smoothstep(-aa, aa, d)) * vLife;
  if (vDash.x > 0.0 && mod(vDashT, vDash.x) > vDash.y) a = 0.0;
  if (a < 0.002) discard;
  gl_FragColor = vec4(vColor * a, a);
}
`

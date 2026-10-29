/**
 * Draws a live `scene` with three.js. Only data crosses the wire: this file
 * turns that data into meshes, and it eases each object from its last place to
 * its new one over one tick, so 10 patches a second still move at 60 fps.
 */

import {
  BufferGeometry,
  Color,
  Float32BufferAttribute,
  Mesh,
  OrthographicCamera,
  PerspectiveCamera,
  Scene,
  ShaderMaterial,
  Vector3,
  WebGLRenderer,
} from 'three';
import type { LiveSceneNode } from './live-view';
import {
  easePose,
  sceneCamera,
  sceneGeometry,
  SCENE_LIGHT,
  type LiveSceneObject,
  type ScenePose,
} from './scene';

// The same shading as the terminal rasterizer: flat faces, a camera-space
// light, and plain sRGB arithmetic, so both surfaces show the same colours.
const VERTEX = `
varying vec3 vNormal;
void main() {
  vNormal = normalize(normalMatrix * normal);
  gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
}`;
const FRAGMENT = `
uniform vec3 uColor;
uniform float uEmissive;
uniform vec3 uLight;
uniform float uAmbient;
uniform float uDiffuse;
varying vec3 vNormal;
void main() {
  float d = max(dot(normalize(vNormal), uLight), 0.0);
  vec3 lit = uColor * min(1.0, uAmbient + uDiffuse * d);
  gl_FragColor = vec4(mix(lit, uColor, uEmissive), 1.0);
}`;

const DEG = Math.PI / 180;

function rgb(hex: string): Vector3 {
  const n = Number.parseInt(hex.slice(1), 16);
  return new Vector3(((n >> 16) & 255) / 255, ((n >> 8) & 255) / 255, (n & 255) / 255);
}

/** A non-indexed geometry with one normal per face: the faces stay flat. */
function flatGeometry(vertices: number[][], faces: number[][]): BufferGeometry {
  const positions: number[] = [];
  const normals: number[] = [];
  for (const [a, b, c] of faces) {
    const [p, q, r] = [vertices[a], vertices[b], vertices[c]];
    const u = [q[0] - p[0], q[1] - p[1], q[2] - p[2]];
    const v = [r[0] - p[0], r[1] - p[1], r[2] - p[2]];
    const n = [u[1] * v[2] - u[2] * v[1], u[2] * v[0] - u[0] * v[2], u[0] * v[1] - u[1] * v[0]];
    const length = Math.hypot(n[0], n[1], n[2]) || 1;
    for (const point of [p, q, r]) {
      positions.push(point[0], point[1], point[2]);
      normals.push(n[0] / length, n[1] / length, n[2] / length);
    }
  }
  const geometry = new BufferGeometry();
  geometry.setAttribute('position', new Float32BufferAttribute(positions, 3));
  geometry.setAttribute('normal', new Float32BufferAttribute(normals, 3));
  return geometry;
}

interface Drawn {
  mesh: Mesh<BufferGeometry, ShaderMaterial>;
  from: ScenePose;
  to: ScenePose;
  start: number;
}

export interface SceneEngine {
  update(node: LiveSceneNode): void;
  dispose(): void;
}

/** Mount a renderer in `host`. Throws when the device has no WebGL. */
export function createSceneEngine(host: HTMLElement): SceneEngine {
  const renderer = new WebGLRenderer({ antialias: true });
  renderer.setPixelRatio(Math.min(2, window.devicePixelRatio || 1));
  const canvas = renderer.domElement;
  canvas.style.display = 'block';
  canvas.style.width = '100%';
  canvas.style.height = '100%';
  host.appendChild(canvas);

  const world = new Scene();
  const geometries = new Map<string, BufferGeometry>();
  const drawn = new Map<string, Drawn>();
  let camera: PerspectiveCamera | OrthographicCamera = new PerspectiveCamera();
  let current: LiveSceneNode | null = null;
  let frame = 0;

  const light = new Vector3(...SCENE_LIGHT.direction).normalize();

  function geometryFor(node: LiveSceneNode, name: string): BufferGeometry | undefined {
    const shape = sceneGeometry(name, node.meshes);
    if (!shape) return undefined;
    // A custom mesh can be replaced under the same name: key by its content.
    const key = `${name}:${JSON.stringify(shape)}`;
    let geometry = geometries.get(key);
    if (!geometry) {
      geometry = flatGeometry(shape.vertices, shape.faces);
      geometries.set(key, geometry);
    }
    return geometry;
  }

  function place(item: Drawn, pose: ScenePose) {
    item.mesh.position.set(pose.x, pose.y, pose.z);
    item.mesh.rotation.set(pose.rx * DEG, pose.ry * DEG, pose.rz * DEG, 'ZYX');
    item.mesh.scale.setScalar(pose.scale);
  }

  function poseAt(item: Drawn, now: number): ScenePose {
    const tick = current?.tick_ms ?? 0;
    const t = tick > 0 ? (now - item.start) / tick : 1;
    const wrap = current?.is_wrapping ? current : undefined;
    return easePose(item.from, item.to, t, wrap);
  }

  function resize() {
    const width = host.clientWidth;
    const height = host.clientHeight;
    if (width > 0 && height > 0) renderer.setSize(width, height, false);
  }

  function render() {
    frame = 0;
    const now = performance.now();
    let isMoving = false;
    for (const item of drawn.values()) {
      place(item, poseAt(item, now));
      if (current && current.tick_ms > 0 && now - item.start < current.tick_ms) isMoving = true;
    }
    renderer.render(world, camera);
    if (isMoving) frame = requestAnimationFrame(render);
  }

  function schedule() {
    if (!frame) frame = requestAnimationFrame(render);
  }

  const observer = new ResizeObserver(() => {
    resize();
    schedule();
  });
  observer.observe(host);
  resize();

  function material(object: LiveSceneObject): ShaderMaterial {
    return new ShaderMaterial({
      vertexShader: VERTEX,
      fragmentShader: FRAGMENT,
      uniforms: {
        uColor: { value: rgb(object.color) },
        uEmissive: { value: object.is_emissive ? 1 : 0 },
        uLight: { value: light },
        uAmbient: { value: SCENE_LIGHT.ambient },
        uDiffuse: { value: SCENE_LIGHT.diffuse },
      },
    });
  }

  return {
    update(node) {
      const now = performance.now();
      current = node;
      renderer.setClearColor(new Color(node.background), 1);
      const view = sceneCamera(node.width, node.height, node.fov);
      const [halfWidth, halfHeight] = [node.width / 2, node.height / 2];
      camera =
        view.fov > 0
          ? new PerspectiveCamera(view.fov, node.width / node.height, view.near, view.far)
          : new OrthographicCamera(-halfWidth, halfWidth, halfHeight, -halfHeight, view.near, view.far);
      camera.position.set(...view.position);
      camera.lookAt(view.position[0], view.position[1], 0);

      const kept = new Set<string>();
      for (const object of node.objects) {
        const geometry = geometryFor(node, object.mesh);
        if (!geometry) continue;
        kept.add(object.id);
        const item = drawn.get(object.id);
        if (!item) {
          const mesh = new Mesh(geometry, material(object));
          world.add(mesh);
          drawn.set(object.id, { mesh, from: object, to: object, start: now });
          continue;
        }
        // Ease from where the object is NOW, so a patch that lands mid-tick
        // bends the path instead of jumping back to the last target.
        item.from = poseAt(item, now);
        item.to = object;
        item.start = now;
        item.mesh.geometry = geometry;
        item.mesh.material.uniforms.uColor.value = rgb(object.color);
        item.mesh.material.uniforms.uEmissive.value = object.is_emissive ? 1 : 0;
      }
      for (const [id, item] of drawn) {
        if (kept.has(id)) continue;
        world.remove(item.mesh);
        item.mesh.material.dispose();
        drawn.delete(id);
      }
      schedule();
    },
    dispose() {
      if (frame) cancelAnimationFrame(frame);
      observer.disconnect();
      for (const item of drawn.values()) item.mesh.material.dispose();
      for (const geometry of geometries.values()) geometry.dispose();
      renderer.dispose();
      canvas.remove();
    },
  };
}

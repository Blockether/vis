/**
 * The pure part of a live `scene`: the contract's meshes, light and defaults, the
 * wire parsers, the camera and the easing between two ticks. `LiveScene` draws it.
 */

import viewSchema from '../../../../packages/vis-contract/resources/vis-contract/schema/view.json';

export type Vec3 = [number, number, number];
export type Face = [number, number, number];

/** One object of a scene, every key filled (`view.json#/$defs/scene_object`). */
export interface LiveSceneObject {
  id: string;
  mesh: string;
  x: number;
  y: number;
  z: number;
  rx: number;
  ry: number;
  rz: number;
  scale: number;
  color: string;
  is_emissive: boolean;
}

/** A custom mesh: vertices and counterclockwise triangles. */
export interface LiveSceneMesh {
  id: string;
  vertices: Vec3[];
  faces: Face[];
}

/** The place, turn and size of one object: what eases between ticks. */
export type ScenePose = Pick<LiveSceneObject, 'x' | 'y' | 'z' | 'rx' | 'ry' | 'rz' | 'scale'>;

export interface SceneLight {
  ambient: number;
  diffuse: number;
  direction: Vec3;
}

interface SceneDefs {
  scene_object: { properties: Record<string, { default?: unknown }> };
  scene_builtin_mesh: { oneOf: { const: string; 'x-vis-vertices': Vec3[]; 'x-vis-faces': Face[] }[] };
  component: {
    oneOf: {
      properties: Record<string, { const?: unknown; default?: unknown }>;
      'x-vis-light'?: SceneLight;
    }[];
  };
}

const DEFS = viewSchema.$defs as unknown as SceneDefs;
const SCENE_BRANCH = DEFS.component.oneOf.find((branch) => branch.properties.type?.const === 'scene');
if (!SCENE_BRANCH?.['x-vis-light']) throw new Error('view.json has no scene component');

/** The camera-space light every surface shades a scene with. */
export const SCENE_LIGHT: SceneLight = SCENE_BRANCH['x-vis-light'];

/** The builtin meshes, by name, exactly as the schema ships them. */
export const SCENE_MESHES: Record<string, Omit<LiveSceneMesh, 'id'>> = Object.fromEntries(
  DEFS.scene_builtin_mesh.oneOf.map((mesh) => [
    mesh.const,
    { vertices: mesh['x-vis-vertices'], faces: mesh['x-vis-faces'] },
  ]),
);

const objectDefault = (key: string) => DEFS.scene_object.properties[key]?.default;
const sceneDefault = (key: string) => SCENE_BRANCH.properties[key]?.default;

export const SCENE_DEFAULTS = {
  background: sceneDefault('background') as string,
  fov: sceneDefault('fov') as number,
  is_wrapping: sceneDefault('is_wrapping') as boolean,
  tick_ms: sceneDefault('tick_ms') as number,
};

const COLOR = /^#[0-9a-f]{6}$/i;
const POSE_KEYS = ['x', 'y', 'z', 'rx', 'ry', 'rz', 'scale'] as const;

function finite(value: unknown): number | undefined {
  return typeof value === 'number' && Number.isFinite(value) ? value : undefined;
}

/** A scene colour, lowercased, or `fallback` for anything else. */
export function sceneColor(value: unknown, fallback: string): string {
  return typeof value === 'string' && COLOR.test(value) ? value.toLowerCase() : fallback;
}

/**
 * One object change merged onto `base` (`view/scene-merge`): a key the change
 * leaves out keeps its value. A new object starts from the defaults and needs
 * its mesh, so a change without one answers `null`.
 */
export function sceneObjectFromWire(
  raw: Record<string, unknown>,
  base?: LiveSceneObject,
): LiveSceneObject | null {
  const id = typeof raw.id === 'string' ? raw.id : '';
  const mesh = typeof raw.mesh === 'string' && raw.mesh !== '' ? raw.mesh : base?.mesh;
  if (id === '' || !mesh) return null;
  const pose = Object.fromEntries(
    POSE_KEYS.map((key) => [key, finite(raw[key]) ?? base?.[key] ?? (objectDefault(key) as number)]),
  ) as ScenePose;
  return {
    id,
    mesh,
    ...pose,
    scale: pose.scale > 0 ? pose.scale : 1,
    color: sceneColor(raw.color, base?.color ?? (objectDefault('color') as string)),
    is_emissive:
      typeof raw.is_emissive === 'boolean'
        ? raw.is_emissive
        : (base?.is_emissive ?? (objectDefault('is_emissive') as boolean)),
  };
}

/** `held` with each change merged in by id; a new id joins the end. */
export function mergeSceneObjects(held: LiveSceneObject[], changes: unknown): LiveSceneObject[] {
  if (!Array.isArray(changes) || changes.length === 0) return held;
  const merged = held.slice();
  const at = new Map(held.map((one, index) => [one.id, index]));
  for (const change of changes) {
    if (!change || typeof change !== 'object') continue;
    const raw = change as Record<string, unknown>;
    const index = at.get(String(raw.id));
    const next = sceneObjectFromWire(raw, index === undefined ? undefined : merged[index]);
    if (!next) continue;
    if (index === undefined) {
      at.set(next.id, merged.length);
      merged.push(next);
    } else {
      merged[index] = next;
    }
  }
  return merged;
}

function triples(value: unknown, isIndex: boolean): Vec3[] | null {
  if (!Array.isArray(value)) return null;
  const result: Vec3[] = [];
  for (const one of value) {
    if (!Array.isArray(one) || one.length !== 3) return null;
    if (!one.every((n) => finite(n) !== undefined && (!isIndex || Number.isInteger(n)))) return null;
    result.push([one[0], one[1], one[2]]);
  }
  return result;
}

/** A custom mesh, or `null` when a face names a missing vertex. */
export function sceneMeshFromWire(raw: Record<string, unknown>): LiveSceneMesh | null {
  const id = typeof raw.id === 'string' ? raw.id : '';
  const vertices = triples(raw.vertices, false);
  const faces = triples(raw.faces, true) as Face[] | null;
  if (id === '' || !vertices || !faces) return null;
  if (faces.some((face) => face.some((index) => index < 0 || index >= vertices.length))) return null;
  return { id, vertices, faces };
}

/** The vertices and faces of a mesh name: a custom mesh, else a builtin one. */
export function sceneGeometry(
  name: string,
  meshes: LiveSceneMesh[],
): Omit<LiveSceneMesh, 'id'> | undefined {
  return meshes.find((mesh) => mesh.id === name) ?? SCENE_MESHES[name];
}

export interface SceneCamera {
  /** Where the camera stands; it always looks down -z. */
  position: Vec3;
  /** The vertical field of view in degrees; 0 is orthographic. */
  fov: number;
  near: number;
  far: number;
}

/** The camera that fits the rectangle (0, 0)-(width, height) at z = 0. */
export function sceneCamera(width: number, height: number, fov: number): SceneCamera {
  if (fov <= 0) {
    const depth = Math.max(width, height) * 100;
    return { position: [width / 2, height / 2, depth], fov: 0, near: 0, far: depth * 2 };
  }
  const distance = height / 2 / Math.tan((fov * Math.PI) / 360);
  return {
    position: [width / 2, height / 2, distance],
    fov,
    near: distance / 100,
    far: distance * 100,
  };
}

function modulo(value: number, size: number): number {
  return ((value % size) + size) % size;
}

/** The shortest signed way from `from` to `to` on a loop of `size`. */
function shortest(from: number, to: number, size: number): number {
  return modulo(to - from + size / 2, size) - size / 2;
}

/**
 * The pose at `t` (0..1) between two ticks. A turn takes the short way round.
 * In a wrapping field an object that crosses an edge eases across that edge,
 * not back through the field, and its place stays inside the field.
 */
export function easePose(
  from: ScenePose,
  to: ScenePose,
  t: number,
  wrap?: { width: number; height: number },
): ScenePose {
  const k = Math.min(1, Math.max(0, t));
  const along = (a: number, b: number, size?: number) => {
    const step = size ? shortest(a, b, size) : b - a;
    const value = a + step * k;
    return size ? modulo(value, size) : value;
  };
  return {
    x: along(from.x, to.x, wrap?.width),
    y: along(from.y, to.y, wrap?.height),
    z: along(from.z, to.z),
    rx: from.rx + shortest(from.rx, to.rx, 360) * k,
    ry: from.ry + shortest(from.ry, to.ry, 360) * k,
    rz: from.rz + shortest(from.rz, to.rz, 360) * k,
    scale: from.scale + (to.scale - from.scale) * k,
  };
}

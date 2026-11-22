/** The board layout and camera of a live `canvas`. */

import viewSchema from '../../../../packages/vis-contract/resources/vis-contract/schema/view.json';
import type { CanvasFlowSetting, CanvasMark, CanvasShape, CanvasSize } from './canvas-vocabulary';
import type { LiveCanvasCard, LiveCanvasEdge } from './live-view';
import { clamp, type Point, type Size } from './zoom-pan';

interface CanvasDefs {
  component: { oneOf: { properties: Record<string, { const?: unknown; maximum?: number }> }[] };
}

const CANVAS_BRANCH = (viewSchema.$defs as unknown as CanvasDefs).component.oneOf.find(
  (branch) => branch.properties.type?.const === 'canvas',
);
if (CANVAS_BRANCH?.properties.zoom?.maximum === undefined) {
  throw new Error('view.json has no canvas component');
}

/** The largest scale of a board (`view.json` canvas `zoom`). 1 is a card at full size. */
export const CANVAS_MAX_ZOOM = CANVAS_BRANCH.properties.zoom.maximum;
interface CanvasThreadBounds {
  properties: {
    threads: {
      items: {
        allOf: { properties?: { messages: { items: { properties: { body: { maxLength: number } } } } } }[];
      };
    };
  };
}

const THREAD_BOUNDS = (CANVAS_BRANCH as unknown as CanvasThreadBounds).properties.threads.items.allOf.find(
  (part) => part.properties,
)?.properties;
if (!THREAD_BOUNDS) throw new Error('view.json has no bounds for a canvas thread');

/** The longest message or draft of a thread on a board (`view.json` canvas `threads`). */
export const CANVAS_MESSAGE_CHARS = THREAD_BOUNDS.messages.items.properties.body.maxLength;
/** The smallest scale: a board of the largest size still fits in a phone. */
export const CANVAS_MIN_ZOOM = 0.1;
/** Below this scale the text of a card is too small to read: a card shows its title only. */
export const CANVAS_FAR_ZOOM = 0.55;

/** The free band between the board and the viewport when the board fits. */
export const CANVAS_PAD = 24;
export const CANVAS_NARROW = 560;
export const CANVAS_MIN_HEIGHT = 200;
export const CANVAS_FRAME_PAD = 28;
export const CANVAS_FRAME_HEAD = 44;

const STEP_GAP = 88;
const ITEM_GAP = 32;
const DETOUR_CLEARANCE = 14;
const DETOUR_LANE_GAP = 10;
const DETOUR_LEAD = 18;
/** The room past the clearance that a detour first tries to keep from an item. */
const DETOUR_BERTH = 8;
/** A crossing of another arrow costs as much as a detour of this length. */
const CROSSING_COST = 48;
/** The most steps of one search for a way around the items. */
const SEARCH_STEPS = 6000;
/** The most steps of all the searches of one board. */
const BOARD_STEPS = 60000;
/** The steps of the plain searches for the arrows whose search ran out of steps. */
const RESCUE_STEPS = 8000;
const CAPTION_GAP = 6;
const PORT_STEP = 12;
/** Facing ends nearer than this come in line, so that a line has no small step. */
const ALIGN_REACH = 2 * PORT_STEP;
const MAX_DEPTH = viewSchema.$defs.canvas_card.properties.parent['x-vis-max-depth'];

export type CanvasFlow = 'right' | 'down';

export function canvasFlow(width: number, setting: CanvasFlowSetting = 'auto'): CanvasFlow {
  return setting === 'auto' ? (width > 0 && width < CANVAS_NARROW ? 'down' : 'right') : setting;
}

export interface CanvasMeasure {
  /** The height of the body, or of the header for a frame. */
  height: number;
  /** The full caption height, with the gap, if present. */
  caption?: number;
}

export interface CanvasBox extends Size, Point {
  id: string;
  caption: number;
  depth: number;
  parent?: string;
  head?: number;
}

/** The tip of a mark and its direction into the item. */
export interface CanvasMarkPlace extends Point { angle: number }

export interface CanvasArrow {
  /** The source and the target as JSON, see `arrowId`: the last edge of a pair wins. */
  id: string;
  from: string;
  to: string;
  /** The line between the two marks, in board pixels. */
  path: string;
  start: CanvasMarkPlace;
  end: CanvasMarkPlace;
  /** The label position at half of the line length. */
  middle: Point;
  edge: LiveCanvasEdge;
}

export interface CanvasLayout extends Size {
  /** A frame comes before the items that it holds. */
  boxes: CanvasBox[];
  arrows: CanvasArrow[];
}

/** The pan and the scale of a board. The origin is the top left corner of the viewport. */
export interface CanvasCamera extends Point { scale: number }

const widths = {
  card: [208, 260, 332], note: [144, 184, 224],
  text: [128, 176, 240], shape: [120, 160, 208], pill: [136, 184, 240],
  frame: [208, 260, 332],
};
const heights = {
  card: [56, 64, 72], note: [128, 160, 196],
  text: [28, 32, 40], shape: [72, 96, 120], pill: [56, 64, 76],
  frame: [96, 112, 136],
};
const sizeIndex: Record<CanvasSize, number> = { small: 0, medium: 1, large: 2 };
const sizeKind = (card: LiveCanvasCard) => card.kind === 'shape' && card.shape === 'pill' ? 'pill' : card.kind;

export function canvasItemWidth(card: LiveCanvasCard): number {
  return widths[sizeKind(card)][sizeIndex[card.size]];
}

export function canvasItemMinHeight(card: LiveCanvasCard): number {
  return heights[sizeKind(card)][sizeIndex[card.size]];
}

export function estimatedCardHeight(card: LiveCanvasCard): number {
  if (card.kind === 'frame') return CANVAS_FRAME_HEAD;
  const width = canvasItemWidth(card);
  const lines = (text: string | undefined, available: number, char: number) =>
    text ? Math.ceil(text.length / Math.max(1, Math.floor(available / char))) : 0;
  const titleWidth = width - 32 - (card.icon ? 22 : 0);
  const title = lines(card.title, titleWidth, 8) * 20;
  const text = card.text?.split('\n').reduce((sum, line) => sum + Math.max(1, lines(line, width - 32, 7)), 0) ?? 0;
  const picture = card.picture ? 8 + Math.round((width - 32) * 9 / 16) : 0;
  const media = card.media?.kind === 'video' ? 8 + Math.round((width - 32) * 9 / 16) : card.media ? 44 : 0;
  const detail = (text ? text * 18 + 4 : 0) + (card.meta ? 20 : 0) +
    (card.badges.length ? 24 : 0) + (card.progress === undefined ? 0 : 10);
  return Math.max(canvasItemMinHeight(card), 26 + title + detail + picture + media);
}

/* Use the same primitives for visible outlines and port positions. */
type Segment =
  | { kind: 'line'; a: Point; b: Point }
  | { kind: 'quad'; a: Point; control: Point; b: Point }
  | { kind: 'arc'; a: Point; b: Point; centre: Point; rx: number; ry: number; quadrant: number };
const xy = (x: number, y: number): Point => ({ x, y });
const numberText = (n: number) => String(Math.round(n * 1000) / 1000);
const pt = (p: Point) => `${numberText(p.x)},${numberText(p.y)}`;

function outline(width: number, height: number, shape: CanvasShape, rounded = 12) {
  const w = width;
  const h = height;
  const segments: Segment[] = [];
  let start = xy(0, 0);
  let last = start;
  function move(x: number, y: number) { start = last = xy(x, y); }
  function line(x: number, y: number) {
    const b = xy(x, y);
    segments.push({ kind: 'line', a: last, b });
    last = b;
  }
  function quad(cx: number, cy: number, x: number, y: number) {
    const b = xy(x, y);
    segments.push({ kind: 'quad', a: last, control: xy(cx, cy), b });
    last = b;
  }
  function arc(cx: number, cy: number, rx: number, ry: number, quadrant: number) {
    const b = [xy(cx, cy + ry), xy(cx - rx, cy), xy(cx, cy - ry), xy(cx + rx, cy)][quadrant];
    segments.push({ kind: 'arc', a: last, b, centre: xy(cx, cy), rx, ry, quadrant });
    last = b;
  }
  if (shape === 'ellipse') {
    move(w, h / 2);
    for (let q = 0; q < 4; q++) arc(w / 2, h / 2, w / 2, h / 2, q);
  } else if (shape === 'diamond') {
    move(w / 2, 0); line(w, h / 2); line(w / 2, h); line(0, h / 2);
  } else if (shape === 'hexagon') {
    move(w * 0.2, 0); line(w * 0.8, 0); line(w, h / 2);
    line(w * 0.8, h); line(w * 0.2, h); line(0, h / 2);
  } else if (shape === 'parallelogram') {
    move(w * 0.2, 0); line(w, 0); line(w * 0.8, h); line(0, h);
  } else if (shape === 'cylinder') {
    const rim = cylinderRim(h);
    move(0, rim); arc(w / 2, rim, w / 2, rim, 2); arc(w / 2, rim, w / 2, rim, 3);
    line(w, h - rim); arc(w / 2, h - rim, w / 2, rim, 0); arc(w / 2, h - rim, w / 2, rim, 1);
  } else if (shape === 'document') {
    line(w, 0); line(w, h * 0.88);
    quad(w * 0.75, h * 0.76, w / 2, h * 0.88); quad(w * 0.25, h, 0, h);
  } else if (shape === 'rectangle') {
    line(w, 0); line(w, h); line(0, h);
  } else {
    const r = Math.min(shape === 'pill' ? h / 2 : rounded, w / 2, h / 2);
    move(r, 0); line(w - r, 0); arc(w - r, r, r, r, 3);
    line(w, h - r); arc(w - r, h - r, r, r, 0);
    line(r, h); arc(r, h - r, r, r, 1);
    line(0, r); arc(r, r, r, r, 2);
  }
  if (last.x !== start.x || last.y !== start.y) line(start.x, start.y);
  return segments;
}

/** The point of the outline of a shape nearest the top right corner of its box. */
export function canvasShapeCorner(shape: CanvasShape, width: number, height: number): Point {
  let best = xy(width, 0);
  let nearest = Infinity;
  for (const seg of outline(width, height, shape)) {
    for (let i = 0; i <= 32; i++) {
      const t = i / 32;
      let p: Point;
      if (seg.kind === 'arc') {
        const angle = (seg.quadrant + t) * Math.PI / 2;
        p = xy(seg.centre.x + seg.rx * Math.cos(angle), seg.centre.y + seg.ry * Math.sin(angle));
      } else if (seg.kind === 'quad') {
        const s = 1 - t;
        p = xy(s * s * seg.a.x + 2 * s * t * seg.control.x + t * t * seg.b.x,
          s * s * seg.a.y + 2 * s * t * seg.control.y + t * t * seg.b.y);
      } else p = xy(seg.a.x + (seg.b.x - seg.a.x) * t, seg.a.y + (seg.b.y - seg.a.y) * t);
      const d = (p.x - width) ** 2 + p.y ** 2;
      if (d < nearest) { nearest = d; best = p; }
    }
  }
  return best;
}

export function canvasShapePath(shape: CanvasShape, width: number, height: number): string {
  const segments = outline(width, height, shape);
  const commands = segments.map((seg) => {
    if (seg.kind === 'arc') return `A${numberText(seg.rx)},${numberText(seg.ry)} 0 0 1 ${pt(seg.b)}`;
    if (seg.kind === 'quad') return `Q${pt(seg.control)} ${pt(seg.b)}`;
    return `L${pt(seg.b)}`;
  });
  return `M${pt(segments[0].a)} ${commands.join(' ')} Z`;
}

const cylinderRim = (height: number) => height * 0.16;

export function canvasShapeDetail(shape: CanvasShape, width: number, height: number): string {
  if (shape !== 'cylinder') return '';
  const rim = numberText(cylinderRim(height));
  return `M0,${rim} A${numberText(width / 2)},${rim} 0 0 0 ${numberText(width)},${rim}`;
}

// Each mark ends at zero. The line runs to the back of a filled mark.
const marks: Record<CanvasMark, { path: string; trim: number; vertices?: Point[]; radius?: number }> = {
  none: { path: '', trim: 0 },
  arrow: { path: 'M-8,-4 L0,0 L-8,4', trim: 0, vertices: [xy(-8, -4), xy(-8, 4)] },
  triangle: { path: 'M0,0 L-10,-5 L-10,5 Z', trim: 10, vertices: [xy(-10, -5), xy(-10, 5)] },
  circle: { path: 'M0,0 A4,4 0 1 1 -8,0 A4,4 0 1 1 0,0 Z', trim: 8, radius: 4 },
  diamond: { path: 'M0,0 L-6,-4 L-12,0 L-6,4 Z', trim: 12, vertices: [xy(-6, -4), xy(-12, 0), xy(-6, 4)] },
  bar: { path: 'M0,-5 L0,5', trim: 0, vertices: [xy(0, -5), xy(0, 5)] },
};

export function canvasMarkPath(mark: CanvasMark): string { return marks[mark].path; }

type Axis = 'x' | 'y';
type Side = 'left' | 'right' | 'top' | 'bottom';
const acrossAxis = (side: Side): Axis => side === 'left' || side === 'right' ? 'y' : 'x';
const outward: Record<Side, Point> = { left: xy(-1, 0), right: xy(1, 0), top: xy(0, -1), bottom: xy(0, 1) };
const opposite: Record<Side, Side> = { left: 'right', right: 'left', top: 'bottom', bottom: 'top' };
const shift = (p: Point, d: Point, n: number) => xy(p.x + d.x * n, p.y + d.y * n);
const centre = (b: CanvasBox) => xy(b.x + b.width / 2, b.y + b.height / 2);
const between = (a: Point, b: Point, t: number) => xy(a.x + (b.x - a.x) * t, a.y + (b.y - a.y) * t);

function portPoint(b: CanvasBox, segments: Segment[], side: Side, cross: number): Point {
  const axis = acrossAxis(side);
  const main = axis === 'x' ? 'y' : 'x';
  const isMax = side === 'right' || side === 'bottom';
  let result = isMax ? -Infinity : Infinity;
  function add(n: number) { result = isMax ? Math.max(result, n) : Math.min(result, n); }
  for (const s of segments) {
    const a = s.a[axis];
    const z = s.b[axis];
    if (cross < Math.min(a, z) - 1e-7 || cross > Math.max(a, z) + 1e-7) continue;
    if (s.kind === 'arc') {
      const q = s.quadrant;
      const crossRadius = axis === 'x' ? s.rx : s.ry;
      const mainRadius = axis === 'x' ? s.ry : s.rx;
      const sign = main === 'x' ? (q === 0 || q === 3 ? 1 : -1) : (q < 2 ? 1 : -1);
      const delta = crossRadius ? (cross - s.centre[axis]) / crossRadius : 0;
      add(s.centre[main] + sign * mainRadius * Math.sqrt(Math.max(0, 1 - delta * delta)));
    } else if (a === z) {
      add(s.a[main]);
      add(s.b[main]);
    } else if (s.kind === 'line') {
      add(between(s.a, s.b, (cross - a) / (z - a))[main]);
    } else {
      // Both axes of a document curve are monotone. Find the point at this offset.
      let low = 0;
      let high = 1;
      for (let i = 0; i < 24; i++) {
        const t = (low + high) / 2;
        const value = between(between(s.a, s.control, t), between(s.control, s.b, t), t)[axis];
        if ((value < cross) === (a < z)) low = t;
        else high = t;
      }
      const t = (low + high) / 2;
      add(between(between(s.a, s.control, t), between(s.control, s.b, t), t)[main]);
    }
  }
  return axis === 'y' ? xy(b.x + result, b.y + cross) : xy(b.x + cross, b.y + result);
}

interface Item {
  card: LiveCanvasCard;
  box: CanvasBox;
  parent?: Item;
  children: Item[];
  sources: Set<Item>;
  step: number;
  before: number;
  span: number;
  cross: number;
}

interface Link {
  from: Item;
  to: Item;
  edge: LiveCanvasEdge;
}

function setParents(items: Map<string, Item>): Item[] {
  for (const item of items.values()) {
    const parent = item.card.parent && items.get(item.card.parent);
    if (parent && parent.card.kind === 'frame') item.parent = parent;
  }
  // Remove every member of a parent cycle before assigning a depth.
  const scanned = new Set<Item>();
  for (const item of items.values()) {
    const chain = new Set<Item>();
    for (let current: Item | undefined = item; current && !scanned.has(current); current = current.parent) {
      if (chain.has(current)) {
        const first: Item = current;
        do {
          const next: Item = current.parent!;
          current.parent = undefined;
          current = next;
        } while (current !== first);
        break;
      }
      chain.add(current);
    }
    for (const member of chain) scanned.add(member);
  }
  function depth(item: Item): number {
    if (item.box.depth < 0) {
      const parent = item.parent;
      const next = parent ? depth(parent) + 1 : 0;
      item.box.depth = next > MAX_DEPTH ? 0 : next;
      if (next > MAX_DEPTH) item.parent = undefined;
    }
    return item.box.depth;
  }
  const roots: Item[] = [];
  for (const item of items.values()) {
    depth(item);
    if (item.parent) {
      item.box.parent = item.parent.box.id;
      item.parent.children.push(item);
    } else roots.push(item);
  }
  return roots;
}

/** Use the first distinct boxes under the common frame to order this pair. */
function orderLink({ from, to }: Link) {
  while (from.box.depth > to.box.depth) from = from.parent!;
  while (to.box.depth > from.box.depth) to = to.parent!;
  while (from.parent !== to.parent) {
    from = from.parent!;
    to = to.parent!;
  }
  if (from !== to) to.sources.add(from);
}

function stepsOf(items: Item[]): Item[][] {
  function rank(item: Item): number {
    if (item.step >= 0) return item.step;
    // A rank of -2 marks the open part of the search. Do not follow an edge back into it.
    item.step = -2;
    let n = item.card.column ?? 0;
    if (item.card.column === undefined) {
      for (const source of item.sources) if (source.step !== -2) n = Math.max(n, rank(source) + 1);
    }
    return (item.step = n);
  }
  const byRank = new Map<number, Item[]>();
  for (const item of items) {
    const n = rank(item);
    const step = byRank.get(n);
    if (step) step.push(item);
    else byRank.set(n, [item]);
  }
  return [...byRank].sort(([a], [b]) => a - b).map(([, group]) => group);
}

/** Pack a step as close as possible to the preferred centres without overlap. */
function pack(step: Item[], preferred: (item: Item, index: number) => number) {
  // After subtracting the space needed by the items, the remaining offsets must be in order.
  const runs: { end: number; sum: number; count: number }[] = [];
  let before = 0;
  for (const [index, item] of step.entries()) {
    item.before = before;
    let sum = preferred(item, index) - before - item.span / 2;
    let count = 1;
    for (let prev = runs.at(-1); prev && prev.sum / prev.count > sum / count; prev = runs.at(-1)) {
      sum += prev.sum;
      count += prev.count;
      runs.pop();
    }
    runs.push({ end: index, sum, count });
    before += item.span + ITEM_GAP;
  }
  let index = 0;
  for (const { end, sum, count } of runs) {
    const offset = Math.round(sum / count);
    for (; index <= end; index++) step[index].cross = step[index].before + offset;
  }
}

function arrange(items: Item[], down: boolean): Size {
  if (items.length === 0) return { width: 0, height: 0 };
  for (const item of items) {
    const b = item.box;
    if (b.head !== undefined && item.children.length) {
      const inside = arrange(item.children, down);
      b.width = Math.max(b.width, inside.width + 2 * CANVAS_FRAME_PAD);
      b.height = Math.max(b.height, b.head + inside.height + 2 * CANVAS_FRAME_PAD);
    }
    item.span = down ? b.width : b.height + b.caption;
  }
  const steps = stepsOf(items);
  const breadth = (step: Item[]) => step.reduce((total, item) => total + item.span, (step.length - 1) * ITEM_GAP);
  let across = Math.max(...steps.map(breadth));
  let along = 0;
  let min = 0;
  const placed = new Set<Item>();
  const ordinal = new Map(items.map((one, i) => [one, i]));
  for (const step of steps) {
    const mean = new Map<Item, number>();
    for (const item of step) {
      let sum = 0;
      let count = 0;
      for (const source of item.sources) {
        if (!placed.has(source)) continue;
        sum += source.cross + (down ? source.box.width : source.box.height) / 2;
        count++;
      }
      if (count) mean.set(item, sum / count);
    }
    const bySource = [...step].sort((a, b) => (mean.get(a) ?? across / 2) - (mean.get(b) ?? across / 2));
    const order = new Map(bySource.map((item, i) => [item, i]));
    step.sort((a, b) => (a.card.row ?? order.get(a)!) - (b.card.row ?? order.get(b)!) || ordinal.get(a)! - ordinal.get(b)!);
    let preferredStart = (across - breadth(step)) / 2;
    pack(step, (item) => {
      const centre = preferredStart + item.span / 2;
      preferredStart += item.span + ITEM_GAP;
      const point = mean.get(item);
      return point === undefined ? centre : point + (down ? 0 : item.box.caption / 2);
    });
    let length = 0;
    for (const item of step) {
      const b = item.box;
      b.x = down ? item.cross : along;
      b.y = down ? along : item.cross;
      min = Math.min(min, item.cross);
      across = Math.max(across, item.cross + item.span);
      length = Math.max(length, down ? b.height + b.caption : b.width);
      placed.add(item);
    }
    along += length + STEP_GAP;
  }
  if (min) for (const { box } of items) {
    if (down) box.x -= min;
    else box.y -= min;
  }
  return down ? { width: across - min, height: along - STEP_GAP } : { width: along - STEP_GAP, height: across - min };
}

function captionHeight(card: LiveCanvasCard, measure: CanvasMeasure | undefined, width: number): number {
  if (!card.caption) return 0;
  if (measure?.caption !== undefined) return Math.max(CAPTION_GAP, Math.ceil(measure.caption));
  return CAPTION_GAP + 16 * Math.ceil(card.caption.length / Math.max(1, Math.floor(width / 7)));
}

function hasAncestor(item: Item, ancestor: Item): boolean {
  for (let parent = item.parent; parent; parent = parent.parent) if (parent === ancestor) return true;
  return false;
}

interface Route {
  start: Point;
  parts: Part[];
}

type Part =
  | { to: Point; controls?: undefined }
  | { to: Point; controls: [Point] | [Point, Point] };

function pointOnPart(from: Point, { to, controls }: Part, t: number): Point {
  if (!controls) return between(from, to, t);
  const first = between(from, controls[0], t);
  const second = between(controls[0], controls[1] ?? to, t);
  if (!controls[1]) return between(first, second, t);
  return between(between(first, second, t), between(second, between(controls[1], to, t), t), t);
}

const distance = (a: Point, b: Point) => Math.hypot(a.x - b.x, a.y - b.y);

/** Sample the length of each curve, then give a point on the curve, not on a sample chord. */
function routePoint({ start, parts }: Route, fraction = 0.5): Point {
  const spans: { length: number; from: Point; part: Part; a: number; b: number }[] = [];
  let total = 0;
  for (const part of parts) {
    const count = part.controls ? 16 : 1;
    let prev = start;
    for (let i = 0; i < count; i++) {
      const point = pointOnPart(start, part, (i + 1) / count);
      const length = distance(prev, point);
      if (length > 0) spans.push({ length, from: start, part, a: i / count, b: (i + 1) / count });
      total += length;
      prev = point;
    }
    start = part.to;
  }
  let rest = total * fraction;
  for (const { length, from, part, a, b } of spans) {
    if (rest <= length) return pointOnPart(from, part, a + (b - a) * rest / length);
    rest -= length;
  }
  return start;
}

function straightRuns({ start, parts }: Route) {
  const runs: { from: Point; to: Point; middle: Point; length: number }[] = [];
  for (const { to, controls } of parts) {
    if (!controls) runs.push({ from: start, to, middle: between(start, to, 0.5), length: distance(start, to) });
    start = to;
  }
  return runs.sort((a, b) => b.length - a.length);
}

/** The outermost box that holds `item` and not `other`: the item, or a frame around it. */
function outerBox(item: Item, other: Item): CanvasBox {
  let outer = item;
  while (outer.parent && outer.parent !== other && !hasAncestor(other, outer.parent)) outer = outer.parent;
  return outer.box;
}

/**
 * Where an elbow between two items turns: the middle of the free room between the
 * frames around its ends, so the turn does not run along the border of a frame.
 */
function elbowTurn(from: Port, to: Port): number | undefined {
  const axis = from.direction.x ? 'x' : 'y';
  const size = axis === 'x' ? 'width' : 'height';
  const forward = from.direction[axis] > 0;
  const a = outerBox(from.item, to.item);
  const b = outerBox(to.item, from.item);
  const leave = forward ? a[axis] + a[size] : a[axis];
  const arrive = forward ? b[axis] : b[axis] + b[size];
  return (forward ? leave < arrive : leave > arrive) ? (leave + arrive) / 2 : undefined;
}

function elbows(start: Point, end: Point, direction: Point, enter: Point, free?: number): Route {
  const axis = direction.x ? 'x' : 'y';
  const cross = direction.x ? 'y' : 'x';
  const part = (along: number, across: number) => direction.x ? xy(along, across) : xy(across, along);
  const opposed = direction[axis] !== enter[axis];
  const turn = opposed ? free ?? (start[axis] + end[axis]) / 2 :
    direction[axis] * (Math.max(start[axis] * direction[axis], end[axis] * direction[axis]) + 18);
  return squareWay([start, part(turn, start[cross]), part(turn, end[cross]), end]);
}

function straightWay(points: Point[]): Point[] {
  const distinct = points.filter((point, i) => !i || distance(point, points[i - 1]) > 1e-7);
  return distinct.filter((point, i) => {
    const a = distinct[i - 1];
    const b = distinct[i + 1];
    return !a || !b || !((a.x === point.x && point.x === b.x) || (a.y === point.y && point.y === b.y));
  });
}

/** A way of straight runs with square turns, as each line of the app has. */
function squareWay(points: Point[]): Route {
  const [start, ...rest] = straightWay(points);
  return { start, parts: rest.map((to) => ({ to })) };
}

interface Rect { left: number; right: number; top: number; bottom: number }

function bounds(box: CanvasBox, margin = 0): Rect {
  return { left: box.x - margin, right: box.x + box.width + margin,
    top: box.y - margin, bottom: box.y + box.height + margin };
}

function inside(p: Point, r: Rect): boolean {
  return p.x > r.left && p.x < r.right && p.y > r.top && p.y < r.bottom;
}

function hitsLine(a: Point, b: Point, r: Rect): boolean {
  // A lane is axis-aligned. Only direct routes need the general line clip.
  if (a.y === b.y) return a.y > r.top && a.y < r.bottom && Math.min(a.x, b.x) < r.right && Math.max(a.x, b.x) > r.left;
  if (a.x === b.x) return a.x > r.left && a.x < r.right && Math.min(a.y, b.y) < r.bottom && Math.max(a.y, b.y) > r.top;
  // Clip the line to the open interior of the box.
  let low = 0;
  let high = 1;
  for (const [start, end, min, max] of [[a.x, b.x, r.left, r.right], [a.y, b.y, r.top, r.bottom]]) {
    if (start === end) {
      if (start <= min || start >= max) return false;
    } else {
      const atMin = (min - start) / (end - start);
      const atMax = (max - start) / (end - start);
      low = Math.max(low, Math.min(atMin, atMax));
      high = Math.min(high, Math.max(atMin, atMax));
    }
  }
  return low < high;
}

function hitsCurve(points: Point[], r: Rect, depth = 0): boolean {
  if (points.length === 2) return hitsLine(points[0], points[1], r);
  if (points.every((p) => p.x <= r.left) || points.every((p) => p.x >= r.right) ||
      points.every((p) => p.y <= r.top) || points.every((p) => p.y >= r.bottom)) return false;
  if (inside(points[0], r) || inside(points.at(-1)!, r) || depth >= 12) return true;
  const left = [points[0]];
  const right = [points.at(-1)!];
  while (points.length > 1) {
    points = points.slice(1).map((p, i) => between(points[i], p, 0.5));
    left.push(points[0]);
    right.push(points.at(-1)!);
  }
  return hitsCurve(left, r, depth + 1) || hitsCurve(right.reverse(), r, depth + 1);
}

function hitsRoute({ start, parts }: Route, obstacles: Rect[]): boolean {
  for (const { to, controls } of parts) {
    const points = [start, ...(controls ?? []), to];
    // The points of a part bound it. Most items of a board are far from one arrow.
    let left = Infinity;
    let right = -Infinity;
    let top = Infinity;
    let bottom = -Infinity;
    for (const { x, y } of points) {
      left = Math.min(left, x);
      right = Math.max(right, x);
      top = Math.min(top, y);
      bottom = Math.max(bottom, y);
    }
    for (const rect of obstacles) {
      if (right <= rect.left || left >= rect.right || bottom <= rect.top || top >= rect.bottom) continue;
      if (hitsCurve(points, rect)) return true;
    }
    start = to;
  }
  return false;
}

/** The first index of sorted `values` whose value is above `n`, or at `n` when `isAt`. */
function indexAbove(values: number[], n: number, isAt: boolean): number {
  let low = 0;
  let high = values.length;
  while (low < high) {
    const middle = (low + high) >> 1;
    if (values[middle] < n || (!isAt && values[middle] === n)) low = middle + 1;
    else high = middle;
  }
  return low;
}

/**
 * The gaps of one rail that an obstacle cuts. Gap `i` is the run between `stops[i]`
 * and `stops[i + 1]`. `spans` holds the obstacles that the rail goes through.
 */
function cutGaps(stops: number[], spans: [low: number, high: number][]): Uint8Array {
  const gaps = new Uint8Array(stops.length);
  for (const [low, high] of spans) {
    const first = Math.max(0, indexAbove(stops, low, false) - 1);
    const last = Math.min(stops.length - 2, indexAbove(stops, high, true) - 1);
    for (let i = first; i <= last; i++) gaps[i] = 1;
  }
  return gaps;
}

/**
 * The lanes in use that each gap of one rail crosses. A lane on a stop counts in the
 * gap after it, so a run that goes on through the stop counts it once.
 */
function crossGaps(stops: number[], ats: number[]): Uint16Array {
  const gaps = new Uint16Array(stops.length);
  for (const at of ats) {
    const gap = indexAbove(stops, at, false) - 1;
    if (gap >= 0 && gap < stops.length - 1) gaps[gap]++;
  }
  return gaps;
}

interface Lane { axis: Axis; at: number; low: number; high: number }

/** The lanes that the arrows of a board already use. */
interface Lanes {
  count: number;
  add: (lane: Lane) => void;
  /** The lanes along `axis` that are too near `at` to share, in the order of use. */
  near: (axis: Axis, at: number) => Lane[];
  /** Each lane in use, in the order of use. */
  all: Lane[];
  /** Give back the lanes of one arrow, so that it can search again. */
  remove: (gone: Lane[]) => void;
}

/** An index by place, so a step of a search reads the lanes of one rail only. */
function laneIndex(): Lanes {
  const buckets = new Map<number, { lane: Lane; order: number }[]>();
  const key = (axis: Axis, at: number) => 2 * Math.floor(at / DETOUR_LANE_GAP) + (axis === 'x' ? 0 : 1);
  const lanes: Lanes = {
    count: 0,
    all: [],
    add(lane) {
      lanes.all.push(lane);
      const id = key(lane.axis, lane.at);
      const bucket = buckets.get(id);
      const entry = { lane, order: lanes.count++ };
      if (bucket) bucket.push(entry);
      else buckets.set(id, [entry]);
    },
    remove(gone) {
      const set = new Set(gone);
      lanes.all = lanes.all.filter((one) => !set.has(one));
      for (const one of gone) {
        const id = key(one.axis, one.at);
        const bucket = buckets.get(id)?.filter(({ lane }) => lane !== one);
        if (bucket?.length) buckets.set(id, bucket);
        else buckets.delete(id);
      }
    },
    near(axis, at) {
      const id = key(axis, at);
      return [id - 2, id, id + 2]
        .flatMap((bucket) => buckets.get(bucket) ?? [])
        .filter(({ lane }) => Math.abs(lane.at - at) < DETOUR_LANE_GAP - 1e-7)
        .sort((a, b) => a.order - b.order)
        .map(({ lane }) => lane);
    },
  };
  return lanes;
}

/** The steps that the searches of one board can take. An arrow after them goes direct. */
interface Budget {
  steps: number;
  /** The last search stopped at its limit: a longer search for the same arrow does not help. */
  isCut: boolean;
}

function lane(a: Point, b: Point): Lane {
  const axis = a.y === b.y ? 'x' : 'y';
  return { axis, at: axis === 'x' ? a.y : a.x, low: Math.min(a[axis], b[axis]), high: Math.max(a[axis], b[axis]) };
}

/** The four steps of a search: right, left, down, up. */
const STEP_X = [1, -1, 0, 0];
const STEP_Y = [0, 0, 1, -1];

interface SearchNode { cell: number; axis: number; cost: number; score: number; prev?: SearchNode }

/** Search the grid of obstacle sides and port offsets. */
function findLanes(start: Point, end: Point, obstacles: Rect[], used: Lanes, room: number,
  separate: boolean, limit: Rect | undefined, leave: Point, enter: Point, budget: Budget, isBeside = true, crossingCost = CROSSING_COST): Point[] | undefined {
  budget.isCut = budget.steps <= 0;
  if (budget.isCut) return undefined;
  const local = {
    left: Math.max(Math.min(start.x, end.x) - room, limit?.left ?? -Infinity),
    right: Math.min(Math.max(start.x, end.x) + room, limit?.right ?? Infinity),
    top: Math.max(Math.min(start.y, end.y) - room, limit?.top ?? -Infinity),
    bottom: Math.min(Math.max(start.y, end.y) + room, limit?.bottom ?? Infinity),
  };
  if ([start, end].some((p) => p.x < local.left || p.x > local.right || p.y < local.top || p.y > local.bottom)) return undefined;
  const relevant = obstacles.filter((r) => r.left < local.right && r.right > local.left && r.top < local.bottom && r.bottom > local.top);
  // The rails of a search cost as its steps do.
  budget.steps -= relevant.length;
  // The lanes that a step can cross: a run along `x` crosses the lanes along `y`.
  const across = (axis: Axis) => used.all.filter((r) => r.axis !== axis && (axis === 'x'
    ? r.at > local.left && r.at < local.right && r.high > local.top && r.low < local.bottom
    : r.at > local.top && r.at < local.bottom && r.high > local.left && r.low < local.right));
  const crossable = { x: across('x'), y: across('y') };
  const rail = (axis: Axis): number[] => {
    const [lo, hi] = axis === 'x' ? ['left', 'right'] as const : ['top', 'bottom'] as const;
    // A lane beside each end lets two arrows from one side turn one after the other.
    const values = [start[axis], end[axis], start[axis] - DETOUR_LANE_GAP, start[axis] + DETOUR_LANE_GAP,
      end[axis] - DETOUR_LANE_GAP, end[axis] + DETOUR_LANE_GAP];
    if (limit) values.push(limit[lo], limit[hi]);
    for (const r of relevant) values.push(r[lo] - DETOUR_LANE_GAP, r[lo], r[hi], r[hi] + DETOUR_LANE_GAP);
    // A lane beside each arrow in the box lets a way pass on either side of it.
    if (isBeside) for (const r of crossable[axis]) values.push(r.at - DETOUR_LANE_GAP, r.at + DETOUR_LANE_GAP);
    return [...new Set(values.filter((n) => n >= local[lo] && n <= local[hi]))].sort((a, b) => a - b);
  };
  const xs = rail('x');
  const ys = rail('y');
  const cellOf = (p: Point) => ys.indexOf(p.y) * xs.length + xs.indexOf(p.x);
  const pointOf = (cell: number) => xy(xs[cell % xs.length], ys[Math.floor(cell / xs.length)]);
  const last = cellOf(end);
  const pending: SearchNode[] = [];
  function push(node: SearchNode) {
    let i = pending.length;
    pending.push(node);
    while (i && pending[(i - 1) >> 1].score > node.score) {
      pending[i] = pending[(i - 1) >> 1];
      i = (i - 1) >> 1;
    }
    pending[i] = node;
  }
  function pop(): SearchNode {
    const node = pending[0];
    const tail = pending.pop()!;
    if (pending.length) {
      let i = 0;
      while (2 * i + 1 < pending.length) {
        let next = 2 * i + 1;
        if (next + 1 < pending.length && pending[next + 1].score < pending[next].score) next++;
        if (pending[next].score >= tail.score) break;
        pending[i] = pending[next];
        i = next;
      }
      pending[i] = tail;
    }
    return node;
  }
  const costs = new Map<number, number>();
  const heuristic = (p: Point) => Math.abs(p.x - end.x) + Math.abs(p.y - end.y);
  // Each rail keeps the obstacles that can cut a run along it and the lanes in use
  // near it. A step then reads a few entries, not the whole board.
  const rowCuts: Uint8Array[] = [];
  const columnCuts: Uint8Array[] = [];
  const rowLanes: Lane[][] = [];
  const columnLanes: Lane[][] = [];
  const rowCrossings: Uint16Array[] = [];
  const columnCrossings: Uint16Array[] = [];
  let steps = Math.min(budget.steps, SEARCH_STEPS);
  push({ cell: cellOf(start), axis: 2, cost: 0, score: heuristic(start) });
  while (pending.length && steps > 0) {
    steps--;
    budget.steps--;
    const node = pop();
    if (node.cost > (costs.get(node.cell * 3 + node.axis) ?? Infinity)) continue;
    if (node.cell === last) {
      const points: Point[] = [];
      for (let current: SearchNode | undefined = node; current; current = current.prev) {
        points.push(pointOf(current.cell));
      }
      return straightWay(points.reverse());
    }
    const here = pointOf(node.cell);
    const i = node.cell % xs.length;
    const j = Math.floor(node.cell / xs.length);
    for (let way = 0; way < 4; way++) {
      const di = STEP_X[way];
      const dj = STEP_Y[way];
      const axis = way >> 1;
      if (xs[i + di] === undefined || ys[j + dj] === undefined) continue;
      const cell = (j + dj) * xs.length + i + di;
      if ((node.prev === undefined && (di * leave.x + dj * leave.y) < 0) ||
          (cell === last && (di * enter.x + dj * enter.y) > 0)) continue;
      const there = xy(xs[i + di], ys[j + dj]);
      const cuts = axis === 0
        ? (rowCuts[j] ??= cutGaps(xs, relevant.filter((r) => here.y > r.top && here.y < r.bottom).map((r) => [r.left, r.right])))
        : (columnCuts[i] ??= cutGaps(ys, relevant.filter((r) => here.x > r.left && here.x < r.right).map((r) => [r.top, r.bottom])));
      if (cuts[axis === 0 ? Math.min(i, i + di) : Math.min(j, j + dj)]) continue;
      const track = lane(here, there);
      const lanes = axis === 0
        ? (rowLanes[j] ??= used.near('x', here.y).filter((r) => r.high > local.left && r.low < local.right))
        : (columnLanes[i] ??= used.near('y', here.x).filter((r) => r.high > local.top && r.low < local.bottom));
      const overlap = lanes.reduce((n, r) => n + Math.max(0, Math.min(r.high, track.high) - Math.max(r.low, track.low)), 0);
      if (separate && overlap > 1e-7) continue;
      const gap = axis === 0 ? Math.min(i, i + di) : Math.min(j, j + dj);
      const crossings = axis === 0
        ? (rowCrossings[j] ??= crossGaps(xs, crossable.x.filter((r) => here.y > r.low && here.y < r.high).map((r) => r.at)))[gap]
        : (columnCrossings[i] ??= crossGaps(ys, crossable.y.filter((r) => here.x > r.low && here.x < r.high).map((r) => r.at)))[gap];
      const cost = node.cost + track.high - track.low + (node.axis !== 2 && axis !== node.axis ? 16 : 0) + 20 * overlap +
        crossingCost * crossings;
      const key = cell * 3 + axis;
      if (cost >= (costs.get(key) ?? Infinity)) continue;
      costs.set(key, cost);
      push({ cell, axis, cost, score: cost + heuristic(there), prev: node });
    }
  }
  // The lanes beside the other arrows make the grid larger. When the search runs
  // out of steps on that grid, it searches once more on the smaller grid.
  if (pending.length && isBeside && budget.steps > 0 && crossable.x.length + crossable.y.length) {
    return findLanes(start, end, obstacles, used, room, separate, limit, leave, enter, budget, false, crossingCost);
  }
  budget.isCut = pending.length > 0;
  return undefined;
}

interface Port {
  item: Item;
  side: Side;
  /** From the body, or into a frame that holds the other end. */
  direction: Point;
  other: Point;
  point: Point;
}

function selectSides(from: Item, to: Item, down: boolean): [Side, Side] {
  const a = from.box;
  const b = to.box;
  const p = centre(a);
  const q = centre(b);
  const east = q.x >= p.x ? 'right' : 'left';
  const south = q.y >= p.y ? 'bottom' : 'top';
  if (hasAncestor(to, from) || hasAncestor(from, to)) {
    const holder = hasAncestor(to, from) ? a : b;
    const inside = holder === a ? b : a;
    const gaps: [Side, number][] = [
      ['left', inside.x - holder.x], ['right', holder.x + holder.width - inside.x - inside.width],
      ['top', inside.y - holder.y], ['bottom', holder.y + holder.height - inside.y - inside.height],
    ];
    gaps.sort(([, x], [, y]) => x - y);
    return [gaps[0][0], gaps[0][0]];
  }
  const apartX = a.x + a.width <= b.x || b.x + b.width <= a.x;
  const apartY = a.y + a.height <= b.y || b.y + b.height <= a.y;
  const side = apartX && (!apartY || !down) ? east : south;
  return [side, opposite[side]];
}

function makeRoute(edge: LiveCanvasEdge, from: Port, to: Port): { route: Route; start: CanvasMarkPlace; end: CanvasMarkPlace } {
  const unit = (p: Point, q: Point, fallback: Point) => {
    const length = distance(p, q);
    return length ? xy((q.x - p.x) / length, (q.y - p.y) / length) : fallback;
  };
  const fromDirection = edge.route === 'straight' ? unit(from.point, to.point, from.direction) : from.direction;
  const toDirection = edge.route === 'straight' ? unit(to.point, from.point, to.direction) : to.direction;
  const angle = (dir: Point) => Math.atan2(-dir.y, -dir.x) * 180 / Math.PI;
  const start = { ...from.point, angle: angle(fromDirection) };
  const end = { ...to.point, angle: angle(toDirection) };
  const a = shift(from.point, fromDirection, marks[edge.start_mark].trim);
  const b = shift(to.point, toDirection, marks[edge.end_mark].trim);
  if (edge.route === 'elbow') return { start, end, route: elbows(a, b, fromDirection, toDirection, elbowTurn(from, to)) };
  let controls: [Point, Point] | undefined;
  if (edge.route === 'curve') {
    const run = Math.abs((a.x - b.x) * fromDirection.x + (a.y - b.y) * fromDirection.y);
    const reach = Math.max(0, Math.min(112, run / 2));
    controls = [shift(a, fromDirection, reach), shift(b, toDirection, reach)];
  }
  return { start, end, route: { start: a, parts: [{ to: b, controls }] } };
}

function detour(edge: LiveCanvasEdge, from: Port, to: Port, obstacles: Rect[], used: Lanes, budget: Budget, isPlain = false) {
  const angle = (dir: Point) => Math.atan2(-dir.y, -dir.x) * 180 / Math.PI;
  const start = { ...from.point, angle: angle(from.direction) };
  const end = { ...to.point, angle: angle(to.direction) };
  const a = shift(start, from.direction, marks[edge.start_mark].trim);
  const b = shift(end, to.direction, marks[edge.end_mark].trim);
  const first = shift(a, from.direction, DETOUR_LEAD + DETOUR_BERTH);
  const last = shift(b, to.direction, DETOUR_LEAD + DETOUR_BERTH);
  let holder = from.item.parent;
  while (holder && !hasAncestor(to.item, holder)) holder = holder.parent;
  const limit = holder ? bounds(holder.box, -DETOUR_CLEARANCE) : undefined;
  for (const berth of [DETOUR_BERTH, 4, 0]) {
    const margin = DETOUR_CLEARANCE + berth;
    const rails = obstacles.map((r) => ({ left: r.left - margin, right: r.right + margin,
      top: r.top - margin, bottom: r.bottom + margin }));
    if (!hasAncestor(to.item, from.item)) rails.push(bounds(from.item.box));
    if (!hasAncestor(from.item, to.item)) rails.push(bounds(to.item.box));
    const searches = used.count && !isPlain ? [[96, true], [Infinity, true], [96, false], [Infinity, false]] as const :
      [[96, false], [Infinity, false]] as const;
    for (const [room, separate] of searches) {
      const points = findLanes(first, last, rails, used, room, separate, limit, from.direction, to.direction, budget,
        !isPlain, isPlain ? 0 : CROSSING_COST);
      if (!points) {
        if (budget.isCut) return undefined;
        continue;
      }
      const way = straightWay([a, ...points, b]);
      const route = squareWay(way);
      const clear = obstacles.map((r) => ({ left: r.left - DETOUR_CLEARANCE, right: r.right + DETOUR_CLEARANCE,
        top: r.top - DETOUR_CLEARANCE, bottom: r.bottom + DETOUR_CLEARANCE }));
      if (hitsRoute(route, clear)) continue;
      const lanes: Lane[] = [];
      for (let i = 1; i < way.length; i++) if (distance(way[i - 1], way[i]) > 0) lanes.push(lane(way[i - 1], way[i]));
      return { route, start, end, lanes };
    }
  }
  return undefined;
}

/** The lanes of the straight runs of a route that run along an axis. */
function lanesOf({ start, parts }: Route): Lane[] {
  const lanes: Lane[] = [];
  for (const { to, controls } of parts) {
    if (!controls && (start.x === to.x) !== (start.y === to.y)) lanes.push(lane(start, to));
    start = to;
  }
  return lanes;
}

/** How many times the lanes of one arrow cross the lanes in use. */
function crossingsOf(mine: Lane[], used: Lane[]): number {
  let n = 0;
  for (const a of mine) for (const b of used) {
    if (a.axis !== b.axis && b.at > a.low && b.at < a.high && a.at > b.low && a.at < b.high) n++;
  }
  return n;
}

function outlineOf(item: Item): Segment[] {
  const { card, box: b } = item;
  return outline(b.width, b.height, card.kind === 'shape' ? card.shape : 'rectangle');
}

function makeArrows(links: Map<string, Link>, items: Iterable<Item>, down: boolean) {
  const groups = new Map<Item, Map<Side, Port[]>>();
  function add(item: Item, side: Side, other: Item): Port {
    let sides = groups.get(item);
    if (!sides) groups.set(item, sides = new Map());
    let sameSide = sides.get(side);
    if (!sameSide) sides.set(side, sameSide = []);
    const direction = outward[hasAncestor(other, item) ? opposite[side] : side];
    const port = { item, side, direction, other: centre(other.box), point: xy(0, 0) };
    sameSide.push(port);
    return port;
  }
  const arrows: {
    id: string; from: string; to: string; edge: LiveCanvasEdge;
    start: CanvasMarkPlace; end: CanvasMarkPlace; route: Route; middle: Point;
  }[] = [];
  const ports: { id: string; from: Port; to: Port; edge: LiveCanvasEdge }[] = [];
  for (const [id, { from, to, edge }] of links) {
    if (edge.line === 'none') continue;
    const [out, into] = selectSides(from, to, down);
    ports.push({ id, from: add(from, out, to), to: add(to, into, from), edge });
  }
  for (const [item, sides] of groups) {
    const b = item.box;
    const segments = outlineOf(item);
    for (const [side, group] of sides) {
      const axis = acrossAxis(side);
      const length = axis === 'x' ? b.width : b.height;
      group.sort((a, c) => a.other[axis] - c.other[axis]);
      const step = Math.min(PORT_STEP, (length - Math.min(12, length / 4)) / Math.max(1, group.length - 1));
      for (const [i, port] of group.entries()) {
        const cross = length / 2 + (i - (group.length - 1) / 2) * step;
        port.point = portPoint(b, segments, side, cross);
      }
    }
  }
  // A small step near an end reads as a fault. When two ends face each other, one moves
  // in line with the other if its side has room and keeps the step to its other ends.
  const align = (port: Port, cross: number) => {
    const b = port.item.box;
    const axis = acrossAxis(port.side);
    const length = axis === 'x' ? b.width : b.height;
    const at = cross - b[axis];
    const others = groups.get(port.item)!.get(port.side)!.filter((one) => one !== port);
    if (at < 6 || at > length - 6 || others.some((one) => Math.abs(one.point[axis] - cross) < PORT_STEP)) return false;
    port.point = portPoint(b, outlineOf(port.item), port.side, at);
    return true;
  };
  for (const { from, to } of ports) {
    if (from.side !== opposite[to.side] || from.direction !== outward[from.side] || to.direction !== outward[to.side]) continue;
    const axis = acrossAxis(from.side);
    const step = Math.abs(from.point[axis] - to.point[axis]);
    if (step > 1e-3 && step <= ALIGN_REACH) align(to, from.point[axis]) || align(from, to.point[axis]);
  }
  const bodies = Array.from(items, (item) => ({ item, rect: bounds(item.box) }));
  const labelObstacles = bodies.filter(({ item }) => item.card.kind !== 'frame').map(({ rect }) => rect);
  const labels: Point[] = [];
  const used = laneIndex();
  const budget = { steps: BOARD_STEPS, isCut: false };
  const routed = ports.map(({ id, from, to, edge }) => {
    const obstacles = bodies.filter(({ item }) => item !== from.item && item !== to.item &&
      !hasAncestor(from.item, item) && !hasAncestor(to.item, item)).map(({ rect }) => rect);
    const direct = makeRoute(edge, from, to);
    const lanes = lanesOf(direct.route);
    const isClear = !hitsRoute(direct.route, obstacles);
    // An elbow searches when it crosses or shares a lane of an arrow before it.
    const isElbow = edge.route === 'elbow';
    const isSearched = !isClear || (isElbow && (crossingsOf(lanes, used.all) > 0 || lanes.some((one) =>
      used.near(one.axis, one.at).some((r) => Math.min(r.high, one.high) - Math.max(r.low, one.low) > 1e-7))));
    let around = budget.steps > 0 && isSearched ? detour(edge, from, to, obstacles, used, budget) : undefined;
    // A clear elbow keeps its direct way, and an arrow after it can still move it.
    if (!around && isElbow && isClear) around = { ...direct, lanes };
    // A direct way holds its lanes too, so that the arrows after it see it.
    (around?.lanes ?? lanes).forEach(used.add);
    return { id, from, to, edge, obstacles, direct, around, isClear };
  });
  // An arrow did not see the arrows after it. When two arrows cross, both search
  // again in each order, and the pair keeps the ways with the fewest crossings.
  type Routed = (typeof routed)[number];
  // A crossing of the two arrows counts once, as a crossing with a third arrow does.
  const crossingsOfWays = (a: Lane[], b: Lane[]) => crossingsOf(a, used.all) + crossingsOf(b, used.all) - crossingsOf(a, b);
  const tried = new Set<string>();
  // The second look costs at most as many steps as the first, or as one search.
  const again = { steps: Math.min(budget.steps, Math.max(SEARCH_STEPS, BOARD_STEPS - budget.steps)), isCut: false };
  for (const one of routed) {
    if (!one.around || again.steps <= 0 || !crossingsOf(one.around.lanes, used.all)) continue;
    const other = routed.find((two) => two !== one && two.around && crossingsOf(one.around!.lanes, two.around.lanes));
    if (!other || tried.has(other.id + one.id)) continue;
    tried.add(one.id + other.id);
    const pair = [one, other];
    let best = pair.map((two) => two.around!);
    let fewest = crossingsOfWays(one.around.lanes, other.around!.lanes);
    // One of the two searches again, then the other, then both in each order.
    for (const order of [[one], [other], [other, one], [one, other]]) {
      if (!fewest || again.steps <= 0) break;
      order.forEach((two) => used.remove(two.around!.lanes));
      const ways = new Map<Routed, NonNullable<Routed['around']>>();
      for (const two of order) {
        const way = detour(two.edge, two.from, two.to, two.obstacles, used, again);
        if (!way) break;
        ways.set(two, way);
        way.lanes.forEach(used.add);
      }
      if (ways.size === order.length) {
        const count = crossingsOfWays((ways.get(one) ?? one.around).lanes, (ways.get(other) ?? other.around!).lanes);
        if (count < fewest) {
          fewest = count;
          best = pair.map((two) => ways.get(two) ?? two.around!);
        }
      }
      ways.forEach((way) => used.remove(way.lanes));
      pair.filter((two) => !order.includes(two)).forEach((two) => used.remove(two.around!.lanes));
      pair.forEach((two, i) => {
        two.around = best[i];
        two.around.lanes.forEach(used.add);
      });
    }
  }
  // An arrow whose search ran out of steps does not go through an item. A plain
  // search with its own steps finds a way that can share lanes or cross arrows.
  const rescue = { steps: RESCUE_STEPS, isCut: false };
  for (const one of routed) {
    if (one.around || one.isClear || rescue.steps <= 0) continue;
    rescue.isCut = false;
    one.around = detour(one.edge, one.from, one.to, one.obstacles, used, rescue, true);
    one.around?.lanes.forEach(used.add);
  }
  for (const { id, from, to, edge, direct, around } of routed) {
    const path = around ?? direct;
    const runs = around ? straightRuns(around.route) : [];
    let middle = runs[0]?.middle ?? routePoint(path.route);
    if (edge.label) {
      const free = (p: Point) => !labelObstacles.some((r) => inside(p, r)) && !labels.some((q) => distance(p, q) < 1);
      if (around && !free(middle)) {
        for (const { from, to } of runs) {
          const point = [0.5, 0.4, 0.6, 0.3, 0.7].map((at) => between(from, to, at)).find(free);
          if (point) { middle = point; break; }
        }
      }
      for (let i = 1; i < 50 && !free(middle); i++) {
        middle = routePoint(path.route, 0.5 + (i % 2 ? 1 : -1) * Math.ceil(i / 2) / 52);
      }
      labels.push(middle);
    }
    arrows.push({ id, from: from.item.box.id, to: to.item.box.id, edge, ...path, middle });
  }
  return arrows;
}

/**
 * The inputs that move a box or an arrow. A progress, a tone, a colour or a motion
 * is not one of them, and a measured item gives its height, not its words.
 */
function layoutKey(cards: readonly LiveCanvasCard[], measures: ReadonlyMap<string, CanvasMeasure> | undefined,
  flow: CanvasFlow): string {
  const parts: unknown[] = [flow];
  for (const card of cards) {
    const measure = measures?.get(card.id);
    parts.push(card.id, card.kind, card.parent, card.size, card.shape, card.row, card.column,
      measure?.height ?? estimatedCardHeight(card), captionHeight(card, measure, canvasItemWidth(card)), card.edges.length);
    for (const edge of card.edges) parts.push(edge.from, edge.route, edge.label, edge.line, edge.start_mark, edge.end_mark);
  }
  return JSON.stringify(parts);
}

/**
 * The layout of one board over time. A change that moves no box and no arrow, such
 * as a progress or a tone, gives the last layout again with the new edges. Then the
 * board draws again only the items and the arrows that changed.
 */
/** One key for each pair of items. An id can hold any text, so a joined text such as `a>b` is not unique. */
const arrowId = (from: string, to: string) => JSON.stringify([from, to]);

export function keptCanvasLayout() {
  let held: { key: string; layout: CanvasLayout } | undefined;
  return (cards: readonly LiveCanvasCard[], measures?: ReadonlyMap<string, CanvasMeasure>,
    flow: CanvasFlow = 'right'): CanvasLayout => {
    const key = layoutKey(cards, measures, flow);
    if (held?.key !== key) {
      held = { key, layout: canvasLayout(cards, measures, flow) };
      return held.layout;
    }
    // The last edge of a pair wins, as in the layout.
    const edges = new Map<string, LiveCanvasEdge>();
    for (const card of cards) for (const edge of card.edges) edges.set(arrowId(edge.from, card.id), edge);
    const { layout } = held;
    if (layout.arrows.every((arrow) => edges.get(arrow.id) === arrow.edge)) return layout;
    held = { key, layout: { ...layout, arrows: layout.arrows.map((arrow) => {
      const edge = edges.get(arrow.id) ?? arrow.edge;
      return edge === arrow.edge ? arrow : { ...arrow, edge };
    }) } };
    return held.layout;
  };
}

export function canvasLayout(
  cards: readonly LiveCanvasCard[],
  measures?: ReadonlyMap<string, CanvasMeasure>,
  flow: CanvasFlow = 'right',
): CanvasLayout {
  const down = flow === 'down';
  const items = new Map<string, Item>();
  for (const card of cards) {
    const measure = measures?.get(card.id);
    const width = canvasItemWidth(card);
    const actual = Math.ceil(measure?.height ?? estimatedCardHeight(card));
    const head = card.kind === 'frame' ? Math.max(CANVAS_FRAME_HEAD, actual) : undefined;
    const height = Math.max(canvasItemMinHeight(card), head === undefined ? actual : head + CANVAS_FRAME_PAD);
    const box = { id: card.id, x: 0, y: 0, width, height, caption: captionHeight(card, measure, width), depth: -1, head };
    items.set(card.id, { card, box, children: [], sources: new Set(), step: -1, before: 0, span: 0, cross: 0 });
  }
  const roots = setParents(items);
  const links = new Map<string, Link>();
  for (const to of items.values()) for (const edge of to.card.edges) {
    const from = items.get(edge.from);
    if (from && from !== to) links.set(arrowId(from.box.id, to.box.id), { from, to, edge });
  }
  for (const link of links.values()) orderLink(link);
  const { width, height } = arrange(roots, down);
  const boxes: CanvasBox[] = [];
  function collect(items: Item[], x: number, y: number) {
    for (const { box, children } of items) {
      box.x += x;
      box.y += y;
      boxes.push(box);
      if (children.length) collect(children, box.x + CANVAS_FRAME_PAD, box.y + box.head! + CANVAS_FRAME_PAD);
    }
  }
  collect(roots, 0, 0);
  const arrows = makeArrows(links, items.values(), down);
  let minX = 0;
  let minY = 0;
  let maxX = width;
  let maxY = height;
  function extend({ x, y }: Point, margin = 0) {
    minX = Math.min(minX, x - margin);
    minY = Math.min(minY, y - margin);
    maxX = Math.max(maxX, x + margin);
    maxY = Math.max(maxY, y + margin);
  }
  function extendMark(place: CanvasMarkPlace, mark: CanvasMark) {
    if (mark === 'none') return;
    const angle = place.angle * Math.PI / 180;
    const cos = Math.cos(angle);
    const sin = Math.sin(angle);
    const rotate = ({ x, y }: Point) => xy(place.x + x * cos - y * sin, place.y + x * sin + y * cos);
    const { vertices, radius } = marks[mark];
    if (radius) extend(rotate(xy(-radius, 0)), radius + 2);
    else {
      extend(place, 2);
      for (const vertex of vertices ?? []) extend(rotate(vertex), 2);
    }
  }
  for (const { start, end, route, edge } of arrows) {
    // The control points bound the curves.
    extendMark(start, edge.start_mark);
    extendMark(end, edge.end_mark);
    extend(route.start, 5);
    for (const part of route.parts) {
      extend(part.to, 5);
      for (const control of part.controls ?? []) extend(control, 2);
    }
  }
  const dx = -Math.floor(minX);
  const dy = -Math.floor(minY);
  const offset = ({ x, y }: Point) => xy(x + dx, y + dy);
  for (const box of boxes) {
    box.x += dx;
    box.y += dy;
  }
  return {
    boxes,
    arrows: arrows.map(({ route, start, end, middle, ...link }) => ({
      ...link,
      start: { ...start, ...offset(start) },
      end: { ...end, ...offset(end) },
      middle: offset(middle),
      path: `M${pt(offset(route.start))} ${route.parts.map(({ to, controls }) => {
        if (!controls) return `L${pt(offset(to))}`;
        return `${controls.length === 1 ? 'Q' : 'C'}${controls.map((p) => pt(offset(p))).join(' ')} ${pt(offset(to))}`;
      }).join(' ')}`,
    })),
    width: Math.ceil(maxX) + dx,
    height: Math.ceil(maxY) + dy,
  };
}

/** The scale at which `content` fits in `frame` with the pad around it. Never above 1. */
function fitScale(content: Size, frame: Size): number {
  if (content.width <= 0 || content.height <= 0) return 1;
  const scale = Math.min(
    (frame.width - 2 * CANVAS_PAD) / content.width,
    (frame.height - 2 * CANVAS_PAD) / content.height,
  );
  return clamp(scale, CANVAS_MIN_ZOOM, 1);
}

/**
 * Where the owner points the camera. `focus` names the card in the centre: no id, or
 * an id that the board does not hold, shows the whole board. `zoom` is the scale, and
 * 0 fits the focus in the frame. A frame without a size shows the board as it is.
 */
export function canvasTarget(
  layout: CanvasLayout,
  frame: Size,
  zoom: number,
  focus: string,
): CanvasCamera {
  if (frame.width <= 0 || frame.height <= 0 || layout.boxes.length === 0) {
    return { x: 0, y: 0, scale: 1 };
  }
  const box = (focus && layout.boxes.find((one) => one.id === focus)) || {
    x: 0,
    y: 0,
    width: layout.width,
    height: layout.height,
  };
  const scale = zoom > 0 ? clamp(zoom, CANVAS_MIN_ZOOM, CANVAS_MAX_ZOOM) : fitScale(box, frame);
  return {
    x: Math.round((frame.width - box.width * scale) / 2 - box.x * scale),
    y: Math.round((frame.height - box.height * scale) / 2 - box.y * scale),
    scale,
  };
}

/** The camera scaled about `point` of the frame: what is under the point stays there. */
export function canvasZoomedAt(camera: CanvasCamera, factor: number, point: Point): CanvasCamera {
  const scale = clamp(camera.scale * factor, CANVAS_MIN_ZOOM, CANVAS_MAX_ZOOM);
  const applied = scale / camera.scale;
  return {
    x: point.x - (point.x - camera.x) * applied,
    y: point.y - (point.y - camera.y) * applied,
    scale,
  };
}

/**
 * The height of the frame for a board in a frame of this width: the board at the
 * scale that fits the width, between the lowest frame and `maxHeight`.
 */
export function canvasFrameHeight(layout: CanvasLayout, width: number, maxHeight: number): number {
  if (width <= 0 || layout.width <= 0) return CANVAS_MIN_HEIGHT;
  const scale = clamp((width - 2 * CANVAS_PAD) / layout.width, CANVAS_MIN_ZOOM, 1);
  return clamp(
    Math.ceil(layout.height * scale + 2 * CANVAS_PAD),
    CANVAS_MIN_HEIGHT,
    Math.max(CANVAS_MIN_HEIGHT, maxHeight),
  );
}

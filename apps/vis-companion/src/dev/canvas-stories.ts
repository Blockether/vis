/**
 * Boards for the canvas stories. The showcase is the canvas of the shared contract
 * fixture. The gallery draws each word of the canvas vocabulary one time, so a
 * change of the style shows in one look.
 */

import livePrimitivesWire from '../../../../packages/vis-contract/resources/vis-contract/fixtures/live-primitives.json';
import {
  CANVAS_ANIMATIONS,
  CANVAS_COLORS,
  CANVAS_EDGE_ANIMATIONS,
  CANVAS_FILLS,
  CANVAS_LINES,
  CANVAS_MARKS,
  CANVAS_PATTERNS,
  CANVAS_ROUTES,
  CANVAS_SHAPES,
  CANVAS_SIZES,
} from '../lib/canvas-vocabulary';
import { liveViewFromWire, type LiveView } from '../lib/live-view';

type Wire = Record<string, unknown>;

function view(id: string, title: string, nodes: Wire[]): LiveView {
  const made = liveViewFromWire({ id, title, seq: 0, channel_ids: ['app'], nodes });
  if (!made) throw new Error(`the story board ${id} must be paintable`);
  return made;
}

function board(id: string, label: string, cards: Wire[], more: Wire = {}): Wire {
  return { id, type: 'canvas', label, flow: 'right', is_selectable: true, cards, ...more };
}

const SHOWCASE = livePrimitivesWire.nodes.find((node) => node.id === 'canvas');
if (!SHOWCASE) throw new Error('the shared fixture holds a canvas');

/** The canvas of the contract fixture, alone, so that it has the whole page. */
export const STORY_CANVAS_SHOWCASE = view('canvas-showcase', 'Release board', [SHOWCASE]);

/** The same board at full size, with the camera on the nested frame. */
export const STORY_CANVAS_FOCUSED = view('canvas-focused', 'Release board', [
  { ...SHOWCASE, zoom: 1, focus: 'verify' },
]);

const VISIBLE_LINES = CANVAS_LINES.filter((line) => line !== 'none');

const shapes = CANVAS_SHAPES.map((shape, index) => ({
  id: shape,
  title: shape,
  kind: 'shape',
  shape,
  color: CANVAS_COLORS[index % CANVAS_COLORS.length],
  column: index % 3,
  row: Math.floor(index / 3),
}));

const colors = CANVAS_COLORS.flatMap((color, column) =>
  CANVAS_FILLS.map((fill, row) => ({
    id: `${color}-${fill}`,
    title: color,
    text: fill,
    kind: 'shape',
    size: 'small',
    icon: 'lightning',
    color,
    fill,
    column,
    row,
  })),
);

const borders = CANVAS_LINES.flatMap((border, column) => [
  { id: `card-${border}`, title: border, text: 'A card.', size: 'small', color: 'blue', border, column, row: 0 },
  { id: `plain-${border}`, title: border, size: 'small', border, column, row: 1 },
  { id: `hexagon-${border}`, title: border, kind: 'shape', shape: 'hexagon', size: 'small', color: 'purple', border, column, row: 2 },
  { id: `diamond-${border}`, title: border, kind: 'shape', shape: 'diamond', size: 'small', color: 'orange', fill: 'none', border, column, row: 3 },
]);

/** One arrow between two small shapes, in row `row`. The second shape has the title `to`. */
function pair(id: string, row: number, edge: Wire, column = 0, to = 'to'): Wire[] {
  return [
    { id: `${id}-a`, title: id, kind: 'shape', size: 'small', column, row },
    { id: `${id}-b`, title: to, kind: 'shape', size: 'small', column: column + 1, row, edges: [{ from: `${id}-a`, ...edge }] },
  ];
}

/** Each mark at the two ends of an arrow. The second shape names the line of the arrow. */
const marks = CANVAS_MARKS.flatMap((mark, index) => {
  const line = VISIBLE_LINES[index % VISIBLE_LINES.length];
  const half = Math.ceil(CANVAS_MARKS.length / 2);
  return pair(
    mark,
    index % half,
    { start_mark: mark, end_mark: mark, line, color: CANVAS_COLORS[index + 1] },
    index < half ? 0 : 2,
    line,
  );
});

/** Each route from one shape to a shape above it and to a shape below it. */
const routes = CANVAS_ROUTES.flatMap((route, index) => [
  { id: `${route}-from`, title: route, kind: 'shape', shape: 'ellipse', column: 0, row: index },
  ...['up', 'down'].map((side, at) => ({
    id: `${route}-${side}`,
    title: side,
    kind: 'shape',
    size: 'small',
    color: CANVAS_COLORS[index + 4],
    column: 1,
    row: index * 2 + at,
    edges: [{ from: `${route}-from`, route, color: CANVAS_COLORS[index + 4], end_mark: 'triangle', label: at === 0 ? route : undefined }],
  })),
]);

const edgeMotions = CANVAS_EDGE_ANIMATIONS.flatMap((animation, row) =>
  pair(animation, row, { animation, is_active: true, tone: 'running' }),
);

const itemMotions = CANVAS_ANIMATIONS.map((animation, column) => ({
  id: animation,
  title: animation,
  text: 'It draws the eye.',
  size: 'small',
  color: CANVAS_COLORS[column + 2],
  animation,
  column,
}));

const kinds = [
  ...CANVAS_SIZES.map((size, row) => ({
    id: `card-${size}`,
    title: `Card, ${size}`,
    text: 'Text with **bold** words and `code`.',
    meta: '3 min',
    tone: 'running',
    icon: 'code',
    badges: ['one', 'two'],
    progress: 0.25 + row * 0.3,
    comments: row + 1,
    size,
    column: 0,
    row,
  })),
  ...CANVAS_SIZES.map((size, row) => ({
    id: `note-${size}`,
    title: `Note, ${size}`,
    kind: 'note',
    text: 'A sticky note for a thought.',
    color: ['yellow', 'pink', 'teal'][row],
    size,
    column: 1,
    row,
  })),
  ...CANVAS_SIZES.map((size, row) => ({
    id: `comment-${size}`,
    title: 'Review',
    kind: 'comment',
    author: ['Ada Lovelace', 'Lin', 'Grace Hopper'][row],
    text: 'Does this step need a test?',
    color: ['blue', 'green', 'purple'][row],
    is_resolved: row === 1,
    size,
    column: 2,
    row,
  })),
  ...CANVAS_SIZES.map((size, row) => ({
    id: `text-${size}`,
    title: `Text, ${size}`,
    kind: 'text',
    text: 'Words with no border.',
    caption: row === 2 ? 'A caption stands under each kind of item.' : undefined,
    size,
    column: 3,
    row,
  })),
];

const nested = [
  { id: 'outer', title: 'Outer frame', kind: 'frame', color: 'blue', icon: 'cloud', text: 'A frame holds items and frames.' },
  { id: 'gate', title: 'Gateway', icon: 'server', tone: 'ok', parent: 'outer' },
  { id: 'middle', title: 'Middle frame', kind: 'frame', color: 'purple', border: 'dashed', parent: 'outer' },
  { id: 'api', title: 'API', icon: 'code', parent: 'middle', edges: [{ from: 'gate', route: 'elbow' }] },
  { id: 'inner', title: 'Inner frame', kind: 'frame', color: 'green', fill: 'solid', border: 'dotted', parent: 'middle' },
  { id: 'data', title: 'Data', kind: 'shape', shape: 'cylinder', color: 'green', icon: 'database', parent: 'inner', edges: [{ from: 'api', label: 'reads' }] },
  { id: 'cache', title: 'Cache', kind: 'shape', shape: 'cylinder', color: 'orange', parent: 'inner', edges: [{ from: 'api', line: 'dashed' }] },
  { id: 'empty', title: 'Empty frame', kind: 'frame', fill: 'none', border: 'double' },
  { id: 'user', title: 'Person', kind: 'shape', shape: 'ellipse', icon: 'users', column: 0 },
  { id: 'back', title: 'Answer', kind: 'shape', shape: 'pill', color: 'teal', edges: [{ from: 'data', route: 'elbow', end_mark: 'triangle' }] },
];
(nested[1] as Wire).edges = [{ from: 'user', label: 'asks', is_active: true }];

/** Each word of the canvas vocabulary, one board for each group of words. */
export const STORY_CANVAS_GALLERY = view('canvas-gallery', 'Canvas vocabulary', [
  board('shapes', 'Shapes', shapes),
  board('colors', 'Colours and fills', colors),
  board('borders', 'Borders', borders),
  board('kinds', 'Kinds and sizes', kinds),
  board('nested', 'Frames in frames', nested),
  board('marks', 'Lines and marks', marks),
  board('routes', 'Routes', routes),
  board('edge-motions', 'Arrows with work on them', edgeMotions),
  board('item-motions', 'Items that draw the eye', itemMotions),
  ...CANVAS_PATTERNS.map((pattern) =>
    board(`pattern-${pattern}`, `Pattern: ${pattern}`, pair(pattern, 0, {}), { pattern }),
  ),
]);

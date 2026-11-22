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

/** One posted message of a story thread. The minutes keep the messages in order. */
function message(id: string, kind: 'human' | 'agent' | 'extension', body: string, name?: string): Wire {
  const minute = Number(id.replace(/\D/g, '')) || 0;
  return {
    id,
    author: { kind, ...(name ? { name } : {}) },
    body,
    created_at: Date.UTC(2026, 10, 22, 9, minute),
  };
}

function thread(id: string, itemId: string, messages: Wire[], more: Wire = {}): Wire {
  return { id, item_id: itemId, messages, ...more };
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

const PICTURE = (SHOWCASE.cards as Wire[]).find((card) => card.picture)?.picture;

/** A large board: five frames, two of them in a frame, and each kind of item and arrow. */
export const STORY_CANVAS_COMPLEX = view('canvas-complex', 'One turn, end to end', [
  board(
    'turn',
    'How a request becomes an answer',
    [
      { id: 'heading', kind: 'text', size: 'large', title: 'One turn', text: 'From the request of a person to the answer.' },
      { id: 'person', kind: 'shape', shape: 'pill', size: 'small', color: 'green', fill: 'solid', icon: 'users', title: 'Person' },

      { id: 'clients', kind: 'frame', color: 'blue', icon: 'app', title: 'Clients', text: 'Each client speaks to the gateway over HTTP.' },
      { id: 'tui', parent: 'clients', icon: 'terminal', title: 'TUI', meta: 'terminal', tone: 'ok', edges: [{ from: 'person' }] },
      { id: 'companion', parent: 'clients', icon: 'app', title: 'Companion', meta: 'phone and desk', text: 'Shows the live view of a turn.', edges: [{ from: 'person' }] },
      { id: 'sdk', parent: 'clients', icon: 'code', title: 'SDK', meta: 'Python', border: 'dashed', edges: [{ from: 'person', line: 'dashed' }] },

      { id: 'gateway', kind: 'frame', color: 'purple', icon: 'server', title: 'Gateway', text: 'One daemon for each machine.' },
      { id: 'auth', parent: 'gateway', kind: 'shape', shape: 'hexagon', color: 'purple', icon: 'lock', title: 'Auth', edges: [{ from: 'tui' }, { from: 'companion' }, { from: 'sdk', line: 'dashed' }] },
      { id: 'sessions', parent: 'gateway', icon: 'users', title: 'Sessions', text: 'One queue for each session.', badges: ['12 open'], edges: [{ from: 'auth', route: 'straight', end_mark: 'triangle' }] },
      { id: 'queue', parent: 'gateway', kind: 'frame', color: 'teal', border: 'dotted', title: 'Queue' },
      { id: 'turn', parent: 'queue', icon: 'clock', title: 'Turn', meta: '2 wait', tone: 'running', progress: 0.35, edges: [{ from: 'sessions', label: 'next' }] },
      { id: 'journal', parent: 'gateway', kind: 'shape', shape: 'cylinder', color: 'purple', icon: 'database', title: 'Journal', edges: [{ from: 'sessions', label: 'events', line: 'dashed', start_mark: 'circle' }] },

      { id: 'engine', kind: 'frame', color: 'green', icon: 'settings', title: 'Engine', text: 'Plans the work and runs the code.' },
      { id: 'route', parent: 'engine', kind: 'shape', shape: 'diamond', color: 'orange', title: 'Model?', column: 1, edges: [{ from: 'turn', route: 'elbow' }] },
      { id: 'plan', parent: 'engine', icon: 'checklist', title: 'Plan', text: 'Splits the request into **steps**.', tone: 'ok', edges: [{ from: 'route', label: 'ready' }] },
      { id: 'sandbox', parent: 'engine', kind: 'frame', color: 'orange', border: 'dashed', icon: 'shield', title: 'Sandbox' },
      { id: 'python', parent: 'sandbox', icon: 'code', title: 'Python', text: 'Runs each block of the step.', meta: '4 s', tone: 'running', progress: 0.6, animation: 'glow', badges: ['step 3 of 5'], edges: [{ from: 'plan', is_active: true, animation: 'flow', label: 'code' }] },
      { id: 'extensions', parent: 'sandbox', icon: 'tools', title: 'Extensions', meta: '28', edges: [{ from: 'python', line: 'dashed', start_mark: 'circle' }] },
      { id: 'jail', parent: 'sandbox', kind: 'shape', shape: 'hexagon', size: 'small', color: 'red', icon: 'lock', title: 'Jail', edges: [{ from: 'python', line: 'dotted', end_mark: 'bar' }] },

      { id: 'providers', kind: 'frame', color: 'yellow', icon: 'cloud', title: 'Providers' },
      { id: 'primary', parent: 'providers', icon: 'cloud', title: 'Primary', meta: '1.2 s', tone: 'ok', edges: [{ from: 'route', label: 'ask', color: 'green', line: 'thick', end_mark: 'triangle', is_active: true, animation: 'dot' }] },
      { id: 'fallback', parent: 'providers', icon: 'cloud', title: 'Fallback', meta: 'waits', border: 'dashed', tone: 'idle', edges: [{ from: 'route', label: 'on a refusal', line: 'dashed' }] },
      { id: 'retry', kind: 'note', size: 'small', title: 'Retry', text: 'A dropped connection gets **one** more try.', edges: [{ from: 'primary', line: 'dashed', end_mark: 'none' }] },

      { id: 'answer', kind: 'shape', shape: 'document', color: 'blue', icon: 'book', title: 'Answer', edges: [{ from: 'python', route: 'elbow', color: 'blue' }, { from: 'primary', color: 'blue' }] },
      { id: 'live', icon: 'image', title: 'Live view', meta: 'PNG', picture: PICTURE, caption: 'The board of the turn, as the app draws it.', edges: [{ from: 'answer' }] },
      { id: 'replay', title: 'Replay', meta: '0:42', media: { kind: 'video', url: 'https://example.com/vis/turn-replay.mp4', alt: 'A recorded run of the turn' }, edges: [{ from: 'journal', line: 'dotted', end_mark: 'diamond' }] },
      { id: 'done', kind: 'shape', shape: 'ellipse', size: 'small', color: 'red', fill: 'solid', title: 'Done', edges: [{ from: 'answer', end_mark: 'triangle' }] },
    ],
    {
      selected_ids: ['python'],
      flow: 'auto',
      threads: [
        thread('replay', 'journal', [
          message('q1', 'human', 'Can the journal replay a turn after a restart?', 'Ada'),
          message('q2', 'agent', 'Yes. A resumed turn reads its events again.'),
          message('q3', 'human', 'Good. Then the queue needs no copy of its own.', 'Ada'),
        ], { is_resolved: true }),
        thread('steps', 'plan', [
          message('p1', 'extension', 'The plan has 4 steps. Step 2 waits for the model.', 'planner'),
        ]),
        thread('limits', 'plan', [message('p2', 'human', 'How many steps can one plan hold?', 'Lin')], {
          draft: 'Show the limit in the card.',
        }),
      ],
    },
  ),
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
    id: `text-${size}`,
    title: `Text, ${size}`,
    kind: 'text',
    text: 'Words with no border.',
    caption: row === 2 ? 'A caption stands under each kind of item.' : undefined,
    size,
    column: 2,
    row,
  })),
  // Each kind shows its details, badges and progress.
  ...(['shape', 'text', 'frame'] as const).map((kind, row) => ({
    id: `filled-${kind}`,
    title: `Filled ${kind}`,
    kind,
    shape: kind === 'shape' ? 'hexagon' : undefined,
    color: 'purple',
    meta: '2 of 5',
    badges: ['beta'],
    progress: 0.4,
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
  board('kinds', 'Kinds and sizes', kinds, {
    // The three marks of a discussion: open, resolved, and with a saved draft.
    threads: [
      thread('open', 'card-small', [message('k1', 'human', 'Does this step need a test?', 'Ada Lovelace')]),
      thread('closed', 'card-medium', [
        message('k2', 'human', 'Does this step need a test?', 'Lin'),
        message('k3', 'agent', 'It has one now.'),
      ], { is_resolved: true }),
      thread('draft', 'card-large', [], { draft: 'Make the title shorter.' }),
    ],
  }),
  board('nested', 'Frames in frames', nested),
  board('marks', 'Lines and marks', marks),
  board('routes', 'Routes', routes),
  board('edge-motions', 'Arrows with work on them', edgeMotions),
  board('item-motions', 'Items that draw the eye', itemMotions),
  ...CANVAS_PATTERNS.map((pattern) =>
    board(`pattern-${pattern}`, `Pattern: ${pattern}`, pair(pattern, 0, {}), { pattern }),
  ),
]);

/** Each block of Markdown in the text of an item, as a message of the session shows it. */
export const STORY_CANVAS_MARKDOWN = view('canvas-markdown', 'Markdown in items', [
  board('markdown', 'Markdown', [
    {
      id: 'about',
      kind: 'frame',
      title: 'Release notes',
      text: 'Each item is **Markdown**: the same rules as a message.',
      color: 'blue',
    },
    {
      id: 'plan',
      parent: 'about',
      title: 'Release plan',
      icon: 'checklist',
      size: 'large',
      text: [
        '# Version 0.6',
        '',
        'The board shows **each step**, with _notes_, ~~old ideas~~ and `code`.',
        '',
        '## Steps',
        '',
        '1. Build the binary.',
        '2. Run the checks:',
        '   - unit tests',
        '   - native tests',
        '3. Tag the commit.',
        '',
        '---',
        '',
        'Read [the release guide](https://example.com/release) first.',
      ].join('\n'),
    },
    {
      id: 'times',
      parent: 'about',
      title: 'Check times',
      icon: 'clock',
      size: 'large',
      edges: [{ from: 'plan', label: 'measures' }],
      text: [
        '| Check | Machine | Time |',
        '| :--- | :---: | ---: |',
        '| Engine | 2 | 4 min |',
        '| Companion | 1 | 1 min |',
        '| Native | 1 | **12 min** |',
      ].join('\n'),
    },
    {
      id: 'code',
      title: 'The release call',
      icon: 'code',
      size: 'large',
      color: 'purple',
      edges: [{ from: 'times', is_active: true }],
      text: [
        '```clojure',
        '(defn ship!',
        '  [version]',
        '  (build/release {:version version :sign? true}))',
        '```',
        '',
        'A long line wraps: `clojure -T:build release :version 0.6.0 :channels ["stable" "beta"] :sign true`',
      ].join('\n'),
    },
    {
      id: 'tasks',
      kind: 'note',
      title: 'Before the tag',
      size: 'large',
      edges: [{ from: 'code', line: 'dashed' }],
      text: [
        '- [x] Changelog entry',
        '- [x] Docs page',
        '- [ ] Release notes',
        '',
        '> Tag only **after** the checks pass.',
      ].join('\n'),
    },
    {
      id: 'flow',
      title: 'The flow',
      icon: 'branch',
      size: 'large',
      color: 'teal',
      edges: [{ from: 'code' }],
      text: ['```mermaid', 'flowchart LR', '  Build --> Check --> Tag', '```'].join('\n'),
    },
    {
      id: 'links',
      title: 'Long words',
      color: 'orange',
      edges: [{ from: 'flow' }],
      text: 'The run https://github.com/Blockether/vis/actions/runs/45028548818/jobs/123456789?check_suite_focus=true and averyveryverylongwordwithoutanyspaces wrap in the item.',
    },
    {
      id: 'ready',
      kind: 'shape',
      shape: 'diamond',
      title: 'Ready?',
      color: 'green',
      edges: [{ from: 'tasks' }, { from: 'links' }],
      text: '**All** checks pass',
    },
    {
      id: 'label',
      kind: 'text',
      title: 'Shipped',
      size: 'large',
      edges: [{ from: 'ready', label: 'yes' }],
      text: 'Version `0.6` is on _every_ channel.',
    },
  ], {
    threads: [
      thread('why', 'code', [
        message('m1', 'human', 'Why does `ship!` sign the build?', 'Ada'),
        message('m2', 'agent', 'The installer checks the signature:\n\n```sh\ncodesign --verify vis\n```\n\nAn unsigned build **fails** that check.'),
      ]),
    ],
  }),
]);

/**
 * THE DATA THE GALLERY DRAWS, AND THE ONE PLACE IT IS WRITTEN DOWN.
 *
 * A story is one control at one size; what makes it a DESIGN artifact is the
 * data inside it — a name long enough to truncate, a count that is zero, a
 * machine that is not answering, a table wider than the phone. Those are the
 * cases a gallery exists to catch, so they are values with names here rather
 * than literals inline in whichever story was written last: two stories drawing
 * "a session" have to draw the SAME session, or the sheet stops comparing.
 *
 * Where a shape is a WIRE CONTRACT it is not hand-typed: the projections below
 * are engine payloads pushed through `activityProjectionFromWire`, so a payload
 * that stops parsing fails the story that draws it instead of quietly painting
 * an empty panel. Prose — a note, a log, a caption — is written here, because
 * prose has no wire.
 *
 * Nothing here reaches the product: `src/dev/**` is fixtures, the bundle graphs
 * `index.html` alone, and `ui.test.tsx`'s call-site scan exempts this directory
 * because a fixture may paint a backdrop the app itself never would.
 */

import { activityProjectionFromWire, type ActivityProjection } from '../lib/activity';
import activityWire from '../lib/activity.fixture.json';
import { MACHINE_COLORS, type MachineColor } from '../lib/machine-colors';
import { liveViewFromWire, type LiveView } from '../lib/live-view';
import liveViewWire from '../lib/live-view.fixture.json';
import { COMMENTS_HEADING } from '../lib/markdown-annotations';
import type { RouterProvider } from '../lib/types';
import type { ProviderAuth } from '../components/ProviderAuth';

/** A hue by its palette name, so a story names a colour the way the fleet does. */
export function storyHue(name: string): MachineColor {
  return MACHINE_COLORS.find((one) => one.name === name) ?? MACHINE_COLORS[0];
}

/**
 * THE FLEET EVERY STORY SHARES. Three machines, because two never show that the
 * switcher scrolls; one of them is down, because a drained mark is the state the
 * palette is actually asked about; and one name is long enough to truncate.
 */
export const STORY_MACHINES = [
  { name: 'tower', color: storyHue('teal'), live: 2, unread: 0, isDown: false },
  { name: 'macbook-pro-16-work', color: storyHue('violet'), live: 0, unread: 4, isDown: false },
  { name: 'mini', color: storyHue('orange'), live: 0, unread: 0, isDown: true },
] as const;

/** The engine's own payload, refused loudly rather than drawn empty. */
function projection(wire: unknown): ActivityProjection {
  const parsed = activityProjectionFromWire(wire);
  if (!parsed) throw new Error('story activity payload no longer parses');
  return parsed;
}

/** A form still working: the fixture the app's own parser test reads. */
export const ACTIVITY_RUNNING = projection(activityWire);

/** The same form, settled — what the panel looks like when nothing is moving. */
export const ACTIVITY_SETTLED = projection({
  state: 'succeeded',
  counts: { running: 0, succeeded: 3, failed: 0, cancelled: 0 },
  rows: [
    {
      id: 'call-1',
      sequence: 1,
      operation: 'grep',
      presenter: 'observation',
      signal: 'observation',
      state: 'succeeded',
      summary: '18 matches',
      duration_ms: 41,
      resources: [],
      evidence: [{ kind: 'result', text: '18 matches in 4 files' }],
    },
    {
      id: 'call-2',
      sequence: 2,
      operation: 'patch',
      presenter: 'patch',
      signal: 'mutation',
      state: 'succeeded',
      summary: 'src/components/ui.tsx',
      duration_ms: 96,
      resources: [{ type: 'file', id: 'src/components/ui.tsx' }],
      evidence: [
        {
          kind: 'diff',
          text: '+2 -1',
          lines: [
            { kind: 'header', text: 'src/components/ui.tsx' },
            { kind: 'deletion', text: "  const face = 'rounded-none';" },
            { kind: 'addition', text: "  const face = 'rounded-control';" },
          ],
          additions: 1,
          deletions: 1,
          modifications: 1,
          omitted_lines: 0,
          is_truncated: false,
          is_redacted: false,
        },
      ],
    },
    {
      id: 'call-3',
      sequence: 3,
      operation: 'run_tests',
      presenter: 'tests',
      signal: 'verification',
      state: 'succeeded',
      summary: 'ui.test.tsx',
      duration_ms: 4210,
      result_summary: '285 passed',
      resources: [],
      evidence: [{ kind: 'result', text: '285 passed, 0 failed' }],
    },
  ],
  omitted: { rows: 0, by_classification: {} },
});

/** The state a panel must not swallow: one row failed and says why. */
export const ACTIVITY_FAILED = projection({
  state: 'failed',
  counts: { running: 0, succeeded: 1, failed: 1, cancelled: 0 },
  rows: [
    {
      id: 'call-1',
      sequence: 1,
      operation: 'format_code',
      presenter: 'format',
      signal: 'mutation',
      state: 'succeeded',
      summary: '3 files',
      duration_ms: 812,
      resources: [],
      evidence: [{ kind: 'result', text: '3 files reformatted' }],
    },
    {
      id: 'call-2',
      sequence: 2,
      operation: 'run_tests',
      presenter: 'tests',
      signal: 'verification',
      state: 'failed',
      summary: 'ui.test.tsx',
      duration_ms: 3980,
      error_summary: '1 failed',
      resources: [],
      evidence: [
        {
          kind: 'error',
          text: 'expected the composer to reserve the stop slot, found none',
        },
      ],
    },
  ],
  omitted: { rows: 2, by_classification: { observation: 2 } },
});

/**
 * A `vis-table` fence exactly as `attach` emits it: five header lines, then the
 * CSV. More columns than a phone can hold and a mixed numeric column, because
 * alignment and the horizontal scroll are what this control is asked about.
 */
export const TABLE_BLOCK = [
  '[Table: fleet.csv 7 rows × 5 cols, 268 B]',
  'fleet.csv',
  'text/csv',
  '5 x 7',
  '268 B',
  'machine,sessions,live,tokens,last seen',
  'tower,42,2,1284003,12:04',
  'macbook-pro-16-work,17,0,402911,11:57',
  'mini,3,0,18422,yesterday',
  'ci-runner-1,128,1,9910442,12:03',
  'ci-runner-2,128,0,9810221,12:03',
  'staging,6,0,71204,Monday',
  'laptop,0,0,0,never',
].join('\n');

/** What a `.log` artifact looks like: no markdown, long lines, a real error. */
export const LOG_TEXT = [
  '12:04:01.221 gateway  session fd3c03f9 attached (protocol 7)',
  '12:04:01.224 provider anthropic-coding-plan claude-opus-5 stream opened',
  '12:04:09.508 provider first content block after 8.284s',
  '12:04:12.902 tool     run_tests ui.test.tsx -> 285 passed',
  '12:04:13.004 error    EADDRINUSE 127.0.0.1:6006 — storybook is already up',
].join('\n');

/** A note the model wrote: headings, a list and code, so the prose face is drawn. */
export const NOTE_MARKDOWN = [
  '# Composer, one row',
  '',
  'The strip reads left to right as **what goes into the message**, then **what',
  'happens to the turn**:',
  '',
  '- `+` attaches, `mic` dictates — neither starts a turn',
  '- `stop` ends the running turn, `send` starts the next one',
  '',
  '```ts',
  "<ComposerButton label=\"Send message\" tone=\"send\" />",
  '```',
].join('\n');

/** A memo's loudness, deterministic so two frames of one story agree. */
export const RECORDING_PEAKS: number[] = Array.from({ length: 96 }, (_, i) =>
  Math.abs(Math.sin(i / 3.1)) * (0.35 + 0.65 * Math.abs(Math.sin(i / 17))),
);

/** What the gateway's speech engine heard, for the recording row's quotation. */
export const RECORDING_TRANSCRIPT =
  'Draw the vocabulary in the gallery instead of writing another mockup, and keep the data beside it so the frames stop disagreeing.';

/** A picture with no bytes to fetch: the plate is what the story is about. */
function picture(fill: string, label: string): string {
  const svg = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 240 240"><rect width="240" height="240" fill="${fill}"/><text x="120" y="128" font-family="monospace" font-size="20" fill="#f7f7f7" text-anchor="middle">${label}</text></svg>`;
  return `data:image/svg+xml;utf8,${encodeURIComponent(svg)}`;
}

/** Three shots, one of them named long enough to truncate its caption. */
export const STORY_PICTURES = [
  { name: 'composer.png', meta: 'PNG · 287KB', src: picture('#1f5f5b', '1') },
  {
    name: 'session-header-after-the-radius-ladder.png',
    meta: 'PNG · 1.1MB',
    src: picture('#4b3f72', '2'),
  },
  { name: 'settings.png', meta: 'PNG · 96KB', src: picture('#7a4b16', '3') },
] as const;

/** `3 images · 1.5MB` — the line a grid says about itself. */
export const PICTURE_SUMMARY = '3 images · 1.5MB';

/**
 * A session row's verbs, in the order the list paints them. The icon is not
 * here: a story hands the mark, this says what the verbs ARE and what they mean.
 */
export const SESSION_VERBS = [
  { key: 'star', label: 'Star', name: 'Star this session', tone: 'accent' },
  { key: 'rename', label: 'Rename', name: 'Rename this session', tone: 'neutral' },
  { key: 'delete', label: 'Delete', name: 'Delete this session', tone: 'danger' },
] as const;

/** The verbs of one session, as a menu hangs them: a fact, a hint, a badge. */
export const MENU_VERBS = [
  { title: 'Open on tower', meta: '2 live', hint: 'The machine that ran it' },
  { title: 'Fork from here', hint: 'A new session from this turn' },
  { title: 'Copy session id', meta: 'fd3c03f9' },
  { title: 'Mark as read', badge: '4' },
] as const;

/** One session, in the words a header and a row use for it. */
export const STORY_SESSION = {
  id: 'fd3c03f9',
  title: 'Chciałbym zrobić nowy design dla activity zarówno w TUI jak i w apce',
  project: 'vis',
  where: '~/vis',
  machine: 'tower',
  turns: 61,
  tokens: '152k',
  model: 'claude-opus-5',
} as const;

/**
 * THE ENGINE'S OWN VIEW. `live-view.fixture.json` is the projection
 * `gateway/human_input_test.clj` pins, so the panel here paints what a real run
 * pushes — and a wire change breaks this story before it reaches a screen.
 */
export const STORY_LIVE_VIEW: LiveView = (() => {
  const view = liveViewFromWire(liveViewWire);
  if (!view) throw new Error('the engine fixture must be paintable');
  return view;
})();

/** A fleet with something to say: the default, the fallback, and one never signed in. */
export const STORY_PROVIDERS: RouterProvider[] = [
  {
    id: 'anthropic',
    label: 'Anthropic',
    models: ['claude-opus-5', 'claude-sonnet-4-5'],
    is_default: true,
    default_model: 'claude-opus-5',
    is_fallback: false,
    fallback_model: null,
    status: {
      is_authenticated: true,
      auth_state: 'verified',
      account_type: 'Coding plan',
      source: 'auth-file',
    },
  },
  {
    id: 'openai',
    label: 'OpenAI',
    models: ['gpt-5.2', 'gpt-5-mini'],
    is_default: false,
    default_model: null,
    is_fallback: true,
    fallback_model: 'gpt-5-mini',
    status: {
      is_authenticated: true,
      auth_state: 'degraded',
      warning: 'The last live check timed out; the credential still works.',
    },
  },
  {
    id: 'ollama',
    label: 'Ollama',
    base_url: 'http://127.0.0.1:11434',
    models: [],
    is_default: false,
    default_model: null,
    is_fallback: false,
    fallback_model: null,
    status: {
      is_authenticated: false,
      auth_state: 'unverified',
      detail: 'No credential on this machine',
    },
  },
];

/**
 * The `ProviderAuth` the rows are handed: the fields a COLLAPSED fleet reads, plus
 * a stub for every verb a press could reach. The panel asks the gateway only when
 * a row is opened, so the paint a gallery photographs needs no client at all — and
 * the cast is the same one `ProviderAuth.test.tsx` uses for the same reason.
 */
export function storyProviderAuth(
  providers: RouterProvider[] | null = STORY_PROVIDERS,
): ProviderAuth {
  const nothing = async () => {};
  return {
    providers,
    presets: [],
    err: null,
    note: null,
    flow: null,
    pending: null,
    apiKey: '',
    redirectUrl: '',
    setProviders: () => {},
    setErr: () => {},
    setNote: () => {},
    setPending: () => {},
    setApiKey: () => {},
    setRedirectUrl: () => {},
    reload: nothing,
    refresh: nothing,
    recheck: nothing,
    signIn: nothing,
    finishPkce: nothing,
    finishApiKey: nothing,
    cancelFlow: nothing,
    loadPresets: nothing,
    addProvider: nothing,
    removeProvider: nothing,
  } as unknown as ProviderAuth;
}

/**
 * A DOCUMENT AS BYTES, NOT AS A FETCH. The app hands the frame an object URL for
 * an attachment; a `data:` URL carries the same markup with nothing to download,
 * so the story draws the quarantine itself — the sandbox, the paper, the fitted
 * box — and two frames of it compare.
 */
export const STORY_DOC_HTML = [
  '<!doctype html>',
  '<meta charset="utf-8">',
  '<title>Coverage report</title>',
  '<style>',
  '  body { font: 14px ui-monospace, monospace; margin: 24px; color: #1c1c1c; }',
  '  h1 { font-size: 18px; margin: 0 0 12px; }',
  '  table { border-collapse: collapse; width: 100%; }',
  '  th, td { border-bottom: 1px solid #d8d8d8; padding: 6px 8px; text-align: left; }',
  '  td.n { text-align: right; font-variant-numeric: tabular-nums; }',
  '</style>',
  '<h1>Coverage &mdash; apps/vis-companion</h1>',
  '<table>',
  '  <tr><th>file</th><th>lines</th><th>covered</th></tr>',
  '  <tr><td>src/components/ui.tsx</td><td class="n">2412</td><td class="n">98%</td></tr>',
  '  <tr><td>src/screens/SessionScreen.tsx</td><td class="n">6412</td><td class="n">91%</td></tr>',
  '  <tr><td>src/lib/live-view.ts</td><td class="n">318</td><td class="n">100%</td></tr>',
  '</table>',
].join('\n');

export const STORY_DOC_URL = `data:text/html;charset=utf-8,${encodeURIComponent(STORY_DOC_HTML)}`;

/** The same note after two remarks, in the format the file itself carries. */
export const NOTE_ANNOTATED = [
  NOTE_MARKDOWN,
  '',
  COMMENTS_HEADING,
  '',
  '- **“the strip reads left to right”** — Say WHY, not what: the left half never starts a turn.',
  '- **Whole document** — Worth a screenshot in the gallery once the stop slot lands.',
].join('\n');

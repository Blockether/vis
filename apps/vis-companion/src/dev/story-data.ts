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
import type { SessionArtifact } from '../lib/artifacts';
import type { PendingAttachment } from '../lib/attachments';
import type { GatewayClient } from '../lib/gateway';
import { MACHINE_COLORS, type MachineColor } from '../lib/machine-colors';
import { liveViewFromWire, type LiveView } from '../lib/live-view';
import liveViewWire from '../lib/live-view.fixture.json';
import { createComposerPaste, type ComposerPaste } from '../lib/paste';
import { COMMENTS_HEADING } from '../lib/markdown-annotations';
import type {
  ContentBlock,
  FileSuggestion,
  GatewayConn,
  QueuedTurn,
  QueuePausedInfo,
  RouterProvider,
  Session,
  SessionUsage,
  SlashCommand,
  TranscriptIteration,
  TranscriptTurn,
} from '../lib/types';
import type { GwHealth } from '../components/Machines';
import type { ManagedProject } from '../components/ManageProjectsSheet';
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
          text: 'src/components/ui.tsx',
          lines: [
            { kind: 'header', text: 'src/components/ui.tsx' },
            { kind: 'deletion', text: "  const face = 'rounded-none';" },
            { kind: 'addition', text: "  const face = 'rounded-control';" },
          ],
          additions: 1,
          deletions: 1,
          modifications: 1,
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

/**
 * A TURN OF REAL WORK, which is what the axis is FOR: reads that answered a
 * question, a patch the repository REFUSED, the patch that landed, a check that
 * failed and a step still moving — plus the two rows the engine's own bound
 * dropped, because a chronology that hides its ceiling lies about what it shows.
 *
 * Every shape the axis can draw is here on purpose: a summary sentence, a list
 * of paths with their kinds, a diff's `+7 −3`, and two errors that open
 * themselves and are clamped to their own head.
 */
export const ACTIVITY_CHRONOLOGY = projection({
  state: 'running',
  counts: { running: 1, succeeded: 5, failed: 2, cancelled: 0 },
  rows: [
    {
      id: 'call-1',
      sequence: 1,
      operation: 'grep',
      presenter: 'observation',
      signal: 'observation',
      state: 'succeeded',
      summary: '73 files',
      duration_ms: 340,
      result_summary: 'Found 50 matches across the app and the TUI',
      resources: [],
      evidence: [{ kind: 'arguments', text: '{query: ["ActivityPanel"], paths: ["src"]}' }],
    },
    {
      id: 'call-2',
      sequence: 2,
      operation: 'cat',
      presenter: 'observation',
      signal: 'observation',
      state: 'succeeded',
      summary: '6 files',
      duration_ms: 120,
      resources: [
        { type: 'file', id: 'src/components/ActivityPanel.tsx' },
        { type: 'file', id: 'src/components/ChatContent.tsx' },
        { type: 'file', id: 'src/index.css' },
        { type: 'file', id: 'src/com/blockether/vis/internal/render.clj' },
        { type: 'file', id: 'src/lib/activity.ts' },
        { type: 'file', id: 'src/components/ui.tsx' },
      ],
      evidence: [{ kind: 'result', text: '612 lines read' }],
    },
    {
      id: 'call-3',
      sequence: 3,
      operation: 'patch',
      presenter: 'patch',
      signal: 'mutation',
      state: 'failed',
      summary: 'src/components/ChatContent.tsx',
      duration_ms: 88,
      error_summary: 'no match',
      resources: [],
      evidence: [
        {
          kind: 'error',
          text: [
            'patch refused: hunk 2 of 3, from 208 to 213',
            '  expected  const face = ACTIVITY_FACE[row.state];',
            '  found     const face = ACTIVITY_FACE[row.signal];',
            '  the anchor moved when the file was formatted',
            '  re-read the region and retry',
          ].join('\n'),
        },
      ],
    },
    {
      id: 'call-4',
      sequence: 4,
      operation: 'patch',
      presenter: 'patch',
      signal: 'mutation',
      state: 'succeeded',
      summary: '2 files',
      duration_ms: 96,
      resources: [
        { type: 'file', id: 'src/components/ActivityPanel.tsx' },
        { type: 'file', id: 'src/components/ChatContent.tsx' },
      ],
      evidence: [
        {
          kind: 'diff',
          text: 'src/components/ActivityPanel.tsx',
          lines: [
            { kind: 'context', text: '  const face = ACTIVITY_FACE[row.state];' },
            { kind: 'deletion', text: '  className="border-t border-code-edge bg-result"' },
            { kind: 'addition', text: '  className="grid grid-cols-[auto_minmax(0,1fr)] gap-x-1.5"' },
            { kind: 'addition', text: '  data-activity-row={row.id}' },
          ],
          additions: 4,
          deletions: 2,
          modifications: 2,
          is_truncated: true,
          is_redacted: false,
        },
        {
          kind: 'diff',
          text: 'src/components/ChatContent.tsx',
          lines: [
            { kind: 'context', text: '  {detectedActivity && activity && (' },
            { kind: 'deletion', text: '    <ActivityPanel activity={activity} compact />' },
            { kind: 'addition', text: '    <ActivityPanel activity={activity} />' },
          ],
          additions: 3,
          deletions: 1,
          modifications: 1,
          is_truncated: true,
          is_redacted: false,
        },
      ],
    },
    {
      id: 'call-5',
      sequence: 5,
      operation: 'run_tests',
      presenter: 'tests',
      signal: 'verification',
      state: 'failed',
      summary: 'ui.test.tsx',
      duration_ms: 3980,
      error_summary: '2 failed',
      resources: [],
      evidence: [
        {
          kind: 'error',
          text: [
            'FAIL  src/components/ui.test.tsx > the control vocabulary is closed',
            '  expected the composer to reserve the stop slot, found none',
            '  at src/components/ui.test.tsx:412:7',
            'FAIL  src/components/ui.test.tsx > a disabled send loses its paper',
            '  expected 3:1 on the arrow, measured 1.96:1',
          ].join('\n'),
        },
      ],
    },
    {
      id: 'call-6',
      sequence: 6,
      operation: 'shell',
      presenter: 'shell',
      signal: 'generic',
      state: 'running',
      summary: 'running: npm run build',
      resources: [],
      evidence: [],
    },
  ],
  omitted: { rows: 2, by_classification: { observation: 2 } },
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
 * WHAT A BLOCK DID TO THE TREE, with no tool call to hang it on.
 *
 * The engine emits one row per KIND — a hard link, a move and three writes are
 * three rows, not five — so this is the sheet where the count, the two-ended
 * arrow and the fold under a row are all on screen at once: a move that keeps
 * its file name must still read as a move (`vis/PLAN.md → docs/PLAN.md`), a
 * link names both ends, and six deletions show four paths and a `+2 more
 * files`. Every row is empty of evidence on purpose — a filesystem change has
 * no arguments and no result, it IS its paths.
 */
/**
 * ONE CAUSE, MANY CHANGES: what a code block did to the tree with its own hands.
 *
 * Every mutation from one block shares a cause — the block itself, since nothing else
 * reaches the confined filesystem — so the engine hangs them under ONE head with their
 * paths. The head counts the amount and its operation names the act; the sentence under
 * it says who did it; and a write carries the very diff a `patch` row carries.
 */
export const ACTIVITY_TREE_CHANGES = projection({
  state: 'succeeded',
  counts: { running: 0, succeeded: 5, failed: 0, cancelled: 0 },
  rows: [
    {
      id: 'group-fs-mutations-1',
      sequence: 1,
      operation: 'change',
      presenter: 'observation',
      signal: 'mutation',
      state: 'succeeded',
      summary: '13 files and 2 directories',
      group_token: 'fs-mutations-1',
      duration_ms: 50,
      result_summary:
        'The code block changed these itself, with no `patch` or `shell` call in between.',
      result_format: 'markdown',
      resources: [
        { type: 'file', id: '/Users/dev/vis/apps/vis-companion/dist' },
        { type: 'file', id: '/Users/dev/vis/apps/vis-companion/dist/assets' },
        { type: 'file', id: '/Users/dev/vis/apps/vis-companion/src/dev/story-data.ts' },
        { type: 'file', id: '/Users/dev/vis/apps/vis-companion/src/components/ActivityPanel.tsx' },
        { type: 'file', id: '/Users/dev/vis/apps/vis-companion/src/lib/path.ts' },
        { type: 'file', id: '/Users/dev/vis/apps/vis-companion/dist/index.html' },
        { type: 'file', id: '/Users/dev/vis/docs/PLAN.md' },
        { type: 'file', id: '/Users/dev/vis/target/scratch/probe-1.json' },
      ],
      evidence: [],
      children: [
        {
          id: 'fs-mkdir',
          sequence: 1,
          operation: 'mkdir',
          presenter: 'observation',
          signal: 'mutation',
          state: 'succeeded',
          summary: '2 directories',
          group_token: 'fs-mutations-1',
          duration_ms: 5,
          resources: [
            { type: 'file', id: '/Users/dev/vis/apps/vis-companion/dist' },
            { type: 'file', id: '/Users/dev/vis/apps/vis-companion/dist/assets' },
          ],
          evidence: [],
        },
        {
          id: 'fs-write',
          sequence: 2,
          operation: 'write',
          presenter: 'observation',
          signal: 'mutation',
          state: 'succeeded',
          summary: '3 files',
          group_token: 'fs-mutations-1',
          duration_ms: 9,
          resources: [
            { type: 'file', id: '/Users/dev/vis/apps/vis-companion/src/dev/story-data.ts' },
            { type: 'file', id: '/Users/dev/vis/apps/vis-companion/src/components/ActivityPanel.tsx' },
            { type: 'file', id: '/Users/dev/vis/apps/vis-companion/src/lib/path.ts' },
          ],
          evidence: [
            {
              kind: 'diff',
              text: '/Users/dev/vis/apps/vis-companion/src/dev/story-data.ts',
              lines: [
                { kind: 'hunk', text: '@@ -328,6 +328,7 @@' },
                { kind: 'context', text: "  state: 'succeeded'," },
                { kind: 'deletion', text: "  summary: 'wrote 3 files'," },
                { kind: 'addition', text: "  summary: '3 files'," },
                { kind: 'addition', text: "  summary_format: 'inline'," },
                { kind: 'context', text: '  resources: [' },
              ],
              additions: 2,
              deletions: 1,
              modifications: 1,
              is_truncated: false,
              is_redacted: false,
            },
            {
              kind: 'diff',
              text: '/Users/dev/vis/apps/vis-companion/src/components/ActivityPanel.tsx',
              lines: [
                { kind: 'hunk', text: '@@ -415,7 +415,9 @@' },
                { kind: 'context', text: '  return (' },
                { kind: 'deletion', text: '    <ActivityFiles resources={touched} />' },
                { kind: 'addition', text: '    <ActivityFiles resources={touched} diffs={diffs} />' },
              ],
              additions: 1,
              deletions: 1,
              modifications: 1,
              is_truncated: true,
              is_redacted: false,
            },
            {
              kind: 'diff',
              text: '/Users/dev/vis/apps/vis-companion/src/lib/path.ts',
              lines: [
                { kind: 'hunk', text: '@@ -23,6 +23,7 @@' },
                { kind: 'context', text: '  const candidates = roots' },
                { kind: 'addition', text: '    .flatMap((root) => [root, homeifyPath(root)])' },
              ],
              additions: 1,
              deletions: 0,
              modifications: 0,
              is_truncated: false,
              is_redacted: false,
            },
          ],
        },
        {
          id: 'fs-copy',
          sequence: 3,
          operation: 'copy',
          presenter: 'observation',
          signal: 'mutation',
          state: 'succeeded',
          summary: '3 files',
          group_token: 'fs-mutations-1',
          duration_ms: 6,
          resources: [
            { type: 'file', id: '/Users/dev/vis/apps/vis-companion/dist/index.html' },
            { type: 'file', id: '/Users/dev/vis/apps/vis-companion/dist/assets/app.css' },
            { type: 'file', id: '/Users/dev/vis/apps/vis-companion/dist/assets/app.js' },
          ],
          evidence: [],
        },
        {
          id: 'fs-move',
          sequence: 4,
          operation: 'move',
          presenter: 'observation',
          signal: 'mutation',
          state: 'succeeded',
          summary: '`vis/PLAN.md` → `docs/PLAN.md`',
          summary_format: 'inline',
          group_token: 'fs-mutations-1',
          duration_ms: 11,
          resources: [{ type: 'file', id: '/Users/dev/vis/docs/PLAN.md' }],
          evidence: [],
        },
        {
          id: 'fs-delete',
          sequence: 5,
          operation: 'delete',
          presenter: 'observation',
          signal: 'mutation',
          state: 'succeeded',
          summary: '6 files',
          group_token: 'fs-mutations-1',
          duration_ms: 14,
          resources: [
            { type: 'file', id: '/Users/dev/vis/target/scratch/probe-1.json' },
            { type: 'file', id: '/Users/dev/vis/target/scratch/probe-2.json' },
            { type: 'file', id: '/Users/dev/vis/target/scratch/probe-3.json' },
            { type: 'file', id: '/Users/dev/vis/target/scratch/probe-4.json' },
            { type: 'file', id: '/Users/dev/vis/target/scratch/probe-5.json' },
            { type: 'file', id: '/Users/dev/vis/target/scratch/probe-6.json' },
          ],
          evidence: [],
        },
      ],
    },
  ],
  omitted: { rows: 0, by_classification: {} },
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

/** The same session in the gateway row shape consumed by the real navigator. */
export const STORY_SESSION_ROW: Session = {
  id: STORY_SESSION.id,
  title: STORY_SESSION.title,
  model: STORY_SESSION.model,
  project_name: STORY_SESSION.project,
  favorite_rank: 1,
  status: 'running',
  live: true,
  current_turn_id: 'turn-story',
  is_awaiting_input: true,
  server_time_ms: Date.UTC(2030, 0, 2, 12, 0, 0),
  turn_count: STORY_SESSION.turns,
  created_at: '2030-01-02T11:00:00.000Z',
  modified_at: '2030-01-02T11:59:00.000Z',
  workspace: { root: STORY_SESSION.where, label: STORY_SESSION.project },
};

export const STORY_SESSION_USAGE: SessionUsage = {
  turn_count: 2,
  iteration_count: 5,
  tool_call_count: 59,
  fold_count: 0,
  input_tokens: 85_000,
  output_tokens: 2_300,
  cache_read_share_percent: 77,
  reusable_prefix_coverage_percent: 98,
  prompt_cache_reusable_tokens: 81_000,
  prompt_cache_reused_tokens: 79_400,
  prompt_cache_sample_count: 4,
  prompt_cache_estimated_sample_count: 1,
  cost_usd: 0.21,
  duration_ms: 47_000,
  provider: 'anthropic-coding-plan',
  model: 'claude-opus-5',
};

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

/** The model picker's existing gateway seam, settled locally for a deterministic story. */
export const STORY_ROUTER_CLIENT = {
  cachedRouter: () => STORY_PROVIDERS,
  router: async () => STORY_PROVIDERS,
  sessionModel: async () => ({ provider: 'anthropic', model: 'claude-opus-5' }),
  setSessionModel: async (_sid: string, provider: string, model: string) => ({ provider, model }),
} as unknown as GatewayClient;

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

/** A client-shaped value for views whose shown state performs no gateway operation. */
export const STORY_INERT_CLIENT = {} as GatewayClient;

/** Queue actions settle locally in previews; the rows still remain gateway-owned. */
export const STORY_QUEUE_CLIENT = {
  updateQueuedTurn: async () => undefined,
  deleteQueuedTurn: async () => undefined,
  resumeQueue: async () => undefined,
} as unknown as GatewayClient;

export const STORY_QUEUED_TURNS: QueuedTurn[] = [
  {
    turnId: 'turn-2',
    request: 'Inspect the release manifest',
    preview: 'Inspect the release manifest',
    attachments: [
      { filename: 'manifest.png', mediaType: 'image/png', sizeLabel: '24 KB' },
    ],
  },
  {
    turnId: 'turn-3',
    request: 'Summarize the failed checks',
    preview: 'Summarize the failed checks',
    attachments: [],
  },
];

const STORY_LONG_QUEUE_REQUESTS = [
  'Inspect the release manifest',
  'Summarize the failed checks',
  'Verify the public schema package',
  'Cross-check contract examples',
  'Review generated resources',
  'Measure validation performance',
  'Compare JSON Schema libraries',
  'Remove unnecessary abstractions',
  'Fix the remaining checks',
  'Confirm required schema fields',
  'Profile the complete workflow',
  'Lint and verify the release',
];

export const STORY_LONG_QUEUED_TURNS: QueuedTurn[] =
  STORY_LONG_QUEUE_REQUESTS.map((request, index) => ({
    turnId: `long-queue-${index + 1}`,
    request,
    preview: request,
    attachments: [],
  }));

export const STORY_QUEUE_PAUSED: QueuePausedInfo = {
  held: 2,
  reason: 'turn_failed',
};

export const STORY_COMPOSER_PASTE: ComposerPaste = createComposerPaste(
  4,
  [
    'Release checklist',
    '',
    '- Verify the signed manifest',
    '- Publish the changelog',
    '- Notify the mobile release channel',
  ].join('\n'),
);

const STORY_ATTACHMENT_PREVIEW =
  'data:image/svg+xml,%3Csvg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 32 32"%3E%3Crect width="32" height="32" fill="%23e7c75f"/%3E%3Cpath d="M6 23l7-8 5 5 3-4 5 7z" fill="%2313151a"/%3E%3C/svg%3E';

export const STORY_PENDING_ATTACHMENTS: PendingAttachment[] = [
  {
    id: 'release-map',
    filename: 'release-map.png',
    media_type: 'image/png',
    base64: STORY_ATTACHMENT_PREVIEW,
    previewUrl: STORY_ATTACHMENT_PREVIEW,
    size: 512,
  },
  {
    id: 'voice-note',
    filename: 'release-note.m4a',
    media_type: 'audio/mp4',
    base64: 'data:audio/mp4;base64,AA==',
    previewUrl: 'data:audio/mp4;base64,AA==',
    size: 1024,
  },
];

export const STORY_RESPONSE_CONTROL_VALUES = {
  model: {
    value: 'claude-opus-5',
    title: 'anthropic-coding-plan/claude-opus-5',
  },
  reasoning: { label: 'Reasoning', value: 'high' },
  verbosity: { label: 'Verbosity', value: 'medium' },
  fast: { enabled: true },
} as const;

export const STORY_FILE_SUGGESTIONS: FileSuggestion[] = [
  { name: 'src/components/ActivityPanel.tsx', size: '18 KB', age: '2m', status: 'modified' },
  { name: 'README.md', size: '8 KB', age: '1d', status: 'clean' },
  { name: 'docs/release-checklist.md', size: '4 KB', age: '3d', status: 'untracked' },
];

export const STORY_SLASH_COMMANDS: SlashCommand[] = [
  { name: '/help', doc: 'Show the available slash commands.' },
  { name: '/rename', doc: 'Rename this session title.' },
  { name: '/export', doc: 'Export this session transcript to Markdown or HTML.' },
];

/** The paired fleet: reachable, unavailable, and long enough to exercise truncation. */
export const STORY_GATEWAYS: GatewayConn[] = [
  { url: 'http://10.0.0.5:7890', token: 'story', label: 'tower' },
  { url: 'http://100.64.0.10:7890', token: 'story', label: 'macbook-pro-16-work' },
  { url: 'http://10.0.0.9:7890', token: 'story', label: 'mini' },
];

/** Verdicts are deliberately future-dated so a frozen frame never ages into “Checking”. */
export const STORY_GATEWAY_HEALTH: Record<string, GwHealth> = {
  [STORY_GATEWAYS[0].url]: { state: 'online', at: Number.MAX_SAFE_INTEGER, ms: 18 },
  [STORY_GATEWAYS[1].url]: { state: 'online', at: Number.MAX_SAFE_INTEGER, ms: 42 },
  [STORY_GATEWAYS[2].url]: {
    state: 'offline',
    at: Number.MAX_SAFE_INTEGER,
    why: 'The latest probe timed out',
  },
};

/** Two real project states: one running, one settled. */
export const STORY_PROJECTS: ManagedProject[] = [
  { name: 'vis', root: '/Users/me/code/vis', projectId: 'project-vis', count: 61, live: 2 },
  { name: 'demo', root: '/Users/me/code/demo', projectId: 'project-demo', count: 4, live: 0 },
];

const STORY_HISTORY_CUTS: SessionArtifact[] = [
  {
    key: 'i8:0',
    kind: 'file',
    name: 'build.log',
    media: 'LOG',
    mediaType: 'application/octet-stream',
    size: 20480,
    sizeLabel: '20KB',
    turn: 8,
    iterationId: 'i8',
    index: 0,
    version: 2,
  },
  {
    key: 'i7:0',
    kind: 'file',
    name: 'build.log',
    media: 'LOG',
    mediaType: 'application/octet-stream',
    size: 18240,
    sizeLabel: '18KB',
    turn: 7,
    iterationId: 'i7',
    index: 0,
    version: 1,
  },
];

/** The newest cut and the history control that must remain attached to its tile. */
export const STORY_ARTIFACT_HISTORY: SessionArtifact = {
  ...STORY_HISTORY_CUTS[0],
  versions: STORY_HISTORY_CUTS,
};

/** Files need no eager byte fetch, so this grid is a deterministic index rather than a mock transport. */
export const STORY_ARTIFACTS: SessionArtifact[] = [
  {
    key: 'i7:0',
    kind: 'file',
    name: 'build.log',
    media: 'LOG',
    mediaType: 'application/octet-stream',
    size: 18240,
    sizeLabel: '18KB',
    turn: 7,
    iterationId: 'i7',
    index: 0,
    version: 1,
  },
  {
    key: 'i5:0',
    kind: 'file',
    name: 'release-notes.txt',
    media: 'TXT',
    mediaType: 'application/octet-stream',
    size: 5400,
    sizeLabel: '5.4KB',
    turn: 5,
    iterationId: 'i5',
    index: 0,
    version: 1,
  },
  {
    key: 'i3:0',
    kind: 'file',
    name: 'vis-companion-debug-2026-05-14.zip',
    media: 'ZIP',
    mediaType: 'application/zip',
    size: 834000,
    sizeLabel: '834KB',
    turn: 3,
    iterationId: 'i3',
    index: 0,
    version: 1,
  },
];

/**
 * A TURN AS A THREAD — the fixture the transcript's own shape is drawn from.
 *
 * Three steps of one answer: two settled, the third still moving. It is written
 * as ITERATIONS rather than as finished markup because `IterationTrace` is what
 * groups them — thinking, program, receipt and activity all come out of these
 * objects — so a change to that grouping shows up here instead of in a screen.
 *
 * The shapes that matter for the picture: step one reasons THEN calls (the band
 * has to cross the line above its own step), step two only calls (a step with no
 * reasoning must not leave a hole in the thread), and step three is unfinished,
 * which is the only state that draws the live marker and runs the line past its
 * own step.
 */
export const STORY_TURN_ITERATIONS: TranscriptIteration[] = [
  {
    id: 'i1',
    position: 1,
    thinking:
      'The rail has to be one line per turn, not one per step, or a turn with six steps reads as six unrelated things.\n\nSo the line belongs to the section and the step marker straddles it. Find where the transcript groups iterations before changing anything.',
    forms: [
      {
        scope: 'python',
        display_code:
          'r = grep({"query": ["TraceSegment", "buildSegments"],\n          "paths": ["src/components"], "context": 4})\nprint(r)',
        duration_ms: 1840,
        result_kind: 'ok',
        activity: ACTIVITY_SETTLED,
      },
    ],
    duration_ms: 6100,
  },
  {
    id: 'i2',
    position: 2,
    forms: [
      {
        scope: 'python',
        display_code:
          'print(patch("src/components/ChatContent.tsx", edits))',
        duration_ms: 940,
        result_kind: 'ok',
        // No activity at all: a bare edit that reported nothing but its own
        // result. The thread still marks it — a step is a step whether or not
        // the engine detected operations inside it — and a finished step must
        // carry a finished record, or the turn shows two open rings.
      },
    ],
    duration_ms: 2300,
  },
  {
    id: 'i3',
    position: 3,
    thinking:
      'Now prove it: the suite first, then the story sheet at both widths.',
    forms: [
      {
        scope: 'python',
        display_code: 'sh = await shell("npm test", cwd=companion)\nprint(sh.logs(-20))',
        activity: ACTIVITY_RUNNING,
      },
    ],
  },
];

/**
 * The same turn once it has landed. Nothing is moving, so every marker is closed
 * and the thread stops at the last step — the state a transcript is READ in, and
 * the one a running-only fixture never shows.
 */
export const STORY_TURN_ITERATIONS_SETTLED: TranscriptIteration[] = STORY_TURN_ITERATIONS.map(
  (iteration) => ({
    ...iteration,
    duration_ms: iteration.duration_ms ?? 3100,
    forms: (iteration.forms ?? []).map((form) => ({
      ...form,
      duration_ms: form.duration_ms ?? 3100,
      result_kind: iteration.id === 'i2' ? 'error' : 'ok',
      // The middle step FAILED and the turn carried on past it; the last one was
      // STOPPED before it finished. A settled sheet where everything worked is
      // the one sheet that proves nothing: the marker for a step that ended
      // badly — and the marker for one that never ended at all — have to be
      // findable from the gutter alone, months later, in a transcript nobody is
      // reading closely, because the row beside them no longer says a word
      // about it.
      activity:
        iteration.id === 'i2'
          ? ACTIVITY_FAILED
          : iteration.id === 'i3'
            ? { ...ACTIVITY_SETTLED, state: 'cancelled' as const }
            : ACTIVITY_SETTLED,
    })),
  }),
);

/**
 * A LONG turn — the one the fold exists for.
 *
 * One turn of a real session held 1,116 steps, and painted whole it measured
 * 107,090 px and 23,806 DOM nodes: 180 screens a reader had to drag through to
 * reach the answer. 120 steps is enough to see what the fold does — the rule,
 * the count standing behind it, and the last steps where the reader left them.
 * Cycled from the settled fixture so the marks alternate and every render of
 * this sheet is the same one.
 */
export const STORY_TURN_ITERATIONS_LONG: TranscriptIteration[] = Array.from(
  { length: 120 },
  (_, index) => {
    const source =
      STORY_TURN_ITERATIONS_SETTLED[
        index % STORY_TURN_ITERATIONS_SETTLED.length
      ];
    return { ...source, id: `long-i${index}`, position: index };
  },
);
/**
 * ONE EXCHANGE — what was asked, and the turn that answered it.
 *
 * The only fixture where the human's bubble and the machine's thread are on
 * screen together, which makes it the only place their two vertical strokes can
 * be compared: they are the transcript's whole structure, and a few pixels of
 * drift between them reads as a wobble rather than as a column.
 */
export const STORY_EXCHANGE_TURN: TranscriptTurn = {
  turn_id: 'turn-exchange',
  request: 'The steps of a turn read as a pile. Make them read as one thread.',
  status: 'completed',
  iterations: STORY_TURN_ITERATIONS_SETTLED,
  content: [
    {
      id: 'exchange-answer',
      type: 'prose',
      markdown:
        'The thread now belongs to the turn, not to the step: one line down the column, a marker where each step landed, and the line stops at the last one. The middle step failed, the last one was stopped, and the turn carried on past both — which the gutter says on its own, in rings, without spending a word of the row on it.',
    },
  ],
  model: 'claude-opus-5',
  provider: 'anthropic',
  duration_ms: 11500,
};

/**
 * THE AXIS WITH SOMETHING ON IT — the same turn, drawn over a real chronology.
 *
 * `STORY_TURN_ITERATIONS` proves the THREAD (a line, its markers, a band that
 * stops at it). This one proves what hangs OFF it: the invocation's margin
 * counting what the iteration cost, the paths a read touched with their kinds,
 * a diff's `+7 −3`, and the two steps that ended badly opening themselves with
 * the head of their output already on the page.
 */
export const STORY_TURN_ITERATIONS_ACTIVITY: TranscriptIteration[] = [
  {
    id: 'i1',
    position: 1,
    thinking:
      'A closed step has to answer the only question it is asked — did this change anything, what did it look at, what did it check — and everything else has to be one chevron away.\n\nSo: counters on the band, and inside it the program first, because it is what produced every row under it.',
    assistant_prose:
      'Counters and rows come out of the same projection, so the band can carry them without a second pass — patching the panel and its story sheet next.',
    forms: [
      {
        scope: 'python',
        display_code:
          'r = grep({"query": ["ActivityPanel", "activityCounters"],\n          "paths": ["src/components"], "context": 4})\nprint(r)',
        duration_ms: 8420,
        result_kind: 'ok',
        activity: ACTIVITY_CHRONOLOGY,
      },
    ],
    duration_ms: 9100,
  },
];

/**
 * EVERY PROVIDER FAILURE THE ENGINE CAN NAME. This is the product taxonomy, not
 * a collection of HTTP examples: no route, no response, an HTTP rejection and a
 * successful HTTP stream that later failed are deliberately separate states.
 */
function storyProviderError(
  kind: string,
  title: string,
  explanation: string,
  nextStep: string,
  extra: Partial<ContentBlock> = {},
): ContentBlock {
  return {
    id: `provider-${kind}`,
    type: 'error',
    code: `provider_${kind}`,
    kind,
    title,
    explanation,
    next_step: nextStep,
    message: `${title}\n\nWHAT HAPPENED: ${explanation}\n\nNEXT STEP: ${nextStep}`,
    retryable: false,
    ...extra,
  };
}

export const STORY_PROVIDER_ERRORS: ContentBlock[] = [
  storyProviderError(
    'unroutable',
    'No provider could take this request',
    'Nothing was sent. The requested route is unavailable or cooling down.',
    'Check the first provider failure, or choose another provider/model.',
    { provider: 'openai-codex' },
  ),
  storyProviderError(
    'transport',
    'Could not reach provider',
    'The connection dropped before any HTTP response came back. The model never ran.',
    'Retry. If it keeps failing, check the connection and provider gateway.',
    { retryable: true },
  ),
  storyProviderError(
    'rate-limit',
    'Provider rate-limited',
    'The provider is throttling new requests.',
    'Wait and retry, or switch provider/model.',
    {
      status: 429,
      provider: 'anthropic',
      request_id: 'req_01JISSUE167',
      retryable: true,
      attempts: [
        {
          provider: 'anthropic',
          model: 'claude-opus-4',
          status: 429,
          reason: 'rate-limit',
        },
      ],
    },
  ),
  storyProviderError(
    'stream-interrupted',
    'Provider stream ended early',
    'HTTP 200 opened a response stream, but it closed before the terminal event arrived.',
    'Retry; the incomplete answer was not accepted as final.',
    { status: 200, provider: 'openai', retryable: true },
  ),
  storyProviderError(
    'file-descriptors-exhausted',
    'Too many files are open',
    'This machine cannot open another provider connection.',
    'Close unused processes or raise the file-descriptor limit, then retry.',
  ),
  storyProviderError(
    'context-overflow',
    'Context window exceeded',
    'The request is larger than this model can accept.',
    'Start a shorter session, remove large attachments, or choose a larger context model.',
    { status: 400, provider: 'openai' },
  ),
  storyProviderError(
    'stream-timeout',
    'Stream went quiet — Vis timed out',
    'The provider stream stayed open but sent no model progress before the deadline.',
    'Retry, or raise the stream timeout for long reasoning turns.',
    { status: 200, provider: 'anthropic', retryable: true },
  ),
  storyProviderError(
    'refusal',
    'Provider declined this request',
    'The model returned a refusal instead of an answer.',
    'Change the request or choose a model that can answer it.',
    { status: 200, provider: 'anthropic' },
  ),
  storyProviderError(
    'empty-content',
    'Provider returned no usable content',
    'The response completed without text, reasoning, tool calls or attachments.',
    'Retry once; if it repeats, switch provider/model.',
    { status: 200, provider: 'openai', retryable: true },
  ),
  storyProviderError(
    'invalid-thinking-signature',
    'Provider rejected the thinking signature',
    'The stream began successfully, then rejected a thinking block carried from this session.',
    'Start a fresh turn without the incompatible thinking history.',
    { status: 200, provider: 'anthropic', request_id: 'req_signature_167' },
  ),
  storyProviderError(
    'tool-schema',
    'Provider rejected a tool schema',
    'One tool declaration uses a schema feature this provider does not accept.',
    'Fix the named tool schema or choose a provider that supports it.',
    { status: 400, provider: 'bedrock' },
  ),
  storyProviderError(
    'output-budget-too-small',
    'Output token budget too small',
    'The configured output budget is below the provider minimum.',
    'Raise max_output_tokens to the stated minimum, then retry.',
    { status: 400, provider: 'openai' },
  ),
  storyProviderError(
    'invalid-request',
    'Provider rejected the request',
    'The provider found an invalid request field: service_tier.',
    'Correct the request configuration before retrying.',
    { status: 400, provider: 'openai', request_id: 'req_invalid_167' },
  ),
  storyProviderError(
    'auth',
    'Provider authentication failed',
    'The provider rejected the configured credentials.',
    'Re-authenticate or fix the API key, then retry.',
    { status: 401, provider: 'anthropic' },
  ),
  storyProviderError(
    'quota-exhausted',
    'Provider quota exhausted',
    'This account has no usable quota or credits.',
    'Check the plan and usage limits, add credits, or switch provider.',
    { status: 402, provider: 'anthropic' },
  ),
  storyProviderError(
    'upstream-timeout',
    'Provider timed out upstream',
    'The gateway answered, but its model upstream did not finish in time.',
    'Retry, or switch provider/model if the timeout persists.',
    { status: 504, provider: 'bedrock', retryable: true },
  ),
  storyProviderError(
    'gateway-unavailable',
    'Provider gateway unavailable',
    'The gateway or its upstream service is temporarily unavailable.',
    'Retry after a short wait, or switch provider/model.',
    { status: 502, provider: 'zai-coding-plan', retryable: true },
  ),
  storyProviderError(
    'model-unavailable',
    'Provider model unavailable',
    'The requested model is unavailable on this provider.',
    'Choose another model or provider.',
    { status: 404, provider: 'openai' },
  ),
  storyProviderError(
    'resource-mismatch',
    'Provider resource does not match',
    'The requested item belongs to a different provider resource.',
    'Use the resource that created the item; do not retry this request unchanged.',
    { status: 400, provider: 'azure-openai' },
  ),
  storyProviderError(
    'generic',
    'Provider unavailable',
    'The provider call failed without a more specific verdict.',
    'Retry once, then include the diagnostics when reporting a persistent failure.',
    {
      status: 500,
      provider: 'gateway.example.com',
      body: '<html>upstream service unavailable</html>',
      retryable: true,
    },
  ),
];


/**
 * THE FLEET A LIST STORY READS, AND IT IS A GATEWAY, NOT A PROP.
 *
 * `SessionsScreen` builds its own `GatewayClient` per paired machine, so the one seam a
 * story can hold is `fetch` — the same seam the screen's own tests hold
 * (`screens/sessions-screen-harness.tsx`). Everything above it is production: the project
 * overview, the keyset window a project page is cut from, and the rows themselves.
 */
export interface StoryProject {
  root: string;
  /** What the gateway named the folder, `''` when nothing did. */
  name: string;
  projectId: string;
  rows: Session[];
}

/**
 * The list prints WHEN a row last moved (`timeLabel`), against the reader's own clock, so
 * a fleet frozen in 2030 photographs every row as a full date with a year — a shape the
 * product only ever shows for history a year old. The fixture is therefore anchored to
 * the clock the frame is taken on and offset in fixed minutes: derived, never random,
 * and it paints the relative labels the screen really ships.
 */
const STORY_FLEET_NOW = Date.now();

function fleetRow(
  root: string,
  id: string,
  title: string,
  minutesAgo: number,
  turns: number,
  state: 'idle' | 'running' | 'awaiting' = 'idle',
): Session {
  const at = new Date(STORY_FLEET_NOW - minutesAgo * 60_000).toISOString();
  return {
    id,
    title,
    status: state === 'idle' ? 'idle' : 'running',
    live: state !== 'idle',
    current_turn_id: state === 'idle' ? null : `turn-${id}`,
    is_awaiting_input: state === 'awaiting',
    favorite_rank: null,
    turn_count: turns,
    created_at: at,
    modified_at: at,
    server_time_ms: STORY_FLEET_NOW,
    workspace: { root },
  };
}

/** Four checkouts on one machine — the shape the list is read in every day. */
export const STORY_FLEET_PROJECTS: StoryProject[] = [
  {
    root: '~/vis',
    name: 'vis',
    projectId: 'project-vis',
    rows: [
      fleetRow('~/vis', 'fd3c03f9', STORY_SESSION.title, 1, 61, 'running'),
      fleetRow('~/vis', '41d78df4', 'Scrolling up in a long session is 180 screens', 14, 118, 'awaiting'),
      fleetRow('~/vis', '9c1e77ab', 'Keep the running placeholder out of the render window', 52, 24),
      fleetRow('~/vis', '2b40f6c1', 'Gateway wire: encode NaN before the transport throws', 96, 9),
      fleetRow('~/vis', '77aa1e30', 'Native reachability for the YAML reader', 180, 33),
      fleetRow('~/vis', '0d55c2e8', 'Delimiter repair stays syntax-only', 240, 6),
      fleetRow('~/vis', 'b6f9004d', 'Human input: one vocabulary, five checked seams', 320, 41),
      fleetRow('~/vis', '3ee81b57', 'Sandbox shim handles leak without the registry', 470, 15),
      fleetRow('~/vis', 'a1c4d902', 'Lazytest: clojure.test is silently undiscovered', 690, 12),
      fleetRow('~/vis', '5f7b3c11', 'Audit inventory gate runs offline', 900, 4),
      fleetRow('~/vis', 'c80a6e24', 'Docs catalog and what doc() answers', 1_100, 27),
      fleetRow('~/vis', 'e2d5f087', 'Commit convention cutoff moved past the release', 1_450, 8),
    ],
  },
  {
    root: '~/svar',
    name: 'svar',
    projectId: 'project-svar',
    rows: [
      fleetRow('~/svar', '6ba99088', 'Routing falls back to the coding plan on 429', 38, 31, 'running'),
      fleetRow('~/svar', 'd41ab7c5', 'Structured output refuses a partial object', 210, 17),
      fleetRow('~/svar', '8c02e6f1', 'Provider cache metrics are transport-independent', 640, 22),
      fleetRow('~/svar', 'f93b18a0', 'Retry budget per route, not per call', 1_020, 5),
    ],
  },
  {
    root: '~/work/tools/spel',
    name: 'spel',
    projectId: 'project-spel',
    rows: [
      fleetRow('~/work/tools/spel', '4a7c9d63', 'Content boundaries in the snapshot output', 155, 14),
      fleetRow('~/work/tools/spel', '17e6b02f', 'Wait for copy the story owns, never the shell', 480, 9),
      fleetRow('~/work/tools/spel', 'ba3f5e41', 'Device emulation before the first navigation', 1_320, 3),
    ],
  },
  {
    root: '~/infrastructure',
    name: 'infrastructure',
    projectId: 'project-infra',
    rows: [
      fleetRow('~/infrastructure', '2f8d47b9', 'Rotate the relay signing key', 2_600, 11),
      fleetRow('~/infrastructure', 'cc51a30e', 'Ingress health check answers before the unit starts', 4_100, 2),
    ],
  },
];

/** The machine those projects live on, in the shape the screen takes its fleet in. */
export const STORY_FLEET_CONNS: GatewayConn[] = [STORY_GATEWAYS[0]];

/** A cursor NAMES a row, the way the gateway's own keyset does (`state/->session-cursor`). */
const fleetCursor = (row: Session) => `2:0:${row.id}`;

/**
 * The gateway those projects are served by: `/v1/projects/overview` and the one
 * `/v1/sessions` window, cut by `root=`, `limit=` and `after=` exactly as
 * `state/list-sessions-page` cuts it. Deterministic, and it never touches the network.
 */
export function storyFleetFetch(
  projects: StoryProject[] = STORY_FLEET_PROJECTS,
): typeof fetch {
  const all = projects.flatMap((project) => project.rows);
  const isLive = (row: Session) => row.live === true;
  const isAwaiting = (row: Session) => row.is_awaiting_input === true;
  const overview = {
    projects: projects.map((project) => ({
      root: project.root,
      project_id: project.projectId,
      name: project.name,
      session_count: project.rows.length,
      live_count: project.rows.filter(isLive).length,
      awaiting_count: project.rows.filter(isAwaiting).length,
      last_activity_ms: STORY_FLEET_NOW,
    })),
    project_count: projects.length,
    session_count: all.length,
    live_count: all.filter(isLive).length,
    awaiting_count: all.filter(isAwaiting).length,
    server_time_ms: STORY_FLEET_NOW,
  };
  const answer = (body: unknown) =>
    new Response(JSON.stringify(body), {
      status: 200,
      headers: { 'Content-Type': 'application/json', ETag: '"story-fleet"' },
    });
  return (async (input: RequestInfo | URL, init?: RequestInit) => {
    const href =
      typeof input === 'string' ? input : input instanceof URL ? input.href : input.url;
    const url = new URL(href);
    if (url.pathname === '/v1/projects/overview') return answer(overview);
    if (url.pathname === '/v1/sessions' && (init?.method ?? 'GET') === 'GET') {
      const root = url.searchParams.get('root');
      const listed = root
        ? (projects.find((project) => project.root === root)?.rows ?? [])
        : all;
      const limit = Number(url.searchParams.get('limit') ?? listed.length) || listed.length;
      const after = url.searchParams.get('after');
      const from = after
        ? listed.findIndex((row) => fleetCursor(row) === after) + 1
        : 0;
      const window = after && from === 0 ? [] : listed.slice(from, from + limit);
      const last = window[window.length - 1];
      const hasMore = from + window.length < listed.length;
      return answer({
        sessions: window,
        total: listed.length,
        has_more: hasMore,
        next_cursor: hasMore && last ? fleetCursor(last) : null,
        ...(after || root ? {} : { overview }),
      });
    }
    return answer({});
  }) as typeof fetch;
}

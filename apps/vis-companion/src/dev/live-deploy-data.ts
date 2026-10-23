import type { LiveLogNode, LiveNode, LiveTone, LiveView } from '../lib/live-view';

/**
 * Fixtures for the deploy dashboard recipe (#417). Both deploy views describe the
 * SAME synthetic deploy: the first in the shape the issue reports, the second split
 * into the native nodes the issue asks producers to use. Every value is fixed, so a
 * frame draws the same picture on every run.
 */

const LOG_LINES = 200;
const ERROR_LINES = new Set([121, 164]);
const WARN_LINES = new Set([88, 142]);

function logLine(index: number): string {
  const number = index + 1;
  const clock = `12:${String(20 + Math.floor(number / 20)).padStart(2, '0')}:${String((number * 7) % 60).padStart(2, '0')}`;
  if (ERROR_LINES.has(number)) {
    return `[${clock}] ERROR integration: scheduler-2 readiness probe failed (HTTP 503 from /healthz)`;
  }
  if (WARN_LINES.has(number)) {
    return `[${clock}] WARN  integration: worker-${number % 6} restarted once, back off 10s`;
  }
  return `[${clock}] INFO  integration: pod api-${number % 24} ready check ${number} ok`;
}

function logTone(index: number): LiveTone | null {
  if (ERROR_LINES.has(index + 1)) return 'error';
  if (WARN_LINES.has(index + 1)) return 'warn';
  return null;
}

function deployLog(expanded: boolean): LiveLogNode {
  return {
    id: 'output',
    type: 'log',
    label: 'Output',
    lines: Array.from({ length: LOG_LINES }, (_, index) => logLine(index)),
    line_tones: Array.from({ length: LOG_LINES }, (_, index) => logTone(index)),
    window_lines: 2000,
    total_lines: LOG_LINES,
    default_expanded: expanded,
  };
}

const DESCRIPTION =
  'Stopping this view ends the watch only. Builds that already started keep running.';

/** The issue's reproduction: one long status sentence, a timing paragraph, an open log. */
export const DEPLOY_AS_REPORTED: LiveView = {
  id: 'deploy-reported',
  title: 'Deploy all · DEV',
  description: DESCRIPTION,
  seq: 1,
  nodes: [
    {
      id: 'status',
      type: 'status',
      tone: 'running',
      text: 'Deploying: phase 3 of 5 (integration) · job platform/services/deploy-all-dev #1482 · result pending · elapsed 12m 40s · ETA 7m · params region=eu-central-1, strategy=rolling, dry_run=false',
    },
    { id: 'timing', type: 'paragraph', text: 'Elapsed 12m 40s. Estimated completion in about 7m.' },
    deployLog(true),
  ],
};

/** The same deploy, composed from separate native nodes (#417 item 5). */
export const DEPLOY_RECOMPOSED_NODES: LiveNode[] = [
  {
    id: 'status',
    type: 'status',
    tone: 'running',
    text: 'Deploying to DEV',
    detail: 'Phase 3 of 5: integration tests on the canary pods',
  },
  {
    id: 'overview',
    type: 'group',
    direction: 'row',
    fields: [
      { id: 'phases', type: 'progress', label: 'Phases', done: 3, total: 5 },
      {
        id: 'timing',
        type: 'stat',
        label: 'Timing',
        stats: [
          { id: 'elapsed', label: 'Elapsed', value_text: '12m 40s', tone: 'idle' },
          { id: 'eta', label: 'Expected in', value_text: 'about 7m', tone: 'idle' },
        ],
      },
    ],
  },
  {
    id: 'build',
    type: 'stat',
    label: 'Build',
    stats: [
      { id: 'job', label: 'Job', value_text: 'platform/services/deploy-all-dev', tone: 'idle' },
      { id: 'number', label: 'Build', value_text: '#1482', tone: 'idle' },
      { id: 'region', label: 'Region', value_text: 'eu-central-1', tone: 'idle' },
      { id: 'strategy', label: 'Strategy', value_text: 'rolling', tone: 'idle' },
    ],
  },
  {
    id: 'steps',
    type: 'steps',
    label: 'Phases',
    steps: [
      { id: 'build', label: 'Build images', tone: 'ok', value: '2m 10s' },
      { id: 'unit', label: 'Unit tests', tone: 'ok', value: '3m 02s' },
      {
        id: 'integration',
        label: 'Integration tests',
        tone: 'running',
        value: '7m 28s',
        detail: '18 of 24 pods ready',
      },
      { id: 'canary', label: 'Canary', tone: 'idle' },
      { id: 'promote', label: 'Promote', tone: 'idle' },
    ],
  },
  {
    id: 'services',
    type: 'table',
    label: 'Services',
    columns: [
      { id: 'service', label: 'Service', align: 'left' },
      { id: 'version', label: 'Version', align: 'left' },
      { id: 'pods', label: 'Pods', align: 'right' },
    ],
    rows: [
      { id: 'api', cells: ['api', '4.12.0', '8/8'], tone: 'ok' },
      { id: 'worker', cells: ['worker', '4.12.0', '6/8'], tone: 'running' },
      { id: 'scheduler', cells: ['scheduler', '4.12.0', '1/2'], tone: 'warn' },
      { id: 'gateway', cells: ['gateway', '4.11.3', '0/6'], tone: 'idle' },
    ],
    max_rows: 5000,
    order: 'insertion',
    is_selectable: false,
    selected_ids: [],
    groups: [],
  },
  {
    id: 'links',
    type: 'link',
    label: 'Links',
    links: [
      {
        id: 'run',
        label: 'Build #1482',
        target: 'https://gateway.example.com/ci/deploy-all-dev/1482',
        target_kind: 'url',
      },
      {
        id: 'report',
        label: 'deploy-report.md',
        target: 'deploy-report.md',
        target_kind: 'attachment',
      },
      { id: 'values', label: 'deploy/dev/values.yaml', target: 'deploy/dev/values.yaml', target_kind: 'path' },
    ],
  },
  deployLog(false),
];

export const DEPLOY_RECOMPOSED: LiveView = {
  id: 'deploy-recomposed',
  title: 'Deploy all to DEV',
  description: DESCRIPTION,
  seq: 1,
  nodes: DEPLOY_RECOMPOSED_NODES,
};

/** The same deploy, settled with a failure, for the verdict comparison. */
export const DEPLOY_FAILED: LiveView = {
  ...DEPLOY_RECOMPOSED,
  id: 'deploy-failed',
  nodes: [
    {
      id: 'status',
      type: 'status',
      tone: 'error',
      text: 'Deploy to DEV failed',
      detail: 'Integration tests: scheduler-2 never became ready',
    },
    ...DEPLOY_RECOMPOSED_NODES.slice(1),
  ],
};

/** One node as a whole view, so the current panel can draw it on its own. */
export function single(node: LiveNode, title = 'Deploy all to DEV'): LiveView {
  return { id: `single-${node.id}`, title, seq: 1, nodes: [node] };
}

/** A node of the recomposed deploy, by id. */
export function deployNode(id: string): LiveNode {
  const found = DEPLOY_RECOMPOSED_NODES.find((node) => node.id === id);
  if (!found) throw new Error(`No deploy node ${id}`);
  return found;
}

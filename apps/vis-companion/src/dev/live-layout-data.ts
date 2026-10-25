/**
 * Live views that exercise every layout direction, in the wire shape the gateway
 * sends. `LIVE_LAYOUT_VIEW` is a release dashboard: a capped grid of cards, a
 * wrap row of actions, a row of inputs with a submit button and a column of
 * results. `liveLargeView` builds a view big enough to measure render cost.
 */

const card = (id: string, title: string, text: string, tone: string, meta: string) => ({
  id,
  type: 'card',
  title,
  text,
  tone,
  meta,
  is_selectable: true,
  clicks: 0,
});

const button = (id: string, label: string, action?: 'submit') => ({
  id,
  type: 'button',
  label,
  is_disabled: false,
  clicks: 0,
  ...(action ? { action } : {}),
});

export const LIVE_LAYOUT_VIEW: Record<string, unknown> = {
  id: 'release-board',
  title: 'Release board',
  description: 'Three services wait for the release check.',
  seq: 1,
  nodes: [
    {
      id: 'summary',
      type: 'stat',
      stats: [
        { id: 'ready', label: 'Ready', value_text: '4', tone: 'ok' },
        { id: 'running', label: 'Running', value_text: '1', tone: 'running' },
        { id: 'blocked', label: 'Blocked', value_text: '1', tone: 'warn' },
      ],
    },
    {
      id: 'services',
      type: 'group',
      direction: 'grid',
      max_columns: 3,
      label: 'Services',
      children: [
        card('svc-api', 'API', 'Tests passed', 'ok', 'v2.4.1'),
        card('svc-web', 'Web', 'Bundle builds', 'running', 'v2.4.1'),
        card('svc-worker', 'Worker', 'Needs a migration review', 'warn', 'v2.4.0'),
        card('svc-search', 'Search', 'Tests passed', 'ok', 'v1.9.3'),
        card('svc-billing', 'Billing', 'Tests passed', 'ok', 'v3.0.2'),
      ],
    },
    {
      id: 'actions',
      type: 'group',
      direction: 'wrap',
      children: [
        button('approve', 'Approve release'),
        button('rerun', 'Run checks again'),
        button('hold', 'Hold'),
      ],
    },
    {
      id: 'target',
      type: 'group',
      direction: 'row',
      label: 'Target',
      children: [
        {
          id: 'region',
          name: 'region',
          type: 'select',
          label: 'Region',
          is_required: false,
          is_secret: false,
          options: [
            { value: 'eu', label: 'EU' },
            { value: 'us', label: 'US' },
          ],
          value: 'eu',
        },
        {
          id: 'window',
          name: 'window',
          type: 'plaintext',
          label: 'Window',
          is_required: false,
          is_secret: false,
          value: '18:00',
        },
        button('schedule', 'Schedule', 'submit'),
      ],
    },
    {
      id: 'results',
      type: 'group',
      direction: 'column',
      children: [
        { id: 'now', type: 'status', text: 'Waiting for approval', tone: 'running' },
        { id: 'progress', type: 'progress', label: 'Checks', done: 5, total: 6 },
      ],
    },
  ],
};

/** A view with `rows` table rows, `lines` log lines and `cards` grid cards. */
export function liveLargeView(rows: number, lines: number, cards: number): Record<string, unknown> {
  const tones = ['ok', 'running', 'warn', 'error', 'idle'];
  return {
    id: 'large-view',
    title: 'Fleet scan',
    seq: 1,
    nodes: [
      { id: 'now', type: 'status', text: `Scanned ${rows} hosts`, tone: 'running' },
      {
        id: 'cards',
        type: 'group',
        direction: 'grid',
        label: 'Hosts',
        children: Array.from({ length: cards }, (_, at) =>
          card(`card-${at}`, `Host ${at + 1}`, 'Reachable', tones[at % tones.length], `10.0.0.${at % 250}`),
        ),
      },
      {
        id: 'hosts',
        type: 'table',
        columns: [
          { id: 'host', label: 'Host', align: 'left' },
          { id: 'state', label: 'State', align: 'left' },
          { id: 'ms', label: 'Latency', align: 'right' },
        ],
        rows: Array.from({ length: rows }, (_, at) => ({
          id: `row-${at}`,
          cells: [`host-${at}.example.com`, at % 7 === 0 ? 'Slow' : 'Up', `${(at * 37) % 400} ms`],
          tone: at % 7 === 0 ? 'warn' : 'ok',
        })),
        max_rows: Math.max(rows, 1),
        order: 'insertion',
        is_selectable: true,
        selected_ids: [],
      },
      {
        id: 'log',
        type: 'log',
        label: 'Scan output',
        window_lines: 2000,
        total_lines: lines,
        lines: Array.from({ length: Math.min(lines, 2000) }, (_, at) => `probe ${at}: host-${at % rows}.example.com answered`),
      },
    ],
  };
}

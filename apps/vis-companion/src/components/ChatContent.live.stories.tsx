import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';
import { IterationTrace } from './ChatContent';
import type { ActivityProjection } from '../lib/activity';
import { STORY_INERT_CLIENT } from '../dev/story-data';
import type { LiveView } from '../lib/live-view';

const activity: ActivityProjection = {
  history: {
    id: '11111111-1111-1111-1111-111111111111',
    revision: 1,
    total: 1,
    after: 0,
    next_after: null,
  },
  state: 'running',
  counts: { running: 1, succeeded: 0, failed: 0, cancelled: 0 },
  omitted: { rows: 0, by_classification: {} },
  rows: [
    {
      id: '22222222-2222-2222-2222-222222222222',
      sequence: 1,
      operation: 'jenkins.monitor',
      presenter: 'generic',
      signal: 'observation',
      state: 'running',
      summary: 'Build pool',
      resources: [],
      evidence: [],
      presentation: { headline: 'Monitor builds', summary: 'Build pool', content: [] },
    },
  ],
};
const view: LiveView = {
  id: 'build-pool',
  title: 'Jenkins build pool',
  description: 'Checking the build before continuing',
  seq: 1,
  owner: { invocation_id: activity.rows[0].id, activity_id: activity.history!.id },
  nodes: [
    { id: 'status', type: 'status', text: 'Waiting for integration tests', tone: 'running' },
    {
      id: 'log',
      type: 'log',
      label: 'Build log',
      lines: ['Compile completed', 'Integration tests running'],
      total_lines: 2,
      window_lines: 2000,
    },
  ],
};
const savedView: LiveView = {
  ...view,
  nodes: [{ id: 'status', type: 'status', text: 'Integration tests passed', tone: 'ok' }],
};
const savedRecord = [
  JSON.stringify({ kind: 'open', view }),
  JSON.stringify({
    kind: 'close',
    result: { reason: 'completed', is_completed: true, view: savedView },
  }),
].join('\n');
const client = {
  retainAttachment: fn(() => () => {}),
  attachmentUrl: fn(
    async () => `data:application/vnd.vis.live+ndjson,${encodeURIComponent(savedRecord)}`,
  ),
  viewAction: fn(async () => ({ is_accepted: true })),
  liveViewLog: fn(async () => ({
    node_id: 'log',
    from: 0,
    total: 2,
    lines: ['Compile completed', 'Integration tests running'],
  })),
} as unknown as typeof STORY_INERT_CLIENT;
const meta = {
  title: 'Transcript/Activity live views',
  component: IterationTrace,
  parameters: { layout: 'padded' },
  args: {
    iterations: [{ forms: [{ source: 'jenkins.monitor()', activity }] }],
    liveViews: [view],
    client,
    sid: 'story-session',
    whole: true,
    showCode: false,
  },
} satisfies Meta<typeof IterationTrace>;
export default meta;
type Story = StoryObj<typeof meta>;

// ACTIVITY stands outside the live RUN, which is a band of its own on the shared surface.
async function expectLiveFrame(element: Element) {
  const frame = element.closest<HTMLElement>('[data-execution-run]')!;
  const group = element.closest<HTMLElement>('[data-execution-group]')!;
  const activity = group.querySelector<HTMLElement>('[data-execution-activity]')!;
  await expect(frame).not.toBeNull();
  await expect(frame.contains(activity)).toBe(false);
  await expect(activity.closest('.border')).toBeNull();
  // Regression, user report (screenshot): the run stood in a bordered box on a rhythm no other
  // band shared. CODE, ACTIVITY and RUN are one column of unframed bands, the way the terminal
  // prints them — the run owns no frame and no gap of its own.
  await expect(frame.getBoundingClientRect().top).toBe(activity.getBoundingClientRect().bottom);
  const style = getComputedStyle(frame);
  await expect(style.marginTop).toBe('0px');
  await expect(style.paddingTop).toBe('0px');
  for (const side of ['top', 'right', 'bottom', 'left']) {
    await expect(style.getPropertyValue(`border-${side}-width`)).toBe('0px');
    await expect(getComputedStyle(group).getPropertyValue(`border-${side}-width`)).toBe('0px');
  }
  // The band keeps the card's own column: it begins and ends where the activity beside it does,
  // inside the card's padding — it used to bleed through that padding to the edges.
  const band = frame.getBoundingClientRect();
  const beside = activity.getBoundingClientRect();
  await expect(band.left).toBeCloseTo(beside.left, 0);
  await expect(band.right).toBeCloseTo(beside.right, 0);
  const card = group.getBoundingClientRect();
  await expect(band.left).toBeGreaterThan(card.left);
  await expect(band.right).toBeLessThan(card.right);
  await expect(band.left).toBeGreaterThanOrEqual(0);
  await expect(band.right).toBeLessThanOrEqual(innerWidth);
  return frame;
}

/** Regression #222: production execution surface and live actions, without a gateway. */
export const Running: Story = {
  globals: { viewport: { value: 'desktop', isRotated: false } },
  play: async ({ canvas }) => {
    const title = canvas.getByText('Jenkins build pool');
    const liveFrame = await expectLiveFrame(title);
    const activitySurface = title.closest<HTMLElement>('[data-execution-group]')!;
    await expect(title.closest('.border')).toBeNull();
    const controls = within(activitySurface);
    await expect(controls.getByText('ACTIVITY')).toBeInTheDocument();
    await expect(controls.getByText('RUN')).toBeInTheDocument();
    await expect(title.closest('[data-execution-activity]')).toBeNull();
    await userEvent.click(controls.getByRole('button', { name: 'Expand Activity' }));
    await userEvent.click(controls.getByRole('button', { name: 'Collapse Activity' }));
    await expect(title).toBeVisible();
    await userEvent.click(controls.getByRole('button', { name: 'Expand Activity' }));
    // The transcript STATES the run: its newest status, and no log painted in place.
    await expect(controls.getByText('Waiting for integration tests')).toBeVisible();
    await expect(controls.queryByRole('button', { name: 'Build log' })).toBeNull();
    // The row ends as one line of type — INTERRUPT | LIVE. The verb rides the run's own band,
    // centred in the row and wearing no face at all, and both words stand in the band's full
    // weight with a printed rule between them: the user reported LIVE reading as light type
    // beside a stop that filled the row.
    const interrupt = controls.getByRole('button', { name: 'Interrupt' });
    const band = liveFrame.querySelector('header')!;
    const row = band.getBoundingClientRect();
    const key = interrupt.getBoundingClientRect();
    await expect(key.top - row.top).toBeCloseTo(row.bottom - key.bottom, 0);
    await expect(key.height).toBeLessThan(row.height);
    const verb = getComputedStyle(interrupt);
    await expect(Number(verb.fontWeight)).toBeGreaterThanOrEqual(700);
    await expect(verb.backgroundColor).toBe('rgba(0, 0, 0, 0)');
    await expect(verb.paddingLeft).toBe('0px');
    await expect(band.textContent).toContain('|');
    const live = controls.getByText('LIVE');
    await expect(Number(getComputedStyle(live).fontWeight)).toBeGreaterThanOrEqual(700);
    const state = live.getBoundingClientRect();
    await expect(state.left).toBeGreaterThanOrEqual(key.right);
    await expect(state.right).toBeCloseTo(row.right, 0);
    await userEvent.click(interrupt);
    await expect(
      controls.getByRole('textbox', { name: 'Why are you stopping Jenkins build pool?' }),
    ).toBeInTheDocument();
    await userEvent.click(controls.getByRole('button', { name: 'Keep watching' }));
    const launch = controls.getByRole('button', {
      name: 'Open run Jenkins build pool',
    });
    await expect(launch).not.toHaveAttribute('aria-expanded');
    await expect(launch.querySelector('svg')).toBeNull();
    await userEvent.click(launch);
    const page = within(document.body);
    // The picture belongs to the run's own screen: the log opens there, and leaves with it.
    await userEvent.click(page.getByRole('button', { name: 'Build log' }));
    await expect(page.getByText(/Compile completed/)).toBeInTheDocument();
    await userEvent.click(page.getByRole('button', { name: 'Build log' }));
    await expect(page.queryByText('Compile completed')).not.toBeInTheDocument();
    await userEvent.click(page.getByRole('button', { name: 'Close Jenkins build pool' }));
    await expect(title).toBeVisible();
  },
};

export const RunningPhone: Story = {
  ...Running,
  globals: { viewport: { value: 'phone', isRotated: false } },
};

export const RunningTablet: Story = {
  ...Running,
  globals: { viewport: { value: 'tablet', isRotated: false } },
};

// Regression, user report (screenshot): the live run sat in a bordered box on a rhythm of its
// own. The three bands of an execution are one column — the step from ACTIVITY down to RUN is
// the step from CODE down to ACTIVITY.
export const BandRhythm: Story = {
  args: { showCode: true },
  globals: { viewport: { value: 'desktop', isRotated: false } },
  play: async ({ canvas }) => {
    const code = canvas.getByText('CODE').getBoundingClientRect();
    const activity = canvas.getByText('ACTIVITY').getBoundingClientRect();
    const run = canvas.getByText('RUN').getBoundingClientRect();
    await expect(run.top - activity.bottom).toBeGreaterThan(0);
    await expect(run.top - activity.bottom).toBeCloseTo(activity.top - code.bottom, 0);
    await expect(activity.left).toBeCloseTo(code.left, 0);
    await expect(run.left).toBeCloseTo(activity.left, 0);
  },
};

export const Unmatched: Story = { args: { liveViews: [{ ...view, owner: undefined }] } };
export const Settled: Story = {
  args: {
    liveViews: [],
    iterations: [
      {
        id: 'story-iteration',
        forms: [
          {
            source: 'jenkins.monitor()',
            activity: {
              ...activity,
              state: 'succeeded',
              counts: { running: 0, succeeded: 1, failed: 0, cancelled: 0 },
              rows: activity.rows.map((row) => ({ ...row, state: 'succeeded' })),
            },
          },
        ],
        attachments: [
          {
            index: 0,
            iteration_id: 'story-iteration',
            filename: 'Jenkins build pool.live.ndjson',
            media_type: 'application/vnd.vis.live+ndjson',
            owner: view.owner,
          },
        ],
      },
    ],
  },
  play: async ({ canvas }) => {
    const run = canvas.getByRole('button', { name: 'Open run Jenkins build pool' });
    const group = run.closest('[data-execution-group]')!;
    const band = group.querySelector('[data-execution-activity]')!;
    // Regression: a settled RUN keeps that same rhythm, with no gap of its own either.
    await expect(run.getBoundingClientRect().top).toBe(band.getBoundingClientRect().bottom);
    await expect(group).not.toHaveClass('border');
    await expect(run.closest('[data-execution-activity]')).toBeNull();
    await expect(run.closest('.border')).toBeNull();
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    await userEvent.click(canvas.getByRole('button', { name: 'Collapse Activity' }));
    await expect(run).toBeVisible();
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    await userEvent.click(run);
    const page = within(document.body);
    await expect(await page.findByText('Integration tests passed')).toBeVisible();
    await expect(page.queryByRole('button', { name: 'Interrupt' })).toBeNull();
    await userEvent.click(page.getByRole('button', { name: 'Close Jenkins build pool' }));
    await expect(run).toBeVisible();
  },
};

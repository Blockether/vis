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

// ACTIVITY stands outside the live RUN, which is a bordered box of its own inside the card.
async function expectLiveFrame(element: Element) {
  const frame = element.closest<HTMLElement>('[data-execution-run]')!;
  const group = element.closest<HTMLElement>('[data-execution-group]')!;
  const activity = group.querySelector<HTMLElement>('[data-execution-activity]')!;
  await expect(frame).not.toBeNull();
  await expect(frame.contains(activity)).toBe(false);
  await expect(activity.closest('.border')).toBeNull();
  // Regression: CODE, ACTIVITY and RUN keep one rhythm down the card — the run owns no gap
  // of its own, so its top border is all that sits between it and the activity.
  await expect(frame.getBoundingClientRect().top).toBe(activity.getBoundingClientRect().bottom);
  const style = getComputedStyle(frame);
  await expect(style.marginTop).toBe('0px');
  await expect(style.paddingTop).toBe('0px');
  for (const side of ['top', 'right', 'bottom', 'left']) {
    await expect(style.getPropertyValue(`border-${side}-width`)).toBe('1px');
    await expect(getComputedStyle(group).getPropertyValue(`border-${side}-width`)).toBe('0px');
  }
  await expect(frame).toHaveClass('border-dialog-hint');
  await expect(style.borderRadius).toBe('0px');
  // The box stands INSIDE the card's own padding — it used to bleed through it to the edges.
  const card = group.getBoundingClientRect();
  await expect(frame.getBoundingClientRect().left).toBeGreaterThan(card.left);
  await expect(frame.getBoundingClientRect().right).toBeLessThan(card.right);
  await expect(frame.getBoundingClientRect().left).toBeGreaterThanOrEqual(0);
  await expect(frame.getBoundingClientRect().right).toBeLessThanOrEqual(innerWidth);
  return frame;
}

/** Regression #222: production execution surface and live actions, without a gateway. */
export const Running: Story = {
  globals: { viewport: { value: 'desktop', isRotated: false } },
  play: async ({ canvas }) => {
    const title = canvas.getByText('Jenkins build pool');
    const liveFrame = await expectLiveFrame(title);
    const activitySurface = title.closest<HTMLElement>('[data-execution-group]')!;
    await expect(title.closest('.border')).toBe(liveFrame);
    const controls = within(activitySurface);
    await expect(controls.getByText('ACTIVITY')).toBeInTheDocument();
    await expect(controls.getByText('RUN')).toBeInTheDocument();
    await expect(title.closest('[data-execution-activity]')).toBeNull();
    await userEvent.click(controls.getByRole('button', { name: 'Expand Activity' }));
    await userEvent.click(controls.getByRole('button', { name: 'Collapse Activity' }));
    await expect(title).toBeVisible();
    await userEvent.click(controls.getByRole('button', { name: 'Expand Activity' }));
    await userEvent.click(controls.getByRole('button', { name: 'Build log' }));
    await expect(controls.getByText(/Compile completed/)).toBeInTheDocument();
    await userEvent.click(controls.getByRole('button', { name: 'Build log' }));
    await expect(controls.queryByText('Compile completed')).not.toBeInTheDocument();
    // The interrupt used to sit flush against the run's top border, one hairline from
    // the activity above it; it needs air of its own before the thumb reaches for it.
    const interrupt = controls.getByRole('button', { name: 'Interrupt' });
    await expect(
      interrupt.getBoundingClientRect().top - liveFrame.getBoundingClientRect().top,
    ).toBeGreaterThanOrEqual(6);
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

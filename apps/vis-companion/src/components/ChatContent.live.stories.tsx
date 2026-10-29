import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';
import { IterationTrace } from './ChatContent';
import type { ActivityProjection } from '../lib/activity';
import { STORY_INERT_CLIENT } from '../dev/story-data';
import { openStepDigests } from '../dev/story-steps';
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
  title: 'Session/Live View/In transcript',
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
  return frame;
}

/** Regression #222: production execution surface and live actions, without a gateway. */
export const Running: Story = {
  globals: { viewport: { value: 'desktop', isRotated: false } },
  play: async ({ canvas, canvasElement }) => {
    // A closed digest folds the live run into one control on its row, and it opens the run.
    const control = canvas.getByRole('button', { name: 'Open running live view: Jenkins build pool' });
    await expect(canvas.queryByRole('button', { name: 'Open run Jenkins build pool' })).toBeNull();
    await userEvent.click(control);
    const opened = within(document.body);
    await expect(opened.getByRole('button', { name: 'Build log' })).toBeInTheDocument();
    await userEvent.click(opened.getByRole('button', { name: 'Close Jenkins build pool' }));
    await openStepDigests(canvasElement);
    // Regression, user report: expanding the steps keeps their live control in the header.
    await expect(control).toBeVisible();
    await userEvent.click(control);
    await expect(opened.getAllByRole('button', { name: 'Close Jenkins build pool' })).toHaveLength(1);
    await userEvent.click(opened.getByRole('button', { name: 'Close Jenkins build pool' }));
    const title = canvas.getByText('Jenkins build pool');
    await expectLiveFrame(title);
    const activitySurface = title.closest<HTMLElement>('[data-execution-group]')!;
    const controls = within(activitySurface);
    await userEvent.click(controls.getByRole('button', { name: 'Expand Activity' }));
    await userEvent.click(controls.getByRole('button', { name: 'Collapse Activity' }));
    await expect(title).toBeVisible();
    await userEvent.click(controls.getByRole('button', { name: 'Expand Activity' }));
    // The transcript STATES the run: its newest status, and no log painted in place.
    await expect(controls.getByText('Waiting for integration tests')).toBeVisible();
    await expect(controls.queryByRole('button', { name: 'Build log' })).toBeNull();
    await userEvent.click(controls.getByRole('button', { name: 'Interrupt' }));
    await expect(
      controls.getByRole('textbox', { name: 'Why are you stopping Jenkins build pool?' }),
    ).toBeInTheDocument();
    await userEvent.click(controls.getByRole('button', { name: 'Keep watching' }));
    const launch = controls.getByRole('button', {
      name: 'Open run Jenkins build pool',
    });
    await expect(launch).not.toHaveAttribute('aria-expanded');
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

// A view that no Activity claims joins the last step digest. Its picture opens only on a click.
export const Unmatched: Story = {
  args: { liveViews: [{ ...view, owner: undefined }] },
  play: async ({ canvas, canvasElement }) => {
    const control = canvas.getByRole('button', { name: `Open running live view: ${view.title}` });
    await expect(canvasElement.querySelector('.live-view-panel')).toBeNull();
    await userEvent.click(control);
    const opened = within(document.body);
    await expect(opened.getByRole('dialog', { name: view.title })).toBeVisible();
    await userEvent.click(opened.getByRole('button', { name: `Close ${view.title}` }));
  },
};

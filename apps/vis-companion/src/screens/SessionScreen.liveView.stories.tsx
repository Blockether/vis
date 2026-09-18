import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import { activityHistoryPage } from '../dev/activity-history';
import {
  STORY_COMPOSER_CLIENT as baseClient,
  STORY_COMPOSER_SESSION as baseSession,
  STORY_COMPOSER_SUBSCRIPTIONS as subscriptions,
  STORY_LIVE_VIEW,
} from '../dev/story-data';
import type { RunningTurn } from '../lib/running-turn';
import { SessionScreen } from './SessionScreen';

const activity = activityHistoryPage();
const view = {
  ...STORY_LIVE_VIEW,
  owner: { invocation_id: 'monitor', activity_id: activity.history!.id },
  nodes: [
    ...STORY_LIVE_VIEW.nodes
      .flatMap((node) => (node.type === 'group' ? node.fields : [node]))
      .map((node) => (node.type === 'table' ? { ...node, is_selectable: true } : node)),
    { id: 'separator', type: 'divider' as const },
    { id: 'footer', type: 'paragraph' as const, text: 'Watching for updates.' },
  ],
};
const turn: RunningTurn = {
  id: 'live-view-turn',
  request: 'Watch the fleet scan.',
  answer: '',
  status: 'running',
  startedAt: Date.now(),
  iterations: [{ id: 'monitor-iteration', forms: [{ source: 'monitor()', activity }] }],
};
const session = {
  ...baseSession,
  id: 'live-view-layout',
  status: 'running' as const,
  live: true,
  current_turn_id: turn.id,
};
const client = new Proxy(baseClient, {
  get(target, key) {
    if (key === 'cachedSession') return () => session;
    if (key === 'session') return async () => session;
    if (key === 'cachedRunningTurn') return () => ({ turn, seq: 1 });
    if (key === 'cachedTranscript') return () => [];
    if (key === 'transcript' || key === 'transcriptWindow') return async () => [];
    if (key === 'liveViews') return async () => [view];
    return Reflect.get(target, key);
  },
});

// Embedded, the run is a BAND: unframed, on the message card's own column, with the card's
// padding between it and the reading column. The run's own screen still shows it as a BOX —
// bordered on every side, with every row stopping at the box's content edges. A phone pane and a
// wide desktop pane both keep the run inside the reading column.
async function expectRunBox(panel: Element, column: HTMLElement) {
  const embedded = panel.hasAttribute('data-execution-run');
  const bounds = column.getBoundingClientRect();
  const columnStyle = getComputedStyle(column);
  const inner = bounds.left + column.clientLeft;
  const left = inner + parseFloat(columnStyle.paddingLeft);
  const right = inner + column.clientWidth - parseFloat(columnStyle.paddingRight);
  const style = getComputedStyle(panel);
  const edge = embedded ? '0px' : '1px';
  await expect(style.borderLeftWidth).toBe(edge);
  await expect(style.borderRightWidth).toBe(edge);
  await expect(style.borderTopWidth).toBe(edge);
  await expect(style.borderBottomWidth).toBe(edge);
  const box = panel.getBoundingClientRect();
  // `clientWidth` is a rounded integer, so a column of fractional width — a dialog filling
  // two thirds of a desk — puts its true content edge up to a pixel from the one measured
  // here. Sub-pixel slack, not the multi-pixel overhang this guards.
  await expect(box.left).toBeGreaterThanOrEqual(left - 1);
  await expect(box.right).toBeLessThanOrEqual(right + 1);
  for (const element of panel.querySelectorAll(':scope > ul > li, hr, table')) {
    const row = element.getBoundingClientRect();
    await expect(row.left).toBeCloseTo(box.left + 1, 0);
    await expect(row.right).toBeCloseTo(box.right - 1, 0);
  }
  // Regression: the run used to bleed through the column padding and overhang the card. It stands
  // inside the card's own padding now, and the card still fills the reading column.
  const card = panel.closest<HTMLElement>('[data-execution-group]');
  if (card) {
    const cardBox = card.getBoundingClientRect();
    await expect(cardBox.left).toBeCloseTo(left, 0);
    await expect(cardBox.right).toBeCloseTo(right, 0);
    await expect(box.left).toBeGreaterThan(cardBox.left);
    await expect(box.right).toBeLessThan(cardBox.right);
  }
  const headerElement = panel.querySelector('header')!;
  const header = headerElement.getBoundingClientRect();
  if (embedded) {
    // A band clears the notch through the reading column, exactly as ACTIVITY does: it begins and
    // ends where the activity beside it does instead of insetting itself a second time.
    const activity = card!.querySelector('[data-execution-activity]')!.getBoundingClientRect();
    await expect(header.left).toBeCloseTo(activity.left, 0);
    await expect(header.right).toBeCloseTo(activity.right, 0);
  } else {
    const headerStyle = getComputedStyle(headerElement);
    await expect(parseFloat(headerStyle.paddingLeft)).toBeGreaterThanOrEqual(12);
    await expect(parseFloat(headerStyle.paddingRight)).toBeGreaterThanOrEqual(12);
  }
  await expect(column.scrollWidth).toBe(column.clientWidth);
}

// Regression, user report (paraphrased: an opened run should fill the session it belongs to
// instead of standing as a small window in the middle of it): the dialog takes the whole pane
// it is portalled into, inset only by the scrim's own margin.
async function expectFillsPane(dialog: Element) {
  const box = dialog.parentElement!.getBoundingClientRect();
  const layer = dialog.parentElement!.parentElement!.getBoundingClientRect();
  await expect(box.width).toBeGreaterThan(layer.width - 40);
  await expect(box.height).toBeGreaterThan(layer.height - 40);
}

const meta = {
  title: 'Screens/Session Live view',
  component: SessionScreen,
  parameters: { layout: 'fullscreen' },
  decorators: [
    (Story) => (
      <div className="flex h-dvh w-full flex-col bg-page">
        <Story />
      </div>
    ),
  ],
  args: { client, subscriptions, sid: session.id, onBack: fn(), onOpenSession: fn() },
  play: async ({ canvas }) => {
    const title = await canvas.findByText(view.title);
    await document.fonts.ready;
    const viewport = canvas.getByRole('region', { name: 'Transcript' });
    const column = viewport.firstElementChild as HTMLElement;
    const panel = title.closest('section')!;
    await expectRunBox(panel, column);
    await expect(viewport.scrollWidth).toBe(viewport.clientWidth);
    const launch = canvas.queryByRole('button', { name: `Open run ${view.title}` });
    if (launch) {
      await userEvent.click(launch);
      const page = within(document.body);
      const expanded = page.getAllByText(view.title).at(-1)!.closest('section')!;
      await expectRunBox(expanded, expanded.parentElement!);
      await expectFillsPane(page.getByRole('dialog', { name: view.title }));
      await userEvent.click(page.getByRole('button', { name: `Close ${view.title}` }));
      await expect(title).toBeVisible();
    }
  },
} satisfies Meta<typeof SessionScreen>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Phone: Story = {
  globals: { viewport: { value: 'phone', isRotated: false } },
  play: async (context) => {
    await meta.play(context);
    const viewport = context.canvas.getByRole('region', { name: 'Transcript' });
    // A phone reading column fills the pane; the run rules stop at its message card.
    await expect((viewport.firstElementChild as HTMLElement).clientWidth).toBe(
      viewport.clientWidth,
    );
  },
};

export const Landscape: Story = {
  globals: { viewport: { value: 'phone', isRotated: true } },
  play: async (context) => {
    const title = await context.canvas.findByText(view.title);
    const viewport = context.canvas.getByRole('region', { name: 'Transcript' });
    const column = viewport.firstElementChild as HTMLElement;
    const panel = title.closest('section')!;
    // Browser emulation has no notch; exercise both asymmetric safe-area directions.
    for (const [left, right] of [
      ['59px', '24px'],
      ['24px', '59px'],
    ]) {
      column.style.paddingLeft = left;
      column.style.paddingRight = right;
      await expectRunBox(panel, column);
      await expect(viewport.scrollWidth).toBe(viewport.clientWidth);
    }
    // A band has no inset of its own: the notch is cleared by the reading column it sits in.
    await expect(getComputedStyle(panel.querySelector('header')!).paddingLeft).toBe('0px');
    // The picture — and the safe area it must clear — belongs to the run's own screen now.
    await userEvent.click(context.canvas.getByRole('button', { name: `Open run ${view.title}` }));
    const page = within(document.body);
    const opened = page
      .getByRole('dialog', { name: view.title })
      .querySelector<HTMLElement>('section.live-view-panel')!;
    opened.style.setProperty('--live-view-inset', '59px');
    const select = page.getAllByRole('button', { name: /^Select / })[0];
    await expect(getComputedStyle(select).paddingLeft).toBe('59px');
    await userEvent.click(page.getByRole('button', { name: `Close ${view.title}` }));
    await meta.play(context);
  },
};

export const Tablet: Story = {
  globals: { viewport: { value: 'tablet', isRotated: false } },
};

export const Desktop: Story = {
  globals: { viewport: { value: 'desktop', isRotated: false } },
  play: async (context) => {
    await meta.play(context);
    const viewport = context.canvas.getByRole('region', { name: 'Transcript' });
    // A pane wider than the reading column must not stretch the run across the window.
    await expect((viewport.firstElementChild as HTMLElement).clientWidth).toBeLessThan(
      viewport.clientWidth,
    );
  },
};

export const SplitPane: Story = {
  globals: Desktop.globals,
  decorators: [
    (Story) => (
      <div className="ml-auto flex h-full w-2/3 flex-col">
        <Story />
      </div>
    ),
  ],
  // The desk is a list AND a transcript. An opened run belongs to the session, so its dialog
  // stands in the session pane: the third of the window beside it keeps its own pixels,
  // neither covered nor dimmed.
  play: async (context) => {
    await meta.play(context);
    const pane = document.querySelector<HTMLElement>('[data-session-surface]')!;
    const paneBox = pane.getBoundingClientRect();
    await expect(paneBox.left).toBeGreaterThan(0);
    await userEvent.click(context.canvas.getByRole('button', { name: `Open run ${view.title}` }));
    const page = within(document.body);
    const dialog = page.getByRole('dialog', { name: view.title });
    await expect(pane.contains(dialog)).toBe(true);
    const layer = [...pane.children].find((child) => child.contains(dialog))!;
    const scrim = layer.getBoundingClientRect();
    await expect(scrim.left).toBeGreaterThanOrEqual(paneBox.left);
    await expect(scrim.right).toBeLessThanOrEqual(paneBox.right);
    await expect(scrim.top).toBeGreaterThanOrEqual(paneBox.top);
    await expect(scrim.bottom).toBeLessThanOrEqual(paneBox.bottom);
    await expect(dialog.getBoundingClientRect().left).toBeGreaterThanOrEqual(paneBox.left);
    await expectFillsPane(dialog);
    await userEvent.click(page.getByRole('button', { name: `Close ${view.title}` }));
  },
};

export const Unmatched: Story = {
  args: {
    client: new Proxy(client, {
      get(target, key) {
        if (key === 'liveViews') return async () => [{ ...view, owner: undefined }];
        return Reflect.get(target, key);
      },
    }),
  },
};

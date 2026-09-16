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

// Borders span the conversation pane, not its padded reading column or the window.
async function expectFullWidth(panel: Element, viewport: HTMLElement) {
  const bounds = viewport.getBoundingClientRect();
  const left = bounds.left + viewport.clientLeft;
  const right = left + viewport.clientWidth;
  const style = getComputedStyle(panel);
  await expect(style.borderLeftWidth).toBe('0px');
  await expect(style.borderRightWidth).toBe('0px');
  await expect(style.borderTopWidth).toBe('1px');
  await expect(style.borderBottomWidth).toBe('1px');
  for (const element of [panel, ...panel.querySelectorAll(':scope > ul > li, hr, table')]) {
    const box = element.getBoundingClientRect();
    await expect(box.left).toBeCloseTo(left, 0);
    await expect(box.right).toBeCloseTo(right, 0);
  }
  const header = panel.querySelector('header')!;
  await expect(parseFloat(getComputedStyle(header).paddingLeft)).toBeGreaterThanOrEqual(12);
  await expect(parseFloat(getComputedStyle(header).paddingRight)).toBeGreaterThanOrEqual(12);
  await expect(viewport.scrollWidth).toBe(viewport.clientWidth);
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
    const panel = title.closest('section')!;
    await expectFullWidth(panel, viewport);
    const launch = canvas.queryByRole('button', { name: `Open run ${view.title}` });
    if (launch) {
      await userEvent.click(launch);
      const page = within(document.body);
      const expanded = page.getAllByText(view.title).at(-1)!.closest('section')!;
      await expectFullWidth(expanded, expanded.parentElement!);
      await userEvent.click(page.getByRole('button', { name: `Close ${view.title}` }));
      await expect(title).toBeVisible();
    }
  },
} satisfies Meta<typeof SessionScreen>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Phone: Story = {
  globals: { viewport: { value: 'phone', isRotated: false } },
};

export const Landscape: Story = {
  globals: { viewport: { value: 'phone', isRotated: true } },
  play: async (context) => {
    const title = await context.canvas.findByText(view.title);
    const viewport = context.canvas.getByRole('region', { name: 'Transcript' });
    const column = viewport.firstElementChild as HTMLElement;
    const panel = title.closest('section')!;
    // Browser emulation has no notch; exercise both asymmetric safe-area directions.
    panel.style.setProperty('--live-view-inset', '59px');
    for (const [left, right] of [
      ['59px', '24px'],
      ['24px', '59px'],
    ]) {
      column.style.setProperty('--transcript-inset-left', left);
      column.style.setProperty('--transcript-inset-right', right);
      await expectFullWidth(panel, viewport);
    }
    await expect(getComputedStyle(panel.querySelector('header')!).paddingLeft).toBe('59px');
    const select = context.canvas.getAllByRole('button', { name: /^Select / })[0];
    await expect(getComputedStyle(select).paddingLeft).toBe('59px');
    await meta.play(context);
  },
};

export const Tablet: Story = {
  globals: { viewport: { value: 'tablet', isRotated: false } },
};

export const Desktop: Story = {
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

export const SplitPane: Story = {
  ...Desktop,
  decorators: [
    (Story) => (
      <div className="ml-auto flex h-full w-2/3 flex-col">
        <Story />
      </div>
    ),
  ],
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

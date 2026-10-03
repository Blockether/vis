import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import { activityHistoryPage } from '../dev/activity-history';
import {
  STORY_COMPOSER_CLIENT as baseClient,
  STORY_COMPOSER_SESSION as baseSession,
  STORY_COMPOSER_SUBSCRIPTIONS as subscriptions,
  STORY_LIVE_VIEW,
} from '../dev/story-data';
import { openStepDigests } from '../dev/story-steps';
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
  // A closed step digest offers the running view of its steps on its row.
  play: async ({ canvas }) => {
    const launch = await canvas.findByRole('button', {
      name: `Open 1 live running: ${view.title}`,
    });
    await userEvent.click(launch);
    const page = within(document.body);
    await expect(page.getByRole('dialog', { name: view.title })).toBeVisible();
    await userEvent.click(page.getByRole('button', { name: `Close ${view.title}` }));
    await expect(launch).toBeVisible();
  },
} satisfies Meta<typeof SessionScreen>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Phone: Story = {
  globals: { viewport: { value: 'phone', isRotated: false } },
};

// The jsdom story run has no viewport, so every other size would repeat Phone there.
export const Landscape: Story = {
  tags: ['!test'],
  globals: { viewport: { value: 'phone', isRotated: true } },
};

export const Tablet: Story = {
  tags: ['!test'],
  globals: { viewport: { value: 'tablet', isRotated: false } },
};

export const Desktop: Story = {
  tags: ['!test'],
  globals: { viewport: { value: 'desktop', isRotated: false } },
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
    const pane = document.querySelector<HTMLElement>('[data-session-surface]')!;
    const page = within(document.body);
    const opensInPane = async (launch: HTMLElement) => {
      await userEvent.click(launch);
      await expect(pane.contains(page.getByRole('dialog', { name: view.title }))).toBe(true);
      await userEvent.click(page.getByRole('button', { name: `Close ${view.title}` }));
    };
    // The closed digest opens the view from its row; the open steps open it from the view.
    await opensInPane(
      await context.canvas.findByRole('button', { name: `Open 1 live running: ${view.title}` }),
    );
    await openStepDigests(context.canvasElement);
    await opensInPane(context.canvas.getByRole('button', { name: `Open run ${view.title}` }));
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
  // A view without an owner stays in the turn, outside the closed step digest.
  play: async ({ canvas }) => {
    const title = await canvas.findByText(view.title);
    const launch = canvas.queryByRole('button', { name: `Open run ${view.title}` });
    if (launch) {
      await userEvent.click(launch);
      const page = within(document.body);
      await userEvent.click(page.getByRole('button', { name: `Close ${view.title}` }));
      await expect(title).toBeVisible();
    }
  },
};

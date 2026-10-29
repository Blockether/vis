import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent } from 'storybook/test';

import { STORY_INERT_CLIENT, STORY_LIVE_PRIMITIVES } from '../dev/story-data';
import { LivePanel } from './LivePanel';
import { liveViewFromWire, type LiveView } from '../lib/live-view';

/**
 * Live views outside the transcript. An extension puts a view in the side panel of a
 * session. The panel shows one tab for each extension when two or more extensions own
 * its views.
 */

function wired(wire: Record<string, unknown>): LiveView {
  const view = liveViewFromWire({ seq: 0, ...wire });
  if (!view) throw new Error('the story view must be paintable');
  return view;
}

const JOBS = wired({
  id: 'jobs',
  title: 'Job queue',
  extension: 'jobs',
  placement: 'panel',
  nodes: [
    { id: 's', type: 'status', text: 'Two jobs wait for a lane', tone: 'running' },
    {
      id: 'lanes',
      type: 'group',
      direction: 'row',
      children: [
        { id: 'fast', type: 'card', title: 'Fast lane', text: 'Runs first', meta: '2 jobs', tone: 'ok', is_selectable: true },
        { id: 'slow', type: 'card', title: 'Slow lane', text: 'Runs at night', meta: '5 jobs', tone: 'idle', is_selectable: true },
      ],
    },
    { id: 'name', type: 'plaintext', label: 'Job name', value: 'nightly-build' },
    { id: 'notes', type: 'multiline', label: 'Notes' },
    { id: 'queue', type: 'button', label: 'Queue job', action: 'submit' },
  ],
});

const DEPLOY = { ...STORY_LIVE_PRIMITIVES, id: 'deploy', title: 'Deploy board', extension: 'deploy', placement: 'panel' as const };

const accepted = { action: 'activate' as const, view_id: 'v', is_accepted: true };

const client = {
  viewAction: fn(async () => accepted),
  liveViewLog: fn(async () => ({ lines: [], from: 0, total_lines: 0 })),
} as unknown as typeof STORY_INERT_CLIENT;

const meta = {
  title: 'Session/Live view/Side panel',
  component: LivePanel,
  parameters: { layout: 'fullscreen' },
  args: { views: [DEPLOY, JOBS], client, sid: 's1', onClose: fn() },
  decorators: [
    (Story) => (
      <div className="flex h-[40rem] justify-end bg-ink">
        <div className="h-full w-96 border-l border-dialog-edge">
          <Story />
        </div>
      </div>
    ),
  ],
} satisfies Meta<typeof LivePanel>;

export default meta;

type Story = StoryObj<typeof meta>;

/** Views from two extensions: one tab for each, and each view folds to its title. */
export const TwoExtensions: Story = {
  play: async ({ canvas }) => {
    const tabs = canvas.getAllByRole('tab');
    await expect(tabs.map((tab) => tab.textContent)).toEqual(['deploy', 'jobs']);
    await userEvent.click(tabs[1]);
    const row = canvas.getByRole('button', { name: 'Job queue' });
    await expect(row).toHaveAttribute('aria-expanded', 'true');
    await expect(canvas.getByRole('button', { name: /Fast lane/ })).toBeVisible();
    await userEvent.click(row);
    await expect(row).toHaveAttribute('aria-expanded', 'false');
    await expect(canvas.queryByRole('button', { name: /Fast lane/ })).toBeNull();
  },
};

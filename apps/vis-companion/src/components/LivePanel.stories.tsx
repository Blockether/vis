import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import { SWARM_FORGE_BOARD } from '../dev/live-swarm-data';
import { STORY_INERT_CLIENT, STORY_LIVE_PRIMITIVES } from '../dev/story-data';
import { LivePanel } from './LivePanel';
import { LiveViewPanel } from './LiveView';
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

const FORGE = wired({
  id: 'forge',
  title: 'Forge queue',
  extension: 'forge',
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

const SWARM = { ...STORY_LIVE_PRIMITIVES, id: 'swarm', title: 'Swarm board', extension: 'swarm', placement: 'panel' as const };

const accepted = { action: 'activate' as const, view_id: 'v', is_accepted: true };

const client = {
  viewAction: fn(async () => accepted),
  liveViewLog: fn(async () => ({ lines: [], from: 0, total_lines: 0 })),
} as unknown as typeof STORY_INERT_CLIENT;

const meta = {
  title: 'Live View/Side panel',
  component: LivePanel,
  parameters: { layout: 'fullscreen' },
  args: { views: [SWARM, FORGE], client, sid: 's1', onClose: fn() },
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
export const TwoOwners: Story = {
  play: async ({ canvas }) => {
    const tabs = canvas.getAllByRole('tab');
    await expect(tabs.map((tab) => tab.textContent)).toEqual(['swarm', 'forge']);
    await userEvent.click(tabs[1]);
    const row = canvas.getByRole('button', { name: 'Forge queue' });
    await expect(row).toHaveAttribute('aria-expanded', 'true');
    await expect(canvas.getByRole('button', { name: /Fast lane/ })).toBeVisible();
    await userEvent.click(row);
    await expect(row).toHaveAttribute('aria-expanded', 'false');
    await expect(canvas.queryByRole('button', { name: /Fast lane/ })).toBeNull();
  },
};

/** One extension owns every view: its name heads the panel, without tabs. */
export const OneOwner: Story = {
  args: { views: [FORGE] },
  play: async ({ canvas }) => {
    await expect(canvas.queryByRole('tablist')).toBeNull();
    await expect(canvas.getByRole('heading', { name: 'forge' })).toBeVisible();
  },
};

/** The inputs and cards of a view: a value goes out after a pause, a submit sends all. */
export const InputsAndCards: StoryObj<typeof LiveViewPanel> = {
  render: (args) => <LiveViewPanel {...args} />,
  args: { view: FORGE, inDialog: true, onChange: fn(), onSubmit: fn(), onActivate: fn() },
  play: async ({ args, canvasElement }) => {
    const canvas = within(canvasElement);
    const name = canvas.getByLabelText('Job name');
    await userEvent.clear(name);
    await userEvent.type(name, 'release');
    await userEvent.click(canvas.getByRole('button', { name: /Fast lane/ }));
    await expect(args.onActivate).toHaveBeenCalledWith('fast');
    await userEvent.click(canvas.getByRole('button', { name: 'Queue job' }));
    await expect(args.onChange).toHaveBeenCalledWith('name', 'release');
    await expect(args.onSubmit).toHaveBeenCalledWith('queue', { name: 'release', notes: '' });
  },
};

/**
 * The Swarm Forge board as its extension paints it: counters first, then the work that
 * needs you, the lanes, the queue and the controls. No line joins facts with dots.
 */
export const SwarmForgeBoard: Story = {
  args: { views: [wired(SWARM_FORGE_BOARD)] },
  play: async ({ canvas }) => {
    await expect(canvas.getByText('Need you')).toBeVisible();
    await expect(canvas.getByRole('button', { name: 'Approve' })).toBeVisible();
    await expect(canvas.getByRole('textbox', { name: 'Answer, required' })).toBeInTheDocument();
    await expect(canvas.queryByText(/ · /)).toBeNull();
  },
};

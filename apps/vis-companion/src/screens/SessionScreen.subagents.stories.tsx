import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import { STORY_AGENT_LEADER, storyAgentClient } from '../dev/story-agents';
import { draftMessageKey, hydrateDraftMessages, writeDraftMessage } from '../lib/draft-messages';
import { STORY_COMPOSER_SUBSCRIPTIONS as subscriptions } from '../dev/story-data';
import { SessionScreen } from './SessionScreen';

const running = storyAgentClient(true);
const idle = storyAgentClient(false);

const meta = {
  title: 'Screens/Session/Subagents',
  component: SessionScreen,
  parameters: { layout: 'fullscreen' },
  decorators: [
    (Story) => (
      <div className="flex h-dvh w-full flex-col bg-page">
        <Story />
      </div>
    ),
  ],
  beforeEach: async () => {
    await hydrateDraftMessages();
    writeDraftMessage(draftMessageKey(running.base, STORY_AGENT_LEADER), { text: '' });
  },
  args: { client: running, subscriptions, sid: STORY_AGENT_LEADER, onBack: fn(), onOpenSession: fn() },
} satisfies Meta<typeof SessionScreen>;

export default meta;
type Story = StoryObj<typeof meta>;

// The wake from a child stays out of the transcript; the leader's own prose stays.
async function wakeHidden(canvasElement: HTMLElement) {
  const page = within(canvasElement);
  await expect(await page.findByText(/I take the conflict over myself/)).toBeVisible();
  await expect(page.queryByText(/merge conflict in state\.clj\./)).toBeNull();
  await expect(page.queryByText(/Thread #58/)).toBeNull();
}

/**
 * The running turn: its team in the dock above the composer, folded to the team line. The
 * line opens the table. The rows only read: a subagent asks its leader, never the person.
 */
export const Running: Story = {
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const dock = await page.findByRole('region', { name: 'Subagents of this turn' });
    await expect(within(dock).getByRole('button', { expanded: false })).toHaveTextContent(/^team\d+m \d{2}s4 running1 queued1 done$/);
    await userEvent.click(within(dock).getByRole('button', { expanded: false }));
    await expect(within(dock).getByText('Sign in to the store')).toBeVisible();
    await expect(within(dock).queryByText(/more iterations/)).toBeNull();
    await expect(within(dock).queryByText('needs you')).toBeNull();
    await expect(page.queryByRole('region', { name: 'Questions' })).toBeNull();
    await expect(within(dock).queryByRole('button', { name: /^(Answer|Grant|Deny|Later)/ })).toBeNull();
    // The finished team of the first turn stands in that turn, not in the dock.
    await expect(within(dock).queryByText('Unit tests')).toBeNull();
    await wakeHidden(canvasElement);
  },
};

/** After the turns end: no dock, and each team stays in its own turn with its final state. */
export const Finished: Story = {
  args: { client: idle },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    await expect(await page.findByText('Unit tests')).toBeVisible();
    await expect(page.queryByRole('region', { name: 'Subagents of this turn' })).toBeNull();
    await wakeHidden(canvasElement);
  },
};

/** A dock row opens the timeline of its agent. */
export const Timeline: Story = {
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const dock = await page.findByRole('region', { name: 'Subagents of this turn' });
    await userEvent.click(within(dock).getByRole('button', { expanded: false }));
    await userEvent.click(within(dock).getByRole('button', { name: /Map the agents routes/ }));
    const sheet = await within(document.body).findByRole('dialog', { name: 'Map the agents routes' });
    await expect(await within(sheet).findByRole('list', { name: 'Timeline' })).toBeVisible();
  },
};

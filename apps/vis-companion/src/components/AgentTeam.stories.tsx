import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';
import { STORY_AGENT_LEADER, STORY_AGENT_LIVE_TURN, STORY_DONE_TEAM, STORY_LIVE_TEAM, storyTimeline } from '../dev/story-agents';
import { AgentDock } from './AgentDock';
import { TurnAgents } from './AgentTeam';

const timeline = fn(async (child: string) => {
  const agent = [...STORY_LIVE_TEAM, ...STORY_DONE_TEAM].find((row) => row.session_id === child);
  return agent ? storyTimeline(agent) : { session_id: child, items: [] };
});

const meta = {
  title: 'Session/Subagents',
  component: TurnAgents,
  parameters: { layout: 'padded' },
  args: { agents: STORY_DONE_TEAM, pending: null, onOpen: fn(), onCancel: fn(), timeline },
} satisfies Meta<typeof TurnAgents>;
export default meta;
type Story = StoryObj<typeof meta>;

/**
 * The dock line opens the table of the team. A subagent asks only its leader: its row shows
 * its newest note, never the question, and has no control to answer it.
 */
export const Dock: Story = {
  render: (args) => (
    <AgentDock {...args} agents={STORY_LIVE_TEAM} sid={STORY_AGENT_LEADER} runningTurnId={STORY_AGENT_LIVE_TURN} />
  ),
  play: async ({ canvasElement }) => {
    const dock = within(canvasElement).getByRole('region', { name: 'Subagents of this turn' });
    await userEvent.click(within(dock).getByRole('button', { expanded: false }));
    await expect(within(dock).getByRole('button', { expanded: true })).toBeVisible();
    for (const word of ['queued', 'done']) await expect(within(dock).getByText(word)).toBeVisible();
    await expect(within(dock).getByText(/12 of 40 native tests ran/)).toBeVisible();
    await expect(within(dock).queryByText(/more iterations/)).toBeNull();
    for (const control of [/Answer/, /Grant/, /Deny/, /Later/]) {
      await expect(within(dock).queryByRole('button', { name: control })).toBeNull();
    }
    await expect(dock.querySelectorAll('svg')).toHaveLength(1);
  },
};

/** A row opens the timeline of its agent, with the way into its session. */
export const Timeline: Story = {
  args: { agents: STORY_LIVE_TEAM },
  play: async ({ canvasElement }) => {
    await userEvent.click(within(canvasElement).getByRole('button', { name: /Map the agents routes/ }));
    const sheet = await within(document.body).findByRole('dialog', { name: 'Map the agents routes' });
    await expect(await within(sheet).findByRole('list', { name: 'Timeline' })).toBeVisible();
    await expect(within(sheet).getByRole('button', { name: 'Open session' })).toBeVisible();
  },
};

import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';
import { STORY_AGENT_LEADER, STORY_AGENT_LIVE_TURN, STORY_DONE_TEAM, STORY_LIVE_TEAM } from '../dev/story-agents';
import { AgentDock } from './AgentDock';
import { TurnAgents } from './AgentTeam';

const meta = {
  title: 'Session/Subagents',
  component: TurnAgents,
  parameters: { layout: 'padded' },
  args: { agents: STORY_DONE_TEAM, onOpen: fn() },
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

/** A finished team. A row opens the session of its agent. */
export const Finished: Story = {};

// The leader worked, waited in council.run, worked again and waits in council.wait now.
const LEAD_NOW = Date.now();
const LEADER = {
  start: LEAD_NOW - 150_000,
  waits: [{ start: LEAD_NOW - 75_000, end: LEAD_NOW - 30_000 }, { start: LEAD_NOW - 15_000 }],
  note: 'Five agents work. I wait for the native tests, then I read the contract result.',
};

/** Timeline keys each lane, L for the leader and A1, A2 for the agents. The legend explains each key. */
export const LeaderChart: Story = {
  globals: { viewport: { value: 'desktop', isRotated: false } },
  args: { agents: STORY_LIVE_TEAM, leader: LEADER },
};

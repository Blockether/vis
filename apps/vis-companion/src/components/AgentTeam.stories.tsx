import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';
import { STORY_AGENT_LEADER, STORY_AGENT_LIVE_TURN, STORY_DONE_TEAM, STORY_LIVE_TEAM, storyTimeline } from '../dev/story-agents';
import type { Subagent } from '../lib/types';
import { AgentCrumb } from './AgentCrumb';
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

/** The finished team in its turn of the transcript: final statuses, results and durations. */
export const Finished: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByText('out of budget')).toBeVisible();
    await expect(canvas.getByText('passed')).toBeVisible();
    await expect(canvas.getByText('#3')).toBeVisible();
    await expect(canvasElement.querySelector('svg')).toBeNull();
  },
};

/** The dock above the composer while the turn runs: one row for each agent of the turn. */
export const Dock: Story = {
  render: (args) => (
    <AgentDock {...args} agents={STORY_LIVE_TEAM} sid={STORY_AGENT_LEADER} runningTurnId={STORY_AGENT_LIVE_TURN} />
  ),
  play: async ({ canvasElement }) => {
    const dock = within(canvasElement).getByRole('region', { name: 'Subagents of this turn' });
    for (const word of ['queued', 'passed']) await expect(within(dock).getByText(word)).toBeVisible();
    await expect(within(dock).getAllByText('running')[0]).toBeVisible();
    await expect(within(dock).queryByText('needs you')).toBeNull();
    await expect(within(dock).getAllByText('asking')).toHaveLength(2);
    await expect(dock.querySelector('svg')).toBeNull();
  },
};

const waiting = STORY_LIVE_TEAM.filter((agent) => agent.question || agent.budget_request || agent.files_request);

/**
 * Agents that wait on their leader: asking, with the question as the line. The rows
 * only read; the leader answers. A form of an agent opens in the form prompt.
 */
export const AskingRow: Story = {
  render: (args) => (
    <AgentDock {...args} agents={waiting} sid={STORY_AGENT_LEADER} runningTurnId={STORY_AGENT_LIVE_TURN} />
  ),
  play: async ({ canvasElement }) => {
    const dock = within(canvasElement).getByRole('region', { name: 'Subagents of this turn' });
    await expect(within(dock).getAllByText('asking').length).toBeGreaterThan(0);
    for (const control of [/Answer/, /Grant/, /Deny/, /Later/]) {
      await expect(within(dock).queryByRole('button', { name: control })).toBeNull();
    }
    await userEvent.click(within(dock).getAllByRole('button', { name: /asking/ })[0]);
    await expect(await within(document.body).findByRole('dialog')).toBeVisible();
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

/** The timeline of a finished verify agent ends with its verdict and its result. */
export const TimelineFinished: Story = {
  play: async ({ canvasElement }) => {
    await userEvent.click(within(canvasElement).getByRole('button', { name: /Unit tests/ }));
    const sheet = await within(document.body).findByRole('dialog', { name: 'Unit tests' });
    await expect(await within(sheet).findByText(/38 of 38 tests pass/)).toBeVisible();
  },
};

/** The header of a child session: the way back to its leader, and the name and status of the agent. */
export const ChildCrumb: Story = {
  args: { agents: STORY_LIVE_TEAM },
  render: (args) => <AgentCrumb sid="live-native" parentId={STORY_AGENT_LEADER} agents={args.agents} onOpen={fn()} />,
};

const empty: Subagent[] = [];

/** A turn without agents shows no block and no dock. */
export const EmptyTeam: Story = {
  args: { agents: empty },
  render: (args) => (
    <div data-empty-team>
      <TurnAgents {...args} />
      <AgentDock {...args} sid={STORY_AGENT_LEADER} runningTurnId={STORY_AGENT_LIVE_TURN} />
    </div>
  ),
  play: async ({ canvasElement }) => {
    await expect(canvasElement.querySelector('[data-empty-team]')).toBeEmptyDOMElement();
  },
};

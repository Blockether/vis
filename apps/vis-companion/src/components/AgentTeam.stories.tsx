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

/**
 * The finished team in its turn of the transcript: a table in a frame, under the team line
 * with the counts by role and by status. Long titles and results wrap, with no ellipsis.
 */
export const Finished: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const team = canvas.getByRole('region', { name: 'Subagents' });
    await expect(team.querySelector('[data-team-line]')).toHaveTextContent(
      'team2 research2 implement2 verify1 audit2 done1 passed2 failed1 out of budget1 cancelled',
    );
    await expect(canvas.getByText('out of budget')).toBeVisible();
    await expect(canvas.getByText('Rebuild every subagent view of the app')).toBeVisible();
    await expect(canvas.getByText('#3')).toBeVisible();
    await expect(canvasElement.querySelector('svg')).toBeNull();
  },
};

/** The dock above the composer while the turn runs, folded to the team line. */
export const DockFolded: Story = {
  render: (args) => (
    <AgentDock {...args} agents={STORY_LIVE_TEAM} sid={STORY_AGENT_LEADER} runningTurnId={STORY_AGENT_LIVE_TURN} />
  ),
  play: async ({ canvasElement }) => {
    const dock = within(canvasElement).getByRole('region', { name: 'Subagents of this turn' });
    const line = within(dock).getByRole('button', { expanded: false });
    await expect(line).toHaveTextContent('team1 research2 implement2 verify1 audit4 running1 queued1 passed');
    await expect(within(dock).queryByRole('button', { name: /Map the agents routes/ })).toBeNull();
  },
};

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
    for (const word of ['queued', 'passed']) await expect(within(dock).getByText(word)).toBeVisible();
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

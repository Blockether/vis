import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';
import { STORY_SESSION } from '../dev/story-data';
import type { Subagent } from '../lib/types';
import { AgentTeamPanel } from './AgentTeam';
import { SessionHeader } from './SessionHeader';

const child: Subagent = {
  session_id: 'contract-review', parent_id: 'leader', leader_id: 'leader', team_id: 'task',
  task: 'Verify the SDK contract and report remaining risks', status: 'running', depth: 1,
  iteration_budget: 32, iterations_used: 4, provider: 'openai', model: 'gpt-5', pending_input: true,
  routing_locked: true, usage: { cost_usd: 0.0123 },
};

const meta = {
  title: 'Session/Agent team', component: AgentTeamPanel,
  parameters: { layout: 'fullscreen' },
  args: {
    agents: [child], loading: false, error: null, pending: null,
    onRefresh: fn(), onOpen: fn(), onCancel: fn(),
  },
  render: (args) => <SessionHeader
    model={{ title: STORY_SESSION.title, sessionId: STORY_SESSION.id, connected: true, artifacts: { count: 4, isOpen: false } }}
    commands={{ back: fn(), toggleArtifacts: fn() }} team={<AgentTeamPanel {...args} />} />,
} satisfies Meta<typeof AgentTeamPanel>;
export default meta;
type Story = StoryObj<typeof meta>;

const openTeam: NonNullable<Story['play']> = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  const button = canvas.getByRole('button', { name: /^Agents:/ });
  const box = button.getBoundingClientRect();
  expect(box.left).toBeGreaterThanOrEqual(0);
  expect(box.right).toBeLessThanOrEqual(window.innerWidth);
  await userEvent.click(button);
  expect(within(document.body).getByRole('dialog', { name: 'Agent team' })).toBeVisible();
};

export const Running: Story = { play: async (context) => {
  await openTeam(context);
  const page = within(document.body);
  expect(page.getByText('Needs your input')).toBeVisible();
  await userEvent.click(page.getByRole('button', { name: 'Stop agent' }));
  expect(page.getByRole('button', { name: 'Confirm stop' })).toBeVisible();
  await userEvent.click(page.getByRole('button', { name: 'Keep working' }));
} };
export const Empty: Story = {
  args: { agents: [] },
  play: async (context) => {
    await openTeam(context);
    expect(within(document.body).getByText(/No subagents yet/)).toBeVisible();
  },
};
export const Loading: Story = {
  args: { agents: [], loading: true },
  play: async (context) => {
    await openTeam(context);
    expect(within(document.body).getByRole('status')).toHaveTextContent('Loading team');
  },
};
export const Failed: Story = {
  args: { agents: [], error: 'Gateway unavailable. Refresh to try again.' },
  play: async (context) => {
    await openTeam(context);
    expect(within(document.body).getByRole('alert')).toHaveTextContent('Gateway unavailable');
  },
};
export const Completed: Story = {
  args: { agents: [{ ...child, status: 'completed', pending_input: false }] },
  play: async (context) => {
    await openTeam(context);
    const page = within(document.body);
    expect(page.getByText('Completed')).toBeVisible();
    expect(page.queryByRole('button', { name: 'Stop agent' })).toBeNull();
  },
};
export const BudgetReached: Story = {
  args: { agents: [{ ...child, status: 'budget_limited', pending_input: false, iterations_used: 32 }] },
  play: async (context) => {
    await openTeam(context);
    const page = within(document.body);
    expect(page.getByText('Budget reached')).toBeVisible();
    expect(page.queryByRole('button', { name: 'Stop agent' })).toBeNull();
  },
};
export const Hierarchy: Story = {
  args: { parentId: 'leader', agents: [child, {
    ...child, session_id: 'schema-check', parent_id: child.session_id,
    task: 'Check required schema fields', depth: 2, pending_input: false,
    status: 'queued', iterations_used: 0,
  }] },
  play: async (context) => {
    await openTeam(context);
    const page = within(document.body);
    expect(page.getByText(`Parent task: ${child.task}`)).toBeVisible();
    await userEvent.click(page.getByRole('button', { name: 'Open parent' }));
    expect(context.args.onOpen).toHaveBeenCalledWith('leader');
    expect(page.queryByRole('dialog')).toBeNull();
  },
};

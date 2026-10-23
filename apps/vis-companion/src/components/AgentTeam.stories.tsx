import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';
import type { Subagent } from '../lib/types';
import { TurnAgents } from './AgentTeam';

const child: Subagent = {
  session_id: 'contract-review', parent_id: 'leader', leader_id: 'leader', team_id: 'turn-1',
  task: 'Verify the SDK contract and report remaining risks', status: 'running',
  iteration_budget: 32, iterations_used: 4, provider: 'openai', model: 'gpt-5',
  routing_locked: true, usage: { cost_usd: 0.0123 },
};

const meta = {
  title: 'Session/Turn agents', component: TurnAgents,
  parameters: { layout: 'padded' },
  args: { agents: [child], pending: null, onOpen: fn(), onCancel: fn() },
} satisfies Meta<typeof TurnAgents>;
export default meta;
type Story = StoryObj<typeof meta>;

export const Live: Story = {
  args: {
    agents: [
      { ...child, session_id: 'done', status: 'completed', iterations_used: 3, role: 'audit' },
      { ...child, session_id: 'working', task: 'List the components that render the team', role: 'research', iterations_used: 12, activity: 'Search files AgentTeam' },
      { ...child, session_id: 'asking', task: 'Count the Clojure sources', question: 'Should I count tests in test/ or only sources in src/?', role: 'research' },
      { ...child, session_id: 'blocked', task: 'Release the bundle', pending_input: true, pending_input_count: 2, role: 'implement' },
      { ...child, session_id: 'check', task: 'Run the gateway tests', role: 'verify', activity: 'Run tests agents-test' },
    ],
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    expect(canvas.getByText('needs you ×2')).toBeVisible();
    expect(canvas.getByText('asks leader')).toBeVisible();
    expect(canvas.getByText('4/32')).toBeVisible();
    expect(canvas.getByText('Search files AgentTeam')).toBeVisible();
  },
};

export const Expanded: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { expanded: false }));
    await userEvent.click(canvas.getByRole('button', { name: 'Stop agent' }));
    expect(canvas.getByRole('button', { name: 'Confirm stop' })).toBeVisible();
    await userEvent.click(canvas.getByRole('button', { name: 'Keep working' }));
  },
};

export const Finished: Story = {
  args: {
    agents: [
      { ...child, session_id: 'a', status: 'completed' },
      { ...child, session_id: 'b', status: 'failed', task: 'Run the native tests' },
      { ...child, session_id: 'c', status: 'budget_limited', iterations_used: 32, task: 'Review the gateway routes' },
    ],
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    expect(canvas.getByText('done')).toBeVisible();
    await userEvent.click(canvas.getAllByRole('button', { expanded: false })[0]);
    expect(canvas.queryByRole('button', { name: 'Stop agent' })).toBeNull();
  },
};

import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';
import type { Subagent } from '../lib/types';
import { AgentCrumb } from './AgentCrumb';
import { AgentDock } from './AgentDock';
import { AgentInbox } from './AgentInbox';

const agent: Subagent = {
  session_id: 'a1', parent_id: 'leader', leader_id: 'leader', team_id: 'turn-1',
  task: 'Read the bootstrap code and list the cold-start steps', role: 'research', status: 'running',
  iteration_budget: 30, iterations_used: 12, model: 'gpt-5', activity: 'Read src/vis/bootstrap.clj',
};

const team: Subagent[] = [
  agent,
  {
    ...agent, session_id: 'a2', task: 'Cut the cold start of the gateway', role: 'implement',
    iterations_used: 18, question: 'The cache is in two places. Should I keep the disk cache or remove it?',
    question_entry_id: 41, activity: undefined,
  },
  {
    ...agent, session_id: 'a3', task: 'Run the gateway tests', role: 'verify', iterations_used: 30,
    question: 'I used my budget. Can I have 10 more iterations?', question_entry_id: 38,
    budget_request: { iterations: 10, reason: 'The native tests are slow' }, activity: undefined,
  },
  {
    ...agent, session_id: 'a4', task: 'Check the docs for broken links', role: 'audit', iterations_used: 5,
    question: 'May I fix the links that I found?', question_entry_id: 44,
    files_request: { files: { write: ['resources/vis-docs/**'] }, reason: 'Fix four links' }, activity: undefined,
  },
  { ...agent, session_id: 'a5', task: 'Release the bundle', role: 'implement', pending_input: true, activity: undefined },
];

const actions = { answer: fn(async () => null), extend: fn(async () => null), deny: fn(async () => null), grant: fn(async () => null) };

const meta = {
  title: 'Session/Agent questions',
  component: AgentInbox,
  parameters: { layout: 'padded' },
  args: { agents: team, sid: 'leader', pending: null, actions, onOpen: fn() },
} satisfies Meta<typeof AgentInbox>;
export default meta;
type Story = StoryObj<typeof meta>;

/** The questions inbox and the team dock, as they stand above the composer. */
export const Inbox: Story = {
  render: (args) => (
    <div className="max-w-3xl">
      <AgentInbox {...args} />
      <AgentDock agents={args.agents} sid={args.sid} pending={null} onOpen={fn()} onCancel={fn()} />
    </div>
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    expect(canvas.getByText('4 questions · 4 agents')).toBeVisible();
    expect(canvas.getByRole('button', { name: 'Grant +10 iterations' })).toBeVisible();
    await userEvent.click(canvas.getByRole('button', { name: 'Later' }));
    expect(canvas.getByRole('textbox', { name: 'Answer A2' })).toBeVisible();
  },
};

export const OneQuestion: Story = {
  args: { agents: [team[1]] },
};

export const TeamSheet: Story = {
  render: (args) => <AgentDock agents={args.agents} sid={args.sid} pending={null} onOpen={fn()} onCancel={fn()} />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: /Team/ }));
    expect(within(document.body).getByRole('dialog', { name: 'Team' })).toBeVisible();
  },
};

export const ChildCrumb: Story = {
  render: (args) => <AgentCrumb sid="a2" parentId="leader" agents={args.agents} onOpen={fn()} />,
};

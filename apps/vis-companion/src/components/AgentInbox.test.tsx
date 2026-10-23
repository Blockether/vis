// @vitest-environment jsdom
import { act, cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import type { Subagent } from '../lib/types';
import { AgentCrumb } from './AgentCrumb';
import { AgentDock } from './AgentDock';
import { AgentInbox, openRequests } from './AgentInbox';

const base: Subagent = { session_id: 'a1', parent_id: 'lead', leader_id: 'lead', team_id: 'turn-1',
  task: 'Read the logs', role: 'research', status: 'running', iteration_budget: 30, iterations_used: 12 };
const asking: Subagent = { ...base, session_id: 'a2', task: 'Fix the bug', role: 'implement',
  question: 'Keep the old API?', question_entry_id: 40 };
const budget: Subagent = { ...base, session_id: 'a3', task: 'Run the tests', role: 'verify',
  question: 'I need 10 more iterations.', question_entry_id: 20, budget_request: { iterations: 10, reason: 'Tests are slow' } };
const files: Subagent = { ...base, session_id: 'a4', task: 'Audit docs', role: 'audit',
  question: 'May I write docs?', question_entry_id: 30, files_request: { files: { write: ['docs/**'] }, reason: 'Fix links' } };
const form: Subagent = { ...base, session_id: 'a5', task: 'Sign in', pending_input: true, pending_input_count: 2 };

const actions = () => ({
  answer: vi.fn(async () => null as string | null),
  extend: vi.fn(async () => null as string | null),
  deny: vi.fn(async () => null as string | null),
  grant: vi.fn(async () => null as string | null),
});

afterEach(() => { cleanup(); vi.restoreAllMocks(); });

describe('openRequests', () => {
  it('lists the open requests of this leader oldest first, with A-numbers in start order', () => {
    const requests = openRequests([base, asking, budget, files, form, { ...asking, session_id: 'x', parent_id: 'other' }], 'lead');
    expect(requests.map((request) => `${request.kind}:A${request.number}`)).toEqual([
      'budget:A3', 'files:A4', 'question:A2', 'form:A5',
    ]);
    expect(requests[3].text).toBe('Waits for you on 2 forms.');
  });
});

describe('AgentInbox', () => {
  it('stays hidden when no agent asks', () => {
    const { container } = render(<AgentInbox agents={[base]} sid="lead" pending={null} actions={actions()} onOpen={vi.fn()} />);
    expect(container).toBeEmptyDOMElement();
  });

  it('answers a plain question with free text', async () => {
    const calls = actions();
    render(<AgentInbox agents={[asking]} sid="lead" pending={null} actions={calls} onOpen={vi.fn()} />);
    const inbox = screen.getByRole('region', { name: 'Questions' });
    expect(inbox).toHaveTextContent('1 question');
    expect(inbox).toHaveTextContent('Keep the old API?');
    expect(within(inbox).getByText('NEW')).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Send' })).toBeDisabled();
    fireEvent.change(screen.getByRole('textbox', { name: 'Answer A1' }), { target: { value: ' Yes, keep it ' } });
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Send' })); });
    expect(calls.answer).toHaveBeenCalledWith('a2', 'Yes, keep it');
    expect(screen.getByRole('textbox', { name: 'Answer A1' })).toHaveValue('');
  });

  it('grants or denies a budget request with an editable reason, and shows errors inline', async () => {
    const calls = actions();
    calls.deny.mockResolvedValueOnce('No open request');
    render(<AgentInbox agents={[budget]} sid="lead" pending={null} actions={calls} onOpen={vi.fn()} />);
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Grant +10 iterations' })); });
    expect(calls.extend).toHaveBeenCalledWith('a3', 10);
    fireEvent.click(screen.getByRole('button', { name: 'Deny' }));
    const reason = screen.getByRole('textbox', { name: 'Reason' });
    expect(reason).toHaveValue('Not now. Continue with what you have.');
    fireEvent.change(reason, { target: { value: 'Stop here' } });
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Send deny' })); });
    expect(calls.deny).toHaveBeenCalledWith('a3', 'Stop here');
    expect(screen.getByRole('alert')).toHaveTextContent('No open request');
    fireEvent.click(screen.getByRole('button', { name: 'Keep request' }));
    expect(screen.getByRole('button', { name: 'Grant +10 iterations' })).toBeInTheDocument();
  });

  it('grants the requested files', async () => {
    const calls = actions();
    render(<AgentInbox agents={[files]} sid="lead" pending={null} actions={calls} onOpen={vi.fn()} />);
    expect(screen.getByRole('list', { name: 'Requested files' })).toHaveTextContent('write docs/**');
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Grant files' })); });
    expect(calls.grant).toHaveBeenCalledWith('a4', { write: ['docs/**'] });
  });

  it('opens a child that waits on a form', () => {
    const onOpen = vi.fn();
    render(<AgentInbox agents={[form]} sid="lead" pending={null} actions={actions()} onOpen={onOpen} />);
    fireEvent.click(screen.getByRole('button', { name: 'Open agent' }));
    expect(onOpen).toHaveBeenCalledWith('a5');
  });

  it('moves between questions, puts one off for later and keeps keys inside the inbox', () => {
    render(<AgentInbox agents={[asking, budget, files]} sid="lead" pending={null} actions={actions()} onOpen={vi.fn()} />);
    const inbox = screen.getByRole('region', { name: 'Questions' });
    expect(inbox).toHaveTextContent('3 questions · 3 agents');
    expect(inbox).toHaveTextContent('1 of 3');
    expect(screen.getByRole('article')).toHaveTextContent('Run the tests');
    expect(within(screen.getByRole('list', { name: 'Other questions' })).getAllByRole('button')).toHaveLength(2);
    fireEvent.click(screen.getByRole('button', { name: 'Later' }));
    expect(screen.getByRole('article')).toHaveTextContent('Audit docs');
    expect(inbox).toHaveTextContent('1 of 3');
    fireEvent.keyDown(screen.getByRole('button', { name: 'Grant files' }), { key: 'Escape' });
    expect(screen.getByRole('article')).toHaveTextContent('Fix the bug');
    fireEvent.keyDown(screen.getByRole('textbox', { name: 'Answer A1' }), { key: 'Tab' });
    expect(screen.getByRole('article')).toHaveTextContent('Run the tests');
    fireEvent.click(screen.getByRole('button', { name: 'Next question' }));
    expect(screen.getByRole('article')).toHaveTextContent('Audit docs');
    fireEvent.click(within(screen.getByRole('list', { name: 'Other questions' })).getByText('Keep the old API?'));
    expect(screen.getByRole('article')).toHaveTextContent('Fix the bug');
  });

  it('drops the NEW tag after a request had the focus', () => {
    render(<AgentInbox agents={[asking, budget]} sid="lead" pending={null} actions={actions()} onOpen={vi.fn()} />);
    expect(within(screen.getByRole('article')).getByText('NEW')).toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: 'Next question' }));
    fireEvent.click(screen.getByRole('button', { name: 'Next question' }));
    expect(within(screen.getByRole('article')).queryByText('NEW')).not.toBeInTheDocument();
  });

  it('folds to its header', () => {
    render(<AgentInbox agents={[asking]} sid="lead" pending={null} actions={actions()} onOpen={vi.fn()} />);
    fireEvent.click(screen.getByRole('button', { name: /1 question/ }));
    expect(screen.queryByRole('article')).not.toBeInTheDocument();
  });
});

describe('AgentDock', () => {
  const dock = (agents: Subagent[]) =>
    render(<AgentDock agents={agents} sid="lead" pending={null} onOpen={vi.fn()} onCancel={vi.fn()} />);

  it('shows nothing when no agent works or asks', () => {
    const { container } = dock([{ ...base, status: 'completed' }]);
    expect(container).toBeEmptyDOMElement();
  });

  it('shows a chip for the questions and one for each agent, and opens the team sheet', () => {
    dock([base, asking, { ...base, session_id: 'done', status: 'completed', task: 'Done work' }]);
    const group = screen.getByRole('group', { name: 'Agent team' });
    expect(within(group).getByRole('button', { name: /1 question/ })).toBeInTheDocument();
    expect(within(group).getByRole('button', { name: /A1.*12\/30/ })).toBeInTheDocument();
    expect(within(group).getByRole('button', { name: /A2.*asks leader/ })).toBeInTheDocument();
    expect(within(group).getByRole('button', { name: /A3.*done/ })).toBeInTheDocument();
    fireEvent.click(within(group).getByRole('button', { name: /Team/ }));
    const sheet = screen.getByRole('dialog', { name: 'Team' });
    expect(within(sheet).getByRole('list', { name: 'Subagents' })).toHaveTextContent('Done work');
  });
});

describe('AgentCrumb', () => {
  it('names the agent and opens its leader', () => {
    const onOpen = vi.fn();
    render(<AgentCrumb sid="a2" parentId="lead" agents={[base, asking]} onOpen={onOpen} />);
    const crumb = screen.getByRole('navigation', { name: 'Agent' });
    expect(crumb).toHaveTextContent('A2');
    expect(crumb).toHaveTextContent('Fix the bug');
    fireEvent.click(within(crumb).getByRole('button', { name: /Leader/ }));
    expect(onOpen).toHaveBeenCalledWith('lead');
  });

  it('keeps the way back before the team rows arrive', () => {
    render(<AgentCrumb sid="a2" parentId="lead" agents={[]} onOpen={vi.fn()} />);
    expect(screen.getByRole('button', { name: /Leader/ })).toBeInTheDocument();
  });
});

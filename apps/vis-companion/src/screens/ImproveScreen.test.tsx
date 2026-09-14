/** @vitest-environment jsdom */
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';
import { ImproveDialog, ImproveWorkspace } from './ImproveScreen';
import { storyImproveClient, storyImproveFetch, STORY_IMPROVE_PROJECT } from '../dev/story-data';
import { GatewayError } from '../lib/gateway';

async function projectIssues(client = storyImproveClient()) {
  render(<ImproveWorkspace client={client} />);
  await screen.findByRole('option', { name: '/workspace/vis' });
  fireEvent.change(screen.getByLabelText('Improve project'), {
    target: { value: STORY_IMPROVE_PROJECT },
  });
  await screen.findByText('Make collected reports actionable');
  return client;
}

const click = (name: string) => fireEvent.click(screen.getByRole('button', { name }));
const change = (name: string, value: string) =>
  fireEvent.change(screen.getByLabelText(name), { target: { value } });

describe('Improve workspace', () => {
  it.each([
    ['/Users/ana/code/vis', '/Users/ana/code/vis', '~/code/vis'],
    ['', '/Users/ana/code/vis', '~/code/vis'],
    ['/home/ana/code/vis', '/home/ana/code/vis', '~/code/vis'],
    ['', '/root/vis', '~/vis'],
    ['C:\\Users\\Ana\\code\\vis', 'C:\\Users\\Ana\\code\\vis', '~/code/vis'],
    ['~/code/vis', '/Users/ana/code/vis', '~/code/vis'],
    ['Vis', '/Users/ana/code/vis', '~/code/vis'],
    ['/Users/ana/old-vis', '/Users/ana/code/vis', '~/code/vis'],
    ['Workspace', '/srv/vis', '/srv/vis'],
    ['', '/srv/vis', '/srv/vis'],
  ])('labels %j / %j as %j without changing project identity', async (name, root, label) => {
    const client = storyImproveClient();
    const overview = await client.improveProjects();
    const project = { ...overview.projects[0], name, root };
    vi.spyOn(client, 'improveProjects').mockResolvedValue({ ...overview, projects: [project] });
    const records = vi.spyOn(client, 'improveRecords');
    render(<ImproveWorkspace client={client} />);

    const option = await screen.findByRole('option', { name: label });
    expect(option).toHaveValue(STORY_IMPROVE_PROJECT);
    change('Improve project', STORY_IMPROVE_PROJECT);
    await screen.findByText('Make collected reports actionable');
    expect(records).toHaveBeenLastCalledWith(STORY_IMPROVE_PROJECT, 0, expect.any(AbortSignal));
    expect(project).toMatchObject({ name, root });
  });

  it('keeps source evidence outside Markdown editing and sends the record version', async () => {
    const client = await projectIssues();
    const update = vi.spyOn(client, 'updateImproveRecord');
    fireEvent.click(screen.getByText('A failed tool call needs reproduction notes'));
    click('Original report · read only');
    expect(
      screen.getByText('A tool call failed. Reproduction has not been attempted.'),
    ).toBeTruthy();
    click('Edit issue');
    change('Issue content', '## Reproduction\nConfirmed by the focused test.');
    click('Save issue');
    await screen.findByText('Issue saved.');
    expect(update).toHaveBeenCalledWith(
      2,
      expect.objectContaining({
        expected_version: 3,
        parent_id: 1,
        content: '## Reproduction\nConfirmed by the focused test.',
      }),
    );
    expect(update.mock.calls[0][1]).not.toHaveProperty('source_content');
  });

  it('excludes the issue and its descendants from group choices', async () => {
    const client = await projectIssues();
    const update = vi.spyOn(client, 'updateImproveRecord');
    fireEvent.click(screen.getByText('Make collected reports actionable'));
    click('Edit issue');
    const picker = screen.getByLabelText('Improvement group') as HTMLSelectElement;
    expect([...picker.options].map((option) => option.value)).toEqual(['', '3']);
    change('Improvement group', '3');
    click('Save issue');
    await screen.findByText('Issue saved.');
    expect(update).toHaveBeenCalledWith(
      1,
      expect.objectContaining({ parent_id: 3, expected_version: 1 }),
    );
  });

  it('requires confirmation for cascade closure and supports reopen', async () => {
    const client = await projectIssues();
    const update = vi.spyOn(client, 'updateImproveRecord');
    fireEvent.click(screen.getByText('Make collected reports actionable'));
    click('Close issue');
    expect(update).not.toHaveBeenCalled();
    expect(screen.getByText(/including children not loaded here/)).toBeTruthy();
    click('Close issue and children');
    await screen.findByRole('button', { name: 'Reopen' });
    expect(update).toHaveBeenCalledWith(1, { status: 'closed', expected_version: 1 });
    click('Reopen');
    await waitFor(() =>
      expect(update).toHaveBeenLastCalledWith(1, { status: 'open', expected_version: 2 }),
    );
  });

  it('keeps conflicting human drafts', async () => {
    const client = await projectIssues();
    vi.spyOn(client, 'updateImproveRecord').mockRejectedValue(new GatewayError(409, 'conflict'));
    fireEvent.click(screen.getByText('Make collected reports actionable'));
    click('Edit issue');
    change('Issue content', 'My unsaved evidence');
    click('Save issue');
    await screen.findByText(/Your draft is kept/);
    expect((screen.getByLabelText('Issue content') as HTMLTextAreaElement).value).toBe(
      'My unsaved evidence',
    );
  });

  it('creates Markdown issues in the selected project', async () => {
    const client = await projectIssues();
    const create = vi.spyOn(client, 'createImproveRecord');
    click('New issue');
    change('Issue title', 'A focused improvement');
    change('Issue content', '## Proposal\nA small change.');
    click('Save issue');
    await screen.findByText('Issue saved.');
    expect(create).toHaveBeenCalledWith({
      project_id: STORY_IMPROVE_PROJECT,
      title: 'A focused improvement',
      content: '## Proposal\nA small change.',
      parent_id: null,
    });
  });

  it('never reads or offers model choices in human mode', async () => {
    const client = await projectIssues();
    const router = vi.spyOn(client, 'router');
    const save = vi.spyOn(client, 'setImproveSettings');
    click('Review settings');
    expect(screen.queryByLabelText('Improve provider')).toBeNull();
    expect(screen.queryByLabelText('Improve model')).toBeNull();
    expect(router).not.toHaveBeenCalled();
    click('Save review settings');
    await waitFor(() => expect(save).toHaveBeenCalledWith({ mode: 'human' }));
  });

  it('persists the chosen automatic route, then omits it for human governance', async () => {
    const client = await projectIssues();
    const save = vi.spyOn(client, 'setImproveSettings');
    click('Review settings');
    change('Review mode', 'automatic');
    await screen.findByRole('option', { name: 'OpenAI' });
    change('Improve provider', 'openai');
    change('Improve model', 'gpt-5-mini');
    change('Review interval in minutes', '30');
    click('Save review settings');
    await screen.findByText('Review settings saved.');
    expect(save).toHaveBeenCalledWith({
      mode: 'automatic',
      provider: 'openai',
      model: 'gpt-5-mini',
      interval_minutes: 30,
    });
    change('Review mode', 'human');
    expect(screen.queryByLabelText('Improve provider')).toBeNull();
    click('Save review settings');
    await waitFor(() => expect(save).toHaveBeenLastCalledWith({ mode: 'human' }));
  });

  it('shows recoverable failure, empty and disabled states', async () => {
    const client = storyImproveClient('human', true);
    const records = vi
      .spyOn(client, 'improveRecords')
      .mockRejectedValueOnce(new Error('Machine unavailable'));
    render(<ImproveWorkspace client={client} />);
    await screen.findByText('Machine unavailable');
    click('Retry');
    await screen.findByText('No open issues in this project');
    expect(records).toHaveBeenCalledTimes(2);
    click('Review settings');
    change('Review mode', 'off');
    click('Save review settings');
    await screen.findByText('Review settings saved.');
    click('Back to issues');
    expect(screen.queryByRole('button', { name: 'New issue' })).toBeNull();
    expect(screen.getByText(/Improve is off/)).toBeTruthy();
  });

  it('requires an explicit discard before closing an unsaved issue', async () => {
    const close = vi.fn();
    vi.stubGlobal('fetch', storyImproveFetch());
    try {
      render(<ImproveDialog gateways={[{ url: 'http://gateway.example.com' }]} onClose={close} />);
      await screen.findByRole('option', { name: '/workspace/vis' });
      await waitFor(() => expect(screen.getByRole('button', { name: 'New issue' })).toBeEnabled());
      click('New issue');
      change('Issue content', 'Keep these notes');
      expect(screen.getByRole('button', { name: 'Review settings' })).toBeDisabled();
      click('Close Improve');
      expect(close).not.toHaveBeenCalled();
      click('Keep editing');
      expect(screen.getByLabelText('Issue content')).toHaveValue('Keep these notes');
      click('Close Improve');
      click('Discard draft');
      expect(close).toHaveBeenCalledOnce();
    } finally {
      vi.unstubAllGlobals();
    }
  });
});

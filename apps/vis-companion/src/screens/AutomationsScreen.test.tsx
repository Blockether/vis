/** @vitest-environment jsdom */
import { fireEvent, screen } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';
import { AutomationsPanel } from './settings/AutomationsPanel';
import { storyAutomationsClient } from '../dev/story-data';
import { renderOpenBands } from '../test-settings';

/** The shipping Settings band, open; its header + starts a new automation. */
const renderBand = (client = storyAutomationsClient(), gatewayUrl?: string) =>
  renderOpenBands(<AutomationsPanel client={client} gatewayUrl={gatewayUrl} />);
const click = (name: string) => fireEvent.click(screen.getByRole('button', { name }));
/** A form field by the start of its label; the label also holds the hint. */
const field = (name: RegExp) => screen.getByRole('textbox', { name });
const type = (name: RegExp, value: string) => fireEvent.change(field(name), { target: { value } });

async function openAutomation(name: string, client = storyAutomationsClient()) {
  renderBand(client, 'http://gateway.example.com/');
  fireEvent.click(await screen.findByText(name));
  expect(await screen.findByRole('button', { name: 'Back to automations' })).toBeEnabled();
  return client;
}

describe('Automations workspace', () => {
  it('lists automations with their state and last result', async () => {
    renderBand();
    expect(await screen.findByText('Morning summary')).toBeVisible();
    expect(screen.getByText('Paused · Webhook · GitHub · Last failed')).toBeVisible();
    expect(screen.queryByText(/Automations are off/)).toBeNull();
  });

  it('shows how to create the first automation without a feature switch', async () => {
    renderBand(storyAutomationsClient({ empty: true }));
    expect(await screen.findByText('No automations on this machine.')).toBeVisible();
    expect(screen.queryByText(/Automations are off/)).toBeNull();
    expect(screen.getByRole('button', { name: 'New automation' })).toBeEnabled();
  });

  it('pauses and runs an automation now', async () => {
    const client = await openAutomation('Morning summary');
    const update = vi.spyOn(client, 'updateAutomation');
    const start = vi.spyOn(client, 'runAutomation');
    click('Pause');
    expect(await screen.findByText('Automation paused.')).toBeVisible();
    expect(update).toHaveBeenCalledWith('auto-standup', { enabled: false });
    expect(await screen.findByRole('button', { name: 'Resume' })).toBeEnabled();
    click('Run now');
    expect(
      await screen.findByText('Run started. The answer goes to the target session.'),
    ).toBeVisible();
    expect(start).toHaveBeenCalledWith('auto-standup');
    expect(await screen.findByText(/^Queued · Manual · /)).toBeVisible();
  });

  it('shows the webhook address and failed runs with their reason', async () => {
    await openAutomation('Review new pull requests');
    expect(screen.getByText('http://gateway.example.com/v1/hooks/auto-review')).toBeVisible();
    expect(screen.getByText('Callback to https://gateway.example.com/review-results')).toBeVisible();
    expect(await screen.findByText('The target session does not exist.')).toBeVisible();
    expect(screen.getByText(/^Failed · Webhook · /)).toBeVisible();
  });

  it('asks before it replaces a secret and shows the new secret only once', async () => {
    const client = await openAutomation('Review new pull requests');
    const create = vi.spyOn(client, 'createAutomationSecret');
    click('Replace webhook secret');
    expect(screen.getByText(/The current webhook secret stops working at once/)).toBeVisible();
    expect(create).not.toHaveBeenCalled();
    click('Keep current secret');
    expect(screen.queryByText(/The current webhook secret stops working/)).toBeNull();
    click('Replace webhook secret');
    click('Replace webhook secret now');
    expect(await screen.findByText('story-webhook-secret-1')).toBeVisible();
    expect(create).toHaveBeenCalledWith('auto-review', 'webhook');
    expect(screen.getByText('Copy this secret now. Vis does not show it again.')).toBeVisible();
    click('Hide secret');
    expect(screen.queryByText('story-webhook-secret-1')).toBeNull();
  });

  it('creates a first callback secret without a confirmation', async () => {
    const client = await openAutomation('Review new pull requests');
    const create = vi.spyOn(client, 'createAutomationSecret');
    click('Create callback secret');
    expect(await screen.findByText('story-callback-secret-1')).toBeVisible();
    expect(create).toHaveBeenCalledWith('auto-review', 'callback');
    expect(await screen.findByRole('button', { name: 'Replace callback secret' })).toBeEnabled();
  });

  it('deletes an automation only after confirmation', async () => {
    const client = await openAutomation('Morning summary');
    const remove = vi.spyOn(client, 'deleteAutomation');
    click('Delete');
    click('Keep automation');
    expect(remove).not.toHaveBeenCalled();
    click('Delete');
    click('Delete automation');
    expect(await screen.findByText('Automation deleted.')).toBeVisible();
    expect(remove).toHaveBeenCalledWith('auto-standup');
    expect(await screen.findByText('Review new pull requests')).toBeVisible();
    expect(screen.queryByText('Morning summary')).toBeNull();
  });

  it('keeps a failed request visible and loads the list again on retry', async () => {
    const client = storyAutomationsClient();
    // The band reads the list first to show itself; the workspace read fails.
    const list = client.automations.bind(client);
    const read = vi
      .spyOn(client, 'automations')
      .mockImplementationOnce(list)
      .mockRejectedValueOnce(new Error('Machine unavailable.'));
    renderBand(client);
    expect(await screen.findByText('Machine unavailable.')).toBeVisible();
    click('Retry');
    expect(await screen.findByText('Morning summary')).toBeVisible();
    expect(read).toHaveBeenCalledTimes(3);
  });

  it('creates an automation step by step and opens it', async () => {
    const client = storyAutomationsClient();
    const create = vi.spyOn(client, 'createAutomation');
    renderBand(client);
    expect(await screen.findByText('Morning summary')).toBeVisible();
    click('New automation');
    expect(screen.getByRole('heading', { name: 'New automation' })).toBeVisible();
    expect(screen.getByText('What starts this automation?')).toBeVisible();
    expect(screen.getByRole('button', { name: '6. Review' })).toBeDisabled();
    expect(screen.queryByRole('button', { name: 'Create automation' })).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: /^Repeat at an interval/ }));
    expect(screen.getByText('When does it run?')).toBeVisible();
    expect(screen.getByRole('group', { name: 'Trigger' })).toBeVisible();
    click('Next');
    expect(screen.getByText('What does Vis do at each run?')).toBeVisible();
    click('Next');
    expect(screen.getByText('Give the automation a name.')).toBeVisible();
    expect(screen.getByText('What does Vis do at each run?')).toBeVisible();
    type(/^Name/, 'Nightly check');
    type(/^Prompt/, 'Check the build.');
    click('Next');
    expect(screen.getByText('Where does it run?')).toBeVisible();
    click('Next');
    expect(screen.getByText('How does Vis answer?')).toBeVisible();
    click('Next');
    expect(screen.getByText('Check the automation.')).toBeVisible();
    expect(screen.getByText('Every day')).toBeVisible();
    expect(screen.getByText('New session for each run')).toBeVisible();
    click('Create automation');
    expect(await screen.findByText('Automation created.')).toBeVisible();
    expect(create).toHaveBeenCalledWith({
      name: 'Nightly check',
      prompt: 'Check the build.',
      triggers: [{ kind: 'every', seconds: 86_400 }],
      target: { mode: 'new' },
      delivery: { push: true, callback: null },
      model: null,
      deliver_only: false,
    });
    expect(screen.getByRole('heading', { name: 'Nightly check' })).toBeVisible();
    expect(screen.getByText('Every day')).toBeVisible();
    click('Back to automations');
    expect(await screen.findByText('Nightly check')).toBeVisible();
    expect(screen.getByText('Morning summary')).toBeVisible();
  });

  it('asks for the webhook signature after a webhook start', async () => {
    renderBand();
    expect(await screen.findByText('Morning summary')).toBeVisible();
    click('New automation');
    fireEvent.click(screen.getByRole('button', { name: /^Run when a service sends an event/ }));
    expect(screen.getByText('Which webhook starts it?')).toBeVisible();
    const signatures = screen.getByRole('group', { name: 'Trigger signature' });
    expect(signatures).toBeVisible();
    fireEvent.click(screen.getByRole('button', { name: /^Token/ }));
    expect(screen.getByRole('button', { name: /^Token/ })).toHaveAttribute('aria-pressed', 'true');
    expect(screen.queryByRole('combobox', { name: 'Trigger kind' })).toBeNull();
  });

  it('puts the step actions on the question line and frames the choices', async () => {
    renderBand();
    expect(await screen.findByText('Morning summary')).toBeVisible();
    click('New automation');
    fireEvent.click(screen.getByRole('button', { name: /^Run when a service sends an event/ }));
    const question = screen.getByRole('heading', { name: 'Which webhook starts it?' });
    const actions = screen.getByRole('group', { name: 'Step actions' });
    expect(actions.parentElement).toBe(question.parentElement);
    for (const name of ['Next', 'Back', 'Cancel']) {
      expect(actions).toContainElement(screen.getByRole('button', { name }));
    }
    const signatures = screen.getByRole('group', { name: 'Trigger signature' });
    expect(signatures).toHaveClass('border', 'border-dialog-edge');
  });

  it('jumps to the step of a problem when you create the automation', async () => {
    const client = storyAutomationsClient();
    const create = vi.spyOn(client, 'createAutomation');
    renderBand(client);
    expect(await screen.findByText('Morning summary')).toBeVisible();
    click('New automation');
    fireEvent.click(screen.getByRole('button', { name: /^Repeat at an interval/ }));
    click('Next');
    type(/^Name/, 'Session check');
    type(/^Prompt/, 'Check it.');
    click('Next');
    click('Next');
    click('Next');
    click('Change session');
    fireEvent.click(screen.getByRole('button', { name: /^One existing session/ }));
    click('6. Review');
    expect(screen.queryByText('Give the session ID.')).toBeNull();
    click('Create automation');
    expect(screen.getByText('Give the session ID.')).toBeVisible();
    expect(screen.getByText('Where does it run?')).toBeVisible();
    expect(create).not.toHaveBeenCalled();
  });

  it('keeps the form and the typed values when the machine refuses them', async () => {
    const client = storyAutomationsClient();
    vi.spyOn(client, 'createAutomation').mockRejectedValueOnce(
      new Error('An automation store holds at most 256 automations'),
    );
    renderBand(client);
    expect(await screen.findByText('Morning summary')).toBeVisible();
    click('New automation');
    fireEvent.click(screen.getByRole('button', { name: /^Repeat at an interval/ }));
    click('Next');
    type(/^Name/, 'One more');
    type(/^Prompt/, 'Check it.');
    click('Next');
    click('Next');
    click('Next');
    click('Create automation');
    expect(
      await screen.findByText('An automation store holds at most 256 automations'),
    ).toBeVisible();
    click('Change name');
    expect(field(/^Name/)).toHaveValue('One more');
    click('Cancel');
    expect(screen.queryByRole('heading', { name: 'New automation' })).toBeNull();
    expect(screen.queryByText(/holds at most 256/)).toBeNull();
    expect(screen.getByText('Morning summary')).toBeVisible();
  });

  it('adds and removes triggers', async () => {
    renderBand();
    expect(await screen.findByText('Morning summary')).toBeVisible();
    click('New automation');
    fireEvent.click(screen.getByRole('button', { name: /^Run at set times/ }));
    expect(screen.queryByRole('button', { name: /^Remove trigger/ })).toBeNull();
    click('Add trigger');
    expect(screen.getByRole('group', { name: 'Trigger 2' })).toBeVisible();
    click('Remove trigger 2');
    expect(screen.getByRole('group', { name: 'Trigger' })).toBeVisible();
    expect(screen.queryByRole('button', { name: /^Remove trigger/ })).toBeNull();
  });

  it('saves only the changed fields of an automation', async () => {
    const client = await openAutomation('Morning summary');
    const update = vi.spyOn(client, 'updateAutomation');
    click('Edit');
    expect(screen.getByRole('heading', { name: 'Edit Morning summary' })).toBeVisible();
    expect(screen.getByText('Check the automation.')).toBeVisible();
    click('Save automation');
    expect(await screen.findByText('No changes to save.')).toBeVisible();
    expect(update).not.toHaveBeenCalled();
    click('Edit');
    click('Change prompt');
    type(/^Prompt/, 'List the open pull requests.');
    click('2. When');
    expect(field(/^Cron expression/)).toHaveValue('0 9 * * 1-5');
    type(/^Cron expression/, '30 7 * * 1-5');
    click('Save automation');
    expect(await screen.findByText('Automation saved.')).toBeVisible();
    expect(update).toHaveBeenCalledWith('auto-standup', {
      prompt: 'List the open pull requests.',
      triggers: [{ kind: 'cron', expression: '30 7 * * 1-5', timezone: 'Europe/Warsaw' }],
    });
    expect(screen.getByText('List the open pull requests.')).toBeVisible();
    expect(screen.getByText('Cron 30 7 * * 1-5 · Europe/Warsaw')).toBeVisible();
  });

  it('keeps the payload filters of a webhook trigger that you edit', async () => {
    const story = storyAutomationsClient();
    const filters = [{ field: 'action', equals: 'opened' }];
    const client = {
      ...story,
      automations: async () => {
        const page = await story.automations();
        return {
          ...page,
          automations: page.automations.map((automation) =>
            automation.webhook
              ? { ...automation, triggers: [{ ...automation.triggers[0], filters }] }
              : automation,
          ),
        };
      },
    };
    const update = vi.spyOn(client, 'updateAutomation');
    await openAutomation('Review new pull requests', client);
    click('Edit');
    click('Change when it runs');
    expect(
      screen.getByText('This trigger keeps its 1 payload filter. Ask Vis in a chat to change it.'),
    ).toBeVisible();
    type(/^Events/, 'pull_request, push');
    click('Save automation');
    expect(await screen.findByText('Automation saved.')).toBeVisible();
    expect(update).toHaveBeenCalledWith('auto-review', {
      triggers: [
        { kind: 'webhook', signature: 'github', events: ['pull_request', 'push'], filters },
      ],
    });
  });
});

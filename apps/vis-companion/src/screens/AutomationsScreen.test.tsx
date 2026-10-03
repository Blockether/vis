/** @vitest-environment jsdom */
import { fireEvent, render, screen } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';
import { AutomationsWorkspace } from './AutomationsScreen';
import { storyAutomationsClient } from '../dev/story-data';

const click = (name: string) => fireEvent.click(screen.getByRole('button', { name }));

async function openAutomation(name: string, client = storyAutomationsClient()) {
  render(<AutomationsWorkspace client={client} gatewayUrl="http://gateway.example.com/" />);
  fireEvent.click(await screen.findByText(name));
  expect(await screen.findByRole('button', { name: 'Back to automations' })).toBeEnabled();
  return client;
}

describe('Automations workspace', () => {
  it('lists automations with their state and last result', async () => {
    render(<AutomationsWorkspace client={storyAutomationsClient()} />);
    expect(await screen.findByText('2 automations')).toBeVisible();
    expect(screen.getByText('Morning summary')).toBeVisible();
    expect(screen.getByText('Paused · Webhook · GitHub · Last failed')).toBeVisible();
    expect(screen.queryByText(/Automations are off/)).toBeNull();
  });

  it('says when the machine stops every run, and how to start without automations', async () => {
    render(<AutomationsWorkspace client={storyAutomationsClient({ enabled: false, empty: true })} />);
    expect(await screen.findByText(/Automations are off on this machine/)).toBeVisible();
    expect(screen.getByText('No automations on this machine')).toBeVisible();
    expect(screen.getByText('0 automations')).toBeVisible();
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

  it('prefers the public relay address of a webhook', async () => {
    const story = storyAutomationsClient();
    const relayUrl = 'https://relay.example.com/hooks/AAAAAAAAAAAAAAAAAAAAAA/auto-review';
    const client = {
      ...story,
      automations: async () => {
        const page = await story.automations();
        return {
          ...page,
          automations: page.automations.map((automation) =>
            automation.webhook
              ? { ...automation, webhook: { ...automation.webhook, url: relayUrl } }
              : automation,
          ),
        };
      },
    };
    await openAutomation('Review new pull requests', client);
    expect(screen.getByText(relayUrl)).toBeVisible();
    expect(screen.queryByText('http://gateway.example.com/v1/hooks/auto-review')).toBeNull();
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
    expect(await screen.findByText('1 automation')).toBeVisible();
    expect(screen.queryByText('Morning summary')).toBeNull();
  });

  it('keeps a failed request visible and loads the list again on retry', async () => {
    const client = storyAutomationsClient();
    const read = vi
      .spyOn(client, 'automations')
      .mockRejectedValueOnce(new Error('Machine unavailable.'));
    render(<AutomationsWorkspace client={client} />);
    expect(await screen.findByText('Machine unavailable.')).toBeVisible();
    click('Retry');
    expect(await screen.findByText('2 automations')).toBeVisible();
    expect(read).toHaveBeenCalledTimes(2);
  });
});

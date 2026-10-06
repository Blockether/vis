import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent } from 'storybook/test';
import { useState } from 'react';
import { AutomationsPanel } from './settings/AutomationsPanel';
import { storyAutomationsClient } from '../dev/story-data';
import { OpenBands } from '../dev/OpenBands';

function Workspace({
  empty = false,
  failed = false,
}: {
  empty?: boolean;
  failed?: boolean;
}) {
  const [client] = useState(() => {
    const fixture = storyAutomationsClient({ empty });
    if (failed) {
      // The band reads the list once to show itself; every later read fails.
      const list = fixture.automations.bind(fixture);
      let isShown = false;
      fixture.automations = async (signal) => {
        if (isShown) throw new Error('Machine unavailable. Retry when it reconnects.');
        isShown = true;
        return list(signal);
      };
    }
    return fixture;
  });
  return (
    // The shipping Settings band, open as a machine shows it in Settings.
    <div className="min-h-dvh bg-ink sm:p-4">
      <div className="max-w-2xl border border-dialog-edge bg-panel">
        <OpenBands>
          <AutomationsPanel client={client} gatewayUrl="https://gateway.example.com" />
        </OpenBands>
      </div>
    </div>
  );
}

const meta = {
  title: 'Screens/Automations',
  component: Workspace,
  parameters: { layout: 'fullscreen' },
} satisfies Meta<typeof Workspace>;
export default meta;
type Story = StoryObj<typeof meta>;

export const List: Story = {
  play: async ({ canvas }) => {
    await expect(await canvas.findByText('Morning summary')).toBeVisible();
  },
};

export const Empty: Story = {
  args: { empty: true },
  play: async ({ canvas }) => {
    await expect(await canvas.findByText('No automations on this machine.')).toBeVisible();
  },
};

export const Failed: Story = {
  args: { failed: true },
  play: async ({ canvas }) => {
    await expect(
      await canvas.findByText('Machine unavailable. Retry when it reconnects.'),
    ).toBeVisible();
    await expect(canvas.getByRole('button', { name: 'Retry' })).toBeEnabled();
  },
};

export const Details: Story = {
  play: async ({ canvas }) => {
    await userEvent.click(await canvas.findByText('Review new pull requests'));
    await expect(
      await canvas.findByText('https://gateway.example.com/v1/hooks/auto-review'),
    ).toBeVisible();
    await expect(await canvas.findByText('The target session does not exist.')).toBeVisible();
  },
};

export const NewSecret: Story = {
  play: async (context) => {
    await Details.play!(context);
    await userEvent.click(context.canvas.getByRole('button', { name: 'Create callback secret' }));
    await expect(await context.canvas.findByText('story-callback-secret-1')).toBeVisible();
  },
};

export const NewForm: Story = {
  play: async ({ canvas }) => {
    await userEvent.click(await canvas.findByRole('button', { name: 'New automation' }));
    await expect(canvas.getByRole('heading', { name: 'New automation' })).toBeVisible();
    await expect(canvas.getByText('What starts this automation?')).toBeVisible();
    await expect(canvas.getByRole('button', { name: /^Repeat at an interval/ })).toBeEnabled();
  },
};

export const WebhookStep: Story = {
  play: async (context) => {
    await NewForm.play!(context);
    const { canvas } = context;
    await userEvent.click(canvas.getByRole('button', { name: /^Run when a service sends an event/ }));
    await expect(canvas.getByText('Which webhook starts it?')).toBeVisible();
    await expect(canvas.getByRole('group', { name: 'Trigger signature' })).toBeVisible();
  },
};

export const Create: Story = {
  play: async (context) => {
    await NewForm.play!(context);
    const { canvas } = context;
    await userEvent.click(canvas.getByRole('button', { name: /^Repeat at an interval/ }));
    await userEvent.click(canvas.getByRole('button', { name: 'Next: Task' }));
    await userEvent.type(canvas.getByRole('textbox', { name: /^Name/ }), 'Nightly check');
    await userEvent.type(canvas.getByRole('textbox', { name: /^Prompt/ }), 'Check the build.');
    for (let step = 0; step < 3; step += 1)
      await userEvent.click(canvas.getByRole('button', { name: /^Next: / }));
    await expect(canvas.getByText('Check the automation.')).toBeVisible();
    await userEvent.click(canvas.getByRole('button', { name: 'Create automation' }));
    await expect(await canvas.findByText('Automation created.')).toBeVisible();
    await expect(canvas.getByRole('heading', { name: 'Nightly check' })).toBeVisible();
  },
};

export const Edit: Story = {
  play: async ({ canvas }) => {
    await userEvent.click(await canvas.findByText('Morning summary'));
    await userEvent.click(await canvas.findByRole('button', { name: 'Edit' }));
    await expect(canvas.getByText('Check the automation.')).toBeVisible();
    await userEvent.click(canvas.getByRole('button', { name: 'Change prompt' }));
    const prompt = canvas.getByRole('textbox', { name: /^Prompt/ });
    await userEvent.clear(prompt);
    await userEvent.type(prompt, 'List the open pull requests.');
    await userEvent.click(canvas.getByRole('button', { name: 'Save automation' }));
    await expect(await canvas.findByText('Automation saved.')).toBeVisible();
    await expect(canvas.getByText('List the open pull requests.')).toBeVisible();
  },
};

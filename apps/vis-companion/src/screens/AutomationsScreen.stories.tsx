import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent } from 'storybook/test';
import { useState } from 'react';
import { AutomationsWorkspace } from './AutomationsScreen';
import { DialogFrame } from '../components/ui';
import { storyAutomationsClient } from '../dev/story-data';

function Workspace({
  enabled = true,
  empty = false,
  failed = false,
}: {
  enabled?: boolean;
  empty?: boolean;
  failed?: boolean;
}) {
  const [client] = useState(() => {
    const fixture = storyAutomationsClient({ enabled, empty });
    if (failed)
      fixture.automations = async () => {
        throw new Error('Machine unavailable. Retry when it reconnects.');
      };
    return fixture;
  });
  return (
    <div className="flex h-dvh flex-col bg-ink sm:p-4">
      <DialogFrame title="Automations" subtitle="Prompts that run on a schedule or a webhook">
        <AutomationsWorkspace client={client} gatewayUrl="https://gateway.example.com" />
      </DialogFrame>
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
    await expect(await canvas.findByText('2 automations')).toBeVisible();
    await expect(canvas.getByText('Morning summary')).toBeVisible();
  },
};

export const Off: Story = {
  args: { enabled: false },
  play: async ({ canvas }) => {
    await expect(await canvas.findByText(/Automations are off on this machine/)).toBeVisible();
  },
};

export const Empty: Story = {
  args: { empty: true },
  play: async ({ canvas }) => {
    await expect(await canvas.findByText('No automations on this machine')).toBeVisible();
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
    await expect(canvas.getByRole('group', { name: 'Trigger' })).toBeVisible();
    await expect(canvas.getByRole('button', { name: 'Create automation' })).toBeEnabled();
  },
};

export const Create: Story = {
  play: async (context) => {
    await NewForm.play!(context);
    const { canvas } = context;
    await userEvent.type(canvas.getByRole('textbox', { name: /^Name/ }), 'Nightly check');
    await userEvent.type(canvas.getByRole('textbox', { name: /^Prompt/ }), 'Check the build.');
    await userEvent.click(canvas.getByRole('button', { name: 'Create automation' }));
    await expect(await canvas.findByText('Automation created.')).toBeVisible();
    await expect(canvas.getByRole('heading', { name: 'Nightly check' })).toBeVisible();
  },
};

export const Edit: Story = {
  play: async ({ canvas }) => {
    await userEvent.click(await canvas.findByText('Morning summary'));
    await userEvent.click(await canvas.findByRole('button', { name: 'Edit' }));
    const prompt = canvas.getByRole('textbox', { name: /^Prompt/ });
    await userEvent.clear(prompt);
    await userEvent.type(prompt, 'List the open pull requests.');
    await userEvent.click(canvas.getByRole('button', { name: 'Save automation' }));
    await expect(await canvas.findByText('Automation saved.')).toBeVisible();
    await expect(canvas.getByText('List the open pull requests.')).toBeVisible();
  },
};

import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent } from 'storybook/test';
import { useState } from 'react';
import { AutomationsPanel } from './AutomationsPanel';
import { storyAutomationsClient } from '../../dev/story-data';

/** The shipping Settings band; only its gateway boundary uses fixture data. */
function AutomationsPanelPreview({ available = true }: { available?: boolean }) {
  const [client] = useState(() => {
    const fixture = storyAutomationsClient();
    if (!available)
      fixture.automations = async () => {
        throw new Error('Not found');
      };
    return fixture;
  });
  return (
    <div className="max-w-xl border border-dialog-edge bg-panel">
      <AutomationsPanel client={client} gatewayUrl="https://gateway.example.com/" />
      <p className="p-3 font-mono text-ui text-dialog-hint">Next settings band</p>
    </div>
  );
}

const meta = {
  title: 'Settings/Automations',
  component: AutomationsPanelPreview,
  parameters: { layout: 'padded' },
} satisfies Meta<typeof AutomationsPanelPreview>;
export default meta;
type Story = StoryObj<typeof meta>;

export const Closed: Story = {
  play: async ({ canvas }) => {
    await expect(await canvas.findByRole('button', { name: 'Show automations' })).toBeVisible();
  },
};

export const OpenList: Story = {
  play: async ({ canvas }) => {
    await userEvent.click(await canvas.findByRole('button', { name: 'Show automations' }));
    await expect(await canvas.findByText('Morning summary')).toBeVisible();
  },
};

export const NewAutomationWizard: Story = {
  play: async ({ canvas }) => {
    await userEvent.click(await canvas.findByRole('button', { name: 'Show automations' }));
    await userEvent.click(await canvas.findByRole('button', { name: 'New automation' }));
    await expect(canvas.getByText('What starts this automation?')).toBeVisible();
  },
};

// Only a machine that does not answer the request (an older or unavailable gateway)
// leaves Settings without the band.
export const Hidden: Story = {
  args: { available: false },
  play: async ({ canvas }) => {
    await expect(canvas.queryByRole('button', { name: 'Show automations' })).not.toBeInTheDocument();
  },
};

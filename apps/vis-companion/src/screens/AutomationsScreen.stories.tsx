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
  title: 'Settings/Automations/Screen',
  component: Workspace,
  parameters: { layout: 'fullscreen' },
} satisfies Meta<typeof Workspace>;
export default meta;
type Story = StoryObj<typeof meta>;

export const List: Story = {
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

export const NewForm: Story = {
  play: async ({ canvas }) => {
    await userEvent.click(await canvas.findByRole('button', { name: 'New automation' }));
    await expect(canvas.getByRole('heading', { name: 'New automation' })).toBeVisible();
    await expect(canvas.getByText('What starts this automation?')).toBeVisible();
    await expect(canvas.getByRole('button', { name: /^Repeat at an interval/ })).toBeEnabled();
  },
};

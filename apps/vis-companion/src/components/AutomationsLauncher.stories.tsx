import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';
import { useEffect, useState } from 'react';
import { Header } from '../App';
import { AutomationsLauncher } from './AutomationsLauncher';
import { storyAutomationsFetch, STORY_GATEWAYS } from '../dev/story-data';

/** The shipping global header; only its gateway HTTP boundary uses fixture data. */
export function AutomationsHeaderPreview({
  enabled = true,
  empty = false,
}: {
  enabled?: boolean;
  empty?: boolean;
}) {
  const [ready, setReady] = useState(false);
  useEffect(() => {
    const previous = globalThis.fetch;
    globalThis.fetch = storyAutomationsFetch({ enabled, empty });
    setReady(true);
    return () => {
      globalThis.fetch = previous;
    };
  }, [enabled, empty]);
  return ready ? (
    <Header
      onSearch={fn()}
      onAppSettings={fn()}
      automations={
        <AutomationsLauncher gateways={[STORY_GATEWAYS[0]]} primaryUrl={STORY_GATEWAYS[0].url} />
      }
    />
  ) : null;
}
const meta = {
  title: 'Navigation/Automations entry',
  component: AutomationsHeaderPreview,
  parameters: { layout: 'fullscreen' },
} satisfies Meta<typeof AutomationsHeaderPreview>;
export default meta;
type Story = StoryObj<typeof meta>;
export const Enabled: Story = {
  play: async ({ canvas }) => {
    await expect(await canvas.findByRole('button', { name: 'Open automations' })).toBeVisible();
  },
};
export const OpenList: Story = {
  play: async ({ canvas, canvasElement }) => {
    await userEvent.click(await canvas.findByRole('button', { name: 'Open automations' }));
    const page = within(canvasElement.ownerDocument.body);
    await expect(await page.findByRole('dialog', { name: 'Automations' })).toBeVisible();
    await expect(await page.findByText('Morning summary')).toBeVisible();
  },
};
export const Hidden: Story = {
  args: { enabled: false, empty: true },
  play: async ({ canvas }) => {
    await expect(canvas.queryByRole('button', { name: 'Open automations' })).not.toBeInTheDocument();
  },
};

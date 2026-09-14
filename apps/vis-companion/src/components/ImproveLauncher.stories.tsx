import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';
import { useEffect, useState } from 'react';
import { Header } from '../App';
import { ImproveLauncher } from './ImproveLauncher';
import { storyImproveFetch, STORY_GATEWAYS } from '../dev/story-data';
import type { ImproveMode } from '../lib/improve';

/** The shipping global header; only its gateway HTTP boundary uses fixture data. */
export function ImproveHeaderPreview({ mode = 'human' }: { mode?: ImproveMode }) {
  const [ready, setReady] = useState(false);
  useEffect(() => {
    const previous = globalThis.fetch;
    globalThis.fetch = storyImproveFetch(mode);
    setReady(true);
    return () => {
      globalThis.fetch = previous;
    };
  }, [mode]);
  return ready ? (
    <Header
      query=""
      onQuery={fn()}
      isSearching={false}
      onSearch={fn()}
      onCloseSearch={fn()}
      onAppSettings={fn()}
      improve={
        <ImproveLauncher gateways={[STORY_GATEWAYS[0]]} primaryUrl={STORY_GATEWAYS[0].url} />
      }
    />
  ) : null;
}
const meta = {
  title: 'Navigation/Improve entry',
  component: ImproveHeaderPreview,
  parameters: { layout: 'fullscreen' },
} satisfies Meta<typeof ImproveHeaderPreview>;
export default meta;
type Story = StoryObj<typeof meta>;
export const Enabled: Story = {
  play: async ({ canvas }) => {
    await expect(await canvas.findByRole('button', { name: 'Open Improve' })).toBeVisible();
  },
};
export const OpenWorkspace: Story = {
  play: async ({ canvas, canvasElement }) => {
    await userEvent.click(await canvas.findByRole('button', { name: 'Open Improve' }));
    const page = within(canvasElement.ownerDocument.body);
    await expect(await page.findByRole('dialog', { name: 'Improve' })).toBeVisible();
    await expect(await page.findByLabelText('Improve project')).toBeVisible();
  },
};
export const Disabled: Story = {
  args: { mode: 'off' },
  play: async ({ canvas }) => {
    await expect(canvas.queryByRole('button', { name: 'Open Improve' })).not.toBeInTheDocument();
  },
};

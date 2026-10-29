import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect } from 'storybook/test';
import { STORY_PROVIDERS, STORY_DEVICE_AUTH, storyProviderAuth } from '../dev/story-data';
import { ProviderRows } from './ProviderAuth';

/**
 * THE ACCOUNTS, AS A FLEET OF MACHINE-SIZED SLABS.
 *
 * A provider is signed in or it is not, it holds a rank, and it can be dropped —
 * the same row a machine gets, slid the same way. Only the collapsed paint is
 * drawn here, which is the whole first impression: opening one asks the gateway
 * for a fresh verdict, and a gallery has no gateway.
 */
const meta = {
  title: 'Settings/Providers',
  component: ProviderRows,
  parameters: { layout: 'padded' },
} satisfies Meta<typeof ProviderRows>;

export default meta;

type Story = StoryObj<typeof meta>;

/** The verdicts told apart: verified default, degraded fallback, never signed in. */
export const Fleet: Story = {
  args: { auth: storyProviderAuth() },
};

/** One provider, with no credential on this machine yet. */
export const SignedOut: Story = {
  args: { auth: storyProviderAuth(STORY_PROVIDERS.slice(2)) },
};

export const DeviceAuthorization: Story = {
  args: { auth: STORY_DEVICE_AUTH },
  play: async ({ canvas }) => {
    await expect(canvas.getByText('ABCD-EFGH')).toBeVisible();
    await expect(canvas.getByRole('button', { name: 'Open sign-in page again' })).toBeVisible();
    await expect(canvas.queryByLabelText('Paste the final redirect URL')).not.toBeInTheDocument();
    await expect(canvas.queryByRole('button', { name: 'Finish sign-in' })).not.toBeInTheDocument();
  },
};

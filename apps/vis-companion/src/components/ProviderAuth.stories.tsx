import type { Meta, StoryObj } from '@storybook/react-vite';

import { STORY_PROVIDERS, storyProviderAuth } from '../dev/story-data';
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
  title: 'Components/Provider rows',
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

/** Nobody has asked the gateway yet — `null` is not the same as "none". */
export const Unasked: Story = {
  args: { auth: storyProviderAuth(null) },
};

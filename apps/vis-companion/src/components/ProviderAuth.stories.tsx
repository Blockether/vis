import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent } from 'storybook/test';
import { STORY_PROVIDERS, STORY_BROWSER_AUTH, STORY_DEVICE_AUTH, STORY_APP_AUTH, storyProviderAuth } from '../dev/story-data';
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

/** An active gateway loopback flow; the manual path is an explicit fallback. */
export const BrowserReturn: Story = {
  args: { auth: STORY_BROWSER_AUTH },
  play: async ({ canvas }) => {
    await expect(canvas.getByText('Waiting for authorization…')).toBeVisible();
    await expect(canvas.queryByLabelText('Paste the final redirect URL')).not.toBeInTheDocument();
    await expect(canvas.getByRole('button', { name: 'Cancel' })).toBeVisible();
  },
};

export const ManualCallbackFallback: Story = {
  args: { auth: STORY_BROWSER_AUTH },
  play: async ({ canvas }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Use manual callback' }));
    await expect(canvas.getByLabelText('Paste the final redirect URL')).toBeVisible();
    await expect(canvas.getByRole('button', { name: 'Finish sign-in' })).toBeDisabled();
  },
};

export const DeviceAuthorization: Story = {
  args: { auth: STORY_DEVICE_AUTH },
  play: async ({ canvas }) => {
    await expect(canvas.getByText('ABCD-EFGH')).toBeVisible();
    await expect(canvas.getByText('Waiting for authorization…')).toBeVisible();
    await expect(canvas.getByRole('button', { name: 'Open sign-in page again' })).toBeVisible();
    await expect(canvas.queryByLabelText('Paste the final redirect URL')).not.toBeInTheDocument();
    await expect(canvas.queryByRole('button', { name: 'Finish sign-in' })).not.toBeInTheDocument();
  },
 };

export const AppReturn: Story = {
  args: { auth: STORY_APP_AUTH },
  play: async ({ canvas }) => {
    await expect(canvas.getByText('Waiting for authorization…')).toBeVisible();
    await expect(canvas.getByText('Approve sign-in in the browser. The callback opens Vis and sign-in finishes automatically.')).toBeVisible();
    await expect(canvas.queryByLabelText('Paste the final redirect URL')).not.toBeInTheDocument();
    await expect(canvas.getByRole('button', { name: 'Cancel' })).toBeVisible();
  },
 };

/** Shipped native-loopback presentation; this fixture does not simulate provider consent. */
export const NativeLoopbackReturn: Story = {
  args: { auth: { ...STORY_APP_AUTH, openSignInPage: fn(), flow: { ...STORY_APP_AUTH.flow!,
    callback_mode: 'loopback', redirect_uri: 'http://localhost:53692/callback',
    url: 'https://gateway.example.com/authorize?state=test-state&redirect_uri=http%3A%2F%2Flocalhost%3A53692%2Fcallback',
    instructions: ['Approve sign-in in the browser. Vis receives the callback on this device and finishes automatically.'],
  } } },
  play: async ({ canvas, args }) => {
    await expect(canvas.queryByLabelText('Paste the final redirect URL')).not.toBeInTheDocument();
    await userEvent.click(canvas.getByRole('button', { name: 'Open sign-in page again' }));
    await expect(args.auth.openSignInPage).toHaveBeenCalledOnce();
  },
};

/** Production provider row; only the gateway callbacks and account report are fixtures. */
export const CodexLimits: Story = {
  args: { auth: {
    ...storyProviderAuth([{
      id: 'openai-codex', label: 'OpenAI Codex (ChatGPT OAuth)',
      models: ['gpt-5'], is_default: true, default_model: 'gpt-5', is_fallback: false, fallback_model: null,
      status: { is_authenticated: true, auth_state: 'verified', source: 'auth-file' },
      limits: { status: 'ok', dynamic: {
        limits: [{ label: 'Codex 5h quota (%)', limit: 100, remaining: 100 }, { label: 'Codex 7d quota (%)', limit: 100, remaining: 76 }],
        reset_credits: { status: 'ok', available_count: 3, account_id: 'account-1' },
      } },
    }]),
    recheck: fn(async () => {}),
    resetLimits: fn(async () => 'reset' as const),
  } },
  play: async ({ canvas, args }) => {
    await userEvent.click(canvas.getByRole('button', { name: /OpenAI Codex/i, expanded: false }));
    await expect(canvas.getByText('3 resets available')).toBeVisible();
    await expect(canvas.queryByRole('button', { name: 'Refresh limits' })).not.toBeInTheDocument();
    await userEvent.click(canvas.getByRole('button', { name: 'Refresh limits for OpenAI Codex (ChatGPT OAuth)' }));
    await expect(args.auth.recheck).toHaveBeenCalledTimes(2);
    await expect(args.auth.resetLimits).not.toHaveBeenCalled();
  },
};

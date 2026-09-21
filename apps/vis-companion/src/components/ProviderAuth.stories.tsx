import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';
import {
  STORY_PROVIDERS,
  STORY_BROWSER_AUTH,
  STORY_DEVICE_AUTH,
  STORY_APP_AUTH,
  storyProviderAuth,
} from '../dev/story-data';
import type { ProviderPreset } from '../lib/types';
import { AddProviderPicker, ProviderRows } from './ProviderAuth';

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
  play: async ({ args, canvas }) => {
    for (const provider of args.auth.providers!) {
      const name = canvas.getByText(provider.label, { exact: true });
      await name.ownerDocument.fonts.ready;
      await expect(getComputedStyle(name).fontSize).toBe('13px');
      await expect(getComputedStyle(name).lineHeight).toBe('20px');
      await expect(getComputedStyle(name).fontWeight).toBe('500');
      // Names lead; routing status is readable metadata, not another bold heading.
      const label = provider.is_default ? 'Default' : provider.is_fallback ? 'Fallback' : null;
      if (label) {
        const status = within(name.closest('button')!).getByText(label, { exact: true });
        const style = getComputedStyle(status);
        await expect(style.fontSize).toBe('11px');
        await expect(style.lineHeight).toBe('16px');
        await expect(style.fontWeight).toBe('400');
        await expect(style.textTransform).toBe('none');
        await expect(style.letterSpacing).toBe('normal');
      }
    }
  },
};

export const FleetPointer: Story = {
  ...Fleet,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

/** Extension-owned providers keep their routing controls but cannot be removed. */
export const ManagedProvider: Story = {
  args: {
    auth: storyProviderAuth(
      STORY_PROVIDERS.map((provider, index) => ({
        ...provider,
        is_managed: index === 0,
      })),
    ),
  },
  play: async ({ canvas, args }) => {
    const [managed, ordinary] = args.auth.providers!;
    await expect(
      canvas.queryByRole('button', {
        name: `Sign out of ${managed!.label} and remove it from this machine`,
        hidden: true,
      }),
    ).not.toBeInTheDocument();
    await expect(
      canvas.getByRole('button', {
        name: `Run every turn on ${managed!.label}`,
        hidden: true,
      }),
    ).toBeInTheDocument();
    await expect(
      canvas.getByRole('button', {
        name: `Sign out of ${ordinary!.label} and remove it from this machine`,
        hidden: true,
      }),
    ).toBeInTheDocument();
  },
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
    await expect(
      canvas.getByText(
        'Approve sign-in in the browser. The callback opens Vis and sign-in finishes automatically.',
      ),
    ).toBeVisible();
    await expect(canvas.queryByLabelText('Paste the final redirect URL')).not.toBeInTheDocument();
    await expect(canvas.getByRole('button', { name: 'Cancel' })).toBeVisible();
  },
};

/** Shipped native-loopback presentation; this fixture does not simulate provider consent. */
export const NativeLoopbackReturn: Story = {
  args: {
    auth: {
      ...STORY_APP_AUTH,
      openSignInPage: fn(),
      flow: {
        ...STORY_APP_AUTH.flow!,
        callback_mode: 'loopback',
        redirect_uri: 'http://localhost:53692/callback',
        url: 'https://gateway.example.com/authorize?state=test-state&redirect_uri=http%3A%2F%2Flocalhost%3A53692%2Fcallback',
        instructions: [
          'Approve sign-in in the browser. Vis receives the callback on this device and finishes automatically.',
        ],
      },
    },
  },
  play: async ({ canvas, args }) => {
    await expect(canvas.queryByLabelText('Paste the final redirect URL')).not.toBeInTheDocument();
    await userEvent.click(canvas.getByRole('button', { name: 'Open sign-in page again' }));
    await expect(args.auth.openSignInPage).toHaveBeenCalledOnce();
  },
};

/** Production provider row; only the gateway callbacks and account report are fixtures. */
export const CodexLimits: Story = {
  args: {
    auth: {
      ...storyProviderAuth([
        {
          id: 'openai-codex',
          label: 'OpenAI Codex (ChatGPT OAuth)',
          is_managed: false,
          models: ['gpt-5'],
          is_default: true,
          default_model: 'gpt-5',
          is_fallback: false,
          fallback_model: null,
          status: { is_authenticated: true, auth_state: 'verified', source: 'auth-file' },
          limits: {
            status: 'ok',
            dynamic: {
              limits: [
                { label: 'Codex 5h quota (%)', limit: 100, remaining: 100 },
                { label: 'Codex 7d quota (%)', limit: 100, remaining: 76 },
              ],
              reset_credits: { status: 'ok', available_count: 3, account_id: 'account-1' },
            },
          },
        },
      ]),
      recheck: fn(async () => {}),
      resetLimits: fn(async () => 'reset' as const),
    },
  },
  play: async ({ canvas, args }) => {
    await userEvent.click(canvas.getByRole('button', { name: /OpenAI Codex/i, expanded: false }));
    await expect(canvas.getByText('3 resets available')).toBeVisible();
    await expect(canvas.queryByRole('button', { name: 'Refresh limits' })).not.toBeInTheDocument();
    await userEvent.click(
      canvas.getByRole('button', { name: 'Refresh limits for OpenAI Codex (ChatGPT OAuth)' }),
    );
    await expect(args.auth.recheck).toHaveBeenCalledTimes(2);
    await expect(args.auth.resetLimits).not.toHaveBeenCalled();
  },
};

/** What a machine with three accounts still has left, as the gateway reports it. */
const ADDABLE_PRESETS: ProviderPreset[] = [
  { id: 'openai', label: 'OpenAI', auth_kind: 'api-key', is_local: false, models: [] },
  { id: 'github-copilot', label: 'GitHub Copilot', auth_kind: 'oauth', is_local: false, models: [] },
  {
    id: 'ollama',
    label: 'Ollama',
    auth_kind: 'none',
    is_local: true,
    base_url: 'http://localhost:11434/v1',
    models: [],
  },
];

/**
 * WHAT THIS MACHINE CAN STILL BE GIVEN, open as a band of the Providers panel.
 *
 * The settings dialog used to stack a second dialog on itself to show this list.
 * It stands under the verb that opens it instead, one step of paper up from the
 * accounts it is about to join, and at a list's compact height rather than a
 * sheet's.
 */
export const AddProviderBand: Story = {
  args: { auth: { ...storyProviderAuth(), presets: ADDABLE_PRESETS } },
  render: (args) => <AddProviderPicker auth={args.auth} onClose={() => {}} />,
};

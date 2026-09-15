import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, mocked, userEvent, waitFor, within } from 'storybook/test';
import type { ProviderResetOutcome } from '../lib/types';
import { ProviderLimitReset } from './ProviderLimitReset';
import { SettingsPanel } from '../screens/settings/SettingsLayout';

const meta = {
  title: 'Components/Provider limit reset',
  component: ProviderLimitReset,
  parameters: { layout: 'fullscreen' },
  render: (args) => (
    <div className="p-4">
      <ProviderLimitReset {...args} />
    </div>
  ),
  args: {
    credits: {
      status: 'ok',
      account_id: '00000000-0000-4000-8000-000000000001',
      available_count: 2,
    },
    onConsume: fn(async (): Promise<ProviderResetOutcome> => 'reset'),
  },
} satisfies Meta<typeof ProviderLimitReset>;
export default meta;
type Story = StoryObj<typeof meta>;

export const Available: Story = {
  play: async ({ canvas }) => {
    const group = canvas.getByRole('group', { name: 'Codex limit resets' });
    const button = canvas.getByRole('button', { name: 'Reset limits…' });
    const status = canvas.getByRole('status');
    await expect(button.getBoundingClientRect().right).toBeCloseTo(
      group.getBoundingClientRect().right,
      0,
    );
    await expect(status.getBoundingClientRect().left).toBeCloseTo(
      group.getBoundingClientRect().left,
      0,
    );
    const statusBox = status.getBoundingClientRect();
    const buttonBox = button.getBoundingClientRect();
    await expect(statusBox.top + statusBox.height / 2).toBeCloseTo(
      buttonBox.top + buttonBox.height / 2,
      0,
    );
  },
};
export const Confirmation: Story = {
  play: async ({ canvas, args, canvasElement }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Reset limits…' }));
    const page = within(canvasElement.ownerDocument.body);
    await expect(page.getByRole('button', { name: 'Cancel' })).toHaveFocus();
    await expect(page.getByText(/all devices and sessions/)).toBeVisible();
    await expect(args.onConsume).not.toHaveBeenCalled();
  },
};
export const NoResets: Story = {
  args: { credits: { status: 'ok', account_id: 'account-1', available_count: 0 } },
};
export const Loading: Story = { args: { isChecking: true } };
export const GatewayMissing: Story = { args: { credits: undefined } };
export const Unknown: Story = { args: { credits: { status: 'error' } } };
export const Unsupported: Story = { args: { credits: { status: 'unsupported' } } };
export const Uncertain: Story = {
  args: {
    hasPending: true,
    onConsume: fn(async () => {
      throw new Error('Result unknown');
    }),
  },
  play: async ({ canvas, canvasElement }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Check reset result…' }));
    const page = within(canvasElement.ownerDocument.body);
    await userEvent.click(page.getByRole('button', { name: 'Retry same request' }));
    await expect(await page.findByRole('alert')).toHaveTextContent('Reset could not be confirmed');
  },
};
export const Success: Story = {
  play: async ({ canvas, args, canvasElement }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Reset limits…' }));
    const page = within(canvasElement.ownerDocument.body);
    await userEvent.click(page.getByRole('button', { name: 'Use 1 reset' }));
    await expect(args.onConsume).toHaveBeenCalledOnce();
    await expect(args.onConsume).toHaveBeenCalledWith('00000000-0000-4000-8000-000000000001');
    await expect(
      await page.findByText('Limits reset. Your task has not been resent.'),
    ).toBeVisible();
  },
};

// Reset confirmation must not push the settings below it down and pull them back up.
export const StableSettingsLayout: Story = {
  render: (args) => (
    <div className="divide-y divide-dialog-edge">
      <SettingsPanel title="Providers">
        <div className="p-3">
          <ProviderLimitReset {...args} />
        </div>
      </SettingsPanel>
      <SettingsPanel title="Notifications">
        <p className="p-3 font-mono text-ui">Notify when a task needs your attention.</p>
      </SettingsPanel>
    </div>
  ),
  play: async ({ canvas, args }) => {
    const followingPanel = canvas.getByRole('heading', { name: 'Notifications' });
    await followingPanel.ownerDocument.fonts.ready;
    const page = within(followingPanel.ownerDocument.body);
    const top = followingPanel.getBoundingClientRect().top;
    const trigger = canvas.getByRole('button', { name: 'Reset limits…' });
    await userEvent.click(trigger);
    await expect(followingPanel.getBoundingClientRect().top).toBeCloseTo(top, 0);
    await userEvent.click(page.getByRole('button', { name: 'Cancel' }));
    await expect(trigger).toHaveFocus();
    await expect(followingPanel.getBoundingClientRect().top).toBeCloseTo(top, 0);

    let finish!: (outcome: ProviderResetOutcome) => void;
    mocked(args.onConsume).mockImplementationOnce(
      () =>
        new Promise<ProviderResetOutcome>((resolve) => {
          finish = resolve;
        }),
    );
    await userEvent.click(trigger);
    await userEvent.click(page.getByRole('button', { name: 'Use 1 reset' }));
    await expect(page.getByRole('button', { name: 'Checking result…' })).toBeDisabled();
    await expect(followingPanel.getBoundingClientRect().top).toBeCloseTo(top, 0);
    await userEvent.keyboard('{Escape}');
    await expect(page.getByRole('dialog', { name: 'Reset limits' })).toBeVisible();
    finish('reset');
    await expect(
      await page.findByText('Limits reset. Your task has not been resent.'),
    ).toBeVisible();
    await waitFor(() => expect(page.getByRole('button', { name: 'Done' })).toHaveFocus());
    await expect(followingPanel.getBoundingClientRect().top).toBeCloseTo(top, 0);
    await userEvent.click(page.getByRole('button', { name: 'Done' }));
    await expect(trigger).toHaveFocus();
    await expect(followingPanel.getBoundingClientRect().top).toBeCloseTo(top, 0);

    mocked(args.onConsume).mockRejectedValueOnce(new Error('Result unknown'));
    await userEvent.click(trigger);
    await userEvent.click(page.getByRole('button', { name: 'Use 1 reset' }));
    await expect(await page.findByRole('alert')).toHaveTextContent('Reset could not be confirmed');
    await waitFor(() => expect(page.getByRole('button', { name: 'Done' })).toHaveFocus());
    await expect(followingPanel.getBoundingClientRect().top).toBeCloseTo(top, 0);
    await userEvent.click(page.getByRole('button', { name: 'Done' }));
    await expect(followingPanel.getBoundingClientRect().top).toBeCloseTo(top, 0);
  },
};

export const StableSettingsLayoutSmallPhone: Story = {
  ...StableSettingsLayout,
  globals: { viewport: { value: 'phoneSmall', isRotated: false } },
};

export const StableSettingsLayoutDesktop: Story = {
  ...StableSettingsLayout,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

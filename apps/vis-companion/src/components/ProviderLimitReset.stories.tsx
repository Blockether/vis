import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent } from 'storybook/test';
import { ProviderLimitReset } from './ProviderLimitReset';

const meta = {
  title: 'Components/Provider limit reset',
  component: ProviderLimitReset,
  parameters: { layout: 'fullscreen' },
  render: args => <div className="p-4"><ProviderLimitReset {...args} /></div>,
  args: {
    credits: { status: 'ok', account_id: '00000000-0000-4000-8000-000000000001', available_count: 2 },
    onConsume: fn(async () => 'reset' as const),
  },
} satisfies Meta<typeof ProviderLimitReset>;
export default meta;
type Story = StoryObj<typeof meta>;

export const Available: Story = {
  play: async ({ canvas }) => {
    const group = canvas.getByRole('group', { name: 'Codex limit resets' });
    const button = canvas.getByRole('button', { name: 'Reset limits…' });
    const status = canvas.getByRole('status');
    await expect(button.getBoundingClientRect().right).toBeCloseTo(group.getBoundingClientRect().right, 0);
    await expect(status.getBoundingClientRect().left).toBeCloseTo(group.getBoundingClientRect().left, 0);
  },
};
export const Confirmation: Story = {
  play: async ({ canvas, args }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Reset limits…' }));
    await expect(canvas.getByRole('button', { name: 'Cancel' })).toHaveFocus();
    await expect(canvas.getByText(/all devices and sessions/)).toBeVisible();
    await expect(args.onConsume).not.toHaveBeenCalled();
  },
};
export const NoResets: Story = { args: { credits: { status: 'ok', account_id: 'account-1', available_count: 0 } } };
export const Loading: Story = { args: { isChecking: true } };
export const GatewayMissing: Story = { args: { credits: undefined } };
export const Unknown: Story = { args: { credits: { status: 'error' } } };
export const Unsupported: Story = { args: { credits: { status: 'unsupported' } } };
export const Uncertain: Story = {
  args: { hasPending: true, onConsume: fn(async () => { throw new Error('Result unknown'); }) },
  play: async ({ canvas }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Check reset result…' }));
    await userEvent.click(canvas.getByRole('button', { name: 'Retry same request' }));
    await expect(await canvas.findByRole('alert')).toHaveTextContent('Reset could not be confirmed');
  },
};
export const Success: Story = {
  play: async ({ canvas, args }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Reset limits…' }));
    await userEvent.click(canvas.getByRole('button', { name: 'Use 1 reset' }));
    await expect(args.onConsume).toHaveBeenCalledOnce();
    await expect(args.onConsume).toHaveBeenCalledWith('00000000-0000-4000-8000-000000000001');
    await expect(await canvas.findByText('Limits reset. Your task has not been resent.')).toBeVisible();
  },
};

import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';
import type { ProviderResetOutcome } from '../lib/types';
import { ProviderLimitReset } from './ProviderLimitReset';
import { OpenBands } from '../dev/OpenBands';

const meta = {
  title: 'Settings/Providers/Limit reset',
  component: ProviderLimitReset,
  // These stories show what the bands hold, so every band starts open.
  decorators: [
    (Story) => (
      <OpenBands>
        <Story />
      </OpenBands>
    ),
  ],
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

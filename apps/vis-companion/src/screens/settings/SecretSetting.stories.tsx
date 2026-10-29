import { useState } from 'react';
import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent, waitFor, within } from 'storybook/test';
import type { Toggle } from '../../lib/types';
import { SecretSetting } from './MachineSettings';

const TOKEN: Toggle = {
  id: 'graphify_kb_token',
  label: 'Knowledge base token',
  description: 'Sent only to the configured knowledge base server.',
  type: 'string',
  editor: 'secret',
  is_set: false,
  max_length: 4096,
  source: 'default',
  scope: 'global',
  is_override: false,
};

const meta = {
  title: 'Settings/Fields/Secret',
  component: SecretSetting,
  parameters: { layout: 'padded' },
  args: { toggle: TOKEN, busy: false, onSave: async () => true },
  render: function Secret(args) {
    const [isSet, setIsSet] = useState(args.toggle.is_set === true);
    return (
      <div className="divide-y divide-dialog-edge border border-dialog-edge">
        <SecretSetting
          key={String(isSet)}
          {...args}
          toggle={{ ...args.toggle, is_set: isSet }}
          onSave={async () => {
            setIsSet(true);
            return true;
          }}
          onRemove={isSet ? () => setIsSet(false) : undefined}
        />
      </div>
    );
  },
} satisfies Meta<typeof SecretSetting>;
export default meta;
type Story = StoryObj<typeof meta>;

/** Nothing on the row may push the panel wider than the screen. */
async function expectFits(canvasElement: HTMLElement) {
  const panel = canvasElement.querySelector('div')!;
  await expect(panel.scrollWidth).toBeLessThanOrEqual(panel.clientWidth + 1);
}

export const NotSet: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByText('Not set')).toBeVisible();
    await expect(canvas.queryByRole('button', { name: 'Remove' })).not.toBeInTheDocument();
    await userEvent.click(canvas.getByRole('button', { name: 'Set' }));
    const field = canvas.getByLabelText('Knowledge base token');
    await expect(field).toHaveAttribute('type', 'password');
    await expect(canvas.getByRole('button', { name: 'Save' })).toBeDisabled();
    await expectFits(canvasElement);
    await userEvent.type(field, 'tok-123{Enter}');
    await waitFor(() => expect(canvas.getByText('Saved')).toBeVisible());
    await expect(canvasElement.innerHTML).not.toContain('tok-123');
  },
};

export const Saved: Story = {
  args: { toggle: { ...TOKEN, is_set: true, source: 'global', is_override: true } },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByText('Saved')).toBeVisible();
    await expect(canvas.getByRole('button', { name: 'Replace' })).toBeEnabled();
    await expectFits(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Remove' }));
    await expect(canvas.getByRole('group', { name: 'Remove Knowledge base token?' })).toBeVisible();
    await expectFits(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Keep' }));
    await expect(canvas.getByText('Saved')).toBeVisible();
  },
};

export const Replacing: Story = {
  ...Saved,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Replace' }));
    await expect(canvas.getByLabelText('Knowledge base token')).toHaveValue('');
    await expectFits(canvasElement);
  },
};

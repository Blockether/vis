import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn } from 'storybook/test';
import { STORY_MCP_AUTH } from '../dev/story-data';
import { McpAuth } from './McpAuth';
const meta = {
  title: 'Components/MCP sign-in', component: McpAuth,
  parameters: { layout: 'padded' },
  args: { flow: STORY_MCP_AUTH, input: '', busy: false, onInput: fn(), onFinish: fn(), onCancel: fn(), onOpen: fn() },
} satisfies Meta<typeof McpAuth>;
export default meta;
type Story = StoryObj<typeof meta>;
export const AppReturn: Story = {
  play: async ({ canvas }) => {
    await expect(canvas.getByRole('status')).toHaveTextContent('Waiting for authorization');
    await expect(canvas.queryByRole('textbox')).not.toBeInTheDocument();
    await expect(canvas.getByRole('button', { name: 'Cancel' })).toBeVisible();
  },
};
export const ManualReturn: Story = {
  args: { flow: { ...STORY_MCP_AUTH, callback_mode: 'loopback', redirect_uri: 'http://127.0.0.1:4567/mcp-callback' } },
  play: async ({ canvas }) => {
    await expect(canvas.getByLabelText('Paste the final callback URL')).toBeVisible();
    await expect(canvas.getByRole('button', { name: 'Finish sign-in' })).toBeDisabled();
  },
};

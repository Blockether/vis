import type { Meta, StoryObj } from '@storybook/react-vite';
import { JumpToLatestButton } from './JumpToLatestButton';

const meta = {
  title: 'Session/Jump to latest',
  component: JumpToLatestButton,
  parameters: { layout: 'centered' },
} satisfies Meta<typeof JumpToLatestButton>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Default: Story = {};

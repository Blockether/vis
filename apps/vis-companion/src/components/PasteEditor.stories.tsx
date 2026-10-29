import type { Meta, StoryObj } from '@storybook/react-vite';
import { fn } from 'storybook/test';

import { STORY_COMPOSER_PASTE } from '../dev/story-data';
import { PasteEditor } from './PasteEditor';

const meta = {
  title: 'Session/Composer/Paste editor',
  component: PasteEditor,
  parameters: { layout: 'fullscreen' },
  args: {
    paste: STORY_COMPOSER_PASTE,
    onDismiss: fn(),
    onSave: fn(),
  },
  render: (args) => (
    <div className="relative h-screen min-h-[32rem] w-screen overflow-hidden bg-ink">
      <PasteEditor {...args} />
    </div>
  ),
} satisfies Meta<typeof PasteEditor>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Multiline: Story = {
};

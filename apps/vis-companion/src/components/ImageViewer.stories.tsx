import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import { STORY_PICTURES } from '../dev/story-data';
import { ImageViewer } from './ImageViewer';

/** The full-screen image tool, with the drawing state changed in its own footer slot. */
const meta = {
  title: 'Components/Image viewer',
  component: ImageViewer,
  args: {
    src: STORY_PICTURES[0].src,
    name: STORY_PICTURES[0].name,
    onClose: fn(),
    onApply: fn(),
  },
} satisfies Meta<typeof ImageViewer>;

export default meta;
type Story = StoryObj<typeof meta>;

/** Entering Draw replaces its pencil with the check that accepts the drawing. */
export const Drawing: Story = {
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    await userEvent.click(page.getByRole('button', { name: 'Draw on image' }));
    await expect(page.queryByRole('button', { name: 'Draw on image' })).not.toBeInTheDocument();
    await expect(page.getByRole('button', { name: 'Save changes' })).toBeInTheDocument();
  },
};

/** The pen stays active while its rail folds down to a single way back. */
export const DrawingToolsHidden: Story = {
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    await userEvent.click(page.getByRole('button', { name: 'Draw on image' }));
    await userEvent.click(page.getByRole('button', { name: 'Hide drawing tools' }));
    await expect(page.queryByRole('group', { name: 'Drawing tools' })).not.toBeInTheDocument();
    await expect(page.getByRole('button', { name: 'Show drawing tools' })).toBeInTheDocument();
  },
};

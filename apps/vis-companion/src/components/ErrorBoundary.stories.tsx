import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent } from 'storybook/test';

import { ErrorFallback } from './ErrorBoundary';

/** The last surface never needs a fabricated crash: its message and recovery are ordinary props. */
const meta = {
  title: 'Components/Error fallback',
  component: ErrorFallback,
  args: {
    message: 'Could not render the session timeline',
    onReload: () => {},
  },
} satisfies Meta<typeof ErrorFallback>;

export default meta;
type Story = StoryObj<typeof meta>;

const reload = fn();

/** The tree failed, but durable sessions and the one honest recovery remain visible. */
export const Recovery: Story = {
  args: { onReload: reload },
  play: async ({ args, canvas }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Reload Vis' }));
    await expect(args.onReload).toHaveBeenCalledOnce();
  },
};

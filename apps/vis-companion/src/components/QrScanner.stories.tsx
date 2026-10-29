import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent } from 'storybook/test';

import { QrScannerView } from './QrScanner';

/** Camera permissions are side effects; the six surfaces they produce are plain values. */
const meta = {
  title: 'App/Pairing scanner',
  component: QrScannerView,
  args: {
    phase: 'starting',
    error: '',
    canTakePhoto: true,
    onCancel: () => {},
    onPhoto: () => {},
  },
} satisfies Meta<typeof QrScannerView>;

export default meta;
type Story = StoryObj<typeof meta>;

const cancel = fn();
const photo = fn();

/** Frames are being decoded; both exits remain explicit and testable. */
export const Scanning: Story = {
  args: { phase: 'live', onCancel: cancel, onPhoto: photo },
  play: async ({ args, canvas }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Take a photo instead' }));
    await userEvent.click(canvas.getByRole('button', { name: 'Cancel' }));
    await expect(args.onPhoto).toHaveBeenCalledOnce();
    await expect(args.onCancel).toHaveBeenCalledOnce();
  },
};

/** Permission failed; the reason stands next to the two remaining exits. */
export const CameraDenied: Story = {
  args: {
    phase: 'error',
    error: 'Camera access was denied — enable it in Settings ▸ Vis',
  },
};

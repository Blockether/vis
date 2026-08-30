import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent } from 'storybook/test';

import { QrScannerView } from './QrScanner';

/** Camera permissions are side effects; the six surfaces they produce are plain values. */
const meta = {
  title: 'Components/QR scanner',
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

/** The permission prompt has not answered yet. */
export const Starting: Story = {};

/** The wait became long enough to explain where iOS may be blocked. */
export const SlowStart: Story = { args: { slowStart: true } };

const cancel = fn();
const photo = fn();

/** Frames are being decoded; both exits remain explicit and testable. */
export const Live: Story = {
  args: { phase: 'live', onCancel: cancel, onPhoto: photo },
  play: async ({ args, canvas }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Take a photo instead' }));
    await userEvent.click(canvas.getByRole('button', { name: 'Cancel' }));
    await expect(args.onPhoto).toHaveBeenCalledOnce();
    await expect(args.onCancel).toHaveBeenCalledOnce();
  },
};

/** A live preview that has not found a code says what to change. */
export const NoHitYet: Story = { args: { phase: 'live', noHitYet: true } };

/** A still photo is being decoded and cannot be submitted twice. */
export const ReadingPhoto: Story = { args: { phase: 'busy' } };

/** Permission failed; the reason stands next to the two remaining exits. */
export const Denied: Story = {
  args: {
    phase: 'error',
    error: 'Camera access was denied — enable it in Settings ▸ Vis',
  },
};

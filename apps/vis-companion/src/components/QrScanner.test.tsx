// @vitest-environment jsdom
import { cleanup, render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { QrScannerView } from './QrScanner';

afterEach(cleanup);

describe('the camera-free scanner surface', () => {
  it('names a slow camera start without hiding the exits', async () => {
    const cancel = vi.fn();
    const photo = vi.fn();
    render(
      <QrScannerView
        phase="starting"
        error=""
        slowStart
        canTakePhoto
        onCancel={cancel}
        onPhoto={photo}
      />,
    );

    expect(screen.getByText(/Still waiting on the camera/)).toBeTruthy();
    await userEvent.click(screen.getByRole('button', { name: 'Take a photo instead' }));
    await userEvent.click(screen.getByRole('button', { name: 'Cancel' }));
    expect(photo).toHaveBeenCalledOnce();
    expect(cancel).toHaveBeenCalledOnce();
  });

  it('disables a second photo while the first is being read', () => {
    render(
      <QrScannerView
        phase="busy"
        error=""
        canTakePhoto
        onCancel={() => {}}
        onPhoto={() => {}}
      />,
    );
    expect(screen.getByRole('button', { name: 'Reading…' })).toBeDisabled();
  });
});

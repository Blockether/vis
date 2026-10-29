// @vitest-environment jsdom
import { cleanup, render, screen } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';

import { QrScannerView } from './QrScanner';

afterEach(cleanup);

describe('the camera-free scanner surface', () => {
  it('disables a second photo while the first is being read', () => {
    render(
      <QrScannerView phase="busy" error="" canTakePhoto onCancel={() => {}} onPhoto={() => {}} />,
    );
    expect(screen.getByRole('button', { name: 'Reading…' })).toBeDisabled();
  });
});

// @vitest-environment jsdom
import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { describe, expect, it, vi } from 'vitest';

const exportDiagnostics = vi.hoisted(() => vi.fn(async () => 'Diagnostics shared.'));
vi.mock('../lib/diagnostics', async (importOriginal) => ({
  // Only the platform hand-off is a boundary the test owns; the retention
  // policy the panel states as a fact stays the real module's.
  ...(await importOriginal<typeof import('../lib/diagnostics')>()),
  exportDiagnostics,
}));

import { APP_BUILD_COMMIT, APP_BUILD_NUMBER } from '../lib/build-info';
import { APP_MIN_GATEWAY_PROTOCOL, APP_PROTOCOL, APP_VERSION } from '../lib/compat';
import { RETAINED_LOG_POLICY } from '../lib/diagnostics';
import { DiagnosticsPanel } from './settings/DiagnosticsPanel';

describe('application diagnostics settings', () => {
  it('identifies the exact app build and the gateway wire it accepts', () => {
    render(<DiagnosticsPanel />);

    expect(screen.getByText(APP_VERSION)).toBeInTheDocument();
    expect(screen.getByText(APP_BUILD_NUMBER)).toBeInTheDocument();
    expect(screen.getByText(APP_BUILD_COMMIT)).toBeInTheDocument();
    // The wire is two facts in two rows, not one sentence wrapping in the
    // trailing column; the retention the prose used to explain is a row too.
    expect(
      screen.getByText(`${APP_MIN_GATEWAY_PROTOCOL}+`),
    ).toBeInTheDocument();
    expect(screen.getByText(`${APP_PROTOCOL}`)).toBeInTheDocument();
    expect(
      screen.getByText(
        `${RETAINED_LOG_POLICY.days} days · ${RETAINED_LOG_POLICY.megabytes} MB`,
      ),
    ).toBeInTheDocument();
  });

  it('exports the persisted app log through the platform hand-off', async () => {
    render(<DiagnosticsPanel />);

    await userEvent.click(screen.getByRole('button', { name: 'Export app logs' }));

    expect(exportDiagnostics).toHaveBeenCalledOnce();
    await waitFor(() => expect(screen.getByText('Diagnostics shared.')).toBeInTheDocument());
  });
});

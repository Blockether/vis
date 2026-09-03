// @vitest-environment jsdom
import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { useState } from 'react';
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

/** The dialog owns the fold's state, so the harness owns it the same way. */
function Harness({ initialOpen = false }: { initialOpen?: boolean }) {
  const [isOpen, setOpen] = useState(initialOpen);
  return (
    <DiagnosticsPanel isOpen={isOpen} onToggle={() => setOpen((open) => !open)} />
  );
}

describe('application diagnostics settings', () => {
  // Reported over a desktop screenshot of the settings dialog: the app-logs
  // panel stood permanently open at the foot of the Application column — six
  // rows and an export verb always painted for a task this device performs a
  // few times a year. The band folds now, and hidden is hidden.
  // Regression, issue #1169050b-3dc3-4e21-ad3d-03098d149d2f: pressing the named
  // Diagnostics band did nothing; only its trailing chevron opened the panel.
  it('keeps every fact off the page until the band is pressed', async () => {
    render(<Harness />);

    const fold = screen.getByRole('button', { name: 'Show diagnostics' });
    expect(fold).toHaveAttribute('aria-expanded', 'false');
    expect(screen.queryByText(APP_VERSION)).not.toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: 'Export app logs' }),
    ).not.toBeInTheDocument();

    await userEvent.click(screen.getByRole('heading', { name: 'Diagnostics' }));

    expect(fold).toHaveAttribute('aria-expanded', 'true');
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
    render(<Harness initialOpen />);

    await userEvent.click(screen.getByRole('button', { name: 'Export app logs' }));

    expect(exportDiagnostics).toHaveBeenCalledOnce();
    await waitFor(() => expect(screen.getByText('Diagnostics shared.')).toBeInTheDocument());
  });
});

// @vitest-environment jsdom
import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { useState } from 'react';
import { afterEach, describe, expect, it, vi } from 'vitest';

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
import { installPerfProbes } from '../lib/perf';
import { DiagnosticsPanel } from './settings/DiagnosticsPanel';

/** The dialog owns the fold's state, so the harness owns it the same way. */
function Harness({
  initialOpen = false,
  onReload,
}: {
  initialOpen?: boolean;
  onReload?: () => void;
}) {
  const [isOpen, setOpen] = useState(initialOpen);
  return (
    <DiagnosticsPanel
      isOpen={isOpen}
      onToggle={() => setOpen((open) => !open)}
      onReload={onReload}
    />
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
    expect(screen.queryByRole('button', { name: 'Export app logs' })).not.toBeInTheDocument();

    await userEvent.click(screen.getByRole('heading', { name: 'Diagnostics' }));

    expect(fold).toHaveAttribute('aria-expanded', 'true');
    expect(screen.getByText(APP_VERSION)).toBeInTheDocument();
    expect(screen.getByText(APP_BUILD_NUMBER)).toBeInTheDocument();
    expect(screen.getByText(APP_BUILD_COMMIT)).toBeInTheDocument();
    // The compact matrix preserves both wire facts as distinct terms instead of
    // flattening them into one sentence; retention remains an explicit fact too.
    expect(screen.getByText(`${APP_MIN_GATEWAY_PROTOCOL}+`)).toBeInTheDocument();
    expect(screen.getByText(`${APP_PROTOCOL}`)).toBeInTheDocument();
    expect(
      screen.getByText(`${RETAINED_LOG_POLICY.days} days · ${RETAINED_LOG_POLICY.megabytes} MB`),
    ).toBeInTheDocument();
  });

  it('exports the persisted app log through the platform hand-off', async () => {
    render(<Harness initialOpen />);

    await userEvent.click(screen.getByRole('button', { name: 'Export app logs' }));

    expect(exportDiagnostics).toHaveBeenCalledOnce();
    await waitFor(() => expect(screen.getByText('Diagnostics shared.')).toBeInTheDocument());
  });
});

describe('memory overlay setting', () => {
  let undoProbes: (() => void) | null = null;

  afterEach(() => {
    undoProbes?.();
    undoProbes = null;
    localStorage.clear();
  });

  it('turns the overlay on for the next page load', async () => {
    const onReload = vi.fn();
    render(<Harness initialOpen onReload={onReload} />);

    await userEvent.click(screen.getByRole('button', { name: 'Show memory overlay' }));

    expect(localStorage.getItem('vis.perf')).toBe('1');
    expect(onReload).toHaveBeenCalledOnce();
  });

  it('turns a running overlay off', async () => {
    localStorage.setItem('vis.perf', '1');
    undoProbes = installPerfProbes();
    const onReload = vi.fn();
    render(<Harness initialOpen onReload={onReload} />);

    await userEvent.click(screen.getByRole('button', { name: 'Hide memory overlay' }));

    expect(localStorage.getItem('vis.perf')).toBeNull();
    expect(onReload).toHaveBeenCalledOnce();
  });

  it('stays on the page when the device does not save the choice', async () => {
    const setItem = vi.spyOn(localStorage, 'setItem').mockImplementation(() => {
      throw new DOMException('Storage is full.', 'QuotaExceededError');
    });
    const onReload = vi.fn();
    try {
      render(<Harness initialOpen onReload={onReload} />);

      await userEvent.click(screen.getByRole('button', { name: 'Show memory overlay' }));

      expect(screen.getByText('This device did not save the setting.')).toBeInTheDocument();
      expect(onReload).not.toHaveBeenCalled();
    } finally {
      setItem.mockRestore();
    }
  });
});

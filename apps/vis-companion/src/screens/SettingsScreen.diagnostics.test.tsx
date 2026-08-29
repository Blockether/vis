// @vitest-environment jsdom
import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { describe, expect, it, vi } from 'vitest';

const exportDiagnostics = vi.hoisted(() => vi.fn(async () => 'Diagnostics shared.'));
vi.mock('../lib/diagnostics', () => ({ exportDiagnostics, recordDiagnostic: vi.fn() }));

import { APP_BUILD_COMMIT, APP_BUILD_NUMBER } from '../lib/build-info';
import { APP_MIN_GATEWAY_PROTOCOL, APP_PROTOCOL, APP_VERSION } from '../lib/compat';
import { DiagnosticsPanel } from './SettingsScreen';

describe('application diagnostics settings', () => {
  it('identifies the exact app build and the gateway wire it accepts', () => {
    render(<DiagnosticsPanel />);

    expect(screen.getByText(APP_VERSION)).toBeInTheDocument();
    expect(screen.getByText(APP_BUILD_NUMBER)).toBeInTheDocument();
    expect(screen.getByText(APP_BUILD_COMMIT)).toBeInTheDocument();
    expect(
      screen.getByText(
        `Protocol ${APP_MIN_GATEWAY_PROTOCOL}+ · must accept client ${APP_PROTOCOL}`,
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

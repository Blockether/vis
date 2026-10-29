// @vitest-environment jsdom
import { render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { useState } from 'react';
import { afterEach, describe, expect, it, vi } from 'vitest';

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

      expect(onReload).not.toHaveBeenCalled();
    } finally {
      setItem.mockRestore();
    }
  });
});

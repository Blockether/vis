// @vitest-environment jsdom
// Activity is a FIELD of the form that produced it, so every case here hands the
// panel the engine's own bounded snapshot — the fixture the host projects — and
// reads the document that landed. Nothing here opens, patches or closes a view:
// that is the Live View rail, and it is a different file for that reason.
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';
import { ActivityPanel, activityReceiptText } from './ActivityPanel';
import activityPanelSource from './ActivityPanel.tsx?raw';
import activityFixture from '../lib/activity.fixture.json';
import { activityProjectionFromWire, type ActivityProjection } from '../lib/activity';

afterEach(cleanup);

/**
 * The engine's own Activity fixture, parsed. Protocol 7 ships it as a bare
 * projection on the form that produced it, not as a classified view, so the
 * panel takes the snapshot itself.
 */
function activityProjection(): ActivityProjection {
  const projection = activityProjectionFromWire(activityFixture);
  if (!projection) throw new Error('the engine Activity fixture must be paintable');
  return projection;
}

function paintActivity(props: Partial<Parameters<typeof ActivityPanel>[0]> = {}) {
  const { activity = activityProjection(), isSettled = false, ...rest } = props;
  render(<ActivityPanel activity={activity} isSettled={isSettled} {...rest} />);
  return document.body.innerHTML;
}

describe("one form's Activity on the phone", () => {
  it('uses the first in-progress task as the collapsed Activity preview', () => {
    paintActivity();

    expect(screen.getByText('ACTIVITY')).toBeTruthy();
    expect(screen.getByText('Running')).toBeTruthy();
    expect(screen.getByText('RUN_TESTS · suite')).toBeTruthy();
    expect(screen.queryByText(/2 operations|24 passed|truncated/)).toBeNull();
    expect(screen.queryByRole('button', { name: /interrupt/i })).toBeNull();
    expect(screen.queryByRole('list', { name: 'Invocation chronology' })).toBeNull();

    fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
    const chronology = screen.getByRole('list', { name: 'Invocation chronology' });
    const chronologyText = chronology.textContent ?? '';
    expect(chronologyText.indexOf('grep · 18 matches')).toBeLessThan(
      chronologyText.indexOf('run_tests · suite'),
    );
    expect(chronologyText).not.toContain('[{query: needle}]');
    expect(chronologyText).not.toContain('24 passed');
    expect(screen.getByRole('button', { name: 'Collapse Activity' })).toBeTruthy();
  });

  it('matches the compact result-band height without a leading status mark', () => {
    paintActivity();
    const receipt = screen.getByLabelText('Activity');
    const header = receipt.querySelector('header');
    const disclosure = screen.getByRole('button', { name: 'Expand Activity' });

    expect(receipt.classList.contains('border')).toBe(false);
    expect(receipt.className).toContain('border-l-2');
    expect(header?.classList.contains('min-h-8')).toBe(true);
    expect(header?.classList.contains('min-h-10')).toBe(false);
    expect(header?.querySelector('.animate-spinner-frame')).toBeNull();
    expect(header?.querySelector('.text-code-duration')).toBeNull();
    expect(disclosure.className).toContain('min-h-8');
    expect(disclosure.className).toContain('motion-reduce:transition-none');
  });

  // Regression, issue td-5b6b08: settled Companion receipts said SUCCEEDED,
  // omitted the operation and elapsed time, and retained "activities run".
  it('matches the settled TUI receipt grammar and durations', () => {
    const projection = activityProjection();
    const settled = {
      ...projection,
      state: 'succeeded' as const,
      counts: { running: 0, succeeded: 2, failed: 0, cancelled: 0 },
      rows: projection.rows.map((row: ActivityProjection['rows'][number], index: number) => ({
        ...row,
        state: 'succeeded' as const,
        ...(index === 0
          ? { operation: 'shell', summary: 'running: git status', duration_ms: 66 }
          : { duration_ms: 12_500 }),
      })),
    };

    paintActivity({ activity: settled, isSettled: true });

    expect(activityReceiptText(settled, 12_600)).toBe(
      'DONE · SHELL and more · 2 activities · 12.6s',
    );
    expect(screen.getByText('Done')).toBeTruthy();
    expect(screen.getByText('SHELL and more')).toBeTruthy();
    fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
    expect(screen.getByText('shell · cmd: git status')).toBeTruthy();
    expect(screen.getByText('66ms')).toBeTruthy();
    expect(screen.getByText('12.5s')).toBeTruthy();
    expect(screen.getByLabelText('Activity').querySelector('header')?.textContent).not.toContain('✓');
    expect(screen.queryByRole('status')).toBeNull();
  });

  it('shows an explicit quiet empty state', () => {
    paintActivity({ activity: { ...activityProjection(), state: 'idle', rows: [] } });
    expect(screen.getByText('Idle')).toBeTruthy();
    expect(screen.getByText('No operation yet')).toBeTruthy();
    fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
    expect(screen.getByText('No operations yet')).toBeTruthy();
  });});

describe('the panel is built from the closed vocabulary', () => {
  it("borrows the app's controls and writes no styles of its own", () => {
    expect(activityPanelSource).toContain('<Disclosure');
    expect(activityPanelSource).toContain('<BandLabel');
    // No spinner: a mark that turns says only "still here", while one word says
    // whether the form is still working and, once it is not, how it ended.
    expect(activityPanelSource).not.toContain('<Spinner');
    expect(activityPanelSource).not.toContain('<button');
    expect(activityPanelSource).not.toContain('style={');
    expect(activityPanelSource).not.toContain('style="');
  });
});

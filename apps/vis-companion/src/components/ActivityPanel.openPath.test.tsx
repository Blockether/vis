// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, expect, it, vi } from 'vitest';
import type { ActivityProjection, ActivityRow } from '../lib/activity';
import { OpenPathContext } from '../lib/open-path';
import { ActivityPanel } from './ActivityPanel';

afterEach(cleanup);

const TARGET = '~/vis/AGENTS.md';

function patchRow(): ActivityRow {
  return {
    id: 'patch-1',
    sequence: 1,
    operation: 'patch',
    presenter: 'patch',
    signal: 'mutation',
    state: 'succeeded',
    summary: TARGET,
    argument_key: '1'.repeat(64),
    duration_ms: 1,
    resources: [{ type: 'file', id: TARGET }],
    presentation: { headline: 'Patched', summary: TARGET, content: [] },
    evidence: [
      {
        kind: 'diff',
        text: TARGET,
        additions: 1,
        deletions: 1,
        modifications: 1,
        is_truncated: false,
        is_redacted: false,
        lines: [
          { kind: 'hunk', text: '@@ -1 +1 @@' },
          { kind: 'deletion', text: 'before-1' },
          { kind: 'addition', text: 'after-1' },
        ],
      },
    ],
  };
}

function projection(): ActivityProjection {
  return {
    state: 'succeeded',
    rows: [patchRow()],
    counts: { succeeded: 1, running: 0, failed: 0, cancelled: 0 },
    omitted: { rows: 0, by_classification: {} },
  };
}

function openPanel(openPath: ((path: string) => void) | null) {
  render(
    <OpenPathContext.Provider value={openPath}>
      <ActivityPanel activity={projection()} />
    </OpenPathContext.Provider>,
  );
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  const group = screen.queryByRole('button', { name: /Patch ×/ });
  if (group) fireEvent.click(group);
}

function paths(): HTMLElement[] {
  return [...document.querySelectorAll<HTMLElement>('[data-path]')];
}

it.each([
  [TARGET, ' · lines 12–13'],
  [TARGET, ' · lines 12–13, 18–20'],
  [TARGET, ''],
  ['~/vis/notes · lines 12–13', ''],
  ['~/vis/notes · lines 12–13', ' · lines 18–20'],
])(
  'opens the read target %s without its annotation %s',
  (target, note) => {
    const openPath = vi.fn();
    const activity = projection();
    activity.rows = [
      {
        ...patchRow(),
        operation: 'cat',
        presenter: 'generic',
        signal: 'observation',
        summary: target,
        resources: [],
        evidence: [],
        presentation: { headline: 'Read', summary: target + note, content: [] },
      },
    ];
    render(
      <OpenPathContext.Provider value={openPath}>
        <ActivityPanel activity={activity} />
      </OpenPathContext.Provider>,
    );
    fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
    const group = screen.queryByRole('button', { name: /Read ×/ });
    if (group) fireEvent.click(group);
    const path = paths()[0];
    expect(path).toHaveAttribute('data-path', target);
    expect(path).toHaveAttribute('aria-label', `Open ${target}`);
    expect(path).toHaveAttribute('title', target);
    expect(path).toHaveTextContent(target);
    if (note) {
      const annotation = screen.getByText(note.trim());
      expect(annotation).toBeVisible();
      expect(annotation.closest('[data-path]')).toBeNull();
      fireEvent.click(annotation);
      expect(openPath).not.toHaveBeenCalled();
    }
    fireEvent.click(path);
    fireEvent.keyDown(path, { key: 'Enter' });
    fireEvent.keyDown(path, { key: ' ' });
    expect(openPath.mock.calls).toEqual([[target], [target], [target]]);
    expect(document.querySelector('[data-activity-content]')).toBeNull();
  },
);

it('opens the file a path names, on the machine that ran the step', () => {
  const openPath = vi.fn();
  openPanel(openPath);
  const shown = paths();
  expect(shown.length).toBeGreaterThan(0);
  for (const path of shown) expect(path).toHaveAttribute('data-path', TARGET);
  fireEvent.click(shown[0]);
  expect(openPath).toHaveBeenCalledWith(TARGET);
});

it('opens a path from the keyboard, for a reader who never touches the screen', () => {
  const openPath = vi.fn();
  openPanel(openPath);
  const path = paths()[0];
  expect(path).toHaveAttribute('tabindex', '0');
  fireEvent.keyDown(path, { key: 'Enter' });
  fireEvent.keyDown(path, { key: ' ' });
  fireEvent.keyDown(path, { key: 'a' });
  expect(openPath.mock.calls).toEqual([[TARGET], [TARGET]]);
});

it('keeps the press on the path, so the row it sits in does not also open', () => {
  const openPath = vi.fn();
  openPanel(openPath);
  const row = paths().at(-1)!;
  fireEvent.click(row);
  expect(openPath).toHaveBeenCalledWith(TARGET);
  expect(screen.queryByText('after-1')).toBeNull();
});

it('leaves paths as plain words when no opener is published', () => {
  openPanel(null);
  const shown = paths();
  expect(shown.length).toBeGreaterThan(0);
  for (const path of shown) expect(path).not.toHaveAttribute('role', 'button');
  fireEvent.click(shown.at(-1)!);
  expect(screen.getByText('after-1')).toBeVisible();
});

// BLO-172: a listing printed file names nobody could press. Each listed file
// now carries the path that opens it; a directory keeps plain words.
const LISTED = '/w/src/core.clj';

function listingProjection(): ActivityProjection {
  return {
    state: 'succeeded',
    rows: [
      {
        id: 'ls-1',
        sequence: 1,
        operation: 'ls',
        presenter: 'observation',
        signal: 'observation',
        state: 'succeeded',
        summary: '/w/src · 1 directory · 1 file',
        argument_key: '2'.repeat(64),
        duration_ms: 1,
        resources: [],
        presentation: {
          headline: 'Listed directory',
          summary: '/w/src · 1 directory · 1 file',
          content: [
            {
              type: 'table',
              columns: ['Name', 'Kind'],
              rows: [
                ['core.clj', 'File'],
                ['util/', 'Directory'],
              ],
              paths: [LISTED, ''],
            },
          ],
        },
        evidence: [],
      },
    ],
    counts: { succeeded: 1, running: 0, failed: 0, cancelled: 0 },
    omitted: { rows: 0, by_classification: {} },
  };
}

it('opens the file a listed row names, and leaves a directory plain', () => {
  const openPath = vi.fn();
  render(
    <OpenPathContext.Provider value={openPath}>
      <ActivityPanel activity={listingProjection()} />
    </OpenPathContext.Provider>,
  );
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  const group = screen.queryByRole('button', { name: /List ×/ });
  if (group) fireEvent.click(group);
  fireEvent.click(screen.getByRole('button', { name: /Listed directory/ }));

  const cell = screen.getByText('core.clj');
  expect(cell.closest('[data-path]')).toHaveAttribute('data-path', LISTED);
  fireEvent.click(cell);
  expect(openPath).toHaveBeenCalledWith(LISTED);
  expect(screen.getByText('util/').closest('[data-path]')).toBeNull();
});

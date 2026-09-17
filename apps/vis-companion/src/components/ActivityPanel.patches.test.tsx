// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { afterEach, expect, it, vi } from 'vitest';
import type { ActivityDiffEvidence, ActivityProjection, ActivityRow } from '../lib/activity';
import { ActivityPanel } from './ActivityPanel';

afterEach(() => {
  cleanup();
  vi.unstubAllGlobals();
});

function patchRow(sequence: number, target = 'AGENTS.md'): ActivityRow {
  return {
    id: `patch-${sequence}`,
    sequence,
    operation: 'patch',
    presenter: 'patch',
    signal: 'mutation',
    state: 'succeeded',
    summary: target,
    argument_key: String(sequence).repeat(64),
    duration_ms: sequence,
    resources: [{ type: 'file', id: target }],
    presentation: { headline: 'Patched', summary: target, content: [] },
    evidence: [
      {
        kind: 'diff',
        text: target,
        additions: 1,
        deletions: 1,
        modifications: 1,
        is_truncated: false,
        is_redacted: false,
        lines: [
          { kind: 'hunk', text: `@@ -${sequence} +${sequence} @@` },
          { kind: 'deletion', text: `before-${sequence}` },
          { kind: 'addition', text: `after-${sequence}` },
        ],
      },
    ],
  };
}

function projection(rows = [patchRow(1), patchRow(2)]): ActivityProjection {
  return {
    state: 'succeeded',
    rows,
    counts: { succeeded: rows.length, running: 0, failed: 0, cancelled: 0 },
    omitted: { rows: 0, by_classification: {} },
  };
}

function openPatches(activity = projection()) {
  const view = render(<ActivityPanel activity={activity} />);
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  const group = screen.queryByRole('button', { name: /Patch ×/ });
  if (group) fireEvent.click(group);
  return view;
}

it('combines same-file patches into one chronological diff without changing the receipt', () => {
  const activity = projection();
  const original = structuredClone(activity);
  openPatches(activity);
  expect(screen.getByRole('button', { name: 'Collapse Activity' })).toHaveTextContent(
    '2 operations',
  );
  expect(document.querySelectorAll('[data-activity-row]')).toHaveLength(1);
  const row = document.querySelector('[data-activity-row="0:patch-1"]')! as HTMLElement;
  expect(within(row).getByLabelText('Duration 3ms')).toBeVisible();
  expect(within(row).getByRole('button')).toHaveTextContent('+2 −2');
  fireEvent.click(within(row).getByRole('button'));
  const diff = screen.getByLabelText('Unified diff');
  expect(diff.textContent).toMatch(/before-1.*after-1.*before-2.*after-2/);
  expect(activity).toEqual(original);
});

it('keeps an expanded patch mounted when another same-file patch arrives', () => {
  const first = patchRow(1);
  const view = openPatches(projection([first]));
  const row = document.querySelector('[data-activity-row]')! as HTMLElement;
  fireEvent.click(within(row).getByRole('button'));
  const diff = screen.getByLabelText('Unified diff');
  view.rerender(<ActivityPanel activity={projection([first, patchRow(2)])} />);
  expect(row.isConnected).toBe(true);
  expect(diff.isConnected).toBe(true);
  expect(document.querySelectorAll('[data-activity-row]')).toHaveLength(1);
  expect(diff.textContent).toContain('after-2');
});

it.each([
  'different',
  'unknown',
  'fallback',
  'redacted target',
  'missing presentation',
  'failed',
  'running',
  'cancelled',
  'children',
  'sections',
  'content',
  'custom',
  'error',
  'multi-file',
])('keeps %s patches separate rather than hiding evidence', (reason) => {
  const second = patchRow(2);
  if (reason === 'different') second.evidence = patchRow(2, 'other.md').evidence;
  if (reason === 'unknown') (second.evidence[0] as ActivityDiffEvidence).text = '';
  if (reason === 'fallback') (second.evidence[0] as ActivityDiffEvidence).text = 'diff';
  if (reason === 'redacted target')
    (second.evidence[0] as ActivityDiffEvidence).text = '[REDACTED]';
  if (reason === 'missing presentation') delete second.presentation;
  if (reason === 'failed' || reason === 'running' || reason === 'cancelled') second.state = reason;
  if (reason === 'children') second.children = [{ ...patchRow(3), id: 'child' }];
  if (reason === 'sections')
    second.presentation!.sections = [{ headline: 'Details', summary: '', content: [] }];
  if (reason === 'content')
    second.presentation!.content = [{ type: 'text', text: 'Important result' }];
  if (reason === 'custom') second.presentation!.headline = 'Custom patch';
  if (reason === 'error') second.evidence.push({ kind: 'error', text: 'Permission denied' });
  if (reason === 'multi-file') second.evidence.push(...patchRow(3, 'other.md').evidence);
  openPatches(projection([patchRow(1), second]));
  expect(document.querySelectorAll('[data-activity-depth="0"]')).toHaveLength(2);
});

it('omits redacted lines and truncation notices while keeping available changes', () => {
  const first = patchRow(1);
  first.is_truncated = true;
  const diff = first.evidence[0] as ActivityDiffEvidence;
  diff.is_redacted = true;
  diff.lines.push({ kind: 'context', text: '[REDACTED]', is_redacted: true });
  const second = patchRow(2);
  delete second.duration_ms;
  openPatches(projection([first, second]));
  const row = document.querySelector('[data-activity-row]')! as HTMLElement;
  fireEvent.click(within(row).getByRole('button'));
  expect(within(row).queryByLabelText(/^Duration /)).toBeNull();
  expect(row.textContent).toContain('after-1');
  for (const marker of ['[REDACTED]', 'Details truncated', 'partial details']) {
    expect(document.body.textContent).not.toContain(marker);
  }
});

it('copies the original invocations instead of the merged presentation', async () => {
  const writeText = vi.fn().mockResolvedValue(undefined);
  vi.stubGlobal('navigator', { ...navigator, clipboard: { writeText } });
  openPatches();
  fireEvent.click(screen.getByRole('button', { name: 'Copy activity' }));
  await screen.findByRole('button', { name: 'Copied' });
  expect(writeText.mock.calls[0][0].match(/patch \[succeeded\]/g)).toHaveLength(2);
});

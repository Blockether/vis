// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { afterEach, expect, it, vi } from 'vitest';
import readFixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/activity-reads.json';
import { activityProjectionFromWire, type ActivityProjection } from '../lib/activity';
import { ActivityPanel } from './ActivityPanel';

afterEach(() => {
  cleanup();
  vi.unstubAllGlobals();
});

function reads(): ActivityProjection {
  return structuredClone(readFixture) as ActivityProjection;
}

function openReads(activity = reads()) {
  const view = render(<ActivityPanel activity={activity} />);
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  const group = screen.queryByRole('button', { name: /Read ×/ });
  if (group) fireEvent.click(group);
  return view;
}

it('accepts opaque read target identities and rejects malformed ones', () => {
  expect(activityProjectionFromWire(readFixture)).toEqual(readFixture);
  for (const key of ['', 'a'.repeat(63), 'A'.repeat(64), 42, null]) {
    const activity = structuredClone(readFixture);
    Object.assign(activity.rows[0], { read_key: key });
    expect(activityProjectionFromWire(activity)).toBeNull();
  }
});

it('merges ranges into one summary without a disclosure or changing invocation counts', () => {
  const activity = reads();
  openReads(activity);
  expect(screen.getByRole('button', { name: 'Collapse Activity' })).toHaveTextContent(
    '2 operations',
  );
  expect(document.querySelectorAll('[data-activity-row]')).toHaveLength(1);
  const row = document.querySelector('[data-activity-row="0:read-1"]')!;
  expect(row).toHaveTextContent('PLAN.md · lines 583–584, 615–616');
  expect(within(row as HTMLElement).queryByRole('button')).toBeNull();
  expect(within(row as HTMLElement).getByLabelText('Duration 3ms')).toBeVisible();
  expect(row.querySelector('[data-activity-content]')).toBeNull();
  expect(activity).toEqual(readFixture);
});

it.each([1, 2])(
  'labels %i same-file reads with the filename instead of a Read headline',
  (count) => {
    const activity = reads();
    activity.rows = activity.rows.slice(0, count);
    openReads(activity);
    const row = document.querySelector<HTMLElement>('[data-activity-row]')!;
    const headline = within(row).getByRole('heading', { level: 4 });
    const ranges = count === 1 ? '583–584' : '583–584, 615–616';
    const duration = activity.rows.reduce((sum, read) => sum + read.duration_ms!, 0);
    expect(headline.textContent).toBe(`~/vis/PLAN.md · lines ${ranges}${duration}ms`);
    expect(within(row).queryByText('Read')).toBeNull();
    expect(row.querySelector('[data-activity-content]')).toBeNull();
  },
);

it('uses the filename as the disclosure label when a read includes an excerpt', () => {
  const activity = reads();
  activity.rows = activity.rows.slice(0, 1);
  activity.rows[0].presentation!.content = [{ type: 'code', text: 'File snapshot' }];
  openReads(activity);
  const row = document.querySelector<HTMLElement>('[data-activity-row]')!;
  const toggle = within(row).getByRole('button');
  expect(toggle.textContent).toMatch(/^~\/vis\/PLAN\.md · lines 583–584/);
  expect(within(row).queryByText('Read')).toBeNull();
  fireEvent.click(toggle);
  expect(within(row).getByText('File snapshot')).toBeVisible();
  expect(row.querySelector('[data-activity-content] > div')).not.toHaveClass('border');
});

it('uses a filename-only label for a read without an authored presentation', () => {
  const activity = reads();
  activity.rows = activity.rows.slice(0, 1);
  delete activity.rows[0].presentation;
  openReads(activity);
  const headline = screen.getByRole('heading', { level: 4 });
  expect(headline.textContent).toBe(`~/vis/PLAN.md${activity.rows[0].duration_ms}ms`);
});

it.each(['running', 'failed', 'cancelled'] as const)(
  'keeps the read headline for a %s operation',
  (state) => {
    const activity = reads();
    activity.rows = activity.rows.slice(0, 1);
    activity.rows[0].state = state;
    openReads(activity);
    expect(screen.getByText('Read')).toBeVisible();
  },
);

it('keeps a custom read headline', () => {
  const activity = reads();
  activity.rows = activity.rows.slice(0, 1);
  activity.rows[0].presentation!.headline = 'Inspect file';
  openReads(activity);
  expect(screen.getByText('Inspect file')).toBeVisible();
});

it('updates the summary-only row when another range arrives', () => {
  const activity = reads();
  const view = openReads({ ...activity, rows: activity.rows.slice(0, 1) });
  view.rerender(<ActivityPanel activity={activity} />);
  fireEvent.click(screen.getByRole('button', { name: /Read ×2/ }));
  const row = document.querySelector('[data-activity-row]')!;
  expect(document.querySelectorAll('[data-activity-row]')).toHaveLength(1);
  expect(row).toHaveTextContent('PLAN.md · lines 583–584, 615–616');
  expect(row.querySelector('[data-activity-content]')).toBeNull();
});

it('keeps explicitly opened content mounted when another range arrives', () => {
  const activity = reads();
  activity.rows.forEach((row, index) => {
    row.presentation!.content = [{ type: 'code', text: `Snapshot ${index + 1}` }];
  });
  const view = openReads({ ...activity, rows: activity.rows.slice(0, 1) });
  const row = document.querySelector('[data-activity-row]')!;
  fireEvent.click(within(row as HTMLElement).getByRole('button'));
  const content = row.querySelector('[data-activity-content]')!;
  const firstCode = content.firstElementChild!;
  view.rerender(<ActivityPanel activity={activity} />);
  expect(row.isConnected).toBe(true);
  expect(content.isConnected).toBe(true);
  expect(firstCode.isConnected).toBe(true);
  expect(content).toHaveTextContent('Snapshot 2');
});

it.each([
  'missing',
  'different',
  'failed',
  'running',
  'cancelled',
  'sections',
  'children',
  'errors',
  'custom',
])('keeps %s reads separate instead of hiding evidence', (reason) => {
  const activity = reads();
  const row = activity.rows[1];
  if (reason === 'missing') delete row.read_key;
  if (reason === 'different') row.read_key = 'd'.repeat(64);
  if (reason === 'failed') {
    row.state = 'failed';
    row.error_summary = 'Permission denied';
  }
  if (reason === 'running' || reason === 'cancelled') row.state = reason;
  if (reason === 'sections')
    row.presentation!.sections = [{ headline: 'Details', summary: '', content: [] }];
  if (reason === 'children') row.children = [{ ...activity.rows[0], id: 'child' }];
  if (reason === 'errors') row.evidence = [{ kind: 'error', text: 'Read evidence' }];
  if (reason === 'custom') row.presentation!.headline = 'Inspect file';
  openReads(activity);
  expect(document.querySelectorAll('[data-activity-depth="0"]')).toHaveLength(2);
  if (reason === 'failed') {
    expect(screen.queryByText('Permission denied')).toBeNull();
    fireEvent.click(
      document.querySelector<HTMLElement>(
        '[data-activity-row="0:read-2"] [data-disclosure-toggle]',
      )!,
    );
    expect(screen.getAllByText('Permission denied').length).toBeGreaterThan(0);
  }
});

it('preserves explicit content and truncation without inventing durations', () => {
  const activity = reads();
  activity.rows[0].presentation!.content = [
    { type: 'code', text: '583 │ Verify the affected tests.' },
  ];
  activity.rows[1].presentation!.summary = activity.rows[0].presentation!.summary;
  activity.rows[1].presentation!.content = [
    { type: 'code', text: '583 │ Updated plan text.' },
    { type: 'code', text: '' },
  ];
  activity.rows[1].is_truncated = true;
  delete activity.rows[1].duration_ms;
  openReads(activity);
  expect(document.querySelectorAll('[data-activity-row]')).toHaveLength(1);
  const row = document.querySelector('[data-activity-row]')!;
  expect(within(row as HTMLElement).queryByLabelText(/^Duration /)).toBeNull();
  fireEvent.click(within(row as HTMLElement).getByRole('button'));
  expect(row.textContent).toContain('583 │ Verify the affected tests.');
  expect(row.textContent).toContain('583 │ Updated plan text.');
  expect(row.textContent).not.toContain('Details truncated');
});

it('keeps empty reads and captions without line ranges summary-only', () => {
  const activity = reads();
  activity.rows.forEach((row) => {
    row.summary = '~/vis/empty.txt';
    row.presentation!.summary = '~/vis/empty.txt';
    row.presentation!.content = [];
  });
  openReads(activity);
  expect(document.querySelectorAll('[data-activity-row]')).toHaveLength(1);
  const row = document.querySelector('[data-activity-row]')!;
  expect(row).toHaveTextContent('~/vis/empty.txt');
  expect(within(row as HTMLElement).queryByRole('button')).toBeNull();
  expect(row.querySelector('[data-activity-content]')).toBeNull();
});

it('copies the original reads rather than the merged display', async () => {
  const writeText = vi.fn().mockResolvedValue(undefined);
  vi.stubGlobal('navigator', { ...navigator, clipboard: { writeText } });
  openReads();
  fireEvent.click(screen.getByRole('button', { name: 'Copy activity' }));
  await screen.findByRole('button', { name: 'Copied' });
  const copied = writeText.mock.calls[0][0] as string;
  expect(copied.match(/cat \[succeeded\]/g)).toHaveLength(2);
  expect(copied).toContain('~/vis/PLAN.md · lines 583–584');
  expect(copied).toContain('~/vis/PLAN.md · lines 615–616');
  expect(copied).not.toContain('a'.repeat(64));
  expect(copied).not.toContain('Verify the affected tests.');
});

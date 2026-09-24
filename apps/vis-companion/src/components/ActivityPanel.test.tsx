// @vitest-environment jsdom
// Activity is a FIELD of the form that produced it, so every case here hands the
// axis the engine's own bounded snapshot — the fixture the host projects — and
// reads the document that landed. Nothing here opens, patches or closes a view:
// that is the Live View rail, and it is a different file for that reason.
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import { ActivityPanel, activityCostParts, activityReceiptText } from './ActivityPanel';
import activityPanelSource from './ActivityPanel.tsx?raw';
import activityFixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/activity.json';
import argumentCases from '../../../../packages/vis-contract/resources/vis-contract/fixtures/activity-arguments.json';
import { ACTIVITY_LONG_RUNNING, ACTIVITY_TREE_CHANGES } from '../dev/story-data';
import * as storyData from '../dev/story-data';
import { WorkspaceRootsContext } from '../lib/workspace-roots';
import { activityProjectionFromWire, type ActivityProjection } from '../lib/activity';

afterEach(cleanup);

// Regression from session 8c5ed98b-851a-4e65-91c1-14fbdc04f1eb: a fast shell
// finishes before wait, so both calls report the same finished command.
it('shows one finished command when wait supersedes spawn status and output', () => {
  const activity = activityProjection();
  const command = 'git status --short --branch';
  const handle = [{ type: 'shell-handle', id: 'git' }];
  const commandBody = [
    { type: 'heading' as const, text: 'Command' },
    { type: 'code' as const, language: 'bash', text: command },
  ];
  const exit = { type: 'markdown' as const, text: '**Exit code:** 0' };
  const spawn = {
    ...activity.rows[0],
    id: 'spawn',
    sequence: 1,
    operation: 'shell',
    presenter: 'shell' as const,
    state: 'succeeded' as const,
    summary: command,
    resources: handle,
    presentation: {
      headline: 'Command finished',
      summary: command,
      content: [...commandBody, exit],
    },
  };
  const wait = {
    ...spawn,
    id: 'wait',
    sequence: 2,
    operation: '_shell-wait',
    presentation: {
      ...spawn.presentation,
      content: [
        ...commandBody,
        { type: 'heading' as const, text: 'Output' },
        { type: 'code' as const, text: '## main...origin/main' },
        exit,
      ],
    },
  };
  activity.rows = [{ ...spawn, id: 'group-spawn', children: [spawn, wait], presentation: undefined }];
  activity.counts = { running: 0, succeeded: 2, failed: 0, cancelled: 0 };
  paintActivity({ activity });
  fireEvent.click(screen.getByRole('button', { name: /Ran.*git status/ }));
  expect(document.querySelectorAll('[data-activity-children] [data-activity-row]')).toHaveLength(1);
  fireEvent.click(screen.getByRole('button', { name: /Command finished/ }));
  expect(screen.getByText('## main...origin/main')).toBeVisible();
  const exitLabel = screen.getByText('Exit code:', { selector: 'strong' });
  expect(exitLabel).toBeVisible();
  expect(exitLabel.parentElement?.textContent).toBe('Exit code: 0');

  // Do not discard an earlier result if it contains output absent from wait.
  cleanup();
  spawn.presentation.content.splice(2, 0, { type: 'code', language: 'text', text: 'earlier-only output' });
  paintActivity({ activity });
  fireEvent.click(screen.getByRole('button', { name: /Ran.*git status/ }));
  expect(document.querySelectorAll('[data-activity-children] [data-activity-row]')).toHaveLength(2);

  // A running spawn is also transient once wait has the complete finished result.
  cleanup();
  const running = {
    ...spawn,
    presentation: {
      headline: 'Running command',
      summary: command,
      content: [...commandBody, { type: 'text' as const, text: 'Running' }],
    },
  };
  activity.rows[0].children = [running, wait];
  paintActivity({ activity });
  fireEvent.click(screen.getByRole('button', { name: /Ran.*git status/ }));
  expect(document.querySelectorAll('[data-activity-children] [data-activity-row]')).toHaveLength(1);
  expect(screen.queryByText('Running command')).toBeNull();
});

it('does not render technical resource IDs as expandable files', () => {
  const activity = structuredClone(storyData.ACTIVITY_RESULTS);
  activity.rows = [activity.rows[4]];
  activity.rows[0].presentation!.content = [];
  activity.rows[0].resources = [
    { type: 'council-thread', id: 'technical-thread-258' },
    { type: 'shell-handle', id: 'technical-shell' },
  ];
  paintActivity({ activity });
  expect(document.body.textContent).not.toContain('technical-thread-258');
  expect(document.body.textContent).not.toContain('technical-shell');
  expect(screen.queryByRole('button', { name: /Published Council message/ })).toBeNull();
});

it.each([
  ['council.publish', 'Published Council message'],
  ['council.get', 'Read Council message'],
])('shows only the %s label and message body', (operation, headline) => {
  const activity = structuredClone(storyData.ACTIVITY_RESULTS);
  activity.rows = activity.rows.filter((row) => row.operation === operation);
  expect(activity.rows).toHaveLength(1);
  paintActivity({ activity });
  expect(screen.getByRole('button', { name: new RegExp(headline) })).toBeVisible();
  expect(document.querySelector('[data-activity-summary]')).toBeNull();
  openEverySettledStep();
  expect(document.body.textContent).toContain(
    'Read links to the file; Patch shows its changes after one disclosure.',
  );
  expect(screen.queryByRole('table')).toBeNull();
  for (const metadata of [
    'Activity review',
    'informational',
    'Created at',
    'Source',
    'Replies',
    'Details truncated',
  ]) {
    expect(document.body.textContent).not.toContain(metadata);
  }
});

it('keeps embedded document headings and code inside their Activity step', () => {
  paintActivity({ activity: storyData.ACTIVITY_RESULTS });
  openEverySettledStep();
  expect(screen.getByRole('heading', { name: 'Activity', level: 5 })).toBeVisible();
  expect(screen.queryAllByRole('region', { name: 'text code' })).toHaveLength(0);
  // Issue #260: a clean verification row no longer embeds its runner output.
  expect(screen.getAllByRole('group', { name: 'text code' })).toHaveLength(1);
});

it('shares a table layout without losing repeated headers or result groups', () => {
  paintActivity({ activity: storyData.ACTIVITY_TABLES });
  openEverySettledStep();
  expect(screen.getAllByRole('table')).toHaveLength(1);
  expect(screen.getAllByRole('columnheader', { name: 'Result' })).toHaveLength(3);
  expect(screen.getByRole('table').querySelectorAll('tbody')).toHaveLength(3);
  expect(screen.getAllByRole('cell', { name: 'informational' })).toHaveLength(3);
  expect(screen.getByRole('cell', { name: /部署/ })).toBeVisible();
});

it('keeps table schemas and intervening content separate, including empty groups', () => {
  const activity = structuredClone(storyData.ACTIVITY_TABLES);
  const table = activity.rows[0].presentation!.content![0];
  if (table.type !== 'table') throw new Error('expected table fixture');
  activity.rows[0].presentation!.content = [
    table,
    { ...table, rows: [] },
    { type: 'text', text: 'Next result' },
    { type: 'text', text: 'Still separate' },
    table,
    { ...table, columns: ['Metric', 'Result'] },
    { ...table, columns: ['Result', 'Metric'] },
    { type: 'table', columns: ['Detail', 'Result', 'State'], rows: [['Literal', '<tag>', '**ready**']] },
  ];
  paintActivity({ activity });
  openEverySettledStep();
  expect(screen.getAllByRole('table')).toHaveLength(5);
  expect(screen.getByText('No rows').getAttribute('colspan')).toBe('2');
  expect(screen.getByText('Next result')).toBeVisible();
  expect(screen.getByText('Still separate')).toBeVisible();
  expect(screen.getByRole('cell', { name: '<tag>' }).innerHTML).toBe('&lt;tag&gt;');
  expect(screen.getByRole('cell', { name: '**ready**' }).textContent).toBe('**ready**');
});

/**
 * The engine's own Activity fixture, parsed. Protocol 7 ships it as a bare
 * projection on the form that produced it, not as a classified view, so the
 * axis takes the snapshot itself.
 */
function activityProjection(): ActivityProjection {
  const projection = activityProjectionFromWire(activityFixture);
  if (!projection) throw new Error('the engine Activity fixture must be paintable');
  return projection;
}

function paintActivity(props: Partial<Parameters<typeof ActivityPanel>[0]> = {}) {
  const { activity = activityProjection(), ...rest } = props;
  render(<ActivityPanel activity={activity} {...rest} />);
  const expand = screen.queryByRole('button', { name: 'Expand Activity' });
  if (expand) fireEvent.click(expand);
  return document.body.innerHTML;
}

/**
 * Every step starts shut: what it did is one line, and what it left waits behind
 * its chevron. A case that reads UNDER a step presses it first — the step's own
 * headline is the toggle. Grouped changes appear with their group, so the press
 * repeats until no step is shut; the folds inside a step (a patch, a count) are
 * left as they stand.
 */
function openEverySettledStep() {
  for (;;) {
    const shut = document.querySelectorAll<HTMLElement>(
      '[data-activity-row] > div > :is(h4, p) > [data-disclosure-toggle][aria-expanded="false"]',
    );
    if (shut.length === 0) return;
    shut.forEach((toggle) => fireEvent.click(toggle));
  }
}

it('reveals a patch diff on the first step disclosure, without another Diff control', () => {
  const activity = activityProjection();
  const patchRow = {
    ...activity.rows[0],
    operation: 'patch',
    presenter: 'patch' as const,
    state: 'succeeded' as const,
    summary: 'src/example.clj',
    result_summary: undefined,
    resources: [],
    evidence: [
      {
        kind: 'diff' as const,
        text: 'src/example.clj',
        additions: 1,
        deletions: 1,
        modifications: 0,
        is_truncated: false,
        is_redacted: false,
        lines: [
          { kind: 'deletion' as const, text: 'old' },
          { kind: 'addition' as const, text: 'new' },
        ],
      },
    ],
  };
  paintActivity({ activity: { ...activity, rows: [patchRow] } });
  openEverySettledStep();
  expect(screen.getByLabelText('Unified diff')).toBeVisible();
  expect(screen.queryByRole('button', { name: /Expand the patch/ })).toBeNull();
  expect(screen.queryByText('Diff', { selector: 'span' })).toBeNull();
});
describe('Activity copy', () => {
  it('copies retained activity while collapsed without opening the band', async () => {
    const writeText = vi.fn().mockResolvedValue(undefined);
    vi.stubGlobal('navigator', { ...navigator, clipboard: { writeText } });
    try {
      const activity = activityProjectionFromWire(argumentCases[0].projection)!;
      const { rerender } = render(<ActivityPanel activity={activity} />);
      fireEvent.click(screen.getByRole('button', { name: 'Copy activity' }));
      await screen.findByRole('button', { name: 'Copied' });
      expect(writeText).toHaveBeenCalledTimes(1);
      const copied = writeText.mock.calls[0][0] as string;
      expect(copied).toContain('First search: 2 matches');
      expect(copied).toContain('Search directory unavailable');
      expect(copied).not.toContain(activity.rows[0].argument_key);
      expect(
        screen.getByRole('button', { name: 'Expand Activity' }).getAttribute('aria-expanded'),
      ).toBe('false');
      fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
      rerender(
        <ActivityPanel
          activity={{
            ...activity,
            state: 'failed',
            rows: activity.rows.map((row) =>
              row.id === 'search-4'
                ? {
                    ...row,
                    state: 'succeeded',
                    result_summary: 'Latest retained result',
                  }
                : row,
            ),
          }}
        />,
      );
      fireEvent.click(screen.getByRole('button', { name: 'Copied' }));
      expect(writeText).toHaveBeenCalledTimes(2);
      expect(writeText.mock.calls[1][0]).toContain('Latest retained result');
      expect(
        screen.getByRole('button', { name: 'Collapse Activity' }).getAttribute('aria-expanded'),
      ).toBe('true');
    } finally {
      vi.unstubAllGlobals();
    }
  });
  it.each(['running', 'succeeded', 'failed', 'cancelled'] as const)(
    'keeps Copy available in the %s state',
    (state) => {
      render(<ActivityPanel activity={{ ...activityProjection(), state }} />);
      const copy = screen.getByRole('button', { name: 'Copy activity' });
      expect(copy.closest('[data-disclosure-toggle]')).toBeNull();
      fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
      expect(screen.getByRole('button', { name: 'Copy activity' })).toBe(copy);
    },
  );
});

describe('joined Activity operation groups', () => {
  function reads() {
    const base = activityProjection();
    const rows = ['a', 'b', 'c'].map((id, sequence) => ({
      ...base.rows[0],
      id,
      sequence,
      operation: 'cat',
      state: 'succeeded' as const,
      summary: `${id}.clj`,
      result_summary: `${id} content`,
      presentation: undefined,
      children: undefined,
      evidence: [],
      resources: [{ type: 'file' as const, id: 'core.clj' }],
    }));
    return {
      ...base,
      state: 'succeeded' as const,
      counts: { running: 0, succeeded: 3, failed: 0, cancelled: 0 },
      rows,
    };
  }

  it('collapses identical arguments, preserves different/unknown calls and every outcome', () => {
    const activity = activityProjectionFromWire(argumentCases[0].projection)!;
    const { rerender } = render(<ActivityPanel activity={activity} />);
    fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
    fireEvent.click(screen.getByRole('button', { name: /Search ×6/ }));
    const repeat = screen.getByRole('button', { name: /same query ×3/ });
    expect(repeat.textContent).toContain('1 running');
    expect(repeat.textContent).toContain('1 failed');
    expect(repeat.getAttribute('aria-expanded')).toBe('false');
    expect(document.querySelector('[data-activity-row="0:search-1"]')).toBeNull();
    for (const id of ['search-2', 'unknown-1', 'unknown-2', 'read-1']) {
      expect(document.querySelector(`[data-activity-row="0:${id}"]`)).toBeInTheDocument();
    }
    expect(screen.queryByText(/Search directory unavailable/)).toBeNull();
    fireEvent.click(repeat);
    // The opened group lists the failed call; the reason itself waits inside that step.
    expect(screen.queryByText(/Search directory unavailable/)).toBeNull();
    fireEvent.click(
      document.querySelector<HTMLElement>(
        '[data-activity-row="0:search-3"] [data-disclosure-toggle]',
      )!,
    );
    expect(screen.getByText(/Search directory unavailable/)).toBeVisible();
    fireEvent.click(
      document.querySelector<HTMLElement>(
        '[data-activity-row="0:search-1"] [data-disclosure-toggle]',
      )!,
    );
    expect(screen.getByText('First search: 2 matches')).toBeVisible();
    expect(document.querySelector('[data-activity-row="0:search-3"]')).toBeInTheDocument();
    expect(document.querySelector('[data-activity-row="0:search-4"]')).toBeInTheDocument();
    rerender(
      <ActivityPanel
        activity={{
          ...activity,
          state: 'failed',
          rows: activity.rows.map((row) =>
            row.id === 'search-4'
              ? {
                  ...row,
                  state: 'succeeded',
                  result_summary: 'Last search: 5 matches',
                  presentation: {
                    headline: 'Searched',
                    summary: 'same query',
                    content: [{ type: 'text', text: 'Last search: 5 matches' }],
                  },
                }
              : row,
          ),
        }}
      />,
    );
    expect(
      screen.getByRole('button', { name: /same query ×3/ }).getAttribute('aria-expanded'),
    ).toBe('true');
    expect(screen.getByText('First search: 2 matches')).toBeVisible();
    fireEvent.click(
      document.querySelector<HTMLElement>(
        '[data-activity-row="0:search-4"] [data-disclosure-toggle]',
      )!,
    );
    expect(screen.getByText('Last search: 5 matches')).toBeVisible();
  });

  it('groups adjacent reads, counts unique files and preserves disclosure through updates', () => {
    const activity = reads();
    const { rerender } = render(<ActivityPanel activity={activity} />);
    const initiallyShut = screen.getByRole('button', {
      name: 'Expand Activity',
    });
    expect(initiallyShut.textContent).toContain('ACTIVITY');
    expect(initiallyShut.textContent).toContain('3 operations');
    expect(screen.queryByRole('button', { name: /Read ×3/ })).toBeNull();
    fireEvent.click(initiallyShut);
    const group = screen.getByRole('button', { name: /Read ×3/ });
    expect(group.textContent).toContain('1 file');
    expect(group.getAttribute('aria-expanded')).toBe('false');
    fireEvent.click(group);
    expect(document.querySelector('[data-activity-row="0:b"]')).toBeInTheDocument();
    rerender(
      <ActivityPanel
        activity={{
          ...activity,
          rows: [...activity.rows, { ...activity.rows[0], id: 'd', sequence: 3 }],
        }}
      />,
    );
    expect(screen.getByRole('button', { name: /Read ×4/ }).getAttribute('aria-expanded')).toBe(
      'true',
    );
    const band = screen.getByRole('button', { name: /Collapse Activity/ });
    fireEvent.click(band);
    fireEvent.click(screen.getByRole('button', { name: /Expand Activity/ }));
    expect(screen.getByRole('button', { name: /Read ×4/ }).getAttribute('aria-expanded')).toBe(
      'true',
    );
  });

  it('keeps ten interleaved read and search runs in one group each', () => {
    const activity = reads();
    const rows = Array.from({ length: 20 }, (_, sequence) => ({
      ...activity.rows[0],
      id: `call-${sequence}`,
      sequence,
      operation: sequence % 2 ? 'grep' : 'cat',
    }));
    const { rerender } = render(<ActivityPanel activity={{ ...activity, rows }} />);
    fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
    expect(document.querySelectorAll('[data-activity-group]')).toHaveLength(2);
    expect(screen.getByRole('button', { name: /Search ×10/ })).toBeVisible();
    fireEvent.click(screen.getByRole('button', { name: /Read ×10/ }));
    expect(
      [...document.querySelectorAll('[data-activity-row]')].map((row) =>
        row.getAttribute('data-activity-row'),
      ),
    ).toEqual(rows.filter((row) => row.operation === 'cat').map((row) => `0:${row.id}`));
    rerender(
      <ActivityPanel
        activity={{
          ...activity,
          rows: [...rows, { ...rows[0], id: 'last-read', sequence: 20 }],
        }}
      />,
    );
    expect(screen.getByRole('button', { name: /Read ×11/ }).getAttribute('aria-expanded')).toBe(
      'true',
    );
    expect(document.querySelectorAll('[data-activity-group]')).toHaveLength(2);
  });

  it.each(['succeeded', 'running', 'failed', 'cancelled'] as const)(
    'shows every operation group without pagination when work is %s',
    (state) => {
      const activity = reads();
      const rows = ['grep', 'cat', 'ls', 'patch', 'doc', 'attach', 'shell'].map(
        (operation, sequence) => ({
          ...activity.rows[0],
          id: `operation-${sequence}`,
          sequence,
          operation,
          state: sequence === 6 ? state : ('succeeded' as const),
        }),
      );
      const counts = { running: 0, succeeded: 6, failed: 0, cancelled: 0 };
      counts[state] += 1;
      paintActivity({ activity: { ...activity, state, counts, rows } });
      expect(screen.getByRole('list', { name: 'Operation groups' }).children).toHaveLength(7);
      expect(
        [...document.querySelectorAll('[data-activity-row]')].map((row) =>
          row.getAttribute('data-activity-row'),
        ),
      ).toEqual(rows.map((row) => `0:${row.id}`));
      expect(
        screen.queryByRole('button', {
          name: /(?:show|hide).*(?:more|fewer).*groups?/i,
        }),
      ).toBeNull();
    },
  );

  it('counts failed and cancelled outcomes in a collapsed group and prints neither', () => {
    const activity = reads();
    render(
      <ActivityPanel
        activity={{
          ...activity,
          state: 'failed',
          counts: { running: 0, succeeded: 1, failed: 1, cancelled: 1 },
          rows: activity.rows.map((row, index) => ({
            ...row,
            state: index === 1 ? 'failed' : index === 2 ? 'cancelled' : 'succeeded',
            error_summary: index === 1 ? 'Permission denied' : undefined,
          })),
        }}
      />,
    );
    expect(screen.queryByRole('button', { name: /Read ×3/ })).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
    expect(screen.getByRole('button', { name: /Read ×3/ }).textContent).toContain('1 failed');
    expect(screen.getByRole('button', { name: /Read ×3/ }).textContent).toContain('1 cancelled');
    expect(screen.queryByText(/Permission denied/)).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: /Read ×3/ }));
    expect(screen.queryByText(/Permission denied/)).toBeNull();
    fireEvent.click(
      document.querySelector<HTMLElement>('[data-activity-row="0:b"] [data-disclosure-toggle]')!,
    );
    expect(screen.getByText(/Permission denied/)).toBeVisible();
    expect(screen.getByText('Cancelled')).toBeVisible();
    fireEvent.click(screen.getByRole('button', { name: /Collapse Activity/ }));
    expect(screen.getByRole('button', { name: /Expand Activity/ }).textContent).toContain(
      '1 failed',
    );
  });
  it('bounds a collapsed receipt while keeping omitted operations explicit', () => {
    const activity = reads();
    const rows = Array.from({ length: 10 }, (_, index) => ({
      ...activity.rows[0],
      id: String(index),
      sequence: index,
      operation: index % 2 ? 'patch' : 'cat',
    }));
    render(
      <ActivityPanel
        activity={{
          ...activity,
          rows,
          omitted: { rows: 7, by_classification: { observation: 7 } },
        }}
      />,
    );
    const receipt = screen.getByRole('button', { name: 'Expand Activity' });
    expect(receipt.textContent).toContain('17 operations');
    expect(receipt.textContent).toContain('7 omitted');
  });

  it('updates commands by snapshot identity, not by parsing identical command strings', () => {
    const activity = reads();
    const rows = activity.rows.slice(0, 2).map((row) => ({
      ...row,
      operation: 'shell',
      summary: 'npm test',
      state: 'running' as const,
    }));
    const { rerender } = render(<ActivityPanel activity={{ ...activity, rows }} />);
    fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
    fireEvent.click(screen.getByRole('button', { name: /Shell ×2/ }));
    const next = {
      ...activity,
      rows: rows.map((row) => ({ ...row, state: 'succeeded' as const })),
    };
    const original = JSON.stringify(next);
    rerender(<ActivityPanel activity={next} />);
    expect(screen.getByRole('button', { name: /Shell ×2/ }).getAttribute('aria-expanded')).toBe(
      'true',
    );
    expect(document.querySelectorAll('[data-activity-row]')).toHaveLength(2);
    expect(JSON.stringify(next)).toBe(original);
  });
});

describe("one form's Activity on the phone", () => {
  it('keeps root operations at one visual depth', () => {
    paintActivity();
    const rows = [...document.querySelectorAll('[data-activity-row]')];
    expect(rows.length).toBeGreaterThan(1);
    expect(rows.every((row) => row.getAttribute('data-activity-depth') === '0')).toBe(true);
    expect(rows.every((row) => !row.parentElement?.closest('[data-activity-row]'))).toBe(true);
  });
  it('keeps the headline and one-line summary visible; the chevron opens only content', () => {
    const projection = activityProjection();
    paintActivity({
      activity: {
        ...projection,
        rows: [
          {
            ...projection.rows[0],
            operation: 'ls',
            summary: '',
            resources: [],
            evidence: [],
            children: [],
            presentation: {
              headline: 'Listed apps/vis-companion/src',
              summary: '3 directories · 2 files',
              content: [{ type: 'text', text: 'Listing details' }],
            },
          },
        ],
      },
    });
    const toggle = screen.getByRole('button', {
      name: /Listed apps\/vis-companion\/src/,
    });
    expect(toggle.getAttribute('aria-expanded')).toBe('false');
    expect(screen.getByText('3 directories · 2 files')).toBeVisible();
    expect(screen.queryByText('Listing details')).toBeNull();
    fireEvent.click(toggle);
    expect(screen.getByText('Listing details')).toBeVisible();
    expect(screen.getAllByText('3 directories · 2 files')).toHaveLength(1);
    fireEvent.click(toggle);
    expect(screen.getByText('3 directories · 2 files')).toBeVisible();
    expect(screen.queryByText('Listing details')).toBeNull();
  });
  it('shows batch sections after opening their call and replaces them without resetting disclosure', () => {
    const activity = storyData.ACTIVITY_LISTING_BATCH;
    const { rerender } = render(<ActivityPanel activity={activity} />);
    fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
    // Regression #230: opening one result must not expand every long section.
    // #251: directory breakdowns stay behind their specific List call.
    expect(document.querySelector('[data-activity-section]')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: /Listed 2 directories/ }));
    const toggle = screen.getByRole('button', {
      name: activity.rows[0].presentation!.sections![0].headline,
    });
    expect(screen.getByText('3 directories · 2 files')).toBeVisible();
    expect(screen.getByText('0 directories · 2 files')).toBeVisible();
    expect(screen.queryByRole('table')).toBeNull();
    const sections = [...document.querySelectorAll('[data-activity-section]')];
    expect(sections[0].classList.contains('mt-1')).toBe(true);
    expect(sections[1].classList.contains('mt-[var(--text-ui--line-height)]')).toBe(true);
    fireEvent.click(toggle);
    expect(screen.getAllByRole('table')).toHaveLength(1);
    expect(screen.getAllByRole('group', { name: 'Activity table' })).toHaveLength(1);
    const row = activity.rows[0];
    rerender(
      <ActivityPanel
        activity={{
          ...activity,
          rows: [
            {
              ...row,
              presentation: {
                ...row.presentation!,
                summary: '8 entries',
              },
            },
          ],
        }}
      />,
    );
    expect(toggle.getAttribute('aria-expanded')).toBe('true');
    expect(screen.getByText('8 entries')).toBeVisible();
    expect(screen.getAllByRole('table')).toHaveLength(1);
    fireEvent.click(toggle);
    expect(screen.queryByRole('table')).toBeNull();
    expect(screen.getByText('0 directories · 2 files')).toBeVisible();
  });
  it('does not put a chevron on a summary-only presentation or infer content headings', () => {
    const base = activityProjection();
    paintActivity({
      activity: {
        ...base,
        rows: [
          {
            ...base.rows[0],
            resources: [],
            evidence: [],
            presentation: {
              headline: 'Listed src',
              summary: '2 files',
              content: [],
            },
          },
        ],
      },
    });
    expect(screen.getByText('2 files')).toBeVisible();
    expect(screen.queryByRole('button', { name: /Listed src/ })).toBeNull();
  });
  it('keeps a presented failure inside the step until the reader opens it', () => {
    const base = activityProjection();
    paintActivity({
      activity: {
        ...base,
        rows: [
          {
            ...base.rows[0],
            state: 'failed',
            resources: [],
            evidence: [],
            error_summary: 'Permission denied',
            presentation: {
              headline: 'List src',
              summary: 'Preparing listing',
              content: [{ type: 'text', text: 'Listing details' }],
            },
          },
        ],
      },
    });
    expect(screen.queryByText('Permission denied')).toBeNull();
    expect(screen.queryByText('Listing details')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: /List src/ }));
    // Opened, the step says what it was doing, what it produced and why it ended, once each.
    expect(screen.getByText('Preparing listing')).toBeVisible();
    expect(screen.getByText('Listing details')).toBeVisible();
    expect(screen.getByText('Permission denied')).toBeVisible();
  });
  it('draws the chronology without being asked, in engine sequence', () => {
    paintActivity();

    const chronology = screen.getByRole('list', {
      name: 'Operation groups',
    });
    const chronologyText = chronology.textContent ?? '';
    expect(chronologyText.indexOf('Searched 18 matches')).toBeLessThan(
      chronologyText.indexOf('suite'),
    );
    // The band folds independently; it does not add another live region.
    expect(chronologyText).not.toContain('[{query: needle}]');
    expect(chronologyText).not.toContain('24 passed');
    expect(screen.getByRole('button', { name: 'Collapse Activity' })).toBeVisible();
    expect(screen.queryByRole('status')).toBeNull();
    expect(screen.queryByRole('button', { name: /interrupt/i })).toBeNull();
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
          ? {
              operation: 'shell',
              summary: 'running: git status',
              duration_ms: 66,
            }
          : { duration_ms: 12_500 }),
      })),
    };

    paintActivity({ activity: settled });

    expect(activityReceiptText(settled, 12_600)).toBe('SHELL · SUITE · 12.6s');
    expect(activityReceiptText({ ...settled, rows: [settled.rows[0]] }, 66)).toBe(
      'SHELL · git status · 66ms',
    );
    expect(screen.getByLabelText('Operation groups').textContent).toContain('Ran git status');
    expect(screen.getByText('66ms')).toBeVisible();
    expect(screen.getByText('12.5s')).toBeVisible();
  });

  it('adds no empty panel before the first operation', () => {
    paintActivity({
      activity: {
        ...activityProjection(),
        state: 'idle',
        rows: [],
        counts: { running: 0, succeeded: 0, failed: 0, cancelled: 0 },
      },
    });
    expect(document.querySelector('[data-activity-axis]')).toBeNull();
  });
});

// The band is named by WHAT IT COST: what changed the repository, what was only
// read, what was checked. `0 mutations` is about the rows that are NOT there, so
// it always prints; the other two print only when they happened.
describe('what the iteration cost', () => {
  it('states the mutations, and stays quiet about a kind that did not happen', () => {
    const parts = activityCostParts(activityProjection());

    expect(parts.map((part) => part.text)).toEqual(['0 mutations', '1 observation', '1 check']);
    // Colour REPEATS the noun. Reading the words alone must lose nothing, so the
    // quiet count wears the margin's own ink and no tone at all.
    expect(parts.map((part) => part.tone)).toEqual([
      'text-accent-ink',
      'text-code-syntax-keyword',
      '',
    ]);
  });

  it('leaves the failures to the marks and the state word', () => {
    const projection = activityProjection();
    const [first, ...rest] = projection.rows;

    expect(
      activityCostParts({
        ...projection,
        counts: { running: 0, succeeded: 1, failed: 1, cancelled: 0 },
        rows: [{ ...first, signal: 'mutation' as const, state: 'failed' as const }, ...rest],
      }).map((part) => part.text),
    ).toEqual(['1 mutation', '1 check']);
  });

  // The ENGINE's own bound is what drops rows, so the cost covers the whole run:
  // a chronology that shows four of ten calls must not report the cost of four,
  // and its tail can say `+6 more` but never what the six WERE.
  it('counts the rows the engine dropped, so a bounded axis cannot under-report', () => {
    const projection = activityProjection();

    expect(
      activityCostParts({
        ...projection,
        rows: [],
        omitted: {
          rows: 6,
          by_classification: { mutation: 6, observation: 2 },
        },
      }).map((part) => part.text),
    ).toEqual(['6 mutations', '2 observations']);
  });
});

describe('the axis is built from the closed vocabulary', () => {
  it("borrows the app's controls and writes no styles of its own", () => {
    expect(activityPanelSource).toContain('<Disclosure');
    expect(activityPanelSource).toContain('<LoadMore');
    // No spinner: a mark that turns says only "still here", while one word says
    // whether the form is still working and, once it is not, how it ended.
    expect(activityPanelSource).not.toContain('<Spinner');
    expect(activityPanelSource).not.toContain('<button');
    expect(activityPanelSource).not.toContain('style={');
    expect(activityPanelSource).not.toContain('style="');
  });
});

// A flat chronology shares the turn spine; it never draws a second set of markers.
describe('a run reads as one thread', () => {
  it('keeps the one list and removes decorative per-operation marks', () => {
    paintActivity();
    expect(screen.getAllByRole('list', { name: 'Operation groups' })).toHaveLength(1);
    for (const step of document.querySelectorAll('[data-activity-row]')) {
      expect(step.querySelector('span.absolute')).toBeNull();
    }
  });
  it('states running, failed and cancelled work in words rather than colour alone', () => {
    const base = activityProjection();
    paintActivity({
      activity: {
        ...base,
        rows: [
          {
            ...base.rows[0],
            id: 'failed',
            state: 'failed',
            error_summary: 'Read refused',
          },
          { ...base.rows[1], id: 'cancelled', state: 'cancelled' },
        ],
      },
    });
    fireEvent.click(
      document.querySelector<HTMLElement>(
        '[data-activity-row="0:failed"] [data-disclosure-toggle]',
      )!,
    );
    expect(screen.getByText('Read refused')).toBeVisible();
    expect(screen.getByText('Cancelled')).toBeVisible();
  });

  it('names a builtin with a verb and an extension operation by its own name', () => {
    paintActivity();

    const chronology = screen.getByLabelText('Operation groups').textContent ?? '';
    expect(chronology).toContain('Searched 18 matches');
    expect(chronology).toContain('suite');
  });

  it('answers a patch with what it changed, and folds only the patch itself', () => {
    const projection = activityProjection();
    const [first, ...rest] = projection.rows;

    paintActivity({
      activity: {
        ...projection,
        rows: [
          {
            ...first,
            operation: 'patch',
            summary: '2 files',
            resources: [{ type: 'file', id: 'src/components/ui.tsx' }],
            evidence: [
              {
                kind: 'diff' as const,
                text: 'src/components/ui.tsx',
                lines: [{ kind: 'addition' as const, text: 'added' }],
                additions: 7,
                deletions: 3,
                modifications: 0,
                is_truncated: false,
                is_redacted: false,
              },
            ],
          },
          ...rest,
        ],
      },
    });

    // Regression, T107 design review: the patch hung its paths inside a bordered
    // card, under the word "Patch" in bold — the row's own head printed a second
    // time, twenty pixels lower. The head is gone; only the diff still folds.
    openEverySettledStep();
    expect(screen.queryByText('Patch')).toBeNull();
    expect(screen.queryByText('Changed files')).toBeNull();
    expect(screen.queryByText('1 file')).toBeNull();
    expect(document.querySelector('[data-path="src/components/ui.tsx"]')).toBeInTheDocument();

    // Only the patch text folds.
    expect(screen.queryByText('added')).toBeNull();
    fireEvent.click(
      screen.getByRole('button', {
        name: 'Expand the diff of src/components/ui.tsx',
      }),
    );
    expect(screen.getByText('added')).toBeVisible();
  });

  it('gives a step with nothing to open no chevron and no toggle of its own', () => {
    paintActivity();

    // The band folds, but an operation with no details promises no disclosure.
    expect(screen.queryByRole('button', { name: /Searched/ })).toBeNull();
    expect(document.querySelectorAll('[data-activity-row] [data-disclosure-toggle]')).toHaveLength(
      0,
    );
  });

  it('keeps what a step left behind its chevron until the step is pressed', () => {
    const projection = activityProjection();
    const [first, ...rest] = projection.rows;

    paintActivity({
      activity: {
        ...projection,
        rows: [
          {
            ...first,
            resources: [{ type: 'file', id: 'src/components/ui.tsx' }],
            presentation: {
              headline: 'Searched',
              summary: 'src/components/ui.tsx',
              content: [
                {
                  type: 'code',
                  text: 'src/components/ui.tsx:12: matched\nsrc/components/ui.tsx:40: matched',
                },
              ],
            },
          },
          ...rest,
        ],
      },
    });

    const step = screen.getByRole('button', { name: /Searched/ });
    expect(step.getAttribute('aria-expanded')).toBe('false');
    expect(document.querySelector('[data-path="src/components/ui.tsx"]')).toBeNull();
    expect(screen.queryByText(/ui\.tsx:40: matched/)).toBeNull();

    fireEvent.click(step);
    expect(step.getAttribute('aria-expanded')).toBe('true');
    expect(document.querySelector('[data-path="src/components/ui.tsx"]')).toBeInTheDocument();
    expect(screen.getByText(/ui\.tsx:40: matched/)).toBeVisible();
  });

  it('lists the paths a step touched under its own line', () => {
    const projection = activityProjection();
    const [first, ...rest] = projection.rows;

    paintActivity({
      activity: {
        ...projection,
        rows: [
          {
            ...first,
            resources: [{ type: 'file', id: 'src/components/ui.tsx' }],
          },
          ...rest,
        ],
      },
    });

    openEverySettledStep();
    expect(document.querySelector('[data-path="src/components/ui.tsx"]')).toBeInTheDocument();
    // A path is a path everywhere on the axis: the type badge belonged to the
    // patch card, and that card is gone.
    expect(screen.queryByText('TSX')).toBeNull();
  });

  it('prints four paths and folds the rest behind one quiet count', () => {
    const projection = activityProjection();
    const [first] = projection.rows;
    const paths = [
      'src/components/ActivityPanel.tsx',
      'src/components/ChatContent.tsx',
      'src/components/ui.tsx',
      'src/index.css',
      'src/lib/activity.ts',
      'src/dev/story-data.ts',
    ];

    paintActivity({
      activity: {
        ...projection,
        rows: [
          {
            ...first,
            resources: paths.map((id) => ({ type: 'file' as const, id })),
          },
        ],
      },
    });

    // Six paths under one step is the row with the least to say spending the most
    // height on saying it. Four print; the rest are a count, one press away.
    openEverySettledStep();
    expect(document.querySelectorAll('[data-path]')).toHaveLength(4);
    expect(document.querySelector('[data-path="src/lib/activity.ts"]')).toBeNull();

    fireEvent.click(screen.getByRole('button', { name: 'Show 2 more paths' }));
    expect(document.querySelectorAll('[data-path]')).toHaveLength(6);
  });
});

// An error is the one thing on the axis that can be forty lines long, so when a reader
// opens the step it arrives UNCLAMPED — the whole of it, where the invocation is. A panel
// nobody opened stays quiet: a failure paints nothing over a reader who is not reading it.
describe('a step that ended badly', () => {
  function paintFailure(text: string) {
    const projection = activityProjection();
    const [first, ...rest] = projection.rows;

    paintActivity({
      activity: {
        ...projection,
        state: 'failed' as const,
        rows: [
          {
            ...first,
            state: 'failed' as const,
            error_summary: 'no match',
            evidence: [{ kind: 'error' as const, text }],
          },
          ...rest,
        ],
      },
    });
  }

  function openFailure() {
    fireEvent.click(
      document.querySelector<HTMLElement>(
        '[data-activity-row="0:call-1"] [data-disclosure-toggle]',
      )!,
    );
  }

  it('says how it failed on its own line, once the step is opened', () => {
    paintFailure('patch refused: no anchor matched');

    expect(screen.queryByText('patch refused: no anchor matched')).toBeNull();
    openFailure();

    // The machine's own text IS the reason. The row stamps no word on top of it:
    // the filled mark is the whole of the colour a failure gets.
    expect(document.querySelector('[data-activity-row="0:call-1"]')?.textContent).not.toContain(
      'NO MATCH',
    );
    expect(screen.getByText('patch refused: no anchor matched')).toBeVisible();
  });

  // Regression, T131: a refusal was clamped to three lines with the rest behind a
  // rule, so the reader had to leave the axis to learn why the patch was refused.
  it('says the whole of what the machine said, however many lines', () => {
    paintFailure(['one', 'two', 'three', 'four', 'five'].join('\n'));
    openFailure();

    expect(screen.getByText('one')).toBeVisible();
    expect(screen.getByText('three')).toBeVisible();
    expect(screen.getByText('five')).toBeVisible();
    expect(screen.queryByText('2 more lines')).toBeNull();
  });
});

// One fact, one place. The mark, the verb and the pill each said "this failed",
// the error card repeated the row it hangs under, and a row counted the very list
// of paths printed below it — four spellings of two facts.
describe('the axis says a thing once', () => {
  function paintStep(row: Partial<ActivityProjection['rows'][number]>) {
    const projection = activityProjection();
    const [first] = projection.rows;
    paintActivity({
      activity: {
        ...projection,
        state: 'failed' as const,
        rows: [{ ...first, ...row }],
      },
    });
  }

  const refusedPatch = {
    operation: 'patch',
    summary: 'src/components/ui.tsx',
    state: 'failed' as const,
    error_summary: 'no match',
    evidence: [{ kind: 'error' as const, text: 'patch refused: no anchor matched' }],
  };

  it('gives a failed step its own verb instead of the settled one', () => {
    paintStep(refusedPatch);

    const chronology = screen.getByLabelText('Operation groups').textContent ?? '';
    expect(chronology).toContain('Patch refused');
    expect(chronology).not.toContain('Patched');
  });

  it('names the operation and its object once, never again as a card head', () => {
    paintStep(refusedPatch);

    const chronology = screen.getByLabelText('Operation groups').textContent ?? '';
    expect(chronology).not.toContain('NO MATCH');
    expect(chronology.match(/src\/components\/ui\.tsx/g) ?? []).toHaveLength(1);
  });

  it("prints the engine's reason only when no text opens under the step", () => {
    paintStep({
      state: 'failed' as const,
      error_summary: 'the provider closed the stream before the first token',
      evidence: [],
    });
    fireEvent.click(
      document.querySelector<HTMLElement>(
        '[data-activity-row="0:call-1"] [data-disclosure-toggle]',
      )!,
    );

    const chronology = screen.getByLabelText('Operation groups').textContent ?? '';
    expect(chronology).not.toContain('FAILED');
    expect(chronology).toContain('the provider closed the stream before the first token');
  });

  it('lets the paths stand for a summary that does nothing but count them', () => {
    paintStep({
      operation: 'cat',
      summary: '2 files',
      state: 'succeeded' as const,
      resources: [
        { type: 'file', id: 'src/components/ui.tsx' },
        { type: 'file', id: 'src/index.css' },
      ],
      evidence: [],
    });

    openEverySettledStep();
    const chronology = screen.getByLabelText('Operation groups').textContent ?? '';
    expect(chronology).toContain('Read');
    expect(chronology).not.toContain('2 files');
    expect(document.querySelector('[data-path="src/index.css"]')).toBeInTheDocument();
  });

  it('prioritizes the file name and retains the full path', () => {
    paintStep({
      operation: 'cat',
      summary: 'one file',
      state: 'succeeded' as const,
      resources: [
        {
          type: 'file',
          id: 'src/com/blockether/vis/internal/channel/render.clj',
        },
      ],
      evidence: [],
    });

    openEverySettledStep();
    const path = document.querySelector(
      '[data-path="src/com/blockether/vis/internal/channel/render.clj"]',
    );
    const name = path?.lastElementChild;

    expect(name?.textContent).toBe('render.clj');
    expect(name?.className).toContain('shrink-0');
    expect(path?.getAttribute('title')).toBe('src/com/blockether/vis/internal/channel/render.clj');
  });
});

// A chronology inside a live region is re-read from the top on every render, and a
// running step whose time column stands empty reads as a number that went missing.
describe('what the axis does while the work is still moving', () => {
  it('opens retained steps while live and keeps them open across replacements', () => {
    const activity = ACTIVITY_LONG_RUNNING;
    const rows = activity.rows;
    const { rerender } = render(<ActivityPanel activity={activity} />);
    fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
    expect(document.querySelector('[data-activity-row="0:live-4"]')).toBeNull();
    expect(screen.getByText(/search-6 · running/)).toBeVisible();
    fireEvent.click(screen.getByRole('button', { name: /Search ×7/ }));
    const step = screen.getByRole('button', { name: /Searched.*search-4/ });
    fireEvent.click(step);
    expect(screen.getByText('result-4')).toBeVisible();
    rerender(
      <ActivityPanel
        activity={{
          ...activity,
          rows: rows.map((row) => (row.id === 'live-6' ? { ...row, state: 'succeeded' } : row)),
        }}
      />,
    );
    expect(screen.getByText('result-4')).toBeVisible();
    fireEvent.click(screen.getByRole('button', { name: /Search ×7/ }));
    expect(document.querySelector('[data-activity-row="0:live-4"]')).toBeNull();
    expect(screen.getByRole('button', { name: /Search ×7/ }).getAttribute('aria-expanded')).toBe(
      'false',
    );
  });

  it('never hides failed or cancelled steps behind the retained-step fold', () => {
    const base = activityProjection();
    const rows = Array.from({ length: 6 }, (_, index) => ({
      ...base.rows[0],
      id: `step-${index}`,
      sequence: index,
      summary: `search-${index}`,
      operation: 'grep',
      state: (index === 4 ? 'failed' : index === 5 ? 'cancelled' : 'succeeded') as
        | 'failed'
        | 'cancelled'
        | 'succeeded',
    }));
    render(<ActivityPanel activity={{ ...base, rows }} />);
    fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
    const group = screen.getByRole('button', { name: /Search ×6/ });
    expect(group.textContent).toContain('1 failed');
    expect(group.textContent).toContain('1 cancelled');
    fireEvent.click(group);
    expect(document.querySelector('[data-activity-row="0:step-4"]')).toBeInTheDocument();
    expect(document.querySelector('[data-activity-row="0:step-5"]')).toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /more steps/i })).toBeNull();
  });

  it('says the clock is still counting instead of leaving the column empty', () => {
    const projection = activityProjection();
    const [first] = projection.rows;
    const running = { ...first, state: 'running' as const };
    delete running.duration_ms;

    paintActivity({ activity: { ...projection, rows: [running] } });

    expect(screen.getByLabelText('Operation groups').textContent).toContain('…');
  });

  it('silences the live region it sits inside', () => {
    paintActivity();

    expect(document.querySelector('[data-activity-axis]')?.getAttribute('aria-live')).toBe('off');
  });

  it('shows retained summaries without truncation notices or empty disclosures', () => {
    const base = activityProjection();
    const row = {
      ...base.rows[0],
      state: 'succeeded' as const,
      result_summary: undefined,
      evidence: [],
      resources: [],
      presentation: { headline: 'Searched', summary: '2 matches', content: [] },
      is_truncated: true,
    };
    paintActivity({ activity: { ...base, rows: [row] } });
    expect(screen.queryByText('Details truncated')).toBeNull();
    expect(document.querySelector(`[data-activity-row="0:${row.id}"] button`)).toBeNull();
    expect(screen.getByText('2 matches')).toBeVisible();
  });

  it('labels discarded steps as unavailable, not as a show-more control', () => {
    paintActivity({
      activity: {
        ...activityProjection(),
        omitted: { rows: 6, by_classification: { observation: 6 } },
      },
    });
    const tail = screen.getByText('6 steps omitted · Activity limit');
    expect(tail.closest('button')).toBeNull();
    expect(screen.queryByRole('button', { name: /6 more steps/i })).toBeNull();
  });
});

// Regression: a code block's own writes entered the chronology as unrelated top-level
// rows, and every row's words were read literally, so `probe_1.json` could not be marked
// as code and a group had no head naming the one cause that produced it.
describe('what a code block changed with its own hands', () => {
  it('hangs every change under one cause, indented, and stops at three levels', () => {
    render(<ActivityPanel activity={ACTIVITY_TREE_CHANGES} />);
    openEverySettledStep();

    const chronology = screen.getByLabelText('Operation groups');
    const heads = chronology.querySelectorAll('[data-activity-depth="0"]');
    const children = chronology.querySelectorAll('[data-activity-depth="1"]');

    expect(heads).toHaveLength(1);
    expect(heads[0].querySelector('h4')?.textContent).toContain(
      'Changed 13 files and 2 directories',
    );
    expect(children).toHaveLength(5);
    expect(
      Array.from(children, (child) => {
        const head = child.querySelector('p')!;
        const duration = head.querySelector('time')?.textContent ?? '';
        return (head.textContent ?? '').replace(duration, '');
      }),
    ).toEqual([
      'Created 2 directories',
      // A change that carries a diff prints it, and a count already answered by the
      // paths listed under it is not printed a second time.
      'Wrote +4 −2',
      'Copied',
      'Moved vis/PLAN.md → docs/PLAN.md',
      'Deleted 6 files',
    ]);
    // Three levels, hard stop: the cause, the change, the paths it touched.
    expect(chronology.querySelector('[data-activity-depth="2"]')).toBeNull();
  });

  it('says who did it in the head, and marks only what the engine marked', () => {
    render(<ActivityPanel activity={ACTIVITY_TREE_CHANGES} />);
    openEverySettledStep();

    const head = document.querySelector('[data-activity-depth="0"]');
    const moved = document.querySelectorAll('[data-activity-depth="1"]')[3];

    // Engine-observed changes retain paths and diffs, not a generic summary.
    expect(head?.textContent).not.toContain('The code block changed these itself');
    // A marked name is code; the row keeps no backtick of its own.
    expect(moved?.querySelector('code')?.textContent).toBe('vis/PLAN.md');
  });

  it('leaves the paths to the change that touched them', () => {
    render(<ActivityPanel activity={ACTIVITY_TREE_CHANGES} />);
    openEverySettledStep();

    const head = document.querySelector('[data-activity-depth="0"]');
    const deleted = document.querySelectorAll('[data-activity-depth="1"]')[4];

    // The head carries every child's resource on the wire; painting them there AND
    // under each change is the same paths printed twice.
    expect(head?.querySelector(':scope > div [data-path]')).toBeNull();
    expect(deleted?.querySelectorAll('[data-path]').length).toBe(4);
    expect(deleted?.textContent).toContain('show 2 more files');
  });
});

// Regression, T120 design review: a step that changed several files hung ONE fold over
// every patch concatenated behind a `--- (path)` line, so the reader found a file by
// reading a header out of the diff and the payload's bound was spent on whichever file
// came first.
describe('a change opens under the file it changed', () => {
  it('gives every changed file its own fold, and opens only that one', () => {
    render(<ActivityPanel activity={ACTIVITY_TREE_CHANGES} />);
    openEverySettledStep();

    const write = document.querySelectorAll('[data-activity-depth="1"]')[1];
    // The change's own headline opened with the group; what is left to fold is
    // one patch per file.
    const folds = write.querySelectorAll('[data-disclosure-toggle][aria-label]');

    expect(Array.from(folds, (fold) => fold.getAttribute('aria-label'))).toEqual([
      'Expand the diff of /Users/dev/vis/apps/vis-companion/src/dev/story-data.ts',
      'Expand the diff of /Users/dev/vis/apps/vis-companion/src/components/ActivityPanel.tsx',
      'Expand the diff of /Users/dev/vis/apps/vis-companion/src/lib/path.ts',
    ]);

    expect(screen.queryByLabelText('Unified diff')).toBeNull();
    fireEvent.click(folds[2]);

    const opened = screen.getAllByLabelText('Unified diff');
    expect(opened).toHaveLength(1);
    expect(opened[0].textContent).toContain('homeifyPath(root)');
    expect(opened[0].textContent).not.toContain('summary_format');
  });
});

// Regression, T120 design review: every row printed the machine's whole
// `/Users/…/vis/` prefix, which is the one part `truncate` never eats.
describe('a path reads short and stays addressable', () => {
  const pathOf = (id: string) => document.querySelector(`[data-path="${id}"]`);
  const written = '/Users/dev/vis/apps/vis-companion/src/lib/path.ts';

  it('shortens against the workspace root and keeps the absolute id', () => {
    render(
      <WorkspaceRootsContext.Provider value={['/Users/dev/vis']}>
        <ActivityPanel activity={ACTIVITY_TREE_CHANGES} />
      </WorkspaceRootsContext.Provider>,
    );
    openEverySettledStep();

    expect(pathOf(written)?.textContent).toBe('apps/vis-companion/src/lib/path.ts');
  });

  it('falls back to the home form when no root owns the file', () => {
    render(<ActivityPanel activity={ACTIVITY_TREE_CHANGES} />);
    openEverySettledStep();

    expect(pathOf(written)?.textContent).toBe('~/vis/apps/vis-companion/src/lib/path.ts');
  });
});

// Regression, T121: a story fixture spelled a diff line's own `+`/`-` into its text
// while the renderer draws that sign in its own marker column, so the review picture
// showed `+ +` and `- -` on a surface the engine never feeds that way — it strips the
// sign in `internal/activity/event.clj` and leaves the column to say it.
describe('a diff line carries its sign only once', () => {
  const SIGNED = new Set(['addition', 'deletion', 'context']);

  it('leaves the sign to the marker column in every story fixture', () => {
    const doubled: string[] = [];
    const seen = new Set<unknown>();
    const visit = (value: unknown) => {
      if (!value || typeof value !== 'object' || seen.has(value)) return;
      seen.add(value);
      if (Array.isArray(value)) {
        value.forEach(visit);
        return;
      }
      const node = value as Record<string, unknown>;
      if (node.kind === 'diff' && Array.isArray(node.lines)) {
        for (const line of node.lines as { kind: string; text: string }[]) {
          if (SIGNED.has(line.kind) && /^[-+]/.test(line.text)) {
            doubled.push(`${line.kind}: ${line.text}`);
          }
        }
      }
      Object.values(node).forEach(visit);
    };

    visit(storyData);
    expect(doubled).toEqual([]);
  });
});

describe('tool-authored activity results', () => {
  it('does not render generic result summaries or offer an empty disclosure', () => {
    const activity = activityProjection();
    activity.rows = [
      {
        ...activity.rows[0],
        resources: [],
        evidence: [],
        result_summary: 'Unrequested result preview',
      },
    ];
    paintActivity({ activity });
    openEverySettledStep();
    expect(screen.queryByText('Unrequested result preview')).toBeNull();
    expect(screen.queryByRole('button', { name: /result summary/ })).toBeNull();
    expect(document.querySelector('[data-activity-row] button')).toBeNull();
  });
});

it('renders symbol content and replaces progress without changing lifecycle', () => {
  const activity = activityProjection();
  activity.rows = [
    {
      ...activity.rows[0],
      state: 'running',
      presentation: {
        headline: 'Verification',
        summary: '1 of 2 checks',
        content: [
          { type: 'markdown', text: '**Prepared** workspace' },
          {
            type: 'table',
            columns: ['Suite', 'Result'],
            rows: [['unit', 'passed']],
          },
          { type: 'code', language: 'python', text: 'print(42)' },
          { type: 'diff', text: '+added\n-removed' },
          { type: 'progress', label: 'Checking', value: 1, total: 2 },
          { type: 'image', attachment_id: 'screen', label: 'Screenshot' },
        ],
      },
    },
  ];
  const { rerender } = render(<ActivityPanel activity={activity} />);
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  expect(screen.getByRole('heading', { name: /Verification/ })).toBeVisible();
  expect(screen.getByText('Prepared').tagName).toBe('STRONG');
  expect(screen.getByRole('cell', { name: 'passed' })).toBeVisible();
  expect(screen.getByRole('progressbar', { name: 'Checking' }).getAttribute('value')).toBe('1');
  expect(screen.getByText('Attachment unavailable')).toBeVisible();
  const next = {
    ...activity,
    rows: [
      {
        ...activity.rows[0],
        presentation: {
          headline: 'Verification',
          summary: '2 of 2 checks',
          content: [{ type: 'text' as const, text: 'Finished stage' }],
        },
      },
    ],
  };
  rerender(<ActivityPanel activity={next} />);
  expect(screen.queryByRole('progressbar')).toBeNull();
  expect(screen.getByText('Finished stage')).toBeVisible();
});

// Regression #230: long evidence is independent of the primary result disclosure.
it.each(['running', 'succeeded', 'failed', 'cancelled'] as const)(
  'keeps section bodies independently collapsed for a %s activity',
  (state) => {
    const activity = activityProjection();
    activity.rows = [
      {
        ...activity.rows[0],
        state,
        resources: [],
        evidence: [],
        error_summary: undefined,
        presentation: {
          headline: 'Read session',
          summary: 'Three turns',
          content: [{ type: 'text', text: 'Primary overview' }],
          sections: [
            {
              headline: 'Turn details',
              summary: 'Three requests',
              content: [{ type: 'text', text: 'Full request body' }],
            },
            {
              headline: 'Failure details',
              summary: 'One failure',
              content: [{ type: 'code', text: 'Unique failure body' }],
            },
            { headline: 'No further diagnostics', summary: '', content: [] },
          ],
        },
      },
    ];
    paintActivity({ activity });
    const root = screen.getByRole('button', { name: /Read session/ });
    const turns = screen.getByRole('button', { name: 'Turn details' });
    const failures = screen.getByRole('button', { name: 'Failure details' });
    expect(turns.getAttribute('aria-expanded')).toBe('false');
    expect(failures.getAttribute('aria-expanded')).toBe('false');
    expect(screen.queryByRole('button', { name: 'No further diagnostics' })).toBeNull();
    expect(screen.queryByText('Full request body')).toBeNull();
    expect(screen.queryByText('Unique failure body')).toBeNull();
    fireEvent.click(root);
    expect(screen.queryByText('Full request body')).toBeNull();
    fireEvent.click(turns);
    expect(screen.getByText('Full request body')).toBeVisible();
    expect(screen.queryByText('Unique failure body')).toBeNull();
    fireEvent.click(root);
    expect(screen.getByText('Full request body')).toBeVisible();
    fireEvent.click(failures);
    expect(screen.getAllByText('Unique failure body')).toHaveLength(1);
    fireEvent.click(turns);
    expect(screen.queryByText('Full request body')).toBeNull();
    expect(screen.getByText('Unique failure body')).toBeVisible();
  },
);

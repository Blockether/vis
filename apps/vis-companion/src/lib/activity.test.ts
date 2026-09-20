/** The Activity projection against `internal.activity.core`'s own bounded snapshot. */
import { describe, expect, it, vi } from 'vitest';
import {
  activityCopyText,
  activityHistoryCopyText,
  mergeActivity,
  argumentGroups,
  operationGroups,
  activityProjectionFromWire,
  ACTIVITY_PRESENTERS,
  ACTIVITY_SIGNALS,
  ACTIVITY_STATES,
  ACTIVITY_TEXT_FORMATS,
} from './activity';
import contract from '../../../../packages/vis-contract/resources/vis-contract/schema/activity.json';
import cases from '../../../../packages/vis-contract/resources/vis-contract/fixtures/activity-cases.json';
import groupingCases from '../../../../packages/vis-contract/resources/vis-contract/fixtures/activity-groups.json';
import argumentCases from '../../../../packages/vis-contract/resources/vis-contract/fixtures/activity-arguments.json';
import copyCases from '../../../../packages/vis-contract/resources/vis-contract/fixtures/activity-copy.json';
import {
  activityHistoryPage,
  groupedActivityHistoryPage,
  GROUPED_ACTIVITY_HISTORY_IDS,
} from '../dev/activity-history';

for (const sample of copyCases) {
  it(`portable Activity copy: ${sample.name}`, () => {
    const projection = activityProjectionFromWire(sample.projection);
    expect(activityCopyText(projection!)).toBe(sample.text);
  });
}
for (const sample of argumentCases) {
  it(`portable argument grouping: ${sample.name}`, () => {
    const projection = activityProjectionFromWire(sample.projection);
    expect(projection).toEqual(sample.projection);
    expect(
      argumentGroups(projection!.rows).map(({ id, rows }) => ({
        id,
        rows: rows.map((row) => row.id),
      })),
    ).toEqual(sample.groups);
  });
}
// Regression #201: shared fixtures include extension headlines and fallback order.
for (const sample of groupingCases) {
  it(`portable grouping: ${sample.name}`, () => {
    const projection = activityProjectionFromWire(sample.projection);
    expect(
      operationGroups(projection!.rows).map(({ id, label, rows }) => ({
        id,
        label,
        rows: rows.map((row) => row.id),
      })),
    ).toEqual(sample.groups);
    expect(projection).toEqual(sample.projection);
  });
}
const activityProjection = (
  state: 'running' | 'succeeded' | 'failed' | 'cancelled' = 'running',
) => ({
  state,
  counts: {
    running: state === 'running' ? 1 : 0,
    succeeded: state === 'succeeded' ? 1 : 0,
    failed: state === 'failed' ? 1 : 0,
    cancelled: state === 'cancelled' ? 1 : 0,
  },
  rows: [
    {
      id: 'call-1',
      sequence: 1,
      operation: 'suite',
      presenter: 'tests',
      signal: 'verification',
      state,
      summary: 'suite',
      resources: [],
      evidence: [{ kind: 'arguments', text: 'suite' }],
    },
  ],
  omitted: { rows: 0, by_classification: {} },
});

describe('canonical Activity admission across SDK, engine and surfaces', () => {
  it('uses the canonical vocabulary', () => {
    expect(ACTIVITY_PRESENTERS).toEqual(contract.$defs.presenter.enum);
    expect(ACTIVITY_SIGNALS).toEqual(contract.$defs.signal.enum);
    expect(ACTIVITY_STATES).toEqual(contract.$defs.state.enum);
    expect(ACTIVITY_TEXT_FORMATS).toEqual(contract.$defs.text_format.enum);
  });
  for (const sample of cases) {
    it(sample.name, () => {
      expect(activityProjectionFromWire(sample.projection)).toEqual(
        sample.valid ? sample.projection : null,
      );
    });
  }
});

describe("one form's Activity read off the wire", () => {
  // Protocol 7 took Activity off the Live View rail: it is no longer a classified
  // view with a projection hanging off it. Protocol 9 carries every revision on
  // `block.activity`; only the settled replacement is durable.
  it('reads a bare projection off the wire', () => {
    expect(activityProjectionFromWire(activityProjection())).toMatchObject({
      state: 'running',
    });
  });

  it('reads structured diff evidence and rejects incomplete lines', () => {
    const projection = activityProjection();
    const diff = {
      kind: 'diff',
      text: 'fixture.clj',
      lines: [
        { kind: 'hunk', text: '@@ -1 +1 @@' },
        { kind: 'deletion', text: '[REDACTED]', is_redacted: true },
        { kind: 'addition', text: 'after' },
      ],
      additions: 0,
      deletions: 0,
      modifications: 1,
      is_truncated: true,
      is_redacted: true,
    };
    const withDiff = {
      ...projection,
      rows: [{ ...projection.rows[0], evidence: [diff] }],
    };
    expect(activityProjectionFromWire(withDiff)?.rows[0].evidence[0]).toEqual(diff);
    expect(
      activityProjectionFromWire({
        ...withDiff,
        rows: [
          {
            ...withDiff.rows[0],
            evidence: [{ ...diff, lines: [{ kind: 'addition' }] }],
          },
        ],
      }),
    ).toBeNull();
  });

  it('rejects a missing, malformed, or retired Activity projection', () => {
    expect(activityProjectionFromWire(undefined)).toBeNull();
    expect(
      activityProjectionFromWire({
        ...activityProjection(),
        rows: [{ id: 'broken' }],
      }),
    ).toBeNull();
    // Protocol 8 dropped both: a payload still wearing either came from a gateway
    // the compatibility gate should already have refused.
    expect(
      activityProjectionFromWire({
        ...activityProjection(),
        schema_version: 1,
      }),
    ).toBeNull();
    expect(
      activityProjectionFromWire({
        ...activityProjection(),
        anchor: { iteration: 1, form_index: 0 },
      }),
    ).toBeNull();
  });

  it('rejects projections outside the one canonical closed shape', () => {
    const projection = activityProjection();
    const row = projection.rows[0];

    expect(activityProjectionFromWire({ ...projection, extra: true })).toBeNull();
    expect(
      activityProjectionFromWire({
        ...projection,
        counts: { ...projection.counts, total: 1 },
      }),
    ).toBeNull();
    expect(
      activityProjectionFromWire({
        ...projection,
        omitted: { ...projection.omitted, total: 0 },
      }),
    ).toBeNull();
    expect(activityProjectionFromWire({ ...projection, rows: [row, { ...row }] })).toBeNull();
    expect(
      activityProjectionFromWire({
        ...projection,
        rows: [{ ...row, duration_ms: 1.5 }],
      }),
    ).toBeNull();
    expect(
      activityProjectionFromWire({
        ...projection,
        rows: [{ ...row, resources: [{ type: 'file', id: 'a.clj', extra: true }] }],
      }),
    ).toBeNull();
  });

  it('carries a per-field text format and refuses one it does not know', () => {
    const projection = activityProjection();
    const row = projection.rows[0];
    const marked = {
      ...projection,
      rows: [
        {
          ...row,
          summary_format: 'inline',
          result_summary: 'wrote `a.clj`',
          result_format: 'markdown',
        },
      ],
    };

    expect(activityProjectionFromWire(marked)?.rows[0].summary_format).toBe('inline');
    expect(activityProjectionFromWire(marked)?.rows[0].result_format).toBe('markdown');
    // A row with no flag stays plain text; the app never guesses a format.
    expect(activityProjectionFromWire(projection)?.rows[0].summary_format).toBeUndefined();
    expect(
      activityProjectionFromWire({
        ...projection,
        rows: [{ ...row, summary_format: 'html' }],
      }),
    ).toBeNull();
    expect(
      activityProjectionFromWire({
        ...projection,
        rows: [{ ...row, result_format: true }],
      }),
    ).toBeNull();
  });
});

// #212: a bounded page is not a retention cap.
it('admits a durable history window beyond the former receipt size', () => {
  const page = activityProjection('succeeded');
  const row = page.rows[0];
  const rows = Array.from({ length: 24 }, (_, index) => ({
    ...row,
    id: `call-${index}`,
    sequence: index + 1,
    presentation: {
      headline: 'Read',
      summary: '',
      content: [{ type: 'code', language: 'text', text: 'x'.repeat(4000) }],
    },
  }));
  const history = {
    id: '12345678-1234-1234-1234-123456789012',
    revision: 1,
    total: 160,
    after: 0,
    next_after: 24,
  };
  expect(activityProjectionFromWire({ ...page, rows, history })).toMatchObject({
    history,
    rows,
  });
  for (const invalid of [
    { ...history, next_after: 0 },
    { ...history, total: -1 },
    { ...history, id: '' },
  ]) {
    expect(activityProjectionFromWire({ ...page, rows, history: invalid })).toBeNull();
  }
  expect(
    activityProjectionFromWire({
      ...page,
      history,
      rows: Array.from({ length: 33 }, (_, i) => ({ ...row, id: `over-${i}`, sequence: i + 1 })),
    }),
  ).toBeNull();
  // Inline history has no total row cap; only history-bearing transport windows do.
  expect(
    activityProjectionFromWire({
      ...page,
      rows: Array.from({ length: 160 }, (_, i) => ({ ...row, id: `inline-${i}`, sequence: i + 1 })),
    })?.rows,
  ).toHaveLength(160);
  const children = Array.from({ length: 32 }, (_, i) => ({
    ...row,
    id: `nested-${i}`,
    sequence: i + 1,
  }));
  const grouped = {
    ...page,
    history,
    rows: [{ ...row, id: 'group', operation: 'shell', children }],
  };
  expect(activityProjectionFromWire(grouped)?.rows[0].children).toHaveLength(32);
  expect(
    activityProjectionFromWire({ ...grouped, rows: [{ ...grouped.rows[0], id: children[0].id }] }),
  ).toBeNull();
});

// #218: complete result content must survive admission and copying, not only the headline.
describe('complete Activity presentation content', () => {
  const body = `${'x'.repeat(1_100_000)} final-code-detail`;
  const cell = `${'y'.repeat(300)} final-cell-detail`;
  const columns = Array.from({ length: 17 }, (_, i) => `Field ${i}`);
  const rows = Array.from({ length: 201 }, (_, i) =>
    columns.map((_, j) => (i === 200 && j === 16 ? cell : `${i}-${j}`)),
  );
  const presentation = {
    headline: 'Find builds for review',
    summary: 'Complete build results',
    content: [
      { type: 'code', language: 'text', text: body },
      { type: 'table', columns, rows },
      ...Array.from({ length: 33 }, (_, i) => ({ type: 'text', text: `Detail ${i}` })),
    ],
    sections: Array.from({ length: 9 }, (_, i) => ({
      headline: `Build ${i}`,
      summary: 'Completed',
      content: [{ type: 'text', text: `section-${i}-final-detail` }],
    })),
  };
  const page = activityProjection('succeeded');
  const row = { ...page.rows[0], presentation };
  const history = {
    id: '12345678-1234-1234-1234-123456789012',
    revision: 1,
    total: 2,
    after: 0,
    next_after: 1,
  };

  it('retains all large blocks, table cells and sections through parsing and copying', () => {
    const parsed = activityProjectionFromWire({ ...page, rows: [row] });
    expect(parsed!.rows[0].presentation).toEqual(presentation);
    const copied = activityCopyText(parsed!);
    expect(copied).toContain(body);
    expect(copied).toContain(cell);
    expect(copied).toContain('Detail 32');
    expect(copied).toContain('section-8-final-detail');
    expect(copied).not.toContain('Details truncated');
  });

  it('admits one oversized invocation on a history page without losing its cursor', () => {
    const parsed = activityProjectionFromWire({ ...page, rows: [row], history });
    expect(parsed!.history).toEqual(history);
    expect(parsed!.rows[0].presentation).toEqual(presentation);
    expect(
      activityProjectionFromWire({
        ...page,
        rows: [{ ...page.rows[0], id: 'group', children: [row] }],
        history,
      })?.rows[0].children?.[0].presentation,
    ).toEqual(presentation);
  });

  it('still requires multiple invocations to fit the history page byte budget', () => {
    expect(
      activityProjectionFromWire({
        ...page,
        rows: [row, { ...row, id: 'call-2', sequence: 2 }],
        history: { ...history, next_after: null },
      }),
    ).toBeNull();
    expect(
      activityProjectionFromWire({
        ...page,
        rows: [
          { ...page.rows[0], id: 'group', children: [row, { ...row, id: 'call-2', sequence: 2 }] },
        ],
        history: { ...history, next_after: null },
      }),
    ).toBeNull();
  });

  it('still rejects malformed content rather than treating it as complete evidence', () => {
    for (const content of [
      [{ type: 'code', text: 1 }],
      [{ type: 'table', columns: ['Name', 'Result'], rows: [['missing result']] }],
      [{ type: 'progress', label: 'Builds', value: 3, total: 2 }],
    ]) {
      expect(
        activityProjectionFromWire({
          ...page,
          rows: [{ ...row, presentation: { ...presentation, content } }],
        }),
      ).toBeNull();
    }
  });
});

describe('complete retained Activity copy', () => {
  it('copies every page from the beginning without mutating the visible snapshot', async () => {
    const snapshot = activityHistoryPage(96);
    const original = structuredClone(snapshot);
    const signal = new AbortController().signal;
    const load = vi.fn(async (_id: string, after: number) => activityHistoryPage(after));
    const text = await activityHistoryCopyText([snapshot], load, signal);
    expect(load.mock.calls.map(([, after]) => after)).toEqual([0, 32, 64, 96, 128]);
    expect(text.match(/^operation \d+ \[/gm)).toHaveLength(160);
    expect(text).toContain('retained-1\n');
    expect(text).toContain('retained-160\n');
    expect(text).toContain('operation 159 [failed]');
    expect(text).toContain('operation 160 [cancelled]');
    expect(text).not.toContain(snapshot.history!.id);
    expect(text).not.toContain('op-160');
    expect(snapshot).toEqual(original);
  });

  it('preserves mixed source order and repeated calls without reloading complete histories', async () => {
    const complete = groupedActivityHistoryPage(GROUPED_ACTIVITY_HISTORY_IDS[0]);
    const inline = structuredClone(complete);
    delete inline.history;
    const tail = groupedActivityHistoryPage(GROUPED_ACTIVITY_HISTORY_IDS[2], 2);
    const load = vi.fn(async (id: string, after: number) => groupedActivityHistoryPage(id, after));
    const full = groupedActivityHistoryPage(tail.history!.id);
    full.rows.push(...tail.rows);
    const text = await activityHistoryCopyText(
      [complete, inline, tail],
      load,
      new AbortController().signal,
    );
    expect(load.mock.calls).toEqual([
      [tail.history!.id, 0, '', expect.any(AbortSignal)],
      [tail.history!.id, 2, '', expect.any(AbortSignal)],
    ]);
    expect(text).toBe(activityCopyText(mergeActivity([complete, inline, full])));
    expect(text.match(/Read result 1-1/g)).toHaveLength(2);
  });

  it('copies nested invocation content in complete snapshots without a loader call', async () => {
    const snapshot = groupedActivityHistoryPage(GROUPED_ACTIVITY_HISTORY_IDS[0]);
    snapshot.rows = [{ ...snapshot.rows[0], children: snapshot.rows }];
    const load = vi.fn();
    expect(await activityHistoryCopyText([snapshot], load, new AbortController().signal)).toBe(
      activityCopyText(snapshot),
    );
    expect(load).not.toHaveBeenCalled();
  });

  it.each(['id', 'after', 'revision', 'total', 'cursor', 'missing', 'empty', 'empty tail'])(
    'rejects a changed or invalid %s rather than returning a partial copy',
    async (field) => {
      const load = vi.fn(async (_id: string, after: number) => {
        const result = activityHistoryPage(after);
        if (after === 32) {
          if (field === 'id') result.history!.id = GROUPED_ACTIVITY_HISTORY_IDS[0];
          if (field === 'after') result.history!.after = 0;
          if (field === 'revision') result.history!.revision++;
          if (field === 'total') result.history!.total++;
          if (field === 'cursor') result.history!.next_after = after;
          if (field === 'missing') delete result.history;
          if (field === 'empty') result.rows = [];
          if (field === 'empty tail') {
            result.rows = [];
            result.history!.next_after = null;
          }
        }
        return result;
      });
      await expect(
        activityHistoryCopyText([activityHistoryPage()], load, new AbortController().signal),
      ).rejects.toThrow(/Activity changed/);
      expect(load).toHaveBeenCalledTimes(2);
    },
  );

  it('rejects a revision change even on the first reloaded page', async () => {
    const load = vi.fn(async () => {
      const result = activityHistoryPage();
      result.history!.revision++;
      return result;
    });
    await expect(
      activityHistoryCopyText([activityHistoryPage()], load, new AbortController().signal),
    ).rejects.toThrow(/Activity changed/);
  });

  it('propagates retrieval failures without returning the pages already loaded', async () => {
    const load = vi.fn(async (_id: string, after: number) => {
      if (after) throw new Error('Reconnect to copy activity.');
      return activityHistoryPage();
    });
    await expect(
      activityHistoryCopyText([activityHistoryPage()], load, new AbortController().signal),
    ).rejects.toThrow('Reconnect to copy activity.');
  });

  it.each([false, true])(
    'honors cancellation before or during retrieval (during: %s)',
    async (during) => {
      const controller = new AbortController();
      const reason = new Error('Copy cancelled');
      const load = vi.fn(async () => {
        controller.abort(reason);
        return activityHistoryPage();
      });
      if (!during) controller.abort(reason);
      await expect(
        activityHistoryCopyText([activityHistoryPage()], load, controller.signal),
      ).rejects.toBe(reason);
      expect(load).toHaveBeenCalledTimes(during ? 1 : 0);
    },
  );
});

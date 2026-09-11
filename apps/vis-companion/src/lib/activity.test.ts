/** The Activity projection against `internal.activity.core`'s own bounded snapshot. */
import { describe, expect, it } from 'vitest';
import {
  activityCopyText,
  argumentGroups,
  operationGroups,
  activityProjectionFromWire,
  ACTIVITY_PRESENTERS,
  ACTIVITY_SIGNALS,
  ACTIVITY_STATES,
  ACTIVITY_TEXT_FORMATS,
} from './activity';
import contract from '../../../../packages/vis-contract/resources/vis-contract/activity.json';
import cases from '../../../../packages/vis-contract/resources/vis-contract/fixtures/activity-cases.json';
import groupingCases from '../../../../packages/vis-contract/resources/vis-contract/fixtures/activity-groups.json';
import argumentCases from '../../../../packages/vis-contract/resources/vis-contract/fixtures/activity-arguments.json';
import copyCases from '../../../../packages/vis-contract/resources/vis-contract/fixtures/activity-copy.json';

for (const sample of copyCases) {
  it(`portable Activity copy: ${sample.name}`, () => {
    const projection = activityProjectionFromWire(sample.projection);
    expect(projection).not.toBeNull();
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
    expect(projection).not.toBeNull();
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
      operation: 'run_tests',
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
    expect(ACTIVITY_PRESENTERS).toEqual(contract.presenters);
    expect(ACTIVITY_SIGNALS).toEqual(contract.signals);
    expect(ACTIVITY_STATES).toEqual(contract.states);
    expect(ACTIVITY_TEXT_FORMATS).toEqual(contract.text_formats);
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

/** The Activity projection against `internal.activity`'s own bounded snapshot. */
import { describe, expect, it } from 'vitest';
import { activityProjectionFromWire } from './activity';

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

describe("one form's Activity read off the wire", () => {
  // Protocol 7 took Activity off the Live View rail: it is no longer a classified
  // view with a projection hanging off it. Protocol 9 carries every revision on
  // `block.activity`; only the settled replacement is durable.
  it('reads a bare projection off the wire', () => {
    expect(activityProjectionFromWire(activityProjection())).toMatchObject({ state: 'running' });
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
      omitted_lines: 7,
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
    expect(activityProjectionFromWire({ ...activityProjection(), rows: [{ id: 'broken' }] })).toBeNull();
    // Protocol 8 dropped both: a payload still wearing either came from a gateway
    // the compatibility gate should already have refused.
    expect(activityProjectionFromWire({ ...activityProjection(), schema_version: 1 })).toBeNull();
    expect(
      activityProjectionFromWire({ ...activityProjection(), anchor: { iteration: 1, form_index: 0 } }),
    ).toBeNull();
  });

  it('rejects projections outside the one canonical closed shape', () => {
    const projection = activityProjection();
    const row = projection.rows[0];

    expect(activityProjectionFromWire({ ...projection, extra: true })).toBeNull();
    expect(activityProjectionFromWire({ ...projection, counts: { ...projection.counts, total: 1 } })).toBeNull();
    expect(
      activityProjectionFromWire({ ...projection, omitted: { ...projection.omitted, total: 0 } }),
    ).toBeNull();
    expect(activityProjectionFromWire({ ...projection, rows: [row, { ...row }] })).toBeNull();
    expect(
      activityProjectionFromWire({ ...projection, rows: [{ ...row, duration_ms: 1.5 }] }),
    ).toBeNull();
    expect(
      activityProjectionFromWire({
        ...projection,
        rows: [{ ...row, resources: [{ type: 'file', id: 'a.clj', extra: true }] }],
      }),
    ).toBeNull();
  });
});

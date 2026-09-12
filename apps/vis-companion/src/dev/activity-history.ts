import type { ActivityProjection } from '../lib/activity';

/** Deterministic paged transport fixture: 160 records with more than 64 KiB of safe detail. */
export function activityHistoryPage(after = 0, q = ''): ActivityProjection {
  const sequences = Array.from({ length: 160 }, (_, i) => i + 1).filter(
    (i) => i > after && (!q || `operation ${i}`.includes(q)),
  );
  const shown = sequences.slice(0, 32);
  return {
    state: 'succeeded',
    counts: { running: 0, succeeded: 158, failed: 1, cancelled: 1 },
    omitted: { rows: 0, by_classification: {} },
    history: {
      id: '12345678-1234-1234-1234-123456789012',
      revision: 1,
      total: 160,
      after,
      next_after: sequences.length > 32 ? shown.at(-1)! : null,
    },
    rows: shown.map((i) => ({
      id: `op-${i}`,
      sequence: i,
      operation: `operation ${i}`,
      presenter: 'generic',
      signal: 'observation',
      state: i === 159 ? 'failed' : i === 160 ? 'cancelled' : 'succeeded',
      summary: '',
      resources: [],
      evidence: [],
      presentation: {
        headline: `Operation ${i}`,
        summary: '',
        content: [
          {
            type: 'code',
            language: 'text',
            text:
              `retained-${i}\n` +
              Array.from(
                { length: 80 },
                (_, line) => `Check ${line + 1}: input validated; result stored.`,
              ).join('\n'),
          },
        ],
      },
    })),
  };
}

export const GROUPED_ACTIVITY_HISTORY_IDS = [
  '12345678-1234-1234-1234-123456789010',
  '12345678-1234-1234-1234-123456789011',
  '12345678-1234-1234-1234-123456789012',
];

/** Three receipts (2 + 2 + 3 operations), with colliding row ids and an uneven final page. */
export function groupedActivityHistoryPage(id: string, after = 0, query = ''): ActivityProjection {
  const index = GROUPED_ACTIVITY_HISTORY_IDS.indexOf(id);
  if (index < 0) throw new Error('Unknown fixture history');
  const total = index === 2 ? 3 : 2;
  const rows: ActivityProjection['rows'] = Array.from({ length: total }, (_, offset) => ({
    id: `op-${offset + 1}`,
    sequence: offset + 1,
    operation: 'cat',
    presenter: 'observation',
    signal: 'observation',
    state: 'succeeded',
    summary: `src/review-${index + 1}-${offset + 1}.clj`,
    resources: [],
    evidence: [{ kind: 'result', text: `Read result ${index + 1}-${offset + 1}` }],
  }));
  const matches = rows.filter(
    (row) => row.sequence > after && row.summary.toLowerCase().includes(query.toLowerCase()),
  );
  const shown = matches.slice(0, 2);
  return {
    state: 'succeeded',
    counts: { running: 0, succeeded: total, failed: 0, cancelled: 0 },
    omitted: { rows: 0, by_classification: {} },
    history: {
      id,
      revision: 1,
      total,
      after,
      next_after: matches.length > shown.length ? shown.at(-1)!.sequence : null,
    },
    rows: shown,
  };
}

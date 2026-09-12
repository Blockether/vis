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

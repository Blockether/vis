import { describe, expect, it } from 'vitest';
import fixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/plans.json';
import schema from '../../../../packages/vis-contract/resources/vis-contract/schema/plans.json';
import { actionRequest, availableActions, documentInfo } from './plans';
import { parseAnnotated, quoteOf, renderAnnotated } from './markdown-annotations';

describe('shared planning contract', () => {
  it('matches TUI document headers and action gates', () => {
    for (const entry of fixture.documents) {
      const info = documentInfo(entry.filename, entry.text);
      expect(info).toEqual(entry.expected);
      expect(availableActions(info, false)).toEqual(entry.actions);
      if (info) expect(availableActions(info, true)).toEqual(['revise']);
    }
  });
  it('recognizes every schema-declared status and rejects unknown statuses', () => {
    for (const status of schema.$defs.status.enum) {
      const text = `**Feature:** search\n**Status:** ${status}`;
      expect(documentInfo('PLAN-search.md', text)).toEqual({ kind: 'plan', feature: 'search', status });
    }
    expect(documentInfo('PLAN-search.md', '**Feature:** search\n**Status:** unknown')).toBeNull();
  });
  it('addresses exactly the selected saved version', () => {
    for (const action of ['revise', 'approve'] as const) {
      expect(actionRequest('PLAN-search.md', 3, action)).toContain(
        'read_attachment("PLAN-search.md", version=3)',
      );
    }
    expect(actionRequest('PLAN-search.md', 3, 'approve')).toContain('start implementation');
    expect(actionRequest('PLAN-search.md', 3, 'approve')).toContain('latest version');
    expect(() => actionRequest('PLAN-search.md', 0, 'approve')).toThrow();
  });
  it('offers one review action and never executes with pending comments', () => {
    for (const status of ['draft', 'in-review', 'ready', 'accepted', 'implementing', 'done']) {
      for (const kind of ['plan', 'implementation'] as const) {
        const info = { kind, status, feature: 'search' };
        expect(availableActions(info, true)).toEqual(['revise']);
        expect(availableActions(info, false)).toEqual(
          kind === 'plan' && ['ready', 'accepted'].includes(status) ? ['approve'] : [],
        );
      }
    }
    expect(availableActions(null, true)).toEqual([]);
  });
  it('round-trips comments without consuming resolved history', () => {
    for (const entry of fixture.annotations) {
      expect(renderAnnotated(entry.body, entry.comments)).toEqual(entry.rendered);
      const parsed = parseAnnotated(entry.rendered);
      expect(renderAnnotated(parsed.body, parsed.comments)).toEqual(entry.rendered);
    }
    for (const text of fixture.unrecognized)
      expect(parseAnnotated(text)).toEqual({ body: text, comments: [] });
    for (const quote of fixture.quotes) expect(quoteOf(quote.input)).toBe(quote.expected);
  });
});

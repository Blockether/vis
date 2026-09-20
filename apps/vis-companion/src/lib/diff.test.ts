import { describe, expect, it } from 'vitest';
import fixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/diff.json';
import schema from '../../../../packages/vis-contract/resources/vis-contract/schema/diff.json';
import { parseDiff, diffReviewRequest, diffHeaderPath } from './diff';
import { artifactKind, docKindLabel, DIFF_MEDIA, collectArtifacts } from './artifacts';

describe('portable diff attachment', () => {
  it('reads the shared fixture without changing its patch', () => {
    expect(parseDiff(JSON.stringify(fixture))).toEqual(fixture);
  });
  it('accepts schema-declared source kinds and backends', () => {
    expect(DIFF_MEDIA).toBe(schema.$defs.attachment.contentMediaType);
    for (const type of schema.$defs.source.properties.type.enum) {
      for (const backend of schema.$defs.source.properties.backend.enum) {
        const envelope = { ...fixture, source: { type, backend } };
        expect(parseDiff(JSON.stringify(envelope))).toEqual(envelope);
      }
    }
  });
  it('requires every schema-declared envelope field', () => {
    for (const key of schema.$defs.envelope.required) {
      const envelope: Record<string, unknown> = { ...fixture };
      delete envelope[key];
      expect(() => parseDiff(JSON.stringify(envelope))).toThrow();
    }
  });
  it.each([
    null,
    [],
    {},
    { ...fixture, schema_version: 2 },
    { ...fixture, patch: 4 },
    { ...fixture, comments: null },
    { ...fixture, extra: true },
    { ...fixture, comments: [{ quote: '', body: '' }] },
    { ...fixture, comments: [{ quote: '', body: 'Review', extra: true }] },
    { ...fixture, source: { type: ['draft'] } },
    { ...fixture, source: { type: 'other' } },
    { ...fixture, source: { type: 'draft', backend: 'other' } },
    { ...fixture, source: { type: 'draft', label: '' } },
    { ...fixture, source: { type: 'workspace', unknown: true } },
  ])('refuses an invalid envelope %j', (value) => {
    expect(() => parseDiff(JSON.stringify(value))).toThrow();
  });
  it('classifies a diff as a readable document with a diff label', () => {
    expect(artifactKind({ index: 0, kind: 'diff', media_type: DIFF_MEDIA })).toBe('doc');
    expect(docKindLabel(DIFF_MEDIA, 'DIFF-search.json')).toBe('DIFF');
  });
  it("carries each version's explicit review capability into the gallery", () => {
    const rows = collectArtifacts([
      {
        turn_id: 't',
        iterations: [
          {
            id: 'i',
            attachments: [
              {
                index: 0,
                version: 1,
                filename: 'PLAN-search.md',
                commentable: true,
              },
              {
                index: 1,
                version: 2,
                filename: 'PLAN-search.md',
                commentable: false,
              },
              { index: 2, filename: 'IMPLEMENTATION-search.md' },
            ],
          },
        ],
      },
    ]);
    expect(rows.map((row) => row.commentable)).toEqual([false, false, true]);
  });
  it('requests the exact reviewed version without authorizing new scope', () => {
    expect(diffReviewRequest('DIFF-search.json', 4)).toContain(
      'read_attachment("DIFF-search.json", version=4)',
    );
    expect(diffReviewRequest('DIFF-search.json', 4)).toContain(
      'Do not treat this review as approval of new scope',
    );
    expect(() => diffReviewRequest('DIFF-search.json', 0)).toThrow();
  });
});

describe('the file a diff header names', () => {
  it.each([
    ['+++ b/src/app.ts', 'src/app.ts'],
    ['--- a/src/app.ts', 'src/app.ts'],
    ['diff --git a/src/app.ts b/src/app.ts', 'src/app.ts'],
    ['diff --git a/old.ts b/new.ts', 'new.ts'],
    ['+++ b/src/app.ts\t2024-01-01 12:00:00', 'src/app.ts'],
    ['+++ src/app.ts', 'src/app.ts'],
  ])('reads %s', (line, path) => {
    const named = diffHeaderPath(line);
    expect(named?.path).toBe(path);
    expect(line.slice(named?.start, named?.end)).toBe(path);
  });

  it.each([
    '+++ /dev/null',
    '--- /dev/null',
    '@@ -1,2 +1,3 @@',
    '+const value = 1;',
    '-const value = 0;',
    ' unchanged',
    '+++ "b/odd name.ts"',
    '',
  ])('leaves %s alone', (line) => {
    expect(diffHeaderPath(line)).toBeNull();
  });
});

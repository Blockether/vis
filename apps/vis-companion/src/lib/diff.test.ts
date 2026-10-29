import { describe, expect, it } from 'vitest';
import fixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/diff.json';
import schema from '../../../../packages/vis-contract/resources/vis-contract/schema/diff.json';
import { parseDiff, diffHeaderAnchors, diffHeaderPath } from './diff';
import { DIFF_MEDIA } from './artifacts';

describe('portable diff attachment', () => {
  const { comments: _ignored, ...snapshot } = fixture as typeof fixture & { comments?: unknown };
  it('reads the shared fixture without changing its patch', () => {
    expect(parseDiff(JSON.stringify(fixture))).toEqual(snapshot);
  });
  it('accepts schema-declared source kinds and backends', () => {
    expect(DIFF_MEDIA).toBe(schema.$defs.attachment.contentMediaType);
    for (const type of schema.$defs.source.properties.type.enum) {
      for (const backend of schema.$defs.source.properties.backend.enum) {
        const envelope = { ...snapshot, source: { type, backend } };
        expect(parseDiff(JSON.stringify(envelope))).toEqual(envelope);
      }
    }
  });
  it('requires the patch, its source and the schema version', () => {
    for (const key of ['schema_version', 'patch', 'source']) {
      const envelope: Record<string, unknown> = { ...snapshot };
      delete envelope[key];
      expect(() => parseDiff(JSON.stringify(envelope))).toThrow();
    }
  });
  it('ignores the comments of an older snapshot', () => {
    const older = { ...snapshot, comments: [{ quote: 'a', body: 'b' }] };
    expect(parseDiff(JSON.stringify(older))).toEqual(snapshot);
    expect(parseDiff(JSON.stringify(older))).not.toHaveProperty('comments');
  });
  it.each([
    null,
    [],
    {},
    { ...snapshot, schema_version: 2 },
    { ...snapshot, patch: 4 },
    { ...snapshot, extra: true },
    { ...snapshot, source: { type: ['draft'] } },
    { ...snapshot, source: { type: 'other' } },
    { ...snapshot, source: { type: 'draft', backend: 'other' } },
    { ...snapshot, source: { type: 'draft', label: '' } },
    { ...snapshot, source: { type: 'workspace', unknown: true } },
  ])('refuses an invalid envelope %j', (value) => {
    expect(() => parseDiff(JSON.stringify(value))).toThrow();
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

describe('where a diff header points', () => {
  const PATCH = [
    'diff --git a/src/app.ts b/src/app.ts',
    '--- a/src/app.ts',
    '+++ b/src/app.ts',
    '@@ -12,3 +14,4 @@ function main() {',
    '-const value = 0;',
    '+const value = 1;',
    'diff --git a/src/late.ts b/src/late.ts',
    '+++ b/src/late.ts',
    '@@ -0,0 +1,2 @@',
    '+first',
  ];

  it('gives every header the first hunk below it', () => {
    expect(diffHeaderAnchors(PATCH)).toEqual([14, 14, 14, 14, 1, 1, 1, 1, 1, undefined]);
  });
});

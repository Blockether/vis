import { describe, expect, it } from 'vitest';

import { anchorKey, parseUnifiedDiff, sameAnchor } from './app-diff';

const PATCH = [
  'diff --git a/src/core.clj b/src/core.clj',
  '--- a/src/core.clj',
  '+++ b/src/core.clj',
  '@@ -10,3 +10,4 @@ (ns core)',
  ' (defn a [])',
  '-(defn b [])',
  '+(defn b [x])',
  '+(defn c [])',
  ' (defn d [])',
  'diff --git a/old.txt b/old.txt',
  'deleted file mode 100644',
  '--- a/old.txt',
  '+++ /dev/null',
  '@@ -1,1 +0,0 @@',
  '-gone',
].join('\n');

describe('parseUnifiedDiff', () => {
  it('numbers old and new lines and anchors each line on its own side', () => {
    const [core] = parseUnifiedDiff(PATCH);
    expect(core.path).toBe('src/core.clj');
    const lines = core.hunks[0].lines;
    expect(lines.map((line) => [line.kind, line.old_line, line.new_line])).toEqual([
      ['context', 10, 10],
      ['remove', 11, undefined],
      ['add', undefined, 11],
      ['add', undefined, 12],
      ['context', 12, 13],
    ]);
    expect(lines[1].anchor).toEqual({ path: 'src/core.clj', side: 'old', line: 11 });
    expect(lines[2].anchor).toEqual({ path: 'src/core.clj', side: 'new', line: 11 });
    expect(lines[4].anchor).toEqual({ path: 'src/core.clj', side: 'new', line: 13 });
  });

  it('names a deleted file by its old path', () => {
    const files = parseUnifiedDiff(PATCH);
    expect(files[1].path).toBe('old.txt');
    expect(files[1].hunks[0].lines[0].anchor).toEqual({ path: 'old.txt', side: 'old', line: 1 });
  });

  it('gives every line of a file a distinct anchor key', () => {
    const [core] = parseUnifiedDiff(PATCH);
    const keys = core.hunks.flatMap((hunk) => hunk.lines.map((line) => anchorKey(line.anchor)));
    expect(new Set(keys).size).toBe(keys.length);
  });

  it('reads an empty patch as no files', () => {
    expect(parseUnifiedDiff('')).toEqual([]);
  });

  it('compares anchors by path, side and line', () => {
    const anchor = { path: 'a', side: 'new' as const, line: 2 };
    expect(sameAnchor(anchor, { ...anchor })).toBe(true);
    expect(sameAnchor(anchor, { ...anchor, side: 'old' })).toBe(false);
  });
});

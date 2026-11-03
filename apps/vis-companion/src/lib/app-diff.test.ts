import { describe, expect, it } from 'vitest';

import {
  anchorKey,
  changedWords,
  diffModel,
  filterDiffFiles,
  findDiffMatches,
  fileChange,
  fileTree,
  fileStat,
  hiddenLinesBefore,
  hunkWordSpans,
  parseUnifiedDiff,
  splitRows,
} from './app-diff';

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
    expect(lines[1].anchor).toEqual({
      path: 'src/core.clj',
      side: 'old',
      line: 11,
    });
    expect(lines[2].anchor).toEqual({
      path: 'src/core.clj',
      side: 'new',
      line: 11,
    });
    expect(lines[4].anchor).toEqual({
      path: 'src/core.clj',
      side: 'new',
      line: 13,
    });
  });

  it('names a deleted file by its old path', () => {
    const files = parseUnifiedDiff(PATCH);
    expect(files[1].path).toBe('old.txt');
    expect(files[1].hunks[0].lines[0].anchor).toEqual({
      path: 'old.txt',
      side: 'old',
      line: 1,
    });
  });

  it('gives every line of a file a distinct anchor key', () => {
    const [core] = parseUnifiedDiff(PATCH);
    const keys = core.hunks.flatMap((hunk) => hunk.lines.map((line) => anchorKey(line.anchor)));
    expect(new Set(keys).size).toBe(keys.length);
  });
});

describe('the diff look', () => {
  it('counts the lines left out above a hunk and between hunks', () => {
    const hunks = [
      { header: '@@ -10,3 +10,4 @@', lines: [] },
      { header: '@@ -20 +21 @@', lines: [] },
      { header: 'no header', lines: [] },
    ];
    expect(hiddenLinesBefore(hunks, 0)).toBe(9);
    expect(hiddenLinesBefore(hunks, 1)).toBe(7);
    expect(hiddenLinesBefore(hunks, 2)).toBeNull();
  });

  it('counts the added and removed lines and names a deleted file', () => {
    const [core, old] = parseUnifiedDiff(PATCH);
    expect(fileStat(core)).toEqual({ additions: 2, deletions: 1 });
    expect(fileChange(core)).toBe('modified');
    expect(fileChange(old)).toBe('deleted');
  });

  it('tints only the words between the common start and end of a changed line', () => {
    expect(changedWords('(defn b [])', '(defn b [x])')).toEqual({
      old: [
        { text: '(defn b [', isChanged: false },
        { text: '])', isChanged: false },
      ],
      new: [
        { text: '(defn b [', isChanged: false },
        { text: 'x', isChanged: true },
        { text: '])', isChanged: false },
      ],
    });
    expect(changedWords('alpha', 'beta')).toBeNull();
  });

  it('pairs removed lines with the added lines after them, in order', () => {
    const [core] = parseUnifiedDiff(PATCH);
    const parts = hunkWordSpans(core.hunks[0].lines);
    expect([...parts.keys()]).toEqual(['old:11:src/core.clj', 'new:11:src/core.clj']);
  });
});

describe('the diff model', () => {
  it('sums every file and keeps the threads that no shown line holds', () => {
    const on = (side: 'old' | 'new', line: number, path = 'src/core.clj') => ({
      anchor: { path, side, line },
    });
    const model = diffModel(PATCH, [
      on('new', 11),
      on('old', 11),
      on('new', 99),
      on('new', 1, 'other.txt'),
      {},
    ]);
    expect(model.total).toEqual({ additions: 2, deletions: 2 });
    expect(model.elsewhere).toEqual([on('new', 99), on('new', 1, 'other.txt'), {}]);
    expect(model.digits.get('src/core.clj')).toBe(2);
    expect(model.digits.get('old.txt')).toBe(1);
  });

  it('puts context on both sides and pairs removed with added lines, leaving the rest empty', () => {
    const [core] = parseUnifiedDiff(PATCH);
    const rows = splitRows(core.hunks[0].lines).map((row) => [row.old?.old_line, row.new?.new_line]);
    expect(rows).toEqual([
      [10, 10],
      [11, 11],
      [undefined, 12],
      [12, 13],
    ]);
  });

  it('lists folders first and joins a folder that holds only one folder', () => {
    const file = (path: string) => ({ path, hunks: [] });
    const rows = fileTree([
      file('README.md'),
      file('src/vis/lib/a.ts'),
      file('src/vis/lib/b.ts'),
      file('src/x.ts'),
    ]);
    expect(rows.map((row) => [row.kind, row.depth, row.name])).toEqual([
      ['folder', 0, 'src'],
      ['folder', 1, 'vis / lib'],
      ['file', 2, 'a.ts'],
      ['file', 2, 'b.ts'],
      ['file', 1, 'x.ts'],
      ['file', 0, 'README.md'],
    ]);
  });
});

// Report: the review panel classifies header-only files and filters review noise.
describe('review display filters', () => {
  it('classifies new and deleted binary files without text headers', () => {
    const files = parseUnifiedDiff(
      'diff --git a/a.png b/a.png\nnew file mode 100644\nBinary files /dev/null and b/a.png differ\ndiff --git a/b.bin b/b.bin\ndeleted file mode 100644\nBinary files a/b.bin and /dev/null differ',
    );
    expect(files.map(fileChange)).toEqual(['added', 'deleted']);
  });
  it('hides only test paths and keeps non-test code', () => {
    const files = parseUnifiedDiff(
      ['src/a.test.ts', 'test/a.py', 'src/a.ts', 'src/contest.ts']
        .map((p) => `diff --git a/${p} b/${p}\n--- a/${p}\n+++ b/${p}\n@@ -1 +1 @@\n-old\n+new`)
        .join('\n'),
    );
    expect(filterDiffFiles(files, true, false).map((f) => f.path)).toEqual(['src/a.ts', 'src/contest.ts']);
  });
  it('keeps anchors when whitespace-only changes become context', () => {
    const files = parseUnifiedDiff('--- a/a.py\n+++ b/a.py\n@@ -1,2 +1,2 @@\n-x = 1\n+x=1\n-old\n+new');
    const filtered = filterDiffFiles(files, false, true);
    expect(filtered[0].hunks[0].lines.map((l) => [l.kind, l.text, l.anchor.line])).toEqual([
      ['context', 'x=1', 1],
      ['remove', 'old', 2],
      ['add', 'new', 2],
    ]);
    expect(fileStat(files[0])).toEqual({ additions: 2, deletions: 2 });
  });
  it('removes a file whose only changes are whitespace', () => {
    expect(
      filterDiffFiles(parseUnifiedDiff('--- a/a\n+++ b/a\n@@ -1 +1 @@\n-x = 1\n+x=1'), false, true),
    ).toEqual([]);
  });
});

// Report: the review file tree can collapse a folder without hiding sibling folders.
it('does not list descendants of a collapsed folder', () => {
  const files = parseUnifiedDiff(
    ['src/a.ts', 'test/a.ts']
      .map((p) => `diff --git a/${p} b/${p}\n--- a/${p}\n+++ b/${p}\n@@ -1 +1 @@\n-a\n+b`)
      .join('\n'),
  );
  expect(fileTree(files, new Set(['src/'])).map((row) => row.key)).toEqual(['src/', 'test/', 'test/a.ts']);
});

// Report: find-in-diff counts occurrences and supports case, word, and regex modes.
it('finds occurrences and applies each search mode without throwing for invalid regex', () => {
  const files = parseUnifiedDiff('--- a/a\n+++ b/a\n@@ -1 +1 @@\n-Name names NAME\n+name name');
  expect(findDiffMatches(files, 'name').map((m) => [m.line.anchor.side, m.start, m.end])).toEqual([
    ['old', 0, 4],
    ['old', 5, 9],
    ['old', 11, 15],
    ['new', 0, 4],
    ['new', 5, 9],
  ]);
  expect(findDiffMatches(files, 'name', { caseSensitive: true }).map((m) => m.start)).toEqual([5, 0, 5]);
  expect(findDiffMatches(files, 'name', { wholeWord: true }).map((m) => m.start)).toEqual([0, 11, 0, 5]);
  expect(findDiffMatches(files, 'n[a-z]+', { regex: true, caseSensitive: true }).map((m) => m.end)).toEqual([
    10, 4, 9,
  ]);
  expect(findDiffMatches(files, '[', { regex: true })).toEqual([]);
  expect(findDiffMatches(files, '^', { regex: true }).map((m) => [m.start, m.end])).toEqual([
    [0, 0],
    [0, 0],
  ]);
});

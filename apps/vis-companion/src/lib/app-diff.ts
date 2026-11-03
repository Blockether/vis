/**
 * The rows of a unified diff, with a stable anchor on every changed or context
 * line. An anchor is `{path, side, line}`: a removed line answers its old line
 * number, an added or context line its new line number. A thread names that
 * anchor, so it stays on its line when the client reloads.
 */

import type { ViewAnchor } from './view';

export type AppDiffLineKind = 'add' | 'remove' | 'context';

export interface AppDiffLine {
  kind: AppDiffLineKind;
  /** The line without its `+`, `-` or space mark. */
  text: string;
  old_line?: number;
  new_line?: number;
  anchor: ViewAnchor;
}

export interface AppDiffHunk {
  header: string;
  lines: AppDiffLine[];
}

export interface AppDiffFile {
  /** The path a thread names: the new path, or the old path of a deleted file. */
  path: string;
  old_path?: string;
  new_path?: string;
  hunks: AppDiffHunk[];
}

const HUNK = /^@@ -(\d+)(?:,\d+)? \+(\d+)(?:,\d+)? @@/;

/** A path out of a `---`/`+++` header: no `a/` or `b/` prefix, `undefined` for `/dev/null`. */
function headerPath(rest: string): string | undefined {
  let name = rest.split('\t')[0].trim();
  if (name.startsWith('"') && name.endsWith('"') && name.length >= 2) {
    name = name.slice(1, -1);
  }
  if (name === '/dev/null' || name === '') return undefined;
  return name.replace(/^[ab]\//, '');
}

/** The two paths of a `diff --git a/x b/y` line, when they can be read. */
function gitPaths(line: string): { old_path?: string; new_path?: string } {
  const match = /^diff --git a\/(.+) b\/(.+)$/.exec(line);
  return match ? { old_path: match[1], new_path: match[2] } : {};
}

/** The files of a unified diff. Text that is no diff gives no files. */
export function parseUnifiedDiff(patch: string): AppDiffFile[] {
  const files: AppDiffFile[] = [];
  const state: {
    file: AppDiffFile | null;
    hunk: AppDiffHunk | null;
    old: number;
    new: number;
  } = {
    file: null,
    hunk: null,
    old: 0,
    new: 0,
  };
  const start = (paths: { old_path?: string; new_path?: string }): AppDiffFile => {
    const file: AppDiffFile = {
      path: paths.new_path ?? paths.old_path ?? '',
      ...(paths.old_path ? { old_path: paths.old_path } : {}),
      ...(paths.new_path ? { new_path: paths.new_path } : {}),
      hunks: [],
    };
    files.push(file);
    state.file = file;
    state.hunk = null;
    return file;
  };
  const rename = (file: AppDiffFile, side: 'old_path' | 'new_path', path: string | undefined) => {
    if (path) file[side] = path;
    else delete file[side];
    file.path = file.new_path ?? file.old_path ?? file.path;
  };
  for (const raw of patch.split('\n')) {
    const line = raw.endsWith('\r') ? raw.slice(0, -1) : raw;
    if (line.startsWith('diff --git ')) {
      start(gitPaths(line));
      continue;
    }
    if (line.startsWith('--- ') && state.hunk === null) {
      const file = state.file && state.file.hunks.length === 0 ? state.file : start({});
      rename(file, 'old_path', headerPath(line.slice(4)));
      continue;
    }
    if (line.startsWith('+++ ') && state.hunk === null && state.file) {
      rename(state.file, 'new_path', headerPath(line.slice(4)));
      continue;
    }
    const opened = HUNK.exec(line);
    if (opened) {
      const file = state.file ?? start({});
      state.old = Number(opened[1]);
      state.new = Number(opened[2]);
      state.hunk = { header: line, lines: [] };
      file.hunks.push(state.hunk);
      continue;
    }
    const { file, hunk } = state;
    if (!file || !hunk) continue;
    const mark = line[0];
    if (mark === '+') {
      hunk.lines.push({
        kind: 'add',
        text: line.slice(1),
        new_line: state.new,
        anchor: { path: file.path, side: 'new', line: state.new },
      });
      state.new += 1;
    } else if (mark === '-') {
      hunk.lines.push({
        kind: 'remove',
        text: line.slice(1),
        old_line: state.old,
        anchor: { path: file.path, side: 'old', line: state.old },
      });
      state.old += 1;
    } else if (mark === ' ') {
      hunk.lines.push({
        kind: 'context',
        text: line.slice(1),
        old_line: state.old,
        new_line: state.new,
        anchor: { path: file.path, side: 'new', line: state.new },
      });
      state.old += 1;
      state.new += 1;
    } else if (mark !== '\\') {
      // Any other line ends the hunk; a following header starts the next file.
      state.hunk = null;
    }
  }
  return files.filter((file) => file.hunks.length > 0 || file.path !== '');
}

/** A stable text key of an anchor. */
export function anchorKey(anchor: ViewAnchor): string {
  return `${anchor.side}:${anchor.line}:${anchor.path}`;
}

/** True when two anchors name the same line. */
export function sameAnchor(a: ViewAnchor | undefined, b: ViewAnchor | undefined): boolean {
  return Boolean(a && b && a.path === b.path && a.side === b.side && a.line === b.line);
}

/** The line ranges of a hunk header: `@@ -12,7 +12,9 @@`. A missing count is 1. */
export function hunkRange(header: string): { old_start: number; old_count: number } | null {
  const match = /^@@ -(\d+)(?:,(\d+))? \+\d+(?:,\d+)? @@/.exec(header);
  if (!match) return null;
  return {
    old_start: Number(match[1]),
    old_count: match[2] === undefined ? 1 : Number(match[2]),
  };
}

/**
 * How many unchanged lines the diff leaves out before hunk `at`: above the first
 * hunk, or between a hunk and the one before it. `null` when a header cannot be read.
 */
export function hiddenLinesBefore(hunks: readonly AppDiffHunk[], at: number): number | null {
  const range = hunkRange(hunks[at]?.header ?? '');
  if (!range) return null;
  if (at === 0) return Math.max(0, range.old_start - 1);
  const before = hunkRange(hunks[at - 1].header);
  if (!before) return null;
  return Math.max(0, range.old_start - (before.old_start + before.old_count));
}

/** The added and removed lines of one file. */
export function fileStat(file: AppDiffFile): {
  additions: number;
  deletions: number;
} {
  let additions = 0;
  let deletions = 0;
  for (const hunk of file.hunks)
    for (const line of hunk.lines) {
      if (line.kind === 'add') additions += 1;
      else if (line.kind === 'remove') deletions += 1;
    }
  return { additions, deletions };
}

/** What happened to a file: its header says it is new, gone, moved or changed. */
export function fileChange(file: AppDiffFile): 'added' | 'deleted' | 'renamed' | 'modified' {
  if (file.new_path && !file.old_path) return 'added';
  if (file.old_path && !file.new_path) return 'deleted';
  if (file.old_path && file.new_path && file.old_path !== file.new_path) return 'renamed';
  return 'modified';
}

/** A part of a changed line; `isChanged` parts get the word tint. */
export interface DiffSpan {
  text: string;
  isChanged: boolean;
}

const WORD = /\w+|\s+|[^\w\s]/g;

/** The parts of `text` before, inside and after the token range `[from, to)`. */
function spans(tokens: string[], from: number, to: number): DiffSpan[] {
  return [
    { text: tokens.slice(0, from).join(''), isChanged: false },
    { text: tokens.slice(from, to).join(''), isChanged: true },
    { text: tokens.slice(to).join(''), isChanged: false },
  ].filter((part) => part.text !== '');
}

/**
 * The changed words of a removed line and the added line that replaces it: what
 * lies between their common first and last words. `null` when the lines share
 * no first or last word, because then the whole row is the change.
 */
export function changedWords(before: string, after: string): { old: DiffSpan[]; new: DiffSpan[] } | null {
  const a = before.match(WORD) ?? [];
  const b = after.match(WORD) ?? [];
  let start = 0;
  while (start < a.length && start < b.length && a[start] === b[start]) start += 1;
  let end = 0;
  while (end < a.length - start && end < b.length - start && a[a.length - 1 - end] === b[b.length - 1 - end])
    end += 1;
  if (start === 0 && end === 0) return null;
  if (start + end === a.length && start + end === b.length) return null;
  return {
    old: spans(a, start, a.length - end),
    new: spans(b, start, b.length - end),
  };
}

/**
 * The word parts of each changed line of a hunk, by anchor key. A block of
 * removed lines followed by added lines pairs them in order; a line without a
 * partner has no parts.
 */
export function hunkWordSpans(lines: readonly AppDiffLine[]): Map<string, DiffSpan[]> {
  const parts = new Map<string, DiffSpan[]>();
  let at = 0;
  while (at < lines.length) {
    if (lines[at].kind !== 'remove') {
      at += 1;
      continue;
    }
    const removed: AppDiffLine[] = [];
    while (at < lines.length && lines[at].kind === 'remove') removed.push(lines[at++]);
    const added: AppDiffLine[] = [];
    while (at < lines.length && lines[at].kind === 'add') added.push(lines[at++]);
    for (let pair = 0; pair < Math.min(removed.length, added.length); pair += 1) {
      const words = changedWords(removed[pair].text, added[pair].text);
      if (!words) continue;
      parts.set(anchorKey(removed[pair].anchor), words.old);
      parts.set(anchorKey(added[pair].anchor), words.new);
    }
  }
  return parts;
}

/** One row of the split view: the old line on the left, the new one on the right. */
export interface SplitRow {
  old?: AppDiffLine;
  new?: AppDiffLine;
}

/**
 * The rows of a hunk side by side, as T3 draws them. A context line is on both
 * sides. A block of removed lines and the added lines after it share rows in
 * order; the longer block leaves the other side empty.
 */
export function splitRows(lines: readonly AppDiffLine[]): SplitRow[] {
  const rows: SplitRow[] = [];
  let at = 0;
  while (at < lines.length) {
    const line = lines[at];
    if (line.kind === 'context') {
      rows.push({ old: line, new: line });
      at += 1;
      continue;
    }
    const removed: AppDiffLine[] = [];
    while (at < lines.length && lines[at].kind === 'remove') removed.push(lines[at++]);
    const added: AppDiffLine[] = [];
    while (at < lines.length && lines[at].kind === 'add') added.push(lines[at++]);
    for (let row = 0; row < Math.max(removed.length, added.length); row += 1) {
      rows.push({
        ...(removed[row] ? { old: removed[row] } : {}),
        ...(added[row] ? { new: added[row] } : {}),
      });
    }
  }
  return rows;
}

/** One row of the file tree: a folder, or a file with its change and counts. */
export type FileTreeRow =
  | { kind: 'folder'; key: string; name: string; depth: number }
  | {
      kind: 'file';
      key: string;
      name: string;
      depth: number;
      file: AppDiffFile;
    };

interface TreeFolder {
  folders: Map<string, TreeFolder>;
  files: AppDiffFile[];
}

/**
 * The files of a diff as a tree, folders first and each level sorted by name. A
 * folder that holds only one folder joins it into one row (`src / lib`), as T3 does.
 */
export function fileTree(files: readonly AppDiffFile[]): FileTreeRow[] {
  const root: TreeFolder = { folders: new Map(), files: [] };
  for (const file of files) {
    const parts = file.path.split('/');
    let folder = root;
    for (const part of parts.slice(0, -1)) {
      const next = folder.folders.get(part) ?? {
        folders: new Map(),
        files: [],
      };
      folder.folders.set(part, next);
      folder = next;
    }
    folder.files.push(file);
  }
  const rows: FileTreeRow[] = [];
  const walk = (folder: TreeFolder, depth: number, prefix: string) => {
    for (const name of [...folder.folders.keys()].sort()) {
      let inner = folder.folders.get(name) as TreeFolder;
      let label = name;
      let path = `${prefix}${name}/`;
      while (inner.files.length === 0 && inner.folders.size === 1) {
        const [[child, next]] = [...inner.folders];
        label = `${label} / ${child}`;
        path = `${path}${child}/`;
        inner = next;
      }
      rows.push({ kind: 'folder', key: path, name: label, depth });
      walk(inner, depth + 1, path);
    }
    for (const file of [...folder.files].sort((a, b) => a.path.localeCompare(b.path))) {
      rows.push({
        kind: 'file',
        key: file.path,
        name: file.path.split('/').pop() ?? file.path,
        depth,
        file,
      });
    }
  };
  walk(root, 0, '');
  return rows;
}

/** What a diff node shows, worked out once for each patch and thread list. */
export interface DiffModel<T extends { anchor?: ViewAnchor }> {
  files: AppDiffFile[];
  /** The changed words of each paired line, by anchor key. */
  words: Map<string, DiffSpan[]>;
  /** The digits of the widest line number of each file, by path. */
  digits: Map<string, number>;
  /** The added and removed lines of all files. */
  total: { additions: number; deletions: number };
  /** Threads without an anchor or on a line that the diff does not show. */
  elsewhere: T[];
}

/** The model of a diff node: its files, word tints, number widths, totals and stray threads. */
export function diffModel<T extends { anchor?: ViewAnchor }>(
  patch: string,
  threads: readonly T[],
): DiffModel<T> {
  const files = parseUnifiedDiff(patch);
  const words = new Map<string, DiffSpan[]>();
  const digits = new Map<string, number>();
  const shown = new Set<string>();
  const total = { additions: 0, deletions: 0 };
  for (const file of files) {
    let widest = 1;
    for (const hunk of file.hunks) {
      for (const [key, parts] of hunkWordSpans(hunk.lines)) words.set(key, parts);
      for (const line of hunk.lines) {
        shown.add(anchorKey(line.anchor));
        if (line.kind === 'context' && line.old_line !== undefined) {
          shown.add(anchorKey({ ...line.anchor, side: 'old', line: line.old_line }));
        }
        widest = Math.max(widest, line.old_line ?? 0, line.new_line ?? 0);
      }
    }
    digits.set(file.path, String(widest).length);
    const stat = fileStat(file);
    total.additions += stat.additions;
    total.deletions += stat.deletions;
  }
  const elsewhere = threads.filter((thread) => !thread.anchor || !shown.has(anchorKey(thread.anchor)));
  return { files, words, digits, total, elsewhere };
}

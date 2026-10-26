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
  const state: { file: AppDiffFile | null; hunk: AppDiffHunk | null; old: number; new: number } = {
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

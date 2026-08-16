/**
 * WHICH PROJECTS ARE OPEN — the one thing this list remembers for a reader.
 *
 * A machine paints every project it has ever worked in, so a phone showing four
 * checkouts is a scroll through three of them to reach the one in hand. The list
 * therefore opens exactly ONE project by itself: the one its own order puts on top
 * of each machine, which is where the work that moved last already is. Every other
 * project starts folded, and a fold is one tap away from being undone.
 *
 * The reader's own folds outrank that default and OUTLIVE the screen. Component
 * state does not: the sessions list is torn down and rebuilt (a relaunch, a webview
 * the OS reclaimed behind an open transcript, a fleet re-paired under it), and every
 * project the reader had folded came back open. That was the report this module
 * answers.
 *
 * `localStorage`, not the `sessionStorage` of `lib/parked`: that one parks WHERE a
 * reader was, which is only true of this visit. A fold is a PREFERENCE, and one that
 * dies with the webview is not remembered.
 *
 * Nothing here is cached in memory. A fold is read once per project MOUNT and
 * written once per TAP, so the store is always the truth — the per-frame write that
 * forced `lib/parked` to coalesce cannot happen here.
 *
 * Every access is guarded: private mode, a disabled store and a non-browser (node
 * tests) all mean "no fold remembered", never a thrown screen.
 */

/** Where the folds live: one entry per project a reader has ever folded. */
const STORE_KEY = 'vis.projectFolds';

/**
 * How many decisions are kept. A fleet holds a few machines and a machine a few
 * projects, so this is far past any real list — it is here so a device that has seen
 * hundreds of throwaway workspaces cannot grow this entry without bound. The oldest
 * decision is the one that goes.
 */
const KEEP = 200;

/** A project's identity here: the machine that owns it, then its workspace root. */
export function projectFoldKey(machine: string, root: string): string {
  return `${machine}\u0000${root}`;
}

function readFolds(): Record<string, boolean> {
  try {
    const raw = globalThis.localStorage?.getItem(STORE_KEY);
    if (!raw) return {};
    const parsed: unknown = JSON.parse(raw);
    if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) return {};
    const folds: Record<string, boolean> = {};
    for (const [key, value] of Object.entries(parsed as Record<string, unknown>)) {
      if (typeof value === 'boolean') folds[key] = value;
    }
    return folds;
  } catch {
    return {};
  }
}

/** What the reader decided about this project, or `null` when they never said. */
export function readProjectFold(key: string): boolean | null {
  const decided = readFolds()[key];
  return typeof decided === 'boolean' ? decided : null;
}

/**
 * Remember one fold. The newest decision is written last, so insertion order is
 * oldest-first and dropping from the front drops the decisions nobody has touched
 * in the longest time.
 */
export function writeProjectFold(key: string, isOpen: boolean): void {
  const folds = readFolds();
  delete folds[key];
  folds[key] = isOpen;
  const keys = Object.keys(folds);
  for (const stale of keys.slice(0, Math.max(0, keys.length - KEEP))) delete folds[stale];
  try {
    globalThis.localStorage?.setItem(STORE_KEY, JSON.stringify(folds));
  } catch {
    // Private mode / quota: the fold then lasts as long as the screen does, which is
    // exactly what every fold used to do.
  }
}

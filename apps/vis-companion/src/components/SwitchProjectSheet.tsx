/**
 * "Switch project" — a breadcrumb browser of ONE machine's own filesystem.
 *
 * A machine owns its projects, so the only thing that knows which folders exist is
 * the machine: every row here comes from `GET /v1/fs` on that gateway. The sheet
 * commits a FOLDER and lets the gateway decide what that folder is (a repo or not),
 * which is why there is no "is this a git project?" question anywhere in it.
 *
 * Two modes, one header: the crumbs are for recognition, the pencil is for people
 * who know where they are going. The pencil is INK, never a box — a bordered button
 * beside the path reads as a second action competing with it.
 */
import { useCallback, useEffect, useMemo, useRef, useState, type CSSProperties } from 'react';
import { createPortal } from 'react-dom';
import { Button, Input, Spinner } from './ui';
import { ChevronIcon, PencilIcon } from './icons';
import type { GatewayClient } from '../lib/gateway';
import type { BrowseEntry } from '../lib/types';
import type { MenuPosition } from '../lib/anchored-menu';

/** `SessionsScreen`'s menu heading. The sheet's amber is spent on its primary button. */
const BAND = 'px-3 py-2 font-mono text-chip uppercase tracking-[0.08em]';
const QUIET_BAND = 'border-b border-dialog-edge bg-panel-2 text-dialog-hint';
/** `StartOption`: one menu row — a 44px thumb target with a hairline under it. */
const ROW =
  'flex min-h-11 w-full items-start gap-2 border-b border-dialog-edge px-3 py-2 text-left transition-colors duration-150 hover:bg-hover focus-visible:bg-hover focus-visible:outline-none motion-reduce:transition-none';
/** The badge a row hangs on its right edge, exactly as `StartOption` draws it. */
const CHIP =
  'mt-0.5 shrink-0 border border-edge px-1 font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint';

/** How many crumbs stay on screen. A path too long for 390px elides from the LEFT. */
const CRUMB_TAIL = 3;

interface Crumb {
  label: string;
  path: string;
}

/**
 * The path as taps: `~` is the machine's own home, and every ancestor is a way back
 * up. Split on the ABSOLUTE path and label it afterwards, so `~` can never be
 * mistaken for a folder actually called `~`.
 */
export function crumbsOf(path: string, home: string): Crumb[] {
  const inHome = path === home || path.startsWith(`${home}/`);
  const rest = (inHome ? path.slice(home.length) : path).split('/').filter(Boolean);
  const crumbs: Crumb[] = inHome ? [{ label: '~', path: home }] : [{ label: '/', path: '/' }];
  let at = inHome ? home : '';
  for (const part of rest) {
    at = `${at}/${part}`;
    crumbs.push({ label: part, path: at });
  }
  return crumbs;
}

/** The path a human reads: the machine's home is `~`, because it is not this phone's. */
export function homeify(path: string, home: string): string {
  if (!home) return path;
  if (path === home) return '~';
  return path.startsWith(`${home}/`) ? `~${path.slice(home.length)}` : path;
}

/** What a typed path is asking for: the folder to LIST, and the leaf to match in it. */
export function splitTyped(typed: string): { dir: string; leaf: string } {
  const cut = typed.lastIndexOf('/');
  if (cut < 0) return { dir: typed, leaf: '' };
  return { dir: typed.slice(0, cut) || '/', leaf: typed.slice(cut + 1) };
}

function entryHint(entry: BrowseEntry): string {
  const count = `${entry.entry_count} ${entry.entry_count === 1 ? 'entry' : 'entries'}`;
  return entry.branch ? `${count} · ${entry.branch}` : count;
}

export function SwitchProjectSheet({
  label,
  client,
  startAt,
  knownRoots,
  at,
  onCancel,
  onChoose,
}: {
  /** The machine whose files these are — the title says it, so no row has to. */
  label: string;
  client: GatewayClient;
  /** Where browsing opens: the machine's current project, or its home. */
  startAt: string | null;
  /** Roots this machine already runs sessions in, so the common case is recognised. */
  knownRoots: Set<string>;
  /** The control this came from, so desktop anchors it where a phone docks it. */
  at: MenuPosition | null;
  onCancel: () => void;
  onChoose: (root: string) => void;
}) {
  const [dir, setDir] = useState<string>(startAt ?? '~');
  const [listing, setListing] = useState<{
    path: string;
    parent: string | null;
    home: string;
    is_truncated: boolean;
    entries: BrowseEntry[];
  } | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [busy, setBusy] = useState(false);
  // `null` is browsing; a string is the pencil taken, and the string IS the path.
  const [typed, setTyped] = useState<string | null>(null);
  // `null` is not creating; a string is the folder that does not exist yet.
  const [folder, setFolder] = useState<string | null>(null);
  const [saving, setSaving] = useState(false);

  const typedSplit = typed === null ? null : splitTyped(typed);

  // One read path for both modes: the directory being listed is a value, and typing
  // just moves it. A keystroke inside one folder must not re-ask the gateway for it.
  const wanted = typedSplit ? typedSplit.dir : dir;
  useEffect(() => {
    const controller = new AbortController();
    const timer = window.setTimeout(
      () => {
        setBusy(true);
        void client
          .browse(wanted, controller.signal)
          .then((rows) => {
            setListing(rows);
            setError(null);
          })
          .catch((cause: unknown) => {
            if (controller.signal.aborted) return;
            setError((cause as Error).message);
          })
          .finally(() => {
            if (!controller.signal.aborted) setBusy(false);
          });
      },
      typedSplit ? 180 : 0,
    );
    return () => {
      window.clearTimeout(timer);
      controller.abort();
    };
  }, [client, wanted, typedSplit !== null]);

  const home = listing?.home ?? '';
  const here = listing?.path ?? '';
  const crumbs = useMemo(() => (here ? crumbsOf(here, home) : []), [here, home]);
  const shown = crumbs.slice(-CRUMB_TAIL);

  // Typed: the list narrows to what still matches. Browsing: it is the folder itself.
  const rows = useMemo(() => {
    const entries = listing?.entries ?? [];
    const leaf = typedSplit?.leaf.toLowerCase() ?? '';
    return leaf ? entries.filter((entry) => entry.name.toLowerCase().startsWith(leaf)) : entries;
  }, [listing, typedSplit?.leaf]);

  // A typed path that NAMES a folder commits that folder, not the parent being listed.
  const exact = typedSplit?.leaf
    ? (rows.find((entry) => entry.name.toLowerCase() === typedSplit.leaf.toLowerCase()) ?? null)
    : null;
  const target = exact?.path ?? here;

  const commit = useCallback(async () => {
    if (folder === null) {
      if (target) onChoose(target);
      return;
    }
    const name = folder.trim();
    if (!name || !here) return;
    setSaving(true);
    try {
      const made = await client.createDirectory(here, name);
      onChoose(made.path);
    } catch (cause) {
      setError((cause as Error).message);
      setSaving(false);
    }
  }, [client, folder, here, onChoose, target]);

  const enter = useCallback((path: string) => {
    setTyped(null);
    setFolder(null);
    setDir(path);
  }, []);

  const pencilRef = useRef<HTMLButtonElement>(null);
  useEffect(() => {
    const onKey = (event: KeyboardEvent) => {
      if (event.key === 'Escape') onCancel();
    };
    window.addEventListener('keydown', onKey);
    return () => window.removeEventListener('keydown', onKey);
  }, [onCancel]);

  const pencil = (
    <button
      ref={pencilRef}
      type="button"
      aria-label={typed === null ? 'Type a path' : 'Back to browsing'}
      aria-pressed={typed !== null}
      className={`inline-flex size-11 shrink-0 items-center justify-center ${
        typed === null ? 'text-dialog-hint' : 'text-accent-ink'
      }`}
      onClick={() => setTyped(typed === null ? homeify(here, home) : null)}
    >
      <PencilIcon className="size-4" />
    </button>
  );

  return createPortal(
    <div
      className="fixed inset-0 z-50 bg-black/40"
      role="presentation"
      onClick={onCancel}
    >
      <div
        role="dialog"
        aria-modal="true"
        aria-label={`Switch project on ${label}`}
        className="absolute inset-x-0 bottom-0 flex max-h-[82vh] flex-col border-t-2 border-accent bg-panel pb-[env(safe-area-inset-bottom)] transition-[opacity,transform,translate,scale,rotate] duration-150 starting:translate-y-2 starting:opacity-0 motion-reduce:transition-none sm:inset-x-auto sm:bottom-auto sm:left-[var(--menu-left)] sm:top-[var(--menu-top)] sm:max-h-[70vh] sm:w-96 sm:border sm:border-dialog-edge sm:pb-0 sm:shadow-[8px_8px_0_var(--line2)]"
        style={
          at
            ? ({ '--menu-top': `${at.top}px`, '--menu-left': `${at.left}px` } as CSSProperties)
            : undefined
        }
        onClick={(event) => event.stopPropagation()}
      >
        <p className={`${BAND} ${QUIET_BAND} shrink-0 truncate`}>Switch project · {label}</p>

        {typed === null ? (
          <div className="flex shrink-0 items-center gap-1 border-b border-dialog-edge bg-panel-2 px-3 py-2">
            {crumbs.length > shown.length && (
              <span className="shrink-0 font-mono text-meta text-dialog-hint">…</span>
            )}
            <span className="flex min-w-0 flex-1 items-center gap-1 overflow-hidden">
              {shown.map((crumb, index) => (
                <span key={crumb.path} className="flex min-w-0 items-center gap-1">
                  <span aria-hidden className="shrink-0 font-mono text-meta text-dialog-hint">
                    ›
                  </span>
                  <button
                    type="button"
                    disabled={index === shown.length - 1}
                    className={`truncate font-mono text-meta ${
                      index === shown.length - 1 ? 'font-bold text-white' : 'text-accent-ink'
                    }`}
                    onClick={() => enter(crumb.path)}
                  >
                    {crumb.label}
                  </button>
                </span>
              ))}
            </span>
            {pencil}
          </div>
        ) : (
          <div className="flex shrink-0 items-center gap-2 border-b border-dialog-edge bg-panel px-3 py-1">
            <Input
              autoFocus
              value={typed}
              aria-label="Path on this machine"
              placeholder="~/code/thing"
              autoCapitalize="none"
              autoCorrect="off"
              spellCheck={false}
              onChange={(event) => setTyped(event.target.value)}
              onKeyDown={(event) => {
                if (event.key !== 'Enter') return;
                if (exact) enter(exact.path);
                else void commit();
              }}
            />
            {pencil}
          </div>
        )}

        {folder !== null && (
          <div className="flex shrink-0 items-center gap-2 border-b border-dialog-edge bg-panel px-3 py-1.5">
            <span aria-hidden className="shrink-0 font-mono text-ui text-accent-ink">
              +
            </span>
            <Input
              autoFocus
              value={folder}
              maxLength={64}
              aria-label="New folder name"
              placeholder="band-repaint"
              autoCapitalize="none"
              autoCorrect="off"
              spellCheck={false}
              onChange={(event) => setFolder(event.target.value)}
              onKeyDown={(event) => {
                if (event.key === 'Enter') void commit();
              }}
            />
          </div>
        )}

        <div className="min-h-0 flex-1 touch-pan-y overflow-y-auto overscroll-contain">
          {error ? (
            <p className="px-3 py-3 font-mono text-meta text-err">{error}</p>
          ) : busy && !listing ? (
            <p className="flex items-center gap-2 px-3 py-3 font-mono text-meta text-dialog-hint">
              <Spinner className="text-accent-ink" />
              Reading folders...
            </p>
          ) : rows.length === 0 ? (
            <p className="px-3 py-3 font-mono text-meta text-dialog-hint">
              {typedSplit?.leaf ? 'No folder here starts with that.' : 'No folders in here.'}
            </p>
          ) : (
            rows.map((entry) => (
              <button
                key={entry.path}
                type="button"
                className={`${ROW} ${folder === null ? '' : 'opacity-40'}`}
                onClick={() => enter(entry.path)}
              >
                <ChevronIcon className="mt-0.5 size-3.5 shrink-0 text-dialog-hint" />
                <span className="min-w-0 flex-1">
                  <span className="block truncate font-mono text-ui text-white">{entry.name}/</span>
                  <span className="mt-0.5 block truncate font-mono text-meta text-dialog-hint">
                    {entryHint(entry)}
                  </span>
                </span>
                {/* One badge look in the whole app (`StartOption`'s): the WORD says
                    whether Vis already knows this folder or merely that it is a repo. */}
                {knownRoots.has(entry.path) ? (
                  <span className={CHIP}>project</span>
                ) : entry.is_repo ? (
                  <span className={CHIP}>git</span>
                ) : null}
              </button>
            ))
          )}
          {listing?.is_truncated && (
            <p className="px-3 py-2 font-mono text-meta text-dialog-hint">
              Only the first folders are listed — type the path instead.
            </p>
          )}
        </div>

        <div className="shrink-0 border-t border-dialog-edge bg-panel-2 px-3 py-2">
          <p className="truncate font-mono text-meta text-dialog-hint">
            {folder === null ? target || '…' : `${here}/${folder.trim()}`}
          </p>
          {/* `quiet` beside `solid` on purpose: two bordered boxes side by side read
              as rivals, which is the whole reason the shipped Button has that variant. */}
          <div className="mt-1.5 flex items-center justify-between gap-2">
            <Button
              variant="quiet"
              disabled={saving || !here}
              onClick={() => setFolder(folder === null ? '' : null)}
            >
              {folder === null ? 'New folder' : 'Cancel'}
            </Button>
            <Button
              disabled={saving || !target || (folder !== null && !folder.trim())}
              onClick={() => void commit()}
            >
              {folder === null ? 'Switch here' : 'Create & switch'}
            </Button>
          </div>
        </div>
      </div>
    </div>,
    document.body,
  );
}

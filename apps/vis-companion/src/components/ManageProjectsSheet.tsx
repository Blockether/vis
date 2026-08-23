/**
 * Manage projects on ONE machine: browse, create, move, and choose project folders.
 *
 * A machine owns its projects, so the only thing that knows which folders exist is
 * the machine: every row here comes from `GET /v1/fs` on that gateway. The sheet
 * commits a FOLDER and lets the gateway decide what that folder is (a repo or not),
 * which is why there is no "is this a git project?" question anywhere in it.
 *
 * Two modes, one header: the crumbs are for recognition, the pencil is for people
 * who know where they are going. The pencil is INK, never a box — a bordered button
 * beside the path reads as a second action competing with it — which is `IconButton`
 * in its `quiet` variant, not a hand-built one.
 *
 * NOTHING in here paints its own box. It used to paint all of them — its own panel,
 * its own band, its own row, its own badge, its own icon button — each a near-copy of
 * something `Menu` already shipped, and each drifted: the panel had no way out at all
 * on a phone, the rows were the one list in the app that stayed 44px under a mouse,
 * and on a desktop the whole sheet hung off the bottom of the window with `Use
 * project` — the only control it exists to reach — below the fold and unscrollable.
 * It is `AnchoredPanel` + `MenuHeading` + `MenuItem` now, so it cannot drift again.
 */
import { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import { Button, IconButton, Input, Spinner } from './ui';
import { AnchoredPanel, MenuBack, MenuHeading, MenuItem, MenuNote } from './Menu';
import type { MenuPosition } from '../lib/anchored-menu';
import { ChevronIcon, PencilIcon, ProjectsIcon, TrashIcon } from './icons';
import type { GatewayClient } from '../lib/gateway';
import type { BrowseEntry } from '../lib/types';
import { homeifyPath } from '../lib/path';

/** How many crumbs stay on screen. A path too long for 390px elides from the LEFT. */
const CRUMB_TAIL = 3;

/**
 * The sheet's own leading edge, shared by its path bar and its rows, so a path and
 * the folder it lists start at the same x. They used to start at four different ones.
 */
const SHEET_EDGE = 'px-3';

/**
 * Both spellings of the path bar — the crumbs and the typed input — are the SAME band,
 * so the pencil toggles what is inside it and never how tall it is. `min-h` let the
 * crumb row (a 44px button on a fractional line box) settle at 45 while the input row
 * settled at 44, and every row below jumped 1px on the toggle. A fixed height is the
 * only spelling that cannot drift, and there is one of it.
 */
const PATH_BAND = 'flex h-11 shrink-0 items-center border-b border-dialog-edge mouse:h-9';

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

/** A path that names the folder ITSELF rather than its parent. `/` is already one. */
function withSlash(path: string): string {
  return path.endsWith('/') ? path : `${path}/`;
}

/** What a typed path is asking for: the folder to LIST, and the leaf to match in it. */
export function splitTyped(typed: string): { dir: string; leaf: string } {
  const cut = typed.lastIndexOf('/');
  if (cut < 0) return { dir: typed, leaf: '' };
  return { dir: typed.slice(0, cut) || '/', leaf: typed.slice(cut + 1) };
}

/**
 * Where browsing OPENS: one level above the machine's current project, never inside
 * it. A project's siblings are the folders you are actually looking for — the next
 * checkout, the next worktree — and they live in `../`, so opening on the project
 * itself listed its own `src/` and made every add start with a tap on `‹` first.
 * A root (or nothing) stays where it is: `/` has no `..`.
 */
export function startingDir(startAt: string | null): string | null {
  if (!startAt) return null;
  const cut = startAt.replace(/\/+$/, '').lastIndexOf('/');
  if (cut < 0) return startAt;
  return startAt.slice(0, cut) || '/';
}

function entryHint(entry: BrowseEntry): string {
  const count = `${entry.entry_count} ${entry.entry_count === 1 ? 'entry' : 'entries'}`;
  return entry.branch ? `${count} · ${entry.branch}` : count;
}

/** One project this machine already owns, as the portal lists it. */
export interface ManagedProject {
  /** The name the sessions list titles it with. */
  name: string;
  /** Canonical root on that machine — the identity, and what removal acts on. */
  root: string;
  /** The gateway's project id when the root is a saved project, `''` otherwise. */
  projectId: string;
  /** How many transcripts it holds. */
  count: number;
  /** How many of those are running right now. */
  live: number;
}

export function ManageProjectsSheet({
  label,
  isAdding,
  at,
  client,
  startAt,
  knownRoots,
  projects,
  onCancel,
  onChoose,
  onRemove,
}: {
  /** The machine whose files these are — the title says it, so no row has to. */
  label: string;
  /**
   * Opens straight on the folder browser. The row's `New project` button MEANS what
   * it says: landing on the inventory first and making a human hunt for a second
   * "New project…" is a tap spent on a verb the button already named.
   */
  isAdding?: boolean;
  /** Where the panel hangs from `sm:` up — the control that opened it. */
  at: MenuPosition | null;
  client: GatewayClient;
  /** Where browsing opens: the machine's current project, or its home. */
  startAt: string | null;
  /** Roots this machine already runs sessions in, so the common case is recognised. */
  knownRoots: Set<string>;
  /** What this machine ALREADY has. The portal opens on these, not on a filesystem. */
  projects: ManagedProject[];
  onCancel: () => void;
  onChoose: (root: string) => void;
  /** Remove every transcript in one project. The caller owns the confirmation. */
  onRemove: (project: ManagedProject) => void;
}) {
  // The portal opens on what this machine HAS; the filesystem is one step in, behind
  // the verb that needs it. It used to open on `GET /v1/fs` — a folder browser called
  // "Manage projects", which could add a project and never showed you the ones you
  // had, let alone remove one.
  const [adding, setAdding] = useState(isAdding || projects.length === 0);
  const [dir, setDir] = useState<string>(startingDir(startAt) ?? '~');
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
  // Taking the pencil changes the INPUT, not the PLACE: `~/vis/` names the very folder
  // already on screen, so the fetch is skipped and the rows never move. Without this the
  // toggle re-listed the same directory under its other spelling — the list blanked to
  // the folder it had, then landed again, one frame later and one row taller.
  const settled =
    listing !== null &&
    (wanted === listing.path || wanted === homeify(listing.path, listing.home));
  const isTyping = typedSplit !== null;
  useEffect(() => {
    // The inventory is not a filesystem: while it is on screen the gateway is never
    // asked for a folder. Listing behind it spent a round-trip nobody could see and
    // landed its rows under the projects, one frame late.
    if (!adding || settled) return;
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
      isTyping ? 180 : 0,
    );
    return () => {
      window.clearTimeout(timer);
      controller.abort();
    };
  }, [adding, client, wanted, settled, isTyping]);

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

  // Naming a new folder always aims somewhere that does not exist yet, so the check is
  // only about the folder currently AIMED at while browsing.
  const alreadyProject = folder === null && !!target && knownRoots.has(target);

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

  // The pencil is a control like any other, so it is the app's icon button: the same
  // box, the same focus ring and the same desktop rhythm as the `⋯` that opened this
  // sheet. Hand-built, it was a borderless 44px slab that never shrank under a mouse
  // and answered neither hover nor focus.
  //
  // `quiet` keeps the promise this file opened with — the pencil is INK, never a box,
  // because a bordered button beside the path reads as a second action competing with
  // it. The frame arrives on hover and focus, where it answers "can I press this".
  const pencil = (
    <IconButton
      ref={pencilRef}
      variant="quiet"
      label={typed === null ? 'Type a path' : 'Back to browsing'}
      aria-pressed={typed !== null}
      className={typed === null ? '' : 'text-accent-ink'}
      // The path is handed over with its trailing slash: `~/vis/` LISTS `~/vis`, so the
      // pencil keeps the folder you are in. `~/vis` would have listed the PARENT and
      // filtered it to names starting with `vis` — the list jumped a level on a toggle.
      onClick={() => setTyped(typed === null ? withSlash(homeify(here, home)) : null)}
    >
      <PencilIcon className="size-4" />
    </IconButton>
  );

  const projectActions = (
    <>
      <Button
        variant="secondary"
        density="compact"
        disabled={saving || !here || alreadyProject}
        onClick={() => setFolder(folder === null ? '' : null)}
      >
        {folder === null ? 'New folder' : 'Cancel'}
      </Button>
      <Button
        density="compact"
        disabled={saving || !target || alreadyProject || (folder !== null && !folder.trim())}
        onClick={() => void commit()}
      >
        {folder === null ? 'Use project' : 'Create project'}
      </Button>
    </>
  );

  return (
    <AnchoredPanel
      size="browse"
      role="dialog"
      label={`Manage projects on ${label}`}
      at={at}
      onDismiss={onCancel}
    >
      {/* The same panel the draft picker wears, because this asks the same kind of
          question: one loud band naming what the rows act on, the app's one way
          out inside it, and rows that are `MenuItem`s. It used to be a full-height
          `DialogFrame` — a different box, a different header and a different
          entrance for a list one tap away from the menu that opened it. */}
      {!adding && (
        <MenuHeading
          action={
            <Button variant="primary" density="compact" onClick={() => setAdding(true)}>
              New project…
            </Button>
          }
          onClose={onCancel}
          closeLabel={`Close projects on ${label}`}
        >{`Projects · ${label}`}</MenuHeading>
      )}

      {!adding ? (
        <>
          {/* WHAT THIS MACHINE HAS. Each row is the project: press it to make it the
              machine's current one, or take the trash beside it. Removal lives here
              because this is the portal that manages projects — it was a `⋯` on every
              project header opening a popover with one destructive row in it. */}
          <div className="min-h-0 flex-1 touch-pan-y overflow-y-auto overscroll-contain [&>*:last-child]:border-b-0">
            {projects.length === 0 ? (
              <MenuNote>This machine has no projects yet.</MenuNote>
            ) : (
              projects.map((entry) => (
                <MenuItem
                  key={entry.root}
                  icon={<ProjectsIcon className="size-4" />}
                  title={entry.name}
                  meta={`${entry.count} ${entry.count === 1 ? 'transcript' : 'transcripts'}${
                    entry.live > 0 ? `, ${entry.live} running` : ''
                  }`}
                  hint={homeifyPath(entry.root) || entry.root}
                  badge={entry.root === startAt ? 'current' : undefined}
                  onSelect={() => onChoose(entry.root)}
                  action={
                    <IconButton
                      edge
                      variant="remove"
                      label={`Remove every transcript in ${entry.name}`}
                      onClick={() => onRemove(entry)}
                    >
                      <TrashIcon className="size-4" />
                    </IconButton>
                  }
                />
              ))
            )}
          </div>
        </>
      ) : (
        <>
      {/* Adding is a task, not a step in a tour, so its band carries the app's one way
          out when the human ASKED for it by name (the start flow's `New project`).
          Reached from the inventory's own `New project…` it IS a step inside this
          menu, and a step is left the way it was entered — otherwise the only exit
          from the browser is closing the sheet and finding the folder mark again. */}
      {isAdding ? (
        <MenuHeading
          action={projectActions}
          onClose={onCancel}
          closeLabel={`Close new project on ${label}`}
        >{`New project · ${label}`}</MenuHeading>
      ) : (
        <MenuBack
          label={`Back to projects on ${label}`}
          onBack={() => setAdding(false)}
          action={projectActions}
        >{`New project · ${label}`}</MenuBack>
      )}

      {alreadyProject && <MenuNote>It’s already a project</MenuNote>}

      {typed === null ? (
        <div
          className={`${PATH_BAND} gap-1 bg-panel-2 ${SHEET_EDGE}`}
        >
          {crumbs.length > shown.length && (
            <span className="shrink-0 font-mono text-meta text-dialog-hint" aria-hidden>
              …
            </span>
          )}
          <span className="flex min-w-0 flex-1 items-center overflow-hidden">
            {shown.map((crumb, index) => {
              const isHere = index === shown.length - 1;
              return (
                <span key={crumb.path} className="flex min-w-0 items-center">
                  {/* A separator BETWEEN crumbs, never in front of the first one: the
                      bar used to open `› ~ › vis`, a chevron pointing at nothing. */}
                  {(index > 0 || crumbs.length > shown.length) && (
                    <ChevronIcon className="mx-0.5 size-3 shrink-0 text-dialog-hint" aria-hidden />
                  )}
                  {/* A crumb is a real target: the text-only ones were 14px tall in a
                      sheet whose every other row was 44. */}
                  <button
                    type="button"
                    disabled={isHere}
                    aria-current={isHere ? 'location' : undefined}
                    className={`min-h-11 truncate px-1 font-mono text-meta transition-colors duration-150 focus-visible:outline-none motion-reduce:transition-none mouse:min-h-6 ${
                      isHere
                        ? 'font-bold text-white'
                        : 'text-accent-ink hover:bg-hover focus-visible:bg-hover'
                    }`}
                    onClick={() => enter(crumb.path)}
                  >
                    {crumb.label}
                  </button>
                </span>
              );
            })}
          </span>
          {pencil}
        </div>
      ) : (
        <div
          className={`${PATH_BAND} gap-2 bg-panel-2 ${SHEET_EDGE}`}
        >
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
        <div
          className={`${PATH_BAND} gap-2 bg-panel ${SHEET_EDGE}`}
        >
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

      {/* The last folder drops its rule so it cannot double the panel's bottom edge. */}
      <div className="min-h-0 flex-1 touch-pan-y overflow-y-auto overscroll-contain [&>*:last-child]:border-b-0 [&>div>*:last-child]:border-b-0">
        {error ? (
          <p className={`py-3 font-mono text-meta text-err ${SHEET_EDGE}`}>{error}</p>
        ) : busy && !listing ? (
          <MenuNote>
            <Spinner tone="accent" />
            Reading folders...
          </MenuNote>
        ) : rows.length === 0 ? (
          <MenuNote>
            {typedSplit?.leaf ? 'No folder here starts with that.' : 'No folders in here.'}
          </MenuNote>
        ) : (
          // A folder row IS a menu row — a glyph, a name, the consequence of choosing
          // it, and an optional badge — so it is the app's menu row, not a fourth
          // near-copy of one. The badge's WORD says whether Vis already knows this
          // folder or merely that it is a repo.
          // `inert`, not `aria-hidden`: while a new folder is being named these rows
          // are out of play, and a container that is merely hidden from the a11y tree
          // still hands its buttons to the Tab key.
          <div className={folder === null ? '' : 'opacity-40'} inert={folder !== null}>
            {rows.map((entry) => (
              <MenuItem
                key={entry.path}
                icon={<ChevronIcon className="size-3.5" />}
                title={`${entry.name}/`}
                hint={entryHint(entry)}
                badge={
                  entry.path === startAt
                    ? 'current'
                    : knownRoots.has(entry.path)
                      ? 'project'
                      : entry.is_repo
                        ? 'git'
                        : undefined
                }
                onSelect={() => enter(entry.path)}
              />
            ))}
          </div>
        )}
        {listing?.is_truncated && (
          <MenuNote>Only the first folders are listed — type the path instead.</MenuNote>
        )}
      </div>

        </>
      )}
    </AnchoredPanel>
  );
}

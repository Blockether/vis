/**
 * Choose a project folder on ONE machine: browse, create, and pick it.
 *
 * The same sheet adds a project (`add`) and gives an existing project another folder
 * (`move`). The machine's projects themselves are managed from each project's own menu.
 *
 * A machine owns its projects, so the only thing that knows which folders exist is
 * the machine: every row here comes from `GET /v1/fs` on that gateway. The sheet
 * commits a FOLDER and lets the gateway decide what that folder is (a repo or not),
 * which is why there is no "is this a git project?" question anywhere in it.
 *
 * One field says where you are AND what you look for. The crumbs lead the field and
 * the text after them filters that folder, so the place and the search read as one
 * path (`~/code/vis…`). Text that starts with `~` or `/` is a path instead. This
 * replaced a crumb bar over a separate search box, where the reader could not tell
 * which folder the search looked in.
 *
 * NOTHING in here paints its own box. It is `AnchoredPanel` + `MenuHeading` +
 * `MenuItem`, so it stays the app's anchored dialog on every width, never a sheet
 * that slides up from the bottom.
 */
import { useCallback, useEffect, useMemo, useState, type KeyboardEvent } from 'react';
import { BandButton, Button, Chip, Input, Spinner } from './ui';
import { AnchoredPanel, MenuHeading, MenuItem, MenuNote } from './Menu';
import type { MenuPosition } from '../lib/anchored-menu';
import { FolderPlusIcon, ProjectsIcon, SearchIcon, UndoIcon } from './icons';
import type { GatewayClient } from '../lib/gateway';
import type { BrowseEntry } from '../lib/types';

/** How many crumbs stay on screen. A path too long for 390px elides from the LEFT. */
const CRUMB_TAIL = 3;

/** How many folders that hold projects the sheet offers as places to jump to. */
const PLACES_LIMIT = 4;

/**
 * The sheet's own leading edge, shared by its bands, its rows and its footer, so a
 * path and the folder it lists start at the same x.
 */
const SHEET_EDGE = 'px-3';

/** The new-folder line: one fixed height, so opening it never moves the rows by 1px. */
const PATH_BAND = 'flex h-11 shrink-0 items-center border-b border-dialog-edge mouse:h-9';

/**
 * The location field wears `Input`'s own frame and focus, because it IS the sheet's
 * input. The crumbs inside it cannot live in `Input`, so the frame moves to the row.
 */
const LOCATION_FIELD =
  'flex h-8 min-w-0 items-center gap-0.5 border border-edge bg-input pl-2.5 pr-1 font-mono text-ui transition-[border-color,box-shadow] duration-150 focus-within:border-accent focus-within:ring-1 focus-within:ring-accent/30 motion-reduce:transition-none sm:pl-3 mouse:h-7';

/** A crumb inside the field keeps a 44px touch target through invisible reach. */
const CRUMB =
  'relative max-w-40 shrink-0 truncate px-0.5 transition-colors duration-150 after:absolute after:inset-x-0 after:-top-1.5 after:-bottom-1.5 after:content-[""] focus-visible:bg-hover focus-visible:outline-none motion-reduce:transition-none mouse:after:content-none';

/** Text that starts like a path is a path; anything else filters the folder on screen. */
export function isPathQuery(query: string): boolean {
  return query.startsWith('~') || query.startsWith('/');
}

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


/** The fact beside a folder's name: empty, or the branch it has checked out. */
function entryMeta(entry: BrowseEntry): string | undefined {
  if (entry.entry_count === 0) return 'empty';
  return entry.branch ?? undefined;
}

/**
 * Folders that already hold this machine's projects, as places to jump to. The next
 * checkout usually lives beside the last one, so its parent is the useful place.
 */
export function projectPlaces(knownRoots: Set<string>, here: string): [string, number][] {
  const counts = new Map<string, number>();
  for (const root of knownRoots) {
    const place = startingDir(root);
    if (place && place !== root) counts.set(place, (counts.get(place) ?? 0) + 1);
  }
  return [...counts]
    .filter(([place]) => place !== here)
    .sort(([a], [b]) => a.localeCompare(b))
    .slice(0, PLACES_LIMIT);
}

/** What the sheet is for, as its band, its way out and its commit verbs say it. */
const WORDS = {
  add: { title: 'New project', close: 'new project', use: 'Use project', create: 'Create project' },
  move: { title: 'Change folder', close: 'change folder', use: 'Move here', create: 'Create and move' },
} as const;

export function ProjectFolderSheet({
  label,
  mode,
  at,
  client,
  startAt,
  knownRoots,
  onCancel,
  onChoose,
}: {
  /** The machine whose files these are — the title says it, so no row has to. */
  label: string;
  /** `add` makes the chosen folder a project; `move` gives an existing project that folder. */
  mode: 'add' | 'move';
  /** Where the panel hangs from `sm:` up — the control that opened it. */
  at: MenuPosition | null;
  client: GatewayClient;
  /** Where browsing opens: the project's own folder, the machine's current one, or home. */
  startAt: string | null;
  /** Roots this machine already has as projects, so the common case is recognised. */
  knownRoots: Set<string>;
  onCancel: () => void;
  onChoose: (root: string) => void | Promise<void>;
}) {
  const words = WORDS[mode];
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
  // The one field's text: a filter for the folder on screen, or a path (see `isPathQuery`).
  const [query, setQuery] = useState('');
  const [isGitOnly, setGitOnly] = useState(false);
  const [isEmptyHidden, setEmptyHidden] = useState(true);
  // `null` is not creating; a string is the folder that does not exist yet.
  const [folder, setFolder] = useState<string | null>(null);
  const [saving, setSaving] = useState(false);

  const typedSplit = isPathQuery(query) ? splitTyped(query) : null;

  // One read path for both kinds of text: the directory being listed is a value, and a
  // typed path just moves it. A filter keystroke must not re-ask the gateway.
  const wanted = typedSplit ? typedSplit.dir : dir;
  // `~/vis/` names the very folder already on screen, so the fetch is skipped and the
  // rows never move.
  const settled =
    listing !== null && (wanted === listing.path || wanted === homeify(listing.path, listing.home));
  const isTyping = typedSplit !== null;
  useEffect(() => {
    if (settled) return;
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
  }, [client, wanted, settled, isTyping]);

  const home = listing?.home ?? '';
  const here = listing?.path ?? '';
  const crumbs = useMemo(() => (here ? crumbsOf(here, home) : []), [here, home]);
  const shown = crumbs.slice(-CRUMB_TAIL);

  // A typed path narrows by the start of the leaf; a filter matches anywhere in the name.
  // The chips apply to both. The React Compiler memoizes this, so it takes no `useMemo`.
  const pathLeaf = typedSplit ? typedSplit.leaf.toLowerCase() : null;
  const needle = pathLeaf === null ? query.trim().toLowerCase() : '';
  const rows = (listing?.entries ?? []).filter(
    (entry) =>
      (!pathLeaf || entry.name.toLowerCase().startsWith(pathLeaf)) &&
      (!needle || entry.name.toLowerCase().includes(needle)) &&
      (!isGitOnly || entry.is_repo) &&
      (!isEmptyHidden || entry.entry_count > 0),
  );
  const repos = rows.filter((entry) => entry.is_repo);
  const others = rows.filter((entry) => !entry.is_repo);
  const places = projectPlaces(knownRoots, here);
  const isPlacesShown = query === '' && !isGitOnly && places.length > 0 && !!listing;

  // A typed path that NAMES a folder commits that folder, not the parent being listed.
  const exact = typedSplit?.leaf
    ? (rows.find((entry) => entry.name.toLowerCase() === typedSplit.leaf.toLowerCase()) ?? null)
    : null;
  const target = exact?.path ?? here;

  // Naming a new folder always aims somewhere that does not exist yet, so the check is
  // only about the folder currently AIMED at while browsing.
  const alreadyProject = folder === null && !!target && knownRoots.has(target);

  const commit = useCallback(async () => {
    if (!target || alreadyProject) return;
    const name = folder?.trim() ?? '';
    if (folder !== null && (!name || !here)) return;
    setSaving(true);
    try {
      if (folder === null) {
        await onChoose(target);
        return;
      }
      const made = await client.createDirectory(here, name);
      await onChoose(made.path);
    } catch (cause) {
      setError((cause as Error).message);
      setSaving(false);
    }
  }, [alreadyProject, client, folder, here, onChoose, target]);

  const enter = useCallback((path: string) => {
    setQuery('');
    setFolder(null);
    setDir(path);
  }, []);

  useEffect(() => {
    const onKey = (event: globalThis.KeyboardEvent) => {
      if (event.key === 'Escape') onCancel();
    };
    window.addEventListener('keydown', onKey);
    return () => window.removeEventListener('keydown', onKey);
  }, [onCancel]);

  // The field's keys: Backspace on an empty field goes up one folder, and Enter opens
  // the first folder that matches. A typed path keeps its own Enter: open the folder it
  // names, or commit the folder it lists.
  const onFieldKey = (event: KeyboardEvent<HTMLInputElement>) => {
    if (event.key === 'Backspace' && query === '' && listing?.parent) {
      event.preventDefault();
      enter(listing.parent);
      return;
    }
    if (event.key !== 'Enter') return;
    if (typedSplit) {
      if (exact) enter(exact.path);
      else void commit();
      return;
    }
    const first = repos[0] ?? others[0];
    if (query.trim() && first) enter(first.path);
  };

  // The band keeps one cell before the way out: the folder that takes the add stroke.
  // Naming a folder turns it into an UNDO, never a second close.
  const projectCells = (
    <BandButton
      label={folder === null ? 'New folder' : 'Cancel'}
      disabled={saving || !here || alreadyProject}
      onClick={() => setFolder(folder === null ? '' : null)}
    >
      {folder === null ? <FolderPlusIcon /> : <UndoIcon />}
    </BandButton>
  );

  // The commit names the folder it commits, so the reader never guesses what the
  // button takes. It is a word, not a tick: a mark alone did not say "this folder".
  const named = folder?.trim();
  const aimed =
    folder === null
      ? homeify(target, home)
      : `${homeify(here, home).replace(/\/$/, '')}/${named || '…'}`;
  const commitVerb = folder === null ? words.use : words.create;

  const row = (entry: BrowseEntry) => (
    <MenuItem
      key={entry.path}
      icon={<ProjectsIcon className="size-4" />}
      title={`${entry.name}/`}
      meta={entryMeta(entry)}
      badge={knownRoots.has(entry.path) ? 'project' : entry.is_repo ? 'git' : undefined}
      isNavigation
      onSelect={() => enter(entry.path)}
    />
  );

  return (
    <AnchoredPanel
      size="browse"
      role="dialog"
      label={`${words.title} on ${label}`}
      at={at}
      onDismiss={onCancel}
    >
      {/* The band does NOT name the machine: the panel opens FROM that machine's rail.
          The name stays in the panel's `aria-label` and the way out. */}
      <MenuHeading
        cells={projectCells}
        onClose={onCancel}
        closeLabel={`Close ${words.close} on ${label}`}
      >
        {words.title}
      </MenuHeading>

      <div className={`flex shrink-0 flex-col gap-2 border-b border-dialog-edge bg-panel-2 py-2 ${SHEET_EDGE}`}>
        <div className={LOCATION_FIELD}>
          <SearchIcon className="mr-1 size-4 text-dialog-hint" />
          {!typedSplit && (
            <span className="flex min-w-0 shrink items-center overflow-hidden">
              {crumbs.length > shown.length && (
                <span className="shrink-0 text-dialog-hint" aria-hidden>
                  …/
                </span>
              )}
              {shown.map((crumb, index) => {
                const isHere = index === shown.length - 1;
                return (
                  <span key={crumb.path} className="flex min-w-0 shrink-0 items-center">
                    <button
                      type="button"
                      disabled={isHere}
                      aria-current={isHere ? 'location' : undefined}
                      className={`${CRUMB} ${
                        isHere
                          ? 'font-bold text-white'
                          : 'text-accent-ink mouse:hover:text-white'
                      }`}
                      onClick={() => enter(crumb.path)}
                    >
                      {crumb.label}
                    </button>
                    {crumb.label !== '/' && (
                      <span className="text-dialog-hint" aria-hidden>
                        /
                      </span>
                    )}
                  </span>
                );
              })}
            </span>
          )}
          <input
            value={query}
            aria-label="Filter folders or type a path"
            placeholder={typedSplit ? undefined : 'filter or ~/path'}
            autoCapitalize="none"
            autoCorrect="off"
            spellCheck={false}
            className="h-full min-w-16 flex-1 bg-transparent px-0.5 text-white placeholder:text-dialog-hint focus:outline-none"
            onChange={(event) => setQuery(event.target.value)}
            onKeyDown={onFieldKey}
          />
        </div>
        <div className="flex items-center gap-2">
          <Chip isOn={isGitOnly} onClick={() => setGitOnly(!isGitOnly)}>
            Git only
          </Chip>
          <Chip isOn={isEmptyHidden} onClick={() => setEmptyHidden(!isEmptyHidden)}>
            Hide empty
          </Chip>
          <span className="ml-auto hidden truncate font-mono text-meta text-dialog-hint mouse:inline">
            Enter opens · ⌫ goes up
          </span>
        </div>
      </div>

      {folder !== null && (
        <div className={`${PATH_BAND} gap-2 bg-panel ${SHEET_EDGE}`}>
          <FolderPlusIcon className="text-accent-ink" />
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

      {/* The last folder drops its rule so it cannot double the footer's top edge. */}
      <div className="min-h-0 flex-1 touch-pan-y overflow-y-auto overscroll-contain [&>*:last-child]:border-b-0 [&>div>*:last-child]:border-b-0">
        {error ? (
          <p className={`py-3 font-mono text-meta text-err ${SHEET_EDGE}`}>{error}</p>
        ) : busy && !listing ? (
          <MenuNote>
            <Spinner tone="accent" />
            Reading folders...
          </MenuNote>
        ) : (
          // `inert`, not `aria-hidden`: while a new folder is being named these rows are
          // out of play, and a container merely hidden from the a11y tree still hands its
          // buttons to the Tab key.
          <div className={folder === null ? '' : 'opacity-40'} inert={folder !== null}>
            {isPlacesShown && (
              <>
                <MenuHeading tone="quiet">Places</MenuHeading>
                {places.map(([place, count]) => (
                  <MenuItem
                    key={place}
                    icon={<ProjectsIcon className="size-4" />}
                    title={`${homeify(place, home).replace(/\/$/, '')}/`}
                    meta={`${count} ${count === 1 ? 'project' : 'projects'}`}
                    isNavigation
                    onSelect={() => enter(place)}
                  />
                ))}
              </>
            )}
            {repos.length > 0 && (
              <>
                <MenuHeading tone="quiet">Git repos</MenuHeading>
                {repos.map(row)}
              </>
            )}
            {others.length > 0 && (
              <>
                <MenuHeading tone="quiet">Other folders</MenuHeading>
                {others.map(row)}
              </>
            )}
            {rows.length === 0 && (
              <MenuNote>
                {(listing?.entries ?? []).length === 0
                  ? 'No folders in here.'
                  : query.trim()
                    ? 'No folder here matches that.'
                    : 'No folders match these filters.'}
              </MenuNote>
            )}
          </div>
        )}
        {listing?.is_truncated && (
          <MenuNote>Only the first folders are listed — type the path instead.</MenuNote>
        )}
      </div>

      {/* The commit stands in the panel's own footer, outside the scrolling list, so it
          stays on screen while the rows scroll and above a raised keyboard. */}
      <footer className={`shrink-0 border-t border-dialog-edge bg-panel py-2 ${SHEET_EDGE}`}>
        <Button
          className="flex w-full min-w-0 items-center justify-center gap-1.5"
          disabled={
            saving || !target || alreadyProject || (folder !== null && !named)
          }
          onClick={() => void commit()}
        >
          {alreadyProject ? (
            'It’s already a project'
          ) : (
            <>
              <span className="shrink-0">{commitVerb}</span>{' '}
              <span className="min-w-0 truncate font-normal">{aimed}</span>
            </>
          )}
        </Button>
      </footer>
    </AnchoredPanel>
  );
}

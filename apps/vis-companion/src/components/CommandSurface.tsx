import {
  useCallback,
  useEffect,
  useId,
  useLayoutEffect,
  useRef,
  useState,
  useSyncExternalStore,
  type CSSProperties,
} from 'react';

import { useCommandRegistry, useCommands, type CommandRegistry } from '../lib/commands';
import {
  KEYMAP_GROUPS,
  KEYMAP_VERBS,
  PALETTE_KEY,
  PALETTE_ONLY_VERBS,
  PREFIX_LABEL,
  chordKeys,
  chordStep,
  matchVerbs,
  sectionTitle,
  type ChordState,
  type KeymapVerb,
} from '../lib/keymap';
import { SearchIcon } from './icons';
import { DialogFrame, Input, KeyCap, Modal } from './ui';

/** Every command of the palette: the C-x verbs, then the commands without a key. */
const PALETTE_VERBS: readonly KeymapVerb[] = [...KEYMAP_VERBS, ...PALETTE_ONLY_VERBS];

type PaletteMode = 'palette' | 'help';

/** True when the focused field holds a selection, so Ctrl+X must stay the field's cut. */
function fieldHasSelection(at: Element | null): boolean {
  if (at instanceof HTMLInputElement || at instanceof HTMLTextAreaElement) {
    try {
      return at.selectionStart !== null && at.selectionStart !== at.selectionEnd;
    } catch {
      // An input type without a caret, such as a checkbox, has no selection to cut.
      return false;
    }
  }
  if (at instanceof HTMLElement && at.isContentEditable) {
    const selection = window.getSelection();
    return !!selection && !selection.isCollapsed;
  }
  return false;
}

/** Re-render when a screen registers, drops or gates a command. */
function useRegistryVersion(registry: CommandRegistry | null): number {
  return useSyncExternalStore(
    registry?.subscribe ?? (() => () => {}),
    registry?.getVersion ?? (() => 0),
  );
}

/**
 * THE KEYBOARD'S WAY TO EVERY VERB: the C-x chords and the command palette.
 *
 * C-x, then a group key, then a verb key runs a verb, exactly as in the TUI: C-x s s
 * opens the session search, C-x m c chooses a model. While a chord waits, a band at the
 * bottom lists the keys that can follow, like the TUI hydra. C-x p opens the palette,
 * which finds every command by name. Escape or C-g ends a chord.
 *
 * Ctrl+X in a field that holds a selection stays the field's cut. A dialog with the focus
 * keeps the keyboard, as the search key already does.
 */
export function CommandSurface() {
  const registry = useCommandRegistry();
  useRegistryVersion(registry);
  const [chord, setChord] = useState<ChordState>(null);
  const chordRef = useRef<ChordState>(null);
  const [palette, setPalette] = useState<PaletteMode | null>(null);

  const moveChord = useCallback((next: ChordState) => {
    chordRef.current = next;
    setChord(next);
  }, []);

  useCommands({ 'toggle-help': () => setPalette((open) => (open === 'help' ? null : 'help')) });

  useEffect(() => {
    if (!registry) return;
    const onKey = (event: KeyboardEvent) => {
      if (event.isComposing) return;
      const live = KEYMAP_VERBS.filter((verb) => registry.isAvailable(verb.id));
      const groups = KEYMAP_GROUPS.filter((group) => live.some((verb) => verb.group === group.id));
      const step = chordStep(chordRef.current, event, groups, live);
      if (step.kind === 'ignore' || step.kind === 'wait') return;
      if (step.kind === 'start') {
        if (event.defaultPrevented || event.repeat) return;
        const at = document.activeElement;
        if (at?.closest('[role="dialog"]')) return;
        if (fieldHasSelection(at)) return;
      }
      // Each key of a chord belongs to the chord, never to the field under it.
      // The Escape that ends a chord must not also cancel the running turn.
      event.preventDefault();
      event.stopImmediatePropagation();
      if (step.kind === 'start') moveChord({ group: null });
      else if (step.kind === 'group') moveChord({ group: step.group });
      else {
        moveChord(null);
        if (step.kind === 'palette') setPalette('palette');
        else if (step.kind === 'verb') registry.run(step.verb.id);
      }
    };
    // A click elsewhere is a new gesture, so it ends the chord.
    const onPointer = () => {
      if (chordRef.current) moveChord(null);
    };
    window.addEventListener('keydown', onKey, true);
    window.addEventListener('pointerdown', onPointer, true);
    return () => {
      window.removeEventListener('keydown', onKey, true);
      window.removeEventListener('pointerdown', onPointer, true);
    };
  }, [registry, moveChord]);

  if (!registry) return null;
  return (
    <>
      {chord && <ChordBand registry={registry} group={chord.group} />}
      {palette && (
        <CommandPalette
          registry={registry}
          mode={palette}
          onClose={() => setPalette(null)}
        />
      )}
    </>
  );
}

/** One key and what it does, as a row of the band. The title reads larger than its caption. */
function BandRow({ keyName, title, description }: { keyName: string; title: string; description: string }) {
  return (
    <li className="flex min-w-0 items-baseline gap-2.5">
      <KeyCap keyName={keyName} size="text" />
      <span className="shrink-0 text-ui italic text-dialog-foreground mouse:text-title">{title}</span>
      <span aria-hidden="true" className="shrink-0 text-dialog-hint">
        —
      </span>
      <span className="min-w-0 text-meta text-dialog-hint mouse:text-ui">{description}</span>
    </li>
  );
}

/**
 * Where the band stands: just above the composer frame that holds the focus, else above
 * the first one on screen, and as wide as it, as the TUI band takes the chat column.
 * With no composer on screen, the band keeps to the bottom centre of the window.
 */
function bandPlace(): CSSProperties | undefined {
  const frames = Array.from(document.querySelectorAll<HTMLElement>('[data-chord-anchor]'));
  const at = document.activeElement;
  const shown = (frame: HTMLElement) => frame.getBoundingClientRect().width > 0;
  const frame =
    frames.find((one) => !!at && one.contains(at) && shown(one)) ?? frames.find(shown);
  if (!frame) return undefined;
  const box = frame.getBoundingClientRect();
  return { left: box.left, width: box.width, bottom: window.innerHeight - box.top + 8 };
}

/** The keys that can follow the pressed part of a chord, like the TUI's C-x band. */
export function ChordBand({ registry, group }: { registry: CommandRegistry; group: string | null }) {
  const [place, setPlace] = useState<CSSProperties | undefined>(undefined);
  useLayoutEffect(() => {
    const measure = () => setPlace(bandPlace());
    measure();
    window.addEventListener('resize', measure);
    return () => window.removeEventListener('resize', measure);
  }, []);
  const live = KEYMAP_VERBS.filter((verb) => registry.isAvailable(verb.id));
  const heading = group ? KEYMAP_GROUPS.find((item) => item.id === group) : null;
  const pressed = heading ? [PREFIX_LABEL, heading.key] : [PREFIX_LABEL];
  return (
    <div
      role="status"
      aria-label={`${pressed.join(' ')} keys`}
      data-chord-band
      style={place}
      className={`pointer-events-none fixed z-50 flex justify-center ${place ? '' : 'inset-x-0 bottom-[max(0.75rem,env(safe-area-inset-bottom))] px-3'}`}
    >
      <div className={`@container w-full border border-dialog-edge bg-panel px-4 py-3 font-mono shadow-float ${place ? '' : 'max-w-4xl'}`}>
        <div className="-mx-4 mb-3 flex min-w-0 items-center gap-2.5 border-b border-dialog-edge px-4 pb-3 text-ui mouse:text-title">
          <span className="flex shrink-0 gap-1">
            {pressed.map((key) => (
              <KeyCap key={key} keyName={key} size="text" />
            ))}
          </span>
          <span className="min-w-0 truncate font-bold text-dialog-foreground">
            {heading ? heading.title : 'Press a key'}
          </span>
          <span className="ml-auto flex shrink-0 items-center gap-1.5 text-meta text-dialog-hint mouse:text-ui">
            <KeyCap keyName="Escape" size="text" />
            cancels
          </span>
        </div>
        <ul className="grid gap-x-8 gap-y-2 @min-[34rem]:grid-cols-2">
          {heading
            ? live
                .filter((verb) => verb.group === heading.id && verb.key)
                .map((verb) => (
                  <BandRow key={verb.id} keyName={verb.key ?? ''} title={verb.title} description={verb.description} />
                ))
            : (
              <>
                {KEYMAP_GROUPS.filter((item) => live.some((verb) => verb.group === item.id)).map((item) => (
                  <BandRow key={item.id} keyName={item.key} title={`${item.title.toLowerCase()}…`} description={item.description} />
                ))}
                <BandRow keyName={PALETTE_KEY} title="command palette…" description="Find and run any command" />
                {live
                  .filter((verb) => verb.group === null && verb.key)
                  .map((verb) => (
                    <BandRow key={verb.id} keyName={verb.key ?? ''} title={verb.title} description={verb.description} />
                  ))}
              </>
            )}
        </ul>
      </div>
    </div>
  );
}

/**
 * The searchable list of commands. `palette` lists the commands that can run here; `help`
 * lists every key of the map, and the ones that cannot run here are dimmed.
 */
export function CommandPalette({
  registry,
  mode,
  onClose,
}: {
  registry: CommandRegistry;
  mode: PaletteMode;
  onClose: () => void;
}) {
  const [query, setQuery] = useState('');
  const [active, setActive] = useState(0);
  const inputRef = useRef<HTMLInputElement>(null);
  const listId = useId();
  const all = mode === 'help' ? KEYMAP_VERBS : PALETTE_VERBS.filter((verb) => registry.isAvailable(verb.id));
  const rows = matchVerbs(all, query);
  const at = Math.min(active, Math.max(0, rows.length - 1));

  useEffect(() => {
    inputRef.current?.focus();
  }, []);
  useEffect(() => {
    document.getElementById(`${listId}-${at}`)?.scrollIntoView({ block: 'nearest' });
  }, [listId, at]);

  const run = (verb: KeymapVerb | undefined) => {
    if (!verb || !registry.isAvailable(verb.id)) return;
    onClose();
    // The palette leaves first, so the verb opens its own dialog over the screen.
    window.setTimeout(() => registry.run(verb.id), 0);
  };

  return (
    <Modal onDismiss={onClose} size="split" within="session">
      <DialogFrame title={mode === 'help' ? 'Keyboard shortcuts' : 'Command palette'} onClose={onClose}>
        <div className="shrink-0 border-b border-dialog-edge px-3 py-3 sm:px-4">
          <Input
            ref={inputRef}
            value={query}
            type="search"
            autoCorrect="off"
            autoCapitalize="none"
            spellCheck={false}
            placeholder={mode === 'help' ? 'Type to find a key…' : 'Type to search commands…'}
            aria-label={mode === 'help' ? 'Find a key' : 'Search commands'}
            role="combobox"
            aria-expanded="true"
            aria-controls={listId}
            aria-activedescendant={rows.length ? `${listId}-${at}` : undefined}
            icon={<SearchIcon className="size-3" />}
            onChange={(event) => {
              setQuery(event.target.value);
              setActive(0);
            }}
            onKeyDown={(event) => {
              if (event.key === 'ArrowDown' || (event.ctrlKey && event.key === 'n')) {
                event.preventDefault();
                setActive(rows.length ? (at + 1) % rows.length : 0);
              } else if (event.key === 'ArrowUp' || (event.ctrlKey && event.key === 'p')) {
                event.preventDefault();
                setActive(rows.length ? (at - 1 + rows.length) % rows.length : 0);
              } else if (event.key === 'Enter') {
                event.preventDefault();
                run(rows[at]);
              } else if (event.key === 'Escape' || (event.ctrlKey && event.key === 'g')) {
                event.preventDefault();
                event.stopPropagation();
                onClose();
              }
            }}
          />
        </div>
        <ul
          id={listId}
          role="listbox"
          aria-label={mode === 'help' ? 'Keys' : 'Commands'}
          className="min-h-0 flex-1 overflow-y-auto overscroll-contain bg-page pb-2 font-mono text-ui mouse:text-title"
        >
          {rows.length === 0 && <li className="px-4 py-3 text-dialog-hint">No command matches.</li>}
          {rows.map((verb, index) => {
            const title = sectionTitle(verb);
            const heading = index === 0 || title !== sectionTitle(rows[index - 1]) ? title : null;
            const isAvailable = registry.isAvailable(verb.id);
            return (
              <li key={verb.id} role="presentation">
                {heading && (
                  <div
                    role="presentation"
                    className={`px-4 pb-2 pt-4 text-body font-bold text-dialog-foreground mouse:text-subhead ${index === 0 ? '' : 'mt-2 border-t border-dialog-edge'}`}
                  >
                    {heading}
                  </div>
                )}
                <div
                  id={`${listId}-${index}`}
                  role="option"
                  aria-selected={index === at}
                  aria-disabled={!isAvailable || undefined}
                  className={`flex min-w-0 cursor-pointer items-baseline gap-3 px-4 py-1.5 ${index === at ? 'bg-dialog-title text-dialog-title-foreground' : 'text-dialog-foreground'} ${isAvailable ? '' : 'cursor-default opacity-60'}`}
                  onMouseMove={() => setActive(index)}
                  onClick={() => run(verb)}
                >
                  <span className="shrink-0 italic">{verb.title}</span>
                  <span aria-hidden="true" className={`shrink-0 ${index === at ? '' : 'text-dialog-hint'}`}>
                    —
                  </span>
                  <span className={`min-w-0 flex-1 truncate text-meta mouse:text-ui ${index === at ? '' : 'text-dialog-hint'}`}>
                    {verb.description}
                  </span>
                  <span className="flex shrink-0 gap-1 self-center text-meta mouse:text-ui">
                    {chordKeys(verb).map((key, place) => (
                      <KeyCap key={place} keyName={key} size="text" />
                    ))}
                  </span>
                </div>
              </li>
            );
          })}
        </ul>
      </DialogFrame>
    </Modal>
  );
}

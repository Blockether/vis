/**
 * THE TRANSCRIPT OUTLINE (Blockether/vis#316).
 *
 * A short rail of thin lines stands in a gutter beside the transcript, one line for
 * each turn. The line of the turn on screen is darker. The rail opens a small card
 * that names each turn of the session by the words that opened it, oldest first.
 * Picking a row puts that turn at the top of the transcript.
 *
 * A mouse that rests on the rail opens the card, and the card stays open while the
 * mouse is on the rail or on the card. A click, a tap or a key keeps the card open
 * until the reader picks a row, presses Escape or presses outside the card.
 *
 * The row under the mouse or the keyboard shows a PREVIEW left of the card: the
 * whole prompt and the start of its answer. A finger that drags on the rail SCRUBS
 * through the turns. The preview follows the finger, and each held turn comes to the
 * top of the transcript at once. The release jumps to the last turn that it named.
 *
 * A NARROW SCREEN has no room for a gutter, so the rail stands over the text and is
 * hidden. It shows when the reader scrolls the transcript or taps its place, and hides
 * again `RAIL_SHOW_MS` later. A tap on the hidden rail only shows it.
 */
import {
  useCallback,
  useEffect,
  useId,
  useLayoutEffect,
  useRef,
  useState,
  type FocusEvent as ReactFocusEvent,
  type KeyboardEvent as ReactKeyboardEvent,
  type MouseEvent as ReactMouseEvent,
  type PointerEvent as ReactPointerEvent,
  type RefObject,
} from 'react';
import { createPortal } from 'react-dom';

import type { Viewport } from '../lib/anchored-menu';
import { useBackLayer } from '../lib/edge-back';
import {
  mergeOutline,
  NARROW_SCREEN,
  OUTLINE_WIDTH,
  outlinePanelPosition,
  previewPosition,
  RAIL_SHOW_MS,
  railLines,
  readingTurnId,
  scrubIndex,
  scrubStep,
  type OutlineEntry,
  type OutlinePlace,
  type OutlineStatus,
  type PreviewAnchor,
} from '../lib/transcript-outline';
import { JustifiedProse } from './JustifiedProse';
import { Spinner } from './ui';

/** An estimate of the height of one row, used only until the open card is measured. */
const ROW_ESTIMATE = 36;

/** How long a mouse rests on the rail before the card opens, in milliseconds. */
const HOVER_OPEN_MS = 120;

/** How long the card waits for the mouse to come back before it closes, in milliseconds. */
const HOVER_CLOSE_MS = 240;

/** Input on the scroller that comes from the reader. It removes the pin of a jump. */
const READER_INPUT = ['wheel', 'touchstart', 'pointerdown', 'keydown'] as const;

/** The list of the whole session: while the gateway reads it, its rows, or a failure. */
type Listing = 'reading' | 'failed' | readonly OutlineEntry[];

/** How far a finger moves on the rail before a tap becomes a scrub, in pixels. */
const SCRUB_SLOP = 8;

/** The word for each state of an answer in the preview, and its color. */
const STATUS_TEXT: Record<OutlineStatus, { label: string; tone: string }> = {
  done: { label: 'Done', tone: 'text-ok' },
  running: { label: 'Running', tone: 'text-accent-ink' },
  queued: { label: 'Queued', tone: 'text-warn' },
  waiting: { label: 'Waiting', tone: 'text-warn' },
  cancelled: { label: 'Cancelled', tone: 'text-err' },
  failed: { label: 'Failed', tone: 'text-err' },
};

/**
 * The mask of an answer that the preview cuts: the end of its last line fades out. Justice
 * sets each line in a box of its own, so the browser draws no ellipsis there.
 */
const CUT_ANSWER_FADE =
  '[mask-image:linear-gradient(black,black),linear-gradient(to_right,black_60%,transparent)] [mask-position:top,bottom] [mask-repeat:no-repeat] [mask-size:100%_calc(100%_-_1lh),100%_1lh]';

/**
 * The turn that the preview describes, its place in the session and the box that it
 * stands beside. A scrub preview follows a finger on the rail, any other a row.
 */
type Preview = {
  entry: OutlineEntry;
  index: number;
  anchor: PreviewAnchor;
  viewport: Viewport;
  scrub: boolean;
};

/** A finger on the rail: where it started, and the turn that it names now. */
type Scrub = {
  pointerId: number;
  startY: number;
  startId: string | null;
  active: boolean;
  step: number;
  id: string | null;
  index: number;
};

export function TranscriptOutline({
  entries,
  total,
  readAll,
  scroller,
  column,
  onJump,
  className = '',
}: {
  /** The turns that the screen holds, oldest first. They are the newest turns. */
  entries: readonly OutlineEntry[];
  /** The number of turns in the whole session, held or not. */
  total: number;
  /** Reads every turn of the session. Absent when `entries` holds all of them. */
  readAll?: (signal: AbortSignal) => Promise<OutlineEntry[]>;
  /** The transcript scroller. The outline reads the turn on screen from it. */
  scroller: RefObject<HTMLElement | null>;
  /** The transcript column. Its direct children carry `data-turn-id`. */
  column: RefObject<HTMLElement | null>;
  /** Puts a turn at the top of the transcript. `index` is its place in the session. */
  onJump: (id: string, index: number) => void;
  /** Position only. */
  className?: string;
}) {
  const [currentId, setCurrentId] = useState<string | null>(null);
  const [preview, setPreview] = useState<Preview | null>(null);
  // A jump PINS its turn. The end of the transcript can stop the scroller before the
  // turn gets to the top, and the outline must still name the turn that the reader
  // picked. The next input of the reader on the scroller removes the pin.
  const pinnedRef = useRef<string | null>(null);
  // A streamed answer changes no row, so only a new turn reads the screen again.
  const newestId = entries.at(-1)?.id ?? null;
  useEffect(() => {
    const viewport = scroller.current;
    if (!viewport) return;
    let frame: number | null = null;
    const read = () => {
      frame = null;
      setCurrentId(pinnedRef.current ?? readingTurnId(viewport, column.current));
    };
    const schedule = () => {
      if (frame === null) frame = window.requestAnimationFrame(read);
    };
    const release = () => {
      pinnedRef.current = null;
    };
    schedule();
    viewport.addEventListener('scroll', schedule, { passive: true });
    for (const type of READER_INPUT) viewport.addEventListener(type, release, { passive: true });
    return () => {
      viewport.removeEventListener('scroll', schedule);
      for (const type of READER_INPUT) viewport.removeEventListener(type, release);
      if (frame !== null) window.cancelAnimationFrame(frame);
    };
  }, [scroller, column, entries.length, newestId]);

  // The held turns are the newest end of the session, after `offset` older turns.
  const offset = Math.max(0, total - entries.length);
  const heldIndex = currentId === null ? -1 : entries.findIndex((entry) => entry.id === currentId);
  // A scrub marks the line of the turn that it names, also a turn that is not held.
  const rail = railLines(
    total,
    preview?.scrub ? preview.index : heldIndex < 0 ? -1 : offset + heldIndex,
  );

  const railRef = useRef<HTMLButtonElement>(null);
  const cardRef = useRef<HTMLDivElement>(null);
  const readRef = useRef<AbortController | null>(null);
  const timerRef = useRef<number | null>(null);
  // A press keeps the card open until the reader closes it. A resting mouse keeps it
  // open only while the mouse stays on the rail or on the card.
  const pressedRef = useRef(false);
  const focusRef = useRef(false);
  const centredRef = useRef(false);
  const [place, setPlace] = useState<OutlinePlace | null>(null);
  const [listing, setListing] = useState<Listing>('reading');
  const [resizes, setResizes] = useState(0);
  const isOpen = place !== null;
  const previewRef = useRef<HTMLDivElement>(null);
  const [previewHeight, setPreviewHeight] = useState(0);
  const answerRef = useRef<HTMLDivElement>(null);
  const [answerCut, setAnswerCut] = useState(false);
  const previewId = useId();
  const scrubRef = useRef<Scrub | null>(null);
  // Some browsers end a scrub with a click on the rail. That click is not a press.
  const scrubbedRef = useRef(false);
  // The rail of a narrow screen shows for a time after each scroll or tap.
  const [shown, setShown] = useState(false);
  const shownRef = useRef(false);
  const hideRef = useRef<number | null>(null);
  // A tap that starts on the hidden rail only shows it. The tap does not open the card.
  const hiddenTapRef = useRef(false);

  const cancelTimer = useCallback(() => {
    if (timerRef.current === null) return;
    window.clearTimeout(timerRef.current);
    timerRef.current = null;
  }, []);

  const reveal = useCallback(() => {
    shownRef.current = true;
    setShown(true);
    if (hideRef.current !== null) window.clearTimeout(hideRef.current);
    hideRef.current = window.setTimeout(() => {
      hideRef.current = null;
      shownRef.current = false;
      setShown(false);
    }, RAIL_SHOW_MS);
  }, []);

  // A closed card leaves the rail on screen for a time, also on a narrow screen.
  const close = useCallback(() => {
    cancelTimer();
    readRef.current?.abort();
    readRef.current = null;
    pressedRef.current = false;
    setPlace(null);
    setPreview(null);
    reveal();
  }, [cancelTimer, reveal]);

  useEffect(
    () => () => {
      cancelTimer();
      readRef.current?.abort();
      if (hideRef.current !== null) window.clearTimeout(hideRef.current);
    },
    [cancelTimer],
  );

  // The scroll of the reader shows the rail. Momentum scrolls on after the finger lifts,
  // and that scroll keeps a shown rail on screen.
  useEffect(() => {
    const viewport = scroller.current;
    if (!viewport) return;
    const onScroll = () => {
      if (shownRef.current) reveal();
    };
    viewport.addEventListener('touchmove', reveal, { passive: true });
    viewport.addEventListener('wheel', reveal, { passive: true });
    viewport.addEventListener('scroll', onScroll, { passive: true });
    return () => {
      viewport.removeEventListener('touchmove', reveal);
      viewport.removeEventListener('wheel', reveal);
      viewport.removeEventListener('scroll', onScroll);
    };
  }, [scroller, reveal]);

  const later = (delay: number, action: () => void) => {
    cancelTimer();
    timerRef.current = window.setTimeout(() => {
      timerRef.current = null;
      action();
    }, delay);
  };

  const placeFor = (height: number): OutlinePlace | null => {
    const box = railRef.current?.getBoundingClientRect();
    if (!box) return null;
    return outlinePanelPosition(box, OUTLINE_WIDTH, height, {
      width: window.innerWidth,
      height: window.innerHeight,
    });
  };

  // Rows from an earlier reading stay on screen while the gateway reads them again.
  const startRead = () => {
    if (!readAll) return;
    if (listing === 'failed') setListing('reading');
    readRef.current?.abort();
    const reading = new AbortController();
    readRef.current = reading;
    readAll(reading.signal).then(
      (rows) => {
        if (!reading.signal.aborted) setListing(rows);
      },
      () => {
        if (!reading.signal.aborted) setListing((was) => (was === 'reading' ? 'failed' : was));
      },
    );
  };

  const open = (focusList: boolean) => {
    centredRef.current = false;
    focusRef.current = focusList;
    setPlace(placeFor(ROW_ESTIMATE * ((readAll ? total : entries.length) + 1)));
    startRead();
  };

  // Focus that was in the card goes back to the rail, so the keyboard keeps its place.
  const returnFocus = () => {
    if (cardRef.current?.contains(document.activeElement)) {
      railRef.current?.focus({ preventScroll: true });
    }
  };

  const jump = (id: string, index: number) => {
    returnFocus();
    close();
    pinnedRef.current = id;
    setCurrentId(id);
    onJump(id, index);
  };

  const onRailClick = (event: ReactMouseEvent) => {
    cancelTimer();
    // A key presses with `detail` 0. Only a pointer can end a scrub with a click.
    if (scrubbedRef.current && event.detail !== 0) {
      scrubbedRef.current = false;
      return;
    }
    if (hiddenTapRef.current && event.detail !== 0) {
      hiddenTapRef.current = false;
      return;
    }
    if (isOpen && pressedRef.current) {
      close();
      return;
    }
    pressedRef.current = true;
    // A keyboard reader needs focus in the card.
    if (!isOpen) open(event.detail === 0);
  };

  const onRailEnter = (event: ReactPointerEvent) => {
    if (event.pointerType !== 'mouse') return;
    if (isOpen) cancelTimer();
    else later(HOVER_OPEN_MS, () => open(false));
  };

  const onCardEnter = (event: ReactPointerEvent) => {
    if (event.pointerType === 'mouse') cancelTimer();
  };

  const onLeave = (event: ReactPointerEvent) => {
    if (event.pointerType !== 'mouse') return;
    setPreview(null);
    if (!isOpen) cancelTimer();
    else if (!pressedRef.current) later(HOVER_CLOSE_MS, close);
  };

  // The arrow keys move between the rows, as in a list.
  const onCardKey = (event: ReactKeyboardEvent) => {
    if (event.key !== 'ArrowDown' && event.key !== 'ArrowUp') return;
    const rows = Array.from(cardRef.current?.querySelectorAll('button') ?? []);
    const at = rows.findIndex((row) => row === document.activeElement);
    const next = rows[at + (event.key === 'ArrowDown' ? 1 : -1)];
    if (!next) return;
    event.preventDefault();
    next.focus();
  };

  // The rows of the card, and the place of its first row in the whole session.
  const rows: readonly OutlineEntry[] | null = !readAll
    ? entries
    : listing === 'reading'
      ? null
      : listing === 'failed'
        ? entries
        : mergeOutline(listing, entries);
  const base = rows === entries ? offset : 0;
  // The number of turns that the preview counts: the whole session, once it is read.
  const count = rows && rows !== entries ? rows.length : total;

  // THE PREVIEW OF A ROW stands left of the card, level with the row.
  const showRow = (row: HTMLElement, entry: OutlineEntry, index: number) => {
    const card = cardRef.current?.getBoundingClientRect();
    if (!card) return;
    const box = row.getBoundingClientRect();
    setPreview({
      entry,
      index,
      anchor: { left: card.left, top: box.top, bottom: box.bottom },
      viewport: { width: window.innerWidth, height: window.innerHeight },
      scrub: false,
    });
  };

  // A row that scrolls in the card takes its preview with it.
  const onCardScroll = () => {
    if (!preview || preview.scrub) return;
    const row = Array.from(
      cardRef.current?.querySelectorAll<HTMLElement>('[data-outline-id]') ?? [],
    ).find((button) => button.dataset.outlineId === preview.entry.id);
    if (row) showRow(row, preview.entry, preview.index);
    else setPreview(null);
  };

  const onCardBlur = (event: ReactFocusEvent<HTMLElement>) => {
    const next = event.relatedTarget;
    if (next instanceof Node && event.currentTarget.contains(next)) return;
    setPreview((was) => (was?.scrub ? was : null));
  };

  // THE SCRUB. A finger that moves on the rail past `SCRUB_SLOP` hides the card and
  // walks through the turns, from the turn on screen. The preview follows the finger.
  // A held turn comes to the top of the transcript at once. A turn that is still on
  // the gateway waits for the release, because the jump pages it in.
  const onRailDown = (event: ReactPointerEvent) => {
    scrubbedRef.current = false;
    hiddenTapRef.current =
      !shownRef.current && !isOpen && window.matchMedia?.(NARROW_SCREEN).matches === true;
    reveal();
    if (event.pointerType === 'mouse' || !event.isPrimary) return;
    scrubRef.current = {
      pointerId: event.pointerId,
      startY: event.clientY,
      startId: currentId,
      active: false,
      step: 0,
      id: null,
      index: -1,
    };
  };

  const onRailMove = (event: ReactPointerEvent<HTMLElement>) => {
    const scrub = scrubRef.current;
    if (!scrub || scrub.pointerId !== event.pointerId) return;
    const travel = event.clientY - scrub.startY;
    if (!scrub.active) {
      if (Math.abs(travel) < SCRUB_SLOP) return;
      scrub.active = true;
      scrub.step = scrubStep((scroller.current?.clientHeight ?? window.innerHeight) / 2, total);
      // The card makes room for the preview. The scrub needs the list that it reads.
      cancelTimer();
      pressedRef.current = false;
      setPlace(null);
      if (readAll && !readRef.current) startRead();
    }
    const list = rows ?? entries;
    const first = rows === null || rows === entries ? offset : 0;
    const start = list.findIndex((entry) => entry.id === scrub.startId);
    const at = scrubIndex(start < 0 ? list.length - 1 : start, travel, scrub.step, list.length);
    const entry = list[at];
    if (!entry) return;
    const box = event.currentTarget.getBoundingClientRect();
    setPreview({
      entry,
      index: first + at,
      anchor: { left: box.left, top: event.clientY, bottom: event.clientY },
      viewport: { width: window.innerWidth, height: window.innerHeight },
      scrub: true,
    });
    if (entry.id === scrub.id) return;
    scrub.id = entry.id;
    scrub.index = first + at;
    if (!entries.some((held) => held.id === entry.id)) return;
    pinnedRef.current = entry.id;
    setCurrentId(entry.id);
    onJump(entry.id, first + at);
  };

  const onRailUp = (event: ReactPointerEvent) => {
    const scrub = scrubRef.current;
    if (!scrub || scrub.pointerId !== event.pointerId) return;
    scrubRef.current = null;
    if (!scrub.active) return;
    scrubbedRef.current = true;
    if (scrub.id) jump(scrub.id, scrub.index);
    else close();
  };

  const onRailCancel = (event: ReactPointerEvent) => {
    const scrub = scrubRef.current;
    if (!scrub || scrub.pointerId !== event.pointerId) return;
    scrubRef.current = null;
    if (scrub.active) close();
  };

  // ESCAPE BELONGS TO THE OPEN CARD. The screen under it reads Escape as "cancel the
  // running turn", so the key is caught before it gets to that listener. A press
  // outside the rail and the card closes the card.
  useEffect(() => {
    if (!isOpen) return;
    const onKey = (event: KeyboardEvent) => {
      if (event.key !== 'Escape') return;
      event.stopPropagation();
      if (cardRef.current?.contains(document.activeElement)) {
        railRef.current?.focus({ preventScroll: true });
      }
      close();
    };
    const onPress = (event: PointerEvent) => {
      const target = event.target;
      if (!(target instanceof Node)) return;
      if (railRef.current?.contains(target) || cardRef.current?.contains(target)) return;
      close();
    };
    const onResize = () => setResizes((count) => count + 1);
    window.addEventListener('keydown', onKey, true);
    window.addEventListener('pointerdown', onPress, true);
    window.addEventListener('resize', onResize);
    return () => {
      window.removeEventListener('keydown', onKey, true);
      window.removeEventListener('pointerdown', onPress, true);
      window.removeEventListener('resize', onResize);
    };
  }, [isOpen, close]);
  useBackLayer(isOpen ? close : null);

  // The card is placed from an estimate first, because it has no height before it
  // paints. Before the browser paints, it is measured and placed again, and the
  // current row moves to the middle of the card, once for each opening.
  const rowCount = rows?.length ?? -1;
  useLayoutEffect(() => {
    const card = cardRef.current;
    if (!isOpen || !card) return;
    const next = placeFor(card.scrollHeight + card.offsetHeight - card.clientHeight);
    if (next) {
      setPlace((was) =>
        was &&
        was.left === next.left &&
        was.top === next.top &&
        was.width === next.width &&
        was.maxHeight === next.maxHeight
          ? was
          : next,
      );
    }
    const current = card.querySelector<HTMLElement>('[aria-current="true"]');
    if (!centredRef.current && current) {
      centredRef.current = true;
      const shift = current.getBoundingClientRect().top - card.getBoundingClientRect().top;
      card.scrollTop += shift - (card.clientHeight - current.offsetHeight) / 2;
    }
    if (focusRef.current && rowCount >= 0) {
      focusRef.current = false;
      (current ?? card.querySelector<HTMLElement>('button'))?.focus({ preventScroll: true });
    }
    // `placeFor` reads only refs and the window, so it is not a dependency.
  }, [isOpen, rowCount, resizes]);

  // The preview is measured before the browser paints it, and then placed again. Its
  // justified lines can change its height after that, so a resize measures it again.
  // Justice can also break the answer into more lines at the same height, so a change
  // of its lines checks again if the preview cuts the answer.
  useLayoutEffect(() => {
    const element = previewRef.current;
    if (!element) return;
    const answer = answerRef.current?.firstElementChild;
    const measure = () => {
      if (element.offsetHeight) setPreviewHeight(element.offsetHeight);
      setAnswerCut(answer instanceof HTMLElement && answer.scrollHeight - answer.clientHeight > 1);
    };
    measure();
    const observers: { disconnect: () => void }[] = [];
    if (typeof ResizeObserver === 'function') {
      const resizes = new ResizeObserver(measure);
      resizes.observe(element);
      observers.push(resizes);
    }
    if (answer && typeof MutationObserver === 'function') {
      const lines = new MutationObserver(measure);
      lines.observe(answer, { childList: true, subtree: true, characterData: true });
      observers.push(lines);
    }
    return () => observers.forEach((observer) => observer.disconnect());
  }, [preview]);
  const previewPlace = preview && previewPosition(preview.anchor, previewHeight, preview.viewport);
  const previewStatus = preview?.entry.status ? STATUS_TEXT[preview.entry.status] : null;

  return (
    <>
      <button
        ref={railRef}
        type="button"
        aria-label="Jump to a message"
        aria-haspopup="dialog"
        aria-expanded={isOpen}
        className={`flex w-8 touch-none select-none flex-col items-center gap-0.75 py-3 transition-opacity duration-200 [-webkit-touch-callout:none] focus-visible:bg-hover focus-visible:opacity-100 focus-visible:outline-none motion-reduce:transition-none ${
          shown || isOpen || preview ? '' : 'max-sm:opacity-0'
        } ${className}`}
        onClick={onRailClick}
        onPointerDown={onRailDown}
        onPointerMove={onRailMove}
        onPointerUp={onRailUp}
        onPointerCancel={onRailCancel}
        onPointerEnter={onRailEnter}
        onPointerLeave={onLeave}
      >
        {Array.from({ length: rail.count }, (_, line) => (
          <span
            key={line}
            aria-hidden
            data-active={line === rail.active ? '' : undefined}
            className={`block h-0.5 bg-current transition-[color,width] duration-150 motion-reduce:transition-none ${
              line === rail.active ? 'w-5 text-dialog-foreground' : 'w-4 text-dialog-hint/50'
            }`}
          />
        ))}
      </button>
      {place &&
        createPortal(
          <div
            ref={cardRef}
            role="dialog"
            aria-label="Jump to a message"
            className="fixed z-50 overflow-y-auto overscroll-contain border border-dialog-edge bg-panel py-1 shadow-float transition-opacity duration-150 starting:opacity-0 motion-reduce:transition-none"
            style={{
              left: place.left,
              top: place.top,
              width: place.width,
              maxHeight: place.maxHeight,
            }}
            onPointerEnter={onCardEnter}
            onPointerLeave={onLeave}
            onKeyDown={onCardKey}
            onScroll={onCardScroll}
            onBlur={onCardBlur}
          >
            {rows === null ? (
              <p className="flex items-center gap-2 px-3 py-2 text-body text-dialog-hint">
                <Spinner tone="accent" />
                Reading the conversation…
              </p>
            ) : (
              <>
                {listing === 'failed' && readAll && (
                  <p className="px-3 py-2 text-body text-dialog-hint">
                    Earlier turns could not be read. Only loaded turns are listed.
                  </p>
                )}
                {rows.map((entry, index) => {
                  const isCurrent = entry.id === currentId;
                  return (
                    <button
                      key={entry.id}
                      type="button"
                      data-outline-id={entry.id}
                      aria-current={isCurrent ? 'true' : undefined}
                      aria-describedby={
                        preview?.entry.id === entry.id && !preview.scrub ? previewId : undefined
                      }
                      className={`flex w-full items-baseline gap-2 px-3 py-2.5 text-left text-title transition-colors duration-150 focus-visible:bg-hover focus-visible:text-dialog-foreground focus-visible:outline-none motion-reduce:transition-none mouse:py-1.5 ${
                        isCurrent
                          ? 'font-bold text-dialog-foreground'
                          : 'text-dialog-hint mouse:hover:text-dialog-foreground'
                      }`}
                      onClick={() => jump(entry.id, base + index)}
                      onPointerEnter={(event) => {
                        if (event.pointerType === 'mouse') {
                          showRow(event.currentTarget, entry, base + index);
                        }
                      }}
                      onFocus={(event) => {
                        // A press also focuses the row, but only a key shows its preview.
                        if (event.currentTarget.matches(':focus-visible')) {
                          showRow(event.currentTarget, entry, base + index);
                        }
                      }}
                    >
                      <span className="min-w-0 flex-1 truncate">{entry.label}</span>
                      {entry.isCouncil && (
                        <span className="shrink-0 text-meta font-normal uppercase tracking-[0.08em] text-dialog-hint">
                          council
                        </span>
                      )}
                    </button>
                  );
                })}
              </>
            )}
          </div>,
          document.body,
        )}
      {preview &&
        previewPlace &&
        createPortal(
          <div
            ref={previewRef}
            id={previewId}
            role="tooltip"
            className="pointer-events-none fixed z-50 border border-dialog-edge bg-panel px-3 py-2.5 shadow-float transition-opacity duration-150 starting:opacity-0 motion-reduce:transition-none"
            style={{ left: previewPlace.left, top: previewPlace.top, width: previewPlace.width }}
          >
            <div className="flex items-baseline justify-between gap-3 text-meta uppercase tracking-[0.08em]">
              <p className="min-w-0 text-dialog-hint">
                Message {preview.index + 1} of {count}
                {preview.entry.isCouncil && ' · council'}
              </p>
              {previewStatus && <p className={`shrink-0 font-bold ${previewStatus.tone}`}>{previewStatus.label}</p>}
            </div>
            <p className="mt-1 line-clamp-3 text-body font-bold text-dialog-foreground">{preview.entry.label}</p>
            {preview.entry.answer && (
              <div ref={answerRef} className="mt-2">
                <JustifiedProse
                  className={`line-clamp-6 text-body text-dialog-hint${answerCut ? ` ${CUT_ANSWER_FADE}` : ''}`}
                >
                  {preview.entry.answer}
                </JustifiedProse>
              </div>
            )}
          </div>,
          document.body,
        )}
    </>
  );
}

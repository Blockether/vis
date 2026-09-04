import { useCallback, useEffect, useRef, useState, type ReactNode } from 'react';
import { LIST_EDGE_END } from './SessionNavigator';

export interface SwipeAction {
  key: string;
  /** The caption ON the cell, and the shortest true word for it: `Star`, `Rename`, `Forget`. */
  label: string;
  /**
   * The whole sentence, for a reader who cannot see the row the strip belongs
   * to: a machine row's `Primary` is `Make tower primary`. The caption stays
   * one word wide, because the cell is 72px and a wrapped caption is a smear.
   */
  name?: string;
  icon: ReactNode;
  /**
   * What the action MEANS, in colour. The SLAB carries the meaning and the INK
   * carries the caption: `accent` is the amber a star wears — a yellow-tinted
   * cell around a brand-yellow glyph, so "Star" can never read as one more
   * neutral verb beside "Rename" — and `danger` is the red one.
   *
   * Neither slab lends its own colour to the 9px caption on it. `--accent`
   * (#ffc420) and `--err` (#dc2626) are FILLS: as text on their own 15% tint
   * they measure 1.37:1 and 3.50:1, under the 4.5:1 a caption owes, and the
   * amber one arrived as a smear rather than a word. The palette splits each
   * one for exactly this — `accent-ink` reads 6.4:1 and `err-ink` 5.4:1 on the
   * same cells — and the full-strength fill is spent on the hover state, where
   * it becomes the background and takes its own foreground.
   */
  tone?: 'neutral' | 'accent' | 'danger';
  /**
   * Run the verb. The cell that was pressed comes with it, so a verb that opens a
   * menu can hang it under the thing the thumb actually touched — the row's own
   * strip is gone by the time the menu paints.
   */
  onSelect: (anchor: HTMLElement) => void;
}

/**
 * ONE ROW IS OPEN AT A TIME, in the whole app.
 *
 * Two drawers standing open in one list is a list with two right edges: the eye
 * reads the second one as the shape of the list rather than as a row waiting for
 * an answer, and a red `Delete` stays armed under a thumb that has already moved
 * on. Opening a row therefore closes whichever row was open — the same rule a
 * radio group has, kept at module scope because these rows are siblings in a
 * virtualised list and never share a provider.
 */
let openDrawer: (() => void) | null = null;

/**
 * How far the track has to have travelled before the drawer counts as OPEN —
 * clear of the rubber band and of a stray pixel left by a snap, and the SAME
 * number in every reader of that offset: two thresholds would disagree at 4px.
 */
const OPEN_PAST_PX = 8;

/**
 * Shared row-action drawer. Touch uses a two-panel scroll-snap track; fine pointers place
 * an opaque action strip over the trailing edge on hover/focus. Hidden actions cannot
 * intercept input, and hybrid devices retain the touch behavior.
 *
 * ONE CELL, TWO FACES. On touch a cell is the drawer's 72px captioned slab — an icon
 * over a word, tinted by what the verb means, because a thumb slid it open and reads
 * the caption. Under a pointer the same cell is `IconButton`'s 28px disc: the glyph
 * alone, on the row's own hover tint, the row's trailing gutter between the last disc
 * and the paper's edge. The desktop face is written HERE and not composed from
 * `IconButton` because the touch face has no `ui.tsx` control to stand in for it, and
 * two sets of buttons under one name are two rows of verbs to a screen reader and to
 * every test that asks for `Rename` by name. Reported (paraphrased: the star, rename,
 * fork and delete look ridiculous and stand too close to the margins): four 34px slabs
 * on a 32px row, flush against the paper's right edge, over a date they hid.
 */
export function SwipeActions({
  actions,
  children,
  label,
}: {
  actions: SwipeAction[];
  children: ReactNode;
  label?: string;
}) {
  const scrollerRef = useRef<HTMLDivElement>(null);
  const [open, setOpen] = useState(false);
  /** True while this drawer is settling home; see `close`. */
  const isClosing = useRef(false);

  // CLOSED IS A FACT, NEVER AN ANIMATION. `open` flips the moment a close is
  // asked for, so a close that only REQUESTS the slide home leaves the state
  // saying shut while the strip is still standing on the screen whenever the
  // platform declines to run it: the row navigates instead of dismissing, the
  // next row opens beside it rather than in place of it, and the mark the verb
  // just left is scrolled off to the left, behind the strip that left it.
  //
  // Regression, user report ("when I click the star on some other row, first I
  // don't see the star ... only after I do slide once again", with the cell
  // painted over its own old caption): an ANIMATED `scrollTo` inside a mandatory
  // scroll-snap track is exactly what WebKit is free to swallow — measured there
  // at 216px eight hundred milliseconds after `behavior: 'smooth'` was asked for,
  // against home in the same frame for `behavior: 'auto'`, same track, same call
  // — and starring fires a second animated scroll (`ProjectGroup`'s pin) at this
  // same scroller in the same commit. The slide OPEN is still the platform's,
  // finger and momentum and all; only the way home is taken out of its hands.
  const close = useCallback(() => {
    isClosing.current = true;
    setOpen(false);
    scrollerRef.current?.scrollTo({ left: 0, behavior: 'auto' });
  }, []);

  // OPEN IS WHAT THE TRACK SAYS, RE-READ — not what a scroll event last said.
  //
  // Regression, user report (paraphrased: from the fifth row down the star did
  // not arrive on the first tap, only after sliding the row a second time, and
  // the row's state disagreed with what was on the screen): the rows above it
  // are LIVE, every poll re-sorts the list, and a re-sort MOVES this row's node
  // to its new place. WebKit returns a moved scroller home in the same task and
  // fires NO scroll event for it — measured on iOS 26.5, Safari: 216 -> 0
  // synchronously, zero scroll events — so the strip left the screen while
  // `open` went on saying it was standing there, and the row, which is a
  // dismiss target while open, ate the tap that was meant for the star. Only
  // the second slide, which does fire scroll events, put the two back in step.
  //
  // The offset is the platform's to change, with or without an event, so the
  // state RE-READS it every frame it claims to be open.
  useEffect(() => {
    if (!open) return;
    let frame = 0;
    const reread = () => {
      frame = requestAnimationFrame(reread);
      if ((scrollerRef.current?.scrollLeft ?? 0) <= OPEN_PAST_PX) setOpen(false);
    };
    frame = requestAnimationFrame(reread);
    return () => cancelAnimationFrame(frame);
  }, [open]);

  // An open drawer is a modal-ish state: Escape closes it, scrolling the list
  // away from it closes it, and opening another row closes it — otherwise a
  // forgotten row keeps a delete button armed under the user's thumb.
  useEffect(() => {
    if (!open) return;
    openDrawer?.();
    openDrawer = close;
    const onKey = (event: KeyboardEvent) => {
      if (event.key === 'Escape') close();
    };
    const onScroll = (event: Event) => {
      // The LIST moving under an open row closes it; another ROW'S drawer moving
      // does not. Closing row A animates its own scrollLeft back to 0, and every
      // frame of that was reaching row B as "something else scrolled", so the row
      // the thumb had just opened shut itself before the finger left the glass.
      const target = event.target;
      if (target === scrollerRef.current) return;
      if (target instanceof Element && target.hasAttribute('data-swipe-track')) return;
      close();
    };
    window.addEventListener('keydown', onKey);
    window.addEventListener('scroll', onScroll, true);
    return () => {
      if (openDrawer === close) openDrawer = null;
      window.removeEventListener('keydown', onKey);
      window.removeEventListener('scroll', onScroll, true);
    };
  }, [open, close]);

  if (actions.length === 0) return <>{children}</>;

  return (
    <div
      ref={scrollerRef}
      data-swipe-track=""
      // A finger on the track cancels the browser's own animation, so it cancels
      // this flag too: a drag that catches a closing drawer mid-slide is a new
      // gesture, not the tail of the old one.
      onPointerDown={() => {
        isClosing.current = false;
      }}
      onScroll={(event) => {
        const next = event.currentTarget.scrollLeft > OPEN_PAST_PX;
        // A drawer animating home still reports itself OPEN for every frame of the
        // slide. Taken at face value those frames re-opened the row that was
        // closing — and re-opening it closed the row that had just replaced it, so
        // two rows slid back to 0 together and the thumb ended up with nothing.
        if (isClosing.current) {
          if (next) return;
          isClosing.current = false;
        }
        setOpen((current) => (current === next ? current : next));
      }}
      // Under a pointer the TRACK is the row's hover slab: the strip and the row are two
      // panels of one track, and a hover that belongs to only one of them leaves a seam
      // — the row went back to plain paper the moment the cursor reached its own verbs.
      className="group/swipe flex snap-x snap-mandatory overflow-x-auto overflow-y-hidden overscroll-x-contain [-ms-overflow-style:none] [scrollbar-width:none] [&::-webkit-scrollbar]:hidden mouse:relative mouse:snap-none mouse:overflow-hidden mouse:transition-colors mouse:duration-150 mouse:hover:bg-hover mouse:motion-reduce:transition-none"
    >
      {/* A GRID, not a plain block: the track is as tall as its TALLEST panel, and the
          action strip (a 16px icon over a 10px caption) stands 34px against a 32px
          desktop session row. This panel stretches to the track, so its single child
          has to stretch to IT — otherwise the row's hover slab stops short of the rule
          under it and the row reads as if it had lost two pixels of its own height.

          The one column is `minmax(0, 1fr)`, never `auto`: a grid item's automatic
          minimum is its min-content width, and a header whose count, live pulse and
          path are all `nowrap` has a min-content width far past any phone. Measured
          on the 393px rail, a project band of `~/rewrite · 113 sessions · 1 live · 1
          needs input` grew its track to 528px: nothing truncated, and the pager and
          the `+` stood 135px past the edge of the list, under the swipe strip. */}
      <div
        className="grid w-full shrink-0 grid-cols-[minmax(0,1fr)] snap-start bg-panel mouse:bg-transparent"
        onClickCapture={(event) => {
          // While the drawer is open the row itself is a dismiss target, never a
          // navigation: a thumb resting on it must not open the session.
          if (!open) return;
          event.preventDefault();
          event.stopPropagation();
          close();
        }}
      >
        {children}
      </div>
      <div
        className={`flex shrink-0 snap-end mouse:pointer-events-none mouse:absolute mouse:inset-y-0 mouse:right-0 mouse:items-center mouse:gap-1 mouse:pl-6 mouse:bg-hover mouse:opacity-0 mouse:transition-opacity mouse:duration-150 mouse:group-hover/swipe:pointer-events-auto mouse:group-hover/swipe:opacity-100 mouse:group-focus-within/swipe:pointer-events-auto mouse:group-focus-within/swipe:opacity-100 mouse:motion-reduce:transition-none ${LIST_EDGE_END}`}
        role="group"
        aria-label={label ? `${label} actions` : 'Row actions'}
      >
        {actions.map((action) => (
          <button
            key={action.key}
            type="button"
            aria-label={action.name ?? action.label}
            title={action.name ?? action.label}
            className={`flex w-[4.5rem] shrink-0 flex-col items-center justify-center gap-1 border-l font-mono text-chip font-bold uppercase tracking-[0.08em] transition-colors duration-150 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-accent/60 motion-reduce:transition-none mouse:size-7 mouse:rounded-full mouse:border-l-0 mouse:bg-transparent mouse:text-white ${
              action.tone === 'danger'
                ? 'border-err-edge bg-err-surface text-err-ink hover:bg-err hover:text-white mouse:hover:bg-err-surface mouse:hover:text-err'
                : action.tone === 'accent'
                  ? 'border-accent/40 bg-accent/15 text-accent-ink hover:bg-accent hover:text-accent-foreground mouse:hover:bg-panel'
                  : 'border-dialog-edge bg-panel-2 text-accent-ink hover:bg-hover mouse:hover:bg-panel'
            }`}
            onClick={(event) => {
              const anchor = event.currentTarget;
              close();
              action.onSelect(anchor);
            }}
          >
            <span aria-hidden="true">{action.icon}</span>
            <span className="mouse:sr-only">{action.label}</span>
          </button>
        ))}
      </div>
    </div>
  );
}

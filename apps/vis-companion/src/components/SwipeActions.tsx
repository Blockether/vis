import { useCallback, useEffect, useLayoutEffect, useRef, useState, type ReactNode } from 'react';
import { LIST_EDGE_END } from './SessionNavigator';
import { DotsIcon } from './icons';
import { IconButton } from './ui';
import { Menu, MenuHeading, MenuItem, MENU_WIDTH } from './Menu';
import { menuPosition, type MenuPosition } from '../lib/anchored-menu';

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

/** A pointer uses one persistent trigger; touch keeps the swipe drawer below. */
function RowActionMenu({ actions, label }: { actions: SwipeAction[]; label?: string }) {
  const triggerRef = useRef<HTMLButtonElement>(null);
  const panelRef = useRef<HTMLDivElement>(null);
  const [at, setAt] = useState<MenuPosition | null>(null);
  const dismiss = useCallback(() => {
    setAt(null);
    triggerRef.current?.focus({ preventScroll: true });
  }, []);

  useLayoutEffect(() => {
    if (!at) return;
    const panel = panelRef.current?.closest('[role="dialog"]');
    const anchor = triggerRef.current?.getBoundingClientRect();
    const height = panel?.getBoundingClientRect().height;
    if (!height) return;
    const placed = menuPosition(anchor, MENU_WIDTH, undefined, height);
    if (placed && (placed.top !== at.top || placed.left !== at.left)) setAt(placed);
  }, [at]);

  useEffect(() => {
    if (!at) return;
    // Skip the heading's close control and focus the first action.
    panelRef.current?.querySelectorAll('button')[1]?.focus({ preventScroll: true });
    const onScroll = (event: Event) => {
      if (
        event.target instanceof Node &&
        panelRef.current?.closest('[role="dialog"]')?.contains(event.target)
      )
        return;
      dismiss();
    };
    window.addEventListener('resize', dismiss);
    window.addEventListener('scroll', onScroll, true);
    return () => {
      window.removeEventListener('resize', dismiss);
      window.removeEventListener('scroll', onScroll, true);
    };
  }, [at, dismiss]);

  return (
    <>
      <IconButton
        ref={triggerRef}
        label={label ? `Actions for ${label}` : 'Row actions'}
        variant="quiet"
        aria-haspopup="dialog"
        aria-expanded={at !== null}
        onClick={(event) =>
          setAt(menuPosition(event.currentTarget.getBoundingClientRect(), MENU_WIDTH))
        }
      >
        <DotsIcon className="size-3.5 rotate-90" />
      </IconButton>
      {at && (
        <Menu label={label ? `${label} actions` : 'Row actions'} at={at} onDismiss={dismiss}>
          <div
            ref={panelRef}
            onKeyDown={(event) => {
              if (event.key === 'Escape') {
                event.preventDefault();
                event.stopPropagation();
                dismiss();
              } else if (event.key === 'Tab') {
                const buttons = event.currentTarget.querySelectorAll('button');
                const first = buttons[0];
                const last = buttons[buttons.length - 1];
                if (event.shiftKey && document.activeElement === first) {
                  event.preventDefault();
                  last?.focus();
                } else if (!event.shiftKey && document.activeElement === last) {
                  event.preventDefault();
                  first?.focus();
                }
              }
            }}
          >
            <MenuHeading onClose={dismiss} closeLabel="Close row actions">
              {label ?? 'Row actions'}
            </MenuHeading>
            {actions.map((action) => (
              <MenuItem
                key={action.key}
                title={action.name ?? action.label}
                icon={action.icon}
                tone={action.tone === 'danger' ? 'danger' : 'default'}
                onSelect={() => {
                  const anchor = triggerRef.current;
                  dismiss();
                  if (anchor) action.onSelect(anchor);
                }}
              />
            ))}
          </div>
        </Menu>
      )}
    </>
  );
}

/**
 * Shared row actions: a scroll-snap drawer on touch and a vertical-dot dropdown
 * under a pointer. Only the trigger reserves desktop width, regardless of action
 * count. Permanent row controls stay mounted once, after the action trigger.
 */
export function SwipeActions({
  actions,
  children,
  label,
  trailing,
}: {
  actions: SwipeAction[];
  children: ReactNode;
  label?: string;
  /** Permanent row controls: inside the touch panel, after the desktop menu trigger. */
  trailing?: ReactNode;
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

  if (actions.length === 0) {
    if (!trailing) return <>{children}</>;
    return (
      <div className="grid grid-cols-[minmax(0,1fr)_auto]">
        {children}
        {trailing}
      </div>
    );
  }

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
      className="group/swipe flex snap-x snap-mandatory overflow-x-auto overflow-y-hidden overscroll-x-contain [-ms-overflow-style:none] [scrollbar-width:none] [&::-webkit-scrollbar]:hidden mouse:snap-none mouse:overflow-hidden mouse:transition-colors mouse:duration-150 mouse:hover:bg-hover mouse:motion-reduce:transition-none"
    >
      {/* Touch keeps content and permanent controls in one full-width snap panel.
          Desktop reserves just one menu trigger before the permanent trailing edge. */}
      <div
        className="grid w-full shrink-0 grid-cols-[minmax(0,1fr)_auto] snap-start bg-panel mouse:contents"
        onClickCapture={(event) => {
          // While the drawer is open the row itself is a dismiss target, never a
          // navigation: a thumb resting on it must not open the session.
          if (!open) return;
          event.preventDefault();
          event.stopPropagation();
          close();
        }}
      >
        <div className="grid min-w-0 grid-cols-[minmax(0,1fr)] mouse:flex-1">{children}</div>
        {trailing && <div className="flex shrink-0 mouse:order-last">{trailing}</div>}
      </div>
      <div
        className={`hidden shrink-0 items-center mouse:flex ${trailing ? 'pr-2' : LIST_EDGE_END}`}
      >
        <RowActionMenu actions={actions} label={label} />
      </div>
      <div
        className={`flex shrink-0 snap-end mouse:hidden ${LIST_EDGE_END}`}
        role="group"
        aria-label={label ? `${label} actions` : 'Row actions'}
      >
        {actions.map((action) => (
          <button
            key={action.key}
            type="button"
            aria-label={action.name ?? action.label}
            title={action.name ?? action.label}
            className={`flex w-[4.5rem] shrink-0 flex-col items-center justify-center gap-1 border-l font-mono text-chip font-bold uppercase tracking-[0.08em] transition-colors duration-150 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-accent/60 motion-reduce:transition-none ${
              action.tone === 'danger'
                ? 'border-err-edge bg-err-surface text-err-ink hover:bg-err hover:text-white'
                : action.tone === 'accent'
                  ? 'border-accent/40 bg-accent/15 text-accent-ink hover:bg-accent hover:text-accent-foreground'
                  : 'border-dialog-edge bg-panel-2 text-accent-ink hover:bg-hover'
            }`}
            onClick={(event) => {
              const anchor = event.currentTarget;
              close();
              action.onSelect(anchor);
            }}
          >
            <span aria-hidden="true">{action.icon}</span>
            <span>{action.label}</span>
          </button>
        ))}
      </div>
    </div>
  );
}

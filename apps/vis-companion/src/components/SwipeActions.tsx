import {
  useCallback,
  useEffect,
  useImperativeHandle,
  useLayoutEffect,
  useRef,
  useState,
  type ReactNode,
  type RefObject,
} from 'react';
import { LIST_EDGE_END } from './SessionNavigator';
import { DotsIcon } from './icons';
import { IconButton } from './ui';
import { Menu, MenuItem, MENU_WIDTH } from './Menu';
import {
  menuPosition,
  pointerAnchor,
  type AnchorBox,
  type MenuPosition,
} from '../lib/anchored-menu';
import { hasHardwarePointer } from '../lib/pointer';

/**
 * A ROW'S PRESS BELONGS TO THE WHOLE ROW.
 *
 * The pressable half is one cell beside the permanent trailing controls and the
 * desktop menu trigger, so a press painted on that button alone stopped at the
 * chevron: the row read as half-selected, its own trailing cell left standing on
 * unpressed paper. The row paints it here instead — the track carries the desktop
 * row, whose panel is `contents`, and the snap panel carries the touch one — so
 * every cell of the row wears the same paper. A pressable row marks itself with
 * `data-row-surface`; a row with no such control simply never lights up.
 */
const ROW_PRESS_PAPER =
  'has-[[data-row-surface]:active]:bg-hover has-[[data-row-surface]:focus-visible]:bg-hover';

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
   * same cells. Hover leaves these semantic surfaces and their paired ink intact.
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

/** The row's menu asked for from somewhere other than its own trigger. */
export interface RowMenuHandle {
  /** Drop the menu at a point in the viewport — the cursor that asked for it. */
  openAt: (point: { x: number; y: number }) => void;
}

/** A pointer uses one persistent trigger; touch keeps the swipe drawer below. */
function RowActionMenu({
  actions,
  label,
  handle,
}: {
  actions: SwipeAction[];
  label?: string;
  handle: RefObject<RowMenuHandle | null>;
}) {
  const triggerRef = useRef<HTMLButtonElement>(null);
  const panelRef = useRef<HTMLDivElement>(null);
  const [at, setAt] = useState<MenuPosition | null>(null);
  // WHAT THE PANEL HANGS FROM, remembered. The re-place below asks the same question
  // again once the panel has a measured height, and a menu the cursor opened must not
  // walk back under the `⋯` it merely shares its actions with.
  const anchorRef = useRef<AnchorBox | null>(null);
  const openFrom = useCallback((anchor: AnchorBox) => {
    anchorRef.current = anchor;
    setAt(menuPosition(anchor, MENU_WIDTH));
  }, []);
  useImperativeHandle(
    handle,
    () => ({
      openAt: (point) => openFrom(pointerAnchor(point, MENU_WIDTH)),
    }),
    [openFrom],
  );
  const dismiss = useCallback(() => {
    anchorRef.current = null;
    setAt(null);
    triggerRef.current?.focus({ preventScroll: true });
  }, []);

  useLayoutEffect(() => {
    if (!at) return;
    const panel = panelRef.current?.closest('[role="dialog"]');
    const anchor = anchorRef.current ?? triggerRef.current?.getBoundingClientRect();
    const height = panel?.getBoundingClientRect().height;
    if (!height) return;
    const placed = menuPosition(anchor, MENU_WIDTH, undefined, height);
    if (!placed) return;
    if (placed.top !== at.top || placed.bottom !== at.bottom || placed.left !== at.left)
      setAt(placed);
  }, [at]);

  useEffect(() => {
    if (!at) return;
    panelRef.current?.querySelector('button')?.focus({ preventScroll: true });
    // ONLY A SCROLLER THAT CARRIES THE ROW CAN STRAND THIS PANEL. The listener
    // sits on the window in capture, so it hears every scroll in the document —
    // and a transcript following its end rewrites its own `scrollTop` on each
    // update it receives. Read as "the page moved", that shut the menu a reader
    // had open in a settings dialog standing over it, on every turn of a live
    // session. The panel is portaled to the document and never carries the
    // trigger either, so its own scrolling stays out of this by the same rule.
    const onScroll = (event: Event) => {
      const trigger = triggerRef.current;
      const scroller = event.target;
      if (trigger && scroller instanceof Node && scroller.contains(trigger)) dismiss();
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
        onClick={(event) => openFrom(event.currentTarget.getBoundingClientRect())}
      >
        <DotsIcon className="size-3.5" />
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
  /** Permanent row controls: inside the touch panel, before the desktop menu trigger. */
  trailing?: ReactNode;
}) {
  const scrollerRef = useRef<HTMLDivElement>(null);
  const menuRef = useRef<RowMenuHandle | null>(null);
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
      // The LIST moving under an open row closes it; a scroller that does not
      // carry the row does not. Closing row A animates its own scrollLeft back to
      // 0, and every frame of that was reaching row B as "something else
      // scrolled", so the row the thumb had just opened shut itself before the
      // finger left the glass — and a live transcript, which rewrites its own
      // `scrollTop` on every update the session receives, reached every open row
      // on another screen the very same way.
      const track = scrollerRef.current;
      const scroller = event.target;
      if (!track || scroller === track) return;
      if (scroller instanceof Node && scroller.contains(track)) close();
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
      <div className={`grid grid-cols-[minmax(0,1fr)_auto] ${ROW_PRESS_PAPER}`}>
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
      // THE ROW'S OWN MENU, WHERE THE SYSTEM ONE WOULD HAVE BEEN. Under a pointer every
      // verb this row has already lives behind its `⋯`; a right-click is the second way
      // to ask for that same menu, and it opens at the cursor instead of back under the
      // trigger. A finger never reaches here: the drawer below is the row's answer to a
      // swipe, and a long press stays the platform's.
      onContextMenu={(event) => {
        if (!hasHardwarePointer()) return;
        event.preventDefault();
        menuRef.current?.openAt({ x: event.clientX, y: event.clientY });
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
      className={`group/swipe flex snap-x snap-mandatory overflow-x-auto overflow-y-hidden overscroll-x-contain [-ms-overflow-style:none] [scrollbar-width:none] [&::-webkit-scrollbar]:hidden mouse:snap-none mouse:overflow-hidden ${ROW_PRESS_PAPER}`}
    >
      {/* Touch keeps content and permanent controls in one full-width snap panel.
          Desktop ends the row with its menu trigger and stands the permanent controls
          one slot inside it, the two cells every header in the list uses. */}
      <div
        className={`grid w-full shrink-0 grid-cols-[minmax(0,1fr)_auto] snap-start mouse:contents bg-panel ${ROW_PRESS_PAPER}`}
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
        {trailing && <div className="flex shrink-0">{trailing}</div>}
      </div>
      <div
        className={`hidden shrink-0 items-center mouse:flex ${trailing ? 'pr-2 mouse:pr-2.5' : LIST_EDGE_END}`}
      >
        <RowActionMenu actions={actions} label={label} handle={menuRef} />
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
                ? 'border-err-edge bg-err-surface text-err-ink'
                : action.tone === 'accent'
                  ? 'border-accent/40 bg-accent/15 text-accent-ink'
                  : 'border-dialog-edge bg-panel-2 text-accent-ink'
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

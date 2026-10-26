import {
  useCallback,
  useEffect,
  useImperativeHandle,
  useLayoutEffect,
  useRef,
  useState,
  type PointerEvent,
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
 *
 * `data-pressed` is what the press itself says: the row marks it on pointer down and
 * clears it on release, so the paper never waits on `:active` reaching the row and a
 * test can hold one press open. `:active` stays for the keyboard's own press.
 */
const ROW_PRESS_PAPER =
  'has-[[data-row-surface][data-pressed]]:bg-hover has-[[data-row-surface]:active]:bg-hover has-[[data-row-surface]:focus-visible]:bg-hover';

export interface SwipeAction {
  key: string;
  /** The caption ON the cell, and the shortest true word for it: `Star`, `Rename`, `Forget`. */
  label: string;
  /**
   * The full action name for assistive technology and tooltips.
   * Cells share the available row width, up to 72px each. Keep captions short.
   */
  name?: string;
  icon: ReactNode;
  /**
   * What the action MEANS, in colour. The SLAB carries the meaning and the INK
   * carries the caption: `accent` is the amber a star wears — a yellow-tinted
   * cell around a brand-yellow glyph, so "Star" can never read as one more
   * neutral verb beside "Rename" — and `danger` is the red one. A `neutral` verb
   * means nothing in colour, so it takes the row's own hover paper and the
   * theme's ink, and ONE rule — the list's `edge-strong` — divides every cell.
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

/** How far a finger moves before the row takes it as a sideways slide or as the list's scroll. */
const SLIDE_SLOP_PX = 8;

/** A release faster than this, in px per ms, opens or shuts the row in the way it moved. */
const FLICK_PX_PER_MS = 0.3;

/** A finger that rests this long before it lifts makes no flick. */
const FLICK_REST_MS = 150;

/** How long the row takes to settle open or shut after the finger lifts. */
const SETTLE_MS = 200;

/** A click this soon after a slide ends that slide. It is not a press. */
const SLIDE_CLICK_MS = 400;

/** One finger on a row, from its first contact to its release. */
interface Slide {
  pointerId: number;
  x: number;
  y: number;
  /** The row's offset when the finger landed: the open width, or 0. */
  from: number;
  offset: number;
  /** The finger moved sideways past the slop, so the row follows it. */
  isSliding: boolean;
  at: number;
  /** Px per ms toward open, smoothed over the last moves. */
  velocity: number;
}

/** Does this reader want the motion at all? */
function wantsMotion(): boolean {
  return !window.matchMedia?.('(prefers-reduced-motion: reduce)').matches;
}

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
 * A ROW IS NEVER A NATIVE SCROLLER. The finger slides it, and the list keeps the only scroll.
 *
 * Regression, user reports (paraphrased: scrolling over a big group on the phone was
 * extremely slow, then a hard swipe over a 445-session group lagged and showed blank
 * stripes): each row's track was a horizontal scroll-snap scroller. WebKit carried each
 * one through the list's own scroll, and each change of which rows scrolled laid out the
 * whole list. With only still rows on screen as scrollers, the touch that starts or ends
 * a fling still took 40-60 ms. On an iOS 18.6 simulator, 12 flings over that group gave
 * 17 such frames, and 2 with no row scroller.
 *
 * So the track clips its row and is never a scroll container (`overflow: clip`). The page
 * pans only vertically over it (`touch-action: pan-y`), so a sideways drag reaches the row
 * as pointer moves. The row follows the finger with `translate` and settles open or shut
 * on release. A vertical drag is the list's scroll, and the browser cancels the pointer.
 */
/**
 * Shared row actions: a sliding drawer on touch and a vertical-dot dropdown
 * under a pointer. Only the trigger reserves desktop width, regardless of action
 * count. Permanent row controls stay mounted once, after the action trigger.
 */
export function SwipeActions({
  actions,
  children,
  isCurrent = false,
  isSelected = false,
  label,
  paper = 'panel',
  alignMenuWithHeader = false,
  trailing,
}: {
  actions: SwipeAction[];
  children: ReactNode;
  /**
   * The row is the one standing open elsewhere on the screen, and the list says so
   * in its paper. The touch panel carries its own opaque slab over the drawer, so
   * the standing paper has to BE that slab: a colour on the row behind it would be
   * covered exactly where a finger reads the row.
   */
  isCurrent?: boolean;
  /** The selected range must cover the whole row, including the touch panel and actions. */
  isSelected?: boolean;
  label?: string;
  /** Put a settings row's desktop menu mark on its header action rail. */
  alignMenuWithHeader?: boolean;
  /** Opaque touch-row paper; selected and standing rows keep their own states. */
  paper?: 'panel' | 'set-sessions';
  /** Permanent row controls: inside the touch panel, before the desktop menu trigger. */
  trailing?: ReactNode;
}) {
  const trackRef = useRef<HTMLDivElement>(null);
  const panelRef = useRef<HTMLDivElement>(null);
  const trayRef = useRef<HTMLDivElement>(null);
  const menuRef = useRef<RowMenuHandle | null>(null);
  const [open, setOpen] = useState(false);
  const slide = useRef<Slide | null>(null);
  /** When the last slide ended, so the click that follows it goes nowhere. */
  const slidAt = useRef(-Infinity);

  /** Put the row and its drawer `offset` px to the left. Only this changes their place. */
  const place = useCallback((offset: number, isAnimated: boolean) => {
    for (const element of [panelRef.current, trayRef.current]) {
      if (!element) continue;
      element.style.transition =
        isAnimated && wantsMotion() ? `translate ${SETTLE_MS}ms ease-out` : '';
      element.style.translate = offset > 0 ? `${-offset}px` : '';
    }
  }, []);

  // CLOSED IS A FACT. `open` and the row's offset change together, in one call, and
  // nothing else moves the row: a re-sort that moves the row's node keeps its inline
  // offset. A verb closes at once, because starring moves the row in the same commit.
  //
  // Regression, user report about the star on iOS (paraphrased: the star did not show
  // until the row slid a second time): the native track was free to drop an animated
  // close or to reset its offset without an event, so the strip and `open` disagreed.
  const close = useCallback(
    (isAnimated = true) => {
      setOpen(false);
      place(0, isAnimated);
    },
    [place],
  );

  // An open drawer is a modal-ish state: Escape closes it, scrolling the list
  // away from it closes it, and opening another row closes it — otherwise a
  // forgotten row keeps a delete button armed under the user's thumb.
  useEffect(() => {
    if (!open) return;
    if (openDrawer !== close) openDrawer?.();
    openDrawer = close;
    const onKey = (event: KeyboardEvent) => {
      if (event.key === 'Escape') close();
    };
    const onScroll = (event: Event) => {
      // The LIST moving under an open row closes it; a scroller that does not
      // carry the row does not. A live transcript rewrites its own `scrollTop` on
      // every update the session receives, and must not shut a row on another screen.
      const track = trackRef.current;
      const scroller = event.target;
      if (!track) return;
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

  const trayWidth = () => trayRef.current?.offsetWidth ?? 0;

  /** The finger lifts or the browser takes it: settle open or shut. */
  const release = (event: PointerEvent<HTMLDivElement>, isLift: boolean) => {
    const current = slide.current;
    if (!current || current.pointerId !== event.pointerId) return;
    slide.current = null;
    if (!current.isSliding) return;
    const now = performance.now();
    slidAt.current = now;
    const width = trayWidth();
    // A finger that rests before it lifts has no speed left to give.
    const isFresh = now - current.at < FLICK_REST_MS;
    const isFlick = isLift && isFresh && Math.abs(current.velocity) >= FLICK_PX_PER_MS;
    const isOpening = isFlick ? current.velocity > 0 : current.offset > width / 2;
    if (!isOpening) {
      close();
      return;
    }
    place(width, true);
    setOpen(true);
  };

  if (actions.length === 0) {
    if (!trailing) return <>{children}</>;
    return (
      <div className={`grid grid-cols-[minmax(0,1fr)_auto] ${isSelected ? 'bg-accent/15' : ''} ${ROW_PRESS_PAPER}`}>
        {children}
        {trailing}
      </div>
    );
  }

  return (
    <div
      ref={trackRef}
      data-swipe-track=""
      onPointerDown={(event) => {
        if (event.pointerType === 'mouse' || !event.isPrimary) return;
        const from = open ? trayWidth() : 0;
        slide.current = {
          pointerId: event.pointerId,
          x: event.clientX,
          y: event.clientY,
          from,
          offset: from,
          isSliding: false,
          at: performance.now(),
          velocity: 0,
        };
      }}
      onPointerMove={(event) => {
        const current = slide.current;
        if (!current || current.pointerId !== event.pointerId) return;
        const across = current.x - event.clientX;
        if (!current.isSliding) {
          const down = Math.abs(event.clientY - current.y);
          // The list's scroll: the browser takes this finger and cancels the pointer.
          if (down >= SLIDE_SLOP_PX && down >= Math.abs(across)) {
            slide.current = null;
            return;
          }
          if (Math.abs(across) < SLIDE_SLOP_PX || trayWidth() === 0) return;
          current.isSliding = true;
          event.currentTarget.setPointerCapture?.(event.pointerId);
          // Another open row shuts the moment this one starts to move.
          if (openDrawer !== close) openDrawer?.();
        }
        const offset = Math.min(trayWidth(), Math.max(0, current.from + across));
        const now = performance.now();
        const elapsed = now - current.at;
        if (elapsed > 0) {
          current.velocity = 0.8 * ((offset - current.offset) / elapsed) + 0.2 * current.velocity;
        }
        current.offset = offset;
        current.at = now;
        place(offset, false);
      }}
      onPointerUp={(event) => release(event, true)}
      onPointerCancel={(event) => release(event, false)}
      // A slide is not a press: the click that can follow its release goes nowhere.
      onClickCapture={(event) => {
        if (performance.now() - slidAt.current > SLIDE_CLICK_MS) return;
        event.preventDefault();
        event.stopPropagation();
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
      className={`group/swipe flex touch-pan-y overflow-clip ${isSelected ? 'bg-accent/15' : ''} ${ROW_PRESS_PAPER}`}
    >
      {/* Touch keeps content and permanent controls in one full-width panel.
          Desktop ends the row with its menu trigger and stands the permanent controls
          one slot inside it, the two cells every header in the list uses. */}
      <div
        ref={panelRef}
        className={`grid w-full shrink-0 grid-cols-[minmax(0,1fr)_auto] mouse:contents ${isSelected ? 'bg-accent/15' : isCurrent ? 'bg-standing' : paper === 'set-sessions' ? 'bg-set-sessions' : 'bg-panel'} ${ROW_PRESS_PAPER}`}
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
        className={`hidden shrink-0 items-center mouse:flex ${trailing ? 'pr-2 mouse:pr-2.5' : alignMenuWithHeader ? 'pr-1 sm:pr-2' : LIST_EDGE_END}`}
      >
        <RowActionMenu actions={actions} label={label} handle={menuRef} />
      </div>
      {/* Layout sizes every action before the first swipe. A slide does not change
          these widths; only the row width or action count can change them. A focus
          that reaches a verb opens the drawer, so the clip never hides it. */}
      <div
        ref={trayRef}
        className="flex min-w-0 max-w-full shrink-0 mouse:hidden"
        style={{ width: `${actions.length * 4.5}rem` }}
        role="group"
        aria-label={label ? `${label} actions` : 'Row actions'}
        onFocus={() => {
          if (open) return;
          place(trayWidth(), false);
          setOpen(true);
        }}
      >
        {actions.map((action) => (
          <button
            key={action.key}
            type="button"
            aria-label={action.name ?? action.label}
            title={action.name ?? action.label}
            className={`flex min-w-0 flex-1 flex-col items-center justify-center gap-1 border-l border-edge-strong font-mono text-chip font-bold uppercase tracking-[0.08em] transition-colors duration-150 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-accent/60 motion-reduce:transition-none ${
              action.tone === 'danger'
                ? 'bg-err-surface text-err-ink'
                : action.tone === 'accent'
                  ? 'bg-accent/15 text-accent-ink'
                  : 'bg-hover text-white'
            }`}
            onClick={(event) => {
              const anchor = event.currentTarget;
              close(false);
              action.onSelect(anchor);
            }}
          >
            <span aria-hidden="true">{action.icon}</span>
            <span className="max-w-full truncate px-0.5">{action.label}</span>
          </button>
        ))}
      </div>
    </div>
  );
}

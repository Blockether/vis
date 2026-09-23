/**
 * THE ONE MENU.
 *
 * A menu in this app is a POPOVER over a scrim, pinned under the control it came
 * from — the same panel at every width, a phone included, and never a drawer sliding
 * up from the bottom edge. It NAMES what it acts on in one band, and every row below
 * that band is a title, the consequence of pressing it, and an optional badge.
 *
 * It is one component because it used to be several. The machine's `⋯` opened a
 * 320px panel with an amber band and two-line rows; the project's `⋯` opened a
 * 256px panel with no band at all and a single hand-built row that no other menu
 * in the app could have produced. The same glyph, one line apart in the same
 * list, meant two different things — so `⋯` stopped being a promise.
 *
 * The width lives here too: an anchored popover is placed from its own width
 * BEFORE it has ever been measured, so the number and the class it paints itself
 * at have to travel together.
 */
import type { CSSProperties, ReactNode } from 'react';
import { createPortal } from 'react-dom';

import { EDGE_MARGIN, type MenuPosition } from '../lib/anchored-menu';
import { useKeyboardInset } from '../lib/viewport';
import { ChevronIcon } from './icons';
import { CloseButton } from './ui';

/**
 * The two widths an anchored panel comes in, each paired with the class that paints
 * it. An anchored popover is PLACED from its own width before it has ever been
 * measured, so the number and the class have to travel together or they drift — and
 * a drifted pair is a menu that right-aligns to nothing. The class clamps itself to
 * the screen — `24px` is twice the margin the placement keeps — because a phone is
 * narrower than the browse panel is wide.
 */
export const PANEL_SIZES = {
  /** A list of verbs. */
  menu: { width: 320, className: 'w-[min(20rem,calc(100vw-24px))]' },
  /** A list of folders, which need room for a name and a hint on one line. */
  browse: { width: 384, className: 'w-[min(24rem,calc(100vw-24px))]' },
} as const;

export type PanelSize = keyof typeof PANEL_SIZES;

/** The width in px the panel paints where the screen has room for it. */
export const MENU_WIDTH = PANEL_SIZES.menu.width;

/**
 * THE PANEL, and every layer this app anchors to a control is it.
 *
 * An anchored popover over a scrim, pinned under the control it came from at every
 * width: a phone gets the panel a desktop gets, not a drawer docked to the bottom
 * edge of the screen. `Menu` is one of these and the folder browser is the other —
 * and the browser used to spell the whole box out again by hand, forty characters of
 * it, which is how it ended up as the one surface in the app with no way out but the
 * scrim.
 *
 * It is a COLUMN, and it caps its own height; what scrolls inside it is the caller's
 * decision, because a menu scrolls whole while a browser has to keep its path and its
 * commit button still. The height cap is the one `anchored-menu.ts` places against,
 * so the two must agree: see `MAX_HEIGHT_FRACTION` there.
 */
export function AnchoredPanel({
  size,
  role,
  label,
  at,
  onDismiss,
  children,
}: {
  size: PanelSize;
  /** `menu` for a list of verbs, `dialog` for a surface that holds a task. */
  role: 'menu' | 'dialog';
  /** What this panel is about, for a reader who cannot see where it hangs. */
  label: string;
  /** Where the popover sits; `null` while its control has not been measured. */
  at: MenuPosition | null;
  onDismiss: () => void;
  children: ReactNode;
}) {
  // The panel is PORTALED to the document, so the app shell's keyboard pin never
  // moves it: on iOS the webview stays full height and a panel hanging under a
  // control low on the screen disappeared under the software keyboard - with the
  // name field of a new group inside it. It STANDS on the keyboard instead, keeping
  // the column its anchor gave it, and the cap it paints trims it to what is left.
  const keyboardInset = useKeyboardInset();
  const isLifted = keyboardInset > 0;
  // A panel whose control has not been measured yet still belongs on the screen, so
  // it keeps the margin every placement keeps rather than the corner of the document.
  const place = at ?? { left: EDGE_MARGIN, top: EDGE_MARGIN };
  const top = isLifted ? undefined : place.top;
  const bottom = isLifted ? keyboardInset + EDGE_MARGIN : place.bottom;

  return createPortal(
    <div
      className="fixed inset-0 z-50"
      role="presentation"
      onClick={onDismiss}
    >
      <div
        role={role}
        aria-modal={role === 'dialog' ? true : undefined}
        aria-label={label}
        className={`absolute bottom-[var(--menu-bottom,auto)] left-[var(--menu-left)] top-[var(--menu-top,auto)] flex max-h-[min(var(--menu-max-height,70vh),calc(100vh-var(--menu-keyboard,0px)-24px))] flex-col overflow-hidden rounded-none border border-dialog-edge bg-panel shadow-float transition-[opacity,transform,translate,scale,rotate] duration-150 starting:translate-y-2 starting:opacity-0 motion-reduce:transition-none ${PANEL_SIZES[size].className}`}
        style={
          {
            '--menu-keyboard': `${keyboardInset}px`,
            '--menu-left': `${place.left}px`,
            // ONE vertical edge is pinned, and which one says where the panel
            // hangs: a head under the anchor, or a foot standing on top of it. A
            // panel with room for neither keeps its place and wears a tighter cap.
            ...(top === undefined ? {} : { '--menu-top': `${top}px` }),
            ...(bottom === undefined ? {} : { '--menu-bottom': `${bottom}px` }),
            ...(place.maxHeight === undefined
              ? {}
              : { '--menu-max-height': `${place.maxHeight}px` }),
          } as CSSProperties
        }
        onClick={(event) => event.stopPropagation()}
      >
        {children}
      </div>
    </div>,
    document.body,
  );
}

/** The band's shared geometry: one line of small caps, the width of the menu. */
const BAND = 'px-3 py-2 font-mono text-chip font-bold uppercase tracking-[0.08em]';

/**
 * Every primary menu heading uses the same dark title band as a dialog. The accent
 * belongs to the heading's verb, not to the whole strip behind it.
 */
const LOUD = 'border-b-2 border-dialog-title bg-dialog-title text-dialog-title-foreground';
const QUIET = 'border-b border-dialog-edge bg-panel-2 text-dialog-hint';

/**
 * The panel, anchored to the control that opened it at every width.
 *
 * The scrim swallows the tap that dismisses it, so a caller never wires one; the
 * key loop stays with the caller, because Escape usually has to unwind a flow
 * this component knows nothing about.
 */
export function Menu({
  label,
  at,
  onDismiss,
  children,
}: {
  /** What this menu is about, for a reader who cannot see where it hangs. */
  label: string;
  /** Where the popover sits, measured from the control that opened it. */
  at: MenuPosition;
  onDismiss: () => void;
  children: ReactNode;
}) {
  return (
    <AnchoredPanel size="menu" role="dialog" label={label} at={at} onDismiss={onDismiss}>
      {/* This is a dialog containing ordinary buttons, not the ARIA `menu` widget:
          ARIA menus require roving focus and arrow-key navigation, while this panel
          deliberately keeps the browser's familiar Tab order and may also contain
          a heading, an explanation, or an empty-state sentence. */}
      <div className="min-h-0 flex-1 touch-pan-y overflow-y-auto overscroll-contain [&>*:last-child]:border-b-0">
        {children}
      </div>
    </AnchoredPanel>
  );
}

/**
 * The band that says what the rows below act on: the machine you tapped, the
 * project you tapped. `quiet` is for a SECOND band in the same menu — a
 * treatment that shouts once is a barcode when it is charged twice.
 */
export function MenuHeading({
  tone = 'loud',
  cells,
  onClose,
  closeLabel,
  children,
}: {
  tone?: 'loud' | 'quiet';
  /**
   * The band's own CELLS, welded before its way out: `BandButton`, the same box the
   * ✕ is and as tall as the band.
   *
   * Never a `Button`. Paper stacked on the dark title band is a control from another
   * surface parked on this one, and `variant="secondary"` carries `text-white`, which
   * in this app is the PAGE's ink (`--fg`, #262626) — dark on dark, so the sheet's
   * second verb was the one control in it a reader could not see. A band that also
   * shouts an accent slab beside a 2px accent rule then charges the same colour twice.
   */
  cells?: ReactNode;
  children: ReactNode;
} & (
  | {
      /**
       * The way out, for a panel that holds a TASK rather than a list of verbs. A menu
       * is left by picking from it or by tapping the paper around it, so it needs none;
       * a folder browser is a place you can be halfway through, and the scrim behind it
       * on a phone is a 130px strip most thumbs never reach. It is `CloseButton` — the
       * app has exactly one way out and this band does not get to invent a second.
       */
      onClose: () => void;
      /** Icon-only, so the name is not optional: "Close projects on tower". */
      closeLabel: string;
    }
  | { onClose?: undefined; closeLabel?: undefined }
)) {
  const skin = tone === 'loud' ? LOUD : QUIET;
  if (!onClose || !closeLabel) return <p className={`${BAND} ${skin} truncate`}>{children}</p>;
  return (
    // The band stands where the dialog band stands (48, 36 for a mouse), because the
    // way out welded to it is the same cell on both and a cell can only be square in
    // a band of one height.
    <header className={`flex min-h-12 shrink-0 items-stretch mouse:min-h-9 ${skin}`}>
      <p className={`${BAND} min-w-0 flex-1 self-center truncate`}>{children}</p>
      {cells}
      <CloseButton isBand label={closeLabel} onClick={onClose} />
    </header>
  );
}

/**
 * The same band, as the way BACK out of a step taken inside one menu: a step is
 * left the way it was entered, never out to a blank screen.
 */
export function MenuBack({
  label,
  onBack,
  cells,
  children,
}: {
  label: string;
  onBack: () => void;
  /** The step's commit cells, welded beside the way back — see `MenuHeading`. */
  cells?: ReactNode;
  children: ReactNode;
}) {
  return (
    <header className={`flex min-h-12 shrink-0 items-stretch mouse:min-h-9 ${LOUD}`}>
      <button
        type="button"
        className={`${BAND} flex min-w-0 flex-1 items-center gap-2 text-left`}
        aria-label={label}
        onClick={onBack}
      >
        <ChevronIcon back className="size-3 shrink-0" aria-hidden />
        <span className="truncate">{children}</span>
      </button>
      {cells}
    </header>
  );
}

/**
 * One row. The title carries the choice and the hint carries the CONSEQUENCE —
 * a workspace decision is unrecoverable-ish once the agent starts writing, so no
 * row is allowed to be a bare noun. `min-h-11` keeps every row a real thumb
 * target on a phone.
 *
 * `danger` is the same row in the app's red, never a different one: a menu whose
 * destructive row is built by hand is how two `⋯` menus stop looking alike.
 *
 * The row CENTRES what it holds. A hinted row is taller than `min-h-11` on its own,
 * so centring is a no-op there and the icon still hangs off the TITLE's line; a row
 * of one line is shorter than the thumb target it must fill, and top-pinning it left
 * the icon and the title floating in the upper third with the whole reserve of empty
 * paper under them.
 */
export function MenuItem({
  title,
  meta,
  hint,
  badge,
  icon,
  tone = 'default',
  action,
  disabled = false,
  onSelect,
}: {
  title: string;
  /** Supporting fact on the title line; the hint remains its own line. */
  meta?: string;
  hint?: string;
  badge?: string;
  icon?: ReactNode;
  tone?: 'default' | 'danger';
  /** A second verb beside the row; buttons cannot nest. */
  action?: ReactNode;
  /** Keep in-flight creation unavailable without hiding its action. */
  disabled?: boolean;
  /** Receives the row itself, so a sheet can be anchored on what opened it. */
  onSelect: (anchor: HTMLElement) => void;
}) {
  const danger = tone === 'danger';
  // What rides BESIDE the title belongs to the title's own line, and only a hinted
  // row has a second one to be pulled off. On a one-line row it is the centred row
  // that puts it there, so a nudge meant for two lines would only tip it back off.
  const onFirstLine = hint ? 'self-start mt-0.5' : '';
  const row = (
    <button
      type="button"
      disabled={disabled}
      className={`group/menu-item flex min-h-11 items-center gap-2 px-3 py-2 text-left transition-colors duration-150 focus-visible:outline-none motion-reduce:transition-none mouse:min-h-9 ${
        action ? 'min-w-0 flex-1' : 'w-full border-b border-dialog-edge'
      } ${danger ? 'focus-visible:bg-err-surface' : 'focus-visible:bg-hover'}`}
      onClick={(event) => onSelect(event.currentTarget)}
    >
      {icon && (
        <span
          className={`${onFirstLine} shrink-0 ${danger ? 'text-err-ink' : 'text-dialog-hint mouse:group-hover/menu-item:text-accent-ink'}`}
        >
          {icon}
        </span>
      )}
      <span className="min-w-0 flex-1">
        <span className="flex min-w-0 items-baseline gap-2">
          <span
            className={`block min-w-0 flex-1 truncate font-mono text-ui font-bold ${danger ? 'text-err-ink' : 'text-white mouse:group-hover/menu-item:text-accent-ink'}`}
          >
            {title}
          </span>
          {meta && <span className="shrink-0 font-mono text-meta text-dialog-hint">{meta}</span>}
        </span>
        {hint && <span className="mt-0.5 block font-mono text-meta text-dialog-hint">{hint}</span>}
      </span>
      {badge && (
        <span
          className={`${onFirstLine} shrink-0 border border-edge px-1 font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint`}
        >
          {badge}
        </span>
      )}
    </button>
  );
  if (!action) return row;
  return (
    // Keep the independent action inside the same gutter as the row's leading icon.
    <div className="flex items-center gap-2 border-b border-dialog-edge pr-3">
      {row}
      {action}
    </div>
  );
}

/** A menu that has something to SAY rather than to offer: reading, empty, failed. */
export function MenuNote({ children }: { children: ReactNode }) {
  return (
    <p className="flex items-center gap-2 px-3 py-3 font-mono text-meta text-dialog-hint">
      {children}
    </p>
  );
}

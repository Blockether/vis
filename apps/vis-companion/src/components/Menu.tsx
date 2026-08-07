/**
 * THE ONE MENU.
 *
 * A menu in this app is a SHEET on a phone — docked to the bottom edge over a
 * scrim, capped by the Blockether rule, safe-area aware — and a popover pinned
 * under the control it came from from `sm:` up. It NAMES what it acts on in one
 * band, and every row below that band is a title, the consequence of pressing
 * it, and an optional badge.
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

import type { MenuPosition } from '../lib/anchored-menu';
import { ChevronIcon } from './icons';
import { DialogClose } from './ui';

/**
 * The two widths an anchored panel comes in, each paired with the class that paints
 * it. An anchored popover is PLACED from its own width before it has ever been
 * measured, so the number and the class have to travel together or they drift — and
 * a drifted pair is a menu that right-aligns to nothing.
 */
export const PANEL_SIZES = {
  /** A list of verbs. */
  menu: { width: 320, className: 'sm:w-80' },
  /** A list of folders, which need room for a name and a hint on one line. */
  browse: { width: 384, className: 'sm:w-96' },
} as const;

export type PanelSize = keyof typeof PANEL_SIZES;

/** Desktop width in px. Must stay equal to the `sm:w-80` the panel paints. */
export const MENU_WIDTH = PANEL_SIZES.menu.width;

/**
 * THE PANEL, and every layer this app anchors to a control is it.
 *
 * A sheet docked to the bottom edge of a phone over a scrim, capped by the
 * Blockether rule and clear of the home indicator; an anchored popover pinned under
 * the control it came from from `sm:` up. `Menu` is one of these and the folder
 * browser is the other — and the browser used to spell the whole box out again by
 * hand, forty characters of it, which is how it ended up as the one surface in the
 * app with no way out but the scrim.
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
  /** Where the popover sits from `sm:` up; the phone sheet ignores it. */
  at: MenuPosition | null;
  onDismiss: () => void;
  children: ReactNode;
}) {
  return createPortal(
    <div
      className={`fixed inset-0 z-50 bg-black/40 ${role === 'menu' ? 'sm:bg-transparent' : ''}`}
      role="presentation"
      onClick={onDismiss}
    >
      <div
        role={role}
        aria-modal={role === 'dialog' ? true : undefined}
        aria-label={label}
        className={`absolute inset-x-0 bottom-0 flex max-h-[82vh] flex-col border-t-2 border-accent bg-panel pb-[env(safe-area-inset-bottom)] transition-[opacity,transform,translate,scale,rotate] duration-150 starting:translate-y-2 starting:opacity-0 motion-reduce:transition-none sm:inset-x-auto sm:bottom-auto sm:left-[var(--menu-left)] sm:top-[var(--menu-top)] sm:max-h-[70vh] sm:border sm:border-dialog-edge sm:pb-0 sm:shadow-[8px_8px_0_var(--line2)] ${PANEL_SIZES[size].className}`}
        style={
          at
            ? ({ '--menu-top': `${at.top}px`, '--menu-left': `${at.left}px` } as CSSProperties)
            : undefined
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
 * The one question you cannot skip wears the Blockether yellow, and a menu spends
 * that colour exactly once — every other band in it is quiet.
 */
const LOUD = 'border-b-2 border-warn-strong bg-accent text-accent-foreground';
const QUIET = 'border-b border-dialog-edge bg-panel-2 text-dialog-hint';

/**
 * The panel: a docked sheet on a phone, an anchored popover from `sm:` up.
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
  /** Where the popover sits from `sm:` up; the phone sheet ignores it. */
  at: MenuPosition;
  onDismiss: () => void;
  children: ReactNode;
}) {
  return (
    <AnchoredPanel size="menu" role="menu" label={label} at={at} onDismiss={onDismiss}>
      {/* A menu is one list and scrolls whole; only its own body may take the drag,
          so a long list of verbs never rubber-bands the screen behind it.

          The LAST row drops its rule: every row draws the line under itself, and the
          bottom one's line lands exactly on the panel's own bottom edge — two
          hairlines in one pixel row on desktop, and a rule floating over the home
          indicator on a phone, where the panel has no bottom edge to agree with. */}
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
  onClose,
  children,
}: {
  tone?: 'loud' | 'quiet';
  /**
   * The way out, for a panel that holds a TASK rather than a list of verbs. A menu
   * is left by picking from it or by tapping the paper around it, so it needs none;
   * a folder browser is a place you can be halfway through, and the scrim behind it
   * on a phone is a 130px strip most thumbs never reach. It is `DialogClose` — the
   * app has exactly one way out and this band does not get to invent a second.
   */
  onClose?: () => void;
  children: ReactNode;
}) {
  const skin = tone === 'loud' ? LOUD : QUIET;
  if (!onClose) return <p className={`${BAND} ${skin} truncate`}>{children}</p>;
  return (
    <header className={`flex min-h-11 shrink-0 items-stretch mouse:min-h-9 ${skin}`}>
      <p className={`${BAND} min-w-0 flex-1 self-center truncate`}>{children}</p>
      <DialogClose label="Close" tone="panel" onClose={onClose} />
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
  children,
}: {
  label: string;
  onBack: () => void;
  children: ReactNode;
}) {
  return (
    <button
      type="button"
      className={`${BAND} ${LOUD} flex min-h-11 w-full items-center gap-2 text-left`}
      aria-label={label}
      onClick={onBack}
    >
      <ChevronIcon back className="size-3" aria-hidden />
      {children}
    </button>
  );
}

/**
 * One row. The title carries the choice and the hint carries the CONSEQUENCE —
 * a workspace decision is unrecoverable-ish once the agent starts writing, so no
 * row is allowed to be a bare noun. `min-h-11` keeps every row a real thumb
 * target on a phone sheet.
 *
 * `danger` is the same row in the app's red, never a different one: a menu whose
 * destructive row is built by hand is how two `⋯` menus stop looking alike.
 */
export function MenuItem({
  title,
  hint,
  badge,
  icon,
  tone = 'default',
  action,
  onSelect,
}: {
  title: string;
  hint?: string;
  badge?: string;
  icon?: ReactNode;
  tone?: 'default' | 'danger';
  /**
   * A SECOND verb on the same row — the trash beside a project you can also choose.
   * Buttons cannot nest, so the row splits into a pressable half and this one; the
   * hairline moves to the wrapper so the two halves still read as one row. Without
   * it the row is a plain button, exactly as before.
   */
  action?: ReactNode;
  /** Receives the row itself, so a sheet can be anchored on what opened it. */
  onSelect: (anchor: HTMLElement) => void;
}) {
  const danger = tone === 'danger';
  const row = (
    <button
      type="button"
      role="menuitem"
      className={`flex min-h-11 items-start gap-2 px-3 py-2 text-left transition-colors duration-150 focus-visible:outline-none motion-reduce:transition-none mouse:min-h-9 ${
        action ? 'min-w-0 flex-1' : 'w-full border-b border-dialog-edge'
      } ${
        danger ? 'hover:bg-err/15 focus-visible:bg-err/15' : 'hover:bg-hover focus-visible:bg-hover'
      }`}
      onClick={(event) => onSelect(event.currentTarget)}
    >
      {icon && (
        <span className={`mt-0.5 shrink-0 ${danger ? 'text-err' : 'text-dialog-hint'}`}>
          {icon}
        </span>
      )}
      <span className="min-w-0 flex-1">
        <span
          className={`block truncate font-mono text-ui font-bold ${danger ? 'text-err' : 'text-white'}`}
        >
          {title}
        </span>
        {hint && <span className="mt-0.5 block font-mono text-meta text-dialog-hint">{hint}</span>}
      </span>
      {badge && (
        <span className="mt-0.5 shrink-0 border border-edge px-1 font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
          {badge}
        </span>
      )}
    </button>
  );
  if (!action) return row;
  return (
    // The wrapper owns the row's TRAILING GUTTER, exactly as `LIST_TRAIL` does for the
    // list: an `edge` action reclaims it with its own negative margin, so the glyph is
    // `px-3` from the paper just like the title is on the other side. Without it the
    // trash's `-mr-3` had nothing to cancel and hung 12px past the sheet's own edge.
    <div className="flex items-stretch border-b border-dialog-edge pr-3">
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

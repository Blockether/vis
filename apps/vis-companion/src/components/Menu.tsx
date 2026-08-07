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

/** Desktop width in px. Must stay equal to the `sm:w-80` the panel paints. */
export const MENU_WIDTH = 320;

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
  return createPortal(
    <div
      className="fixed inset-0 z-50 bg-black/40 sm:bg-transparent"
      role="presentation"
      onClick={onDismiss}
    >
      <div
        role="menu"
        aria-label={label}
        className="absolute inset-x-0 bottom-0 max-h-[70vh] touch-pan-y overflow-y-auto overscroll-contain border-t-2 border-accent bg-panel pb-[env(safe-area-inset-bottom)] transition-[opacity,transform,translate,scale,rotate] duration-150 starting:translate-y-2 starting:opacity-0 motion-reduce:transition-none sm:inset-x-auto sm:bottom-auto sm:left-[var(--menu-left)] sm:top-[var(--menu-top)] sm:w-80 sm:border sm:border-dialog-edge sm:pb-0 sm:shadow-[8px_8px_0_var(--line2)]"
        style={
          {
            '--menu-top': `${at.top}px`,
            '--menu-left': `${at.left}px`,
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

/**
 * The band that says what the rows below act on: the machine you tapped, the
 * project you tapped. `quiet` is for a SECOND band in the same menu — a
 * treatment that shouts once is a barcode when it is charged twice.
 */
export function MenuHeading({
  tone = 'loud',
  children,
}: {
  tone?: 'loud' | 'quiet';
  children: ReactNode;
}) {
  return <p className={`${BAND} ${tone === 'loud' ? LOUD : QUIET} truncate`}>{children}</p>;
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
      <ChevronIcon className="size-3" aria-hidden />
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
  onSelect,
}: {
  title: string;
  hint?: string;
  badge?: string;
  icon?: ReactNode;
  tone?: 'default' | 'danger';
  /** Receives the row itself, so a sheet can be anchored on what opened it. */
  onSelect: (anchor: HTMLElement) => void;
}) {
  const danger = tone === 'danger';
  return (
    <button
      type="button"
      role="menuitem"
      className={`flex min-h-11 w-full items-start gap-2 border-b border-dialog-edge px-3 py-2 text-left transition-colors duration-150 focus-visible:outline-none motion-reduce:transition-none ${
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
}

/** A menu that has something to SAY rather than to offer: reading, empty, failed. */
export function MenuNote({ children }: { children: ReactNode }) {
  return (
    <p className="flex items-center gap-2 px-3 py-3 font-mono text-meta text-dialog-hint">
      {children}
    </p>
  );
}

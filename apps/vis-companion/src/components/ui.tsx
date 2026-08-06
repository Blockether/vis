import {
  forwardRef,
  type ButtonHTMLAttributes,
  type InputHTMLAttributes,
  type ReactNode,
} from 'react';

import type { MachineColor } from '../lib/machine-colors';
import { CloseIcon } from './icons';

// Ref-forwarding: a button that ANCHORS something (a popover, a focus return) has
// to be measurable by its owner, and cloning the element's classes at the call site
// to get a bare <button> is how a design system drifts.
export const Button = forwardRef<
  HTMLButtonElement,
  ButtonHTMLAttributes<HTMLButtonElement> & {
    variant?: 'solid' | 'ghost' | 'quiet' | 'danger';
    /**
     * Press feedback. `scale` is the default nudge; `none` is for a button that
     * ANCHORS something (a popover) or sits in a segmented group — a transform
     * moves the box the menu was measured against and makes the group breathe
     * under the finger.
     */
    pressEffect?: 'scale' | 'none';
  }
>(function Button({ variant = 'solid', pressEffect = 'scale', className = '', ...props }, ref) {
  // Disabled colours live PER VARIANT, not in the base class: `quiet` has to stay
  // frameless while it is busy, and a shared `disabled:border-edge` would fight it
  // on equal specificity (whoever Tailwind emits last wins).
  const dimmed = 'disabled:border-edge disabled:bg-panel-2 disabled:text-muted';
  const styles = {
    solid: `border-accent bg-accent text-accent-foreground hover:border-accent/85 hover:bg-accent/85 ${dimmed}`,
    ghost: `border-edge-strong bg-transparent text-white hover:border-accent hover:bg-hover hover:text-accent-ink ${dimmed}`,
    // For a SECONDARY action sitting next to a solid primary: two bordered boxes
    // side by side read as rivals, so this one keeps the button's box (transparent
    // border, identical metrics) and only draws a frame on hover/focus.
    quiet:
      'border-transparent bg-transparent text-dialog-hint hover:border-edge-strong hover:bg-hover hover:text-accent-ink disabled:border-transparent disabled:bg-transparent disabled:text-muted',
    danger: `border-err/40 bg-err/10 text-err hover:border-err hover:bg-err hover:text-white ${dimmed}`,
    // A split button's caret half is NOT a second variant: it is `solid` with a
    // hairline in `accent-foreground`. Dark chrome next to an amber primary reads
    // as a disabled slab, and nobody presses a control that looks switched off.
  }[variant];
  // The transform utilities are OMITTED rather than overridden: `active:scale-100`
  // and `active:scale-[0.98]` have equal specificity, so a call-site override would
  // be decided by Tailwind's emission order, not by the call site.
  const press = pressEffect === 'scale' ? 'active:scale-[0.98] disabled:active:scale-100' : '';

  return (
    <button
      ref={ref}
      className={`min-h-7 rounded-none border px-2.5 py-0.5 text-meta font-bold transition-[background-color,border-color,color,opacity,transform,translate,scale,rotate] duration-150 ${press} focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 disabled:cursor-not-allowed disabled:opacity-100 disabled:shadow-none motion-reduce:transition-none sm:min-h-8 sm:px-3 sm:text-ui ${styles} ${className}`}
      {...props}
    />
  );
});

export const Input = forwardRef<HTMLInputElement, InputHTMLAttributes<HTMLInputElement>>(
  function Input({ className = '', ...props }, ref) {
    return (
      <input
        ref={ref}
        className={`min-h-7 w-full rounded-none border border-edge bg-input px-2.5 py-0.5 font-mono text-meta text-white transition-[border-color,box-shadow] duration-150 placeholder:text-dialog-hint focus:border-accent focus:outline-none focus:ring-1 focus:ring-accent/30 motion-reduce:transition-none sm:min-h-8 sm:px-3 sm:text-ui ${className}`}
        {...props}
      />
    );
  },
);

export function Card({ children, className = '' }: { children: ReactNode; className?: string }) {
  return (
    <div className={`border border-dialog-edge bg-panel p-4 ${className}`}>{children}</div>
  );
}

export function Banner({ kind, children }: { kind: 'ok' | 'warn' | 'err'; children: ReactNode }) {
  const colors = {
    ok: 'border-ok/50 bg-ok/10 text-ok',
    warn: 'border-warn-strong/60 bg-warn-surface text-warn',
    err: 'border-err/50 bg-err/10 text-err',
  }[kind];

  return (
    <div className={`border px-3 py-2 font-mono text-body ${colors}`} role="status">
      {children}
    </div>
  );
}

export function Section({ title, children }: { title: string; children: ReactNode }) {
  return (
    <section className="space-y-3">
      <h2 className="border-l-2 border-accent px-2 font-mono text-body font-bold uppercase tracking-[0.1em] text-white/70">
        {title}
      </h2>
      {children}
    </section>
  );
}

export function DialogFrame({
  title,
  children,
  footer,
  onClose,
  className = '',
}: {
  title: string;
  children: ReactNode;
  footer?: ReactNode;
  onClose?: () => void;
  className?: string;
}) {
  return (
    <section
      className={`overflow-hidden border border-dialog-edge bg-panel shadow-none transition-[opacity,transform,translate,scale,rotate] duration-200 starting:translate-y-2 starting:opacity-0 motion-reduce:transition-none sm:shadow-[8px_8px_0_var(--dialog-shadow)] ${className}`}
      role="dialog"
      aria-modal="true"
      aria-label={title}
    >
      <header className="relative flex min-h-9 items-center justify-center bg-dialog-title px-12 py-1.5 text-dialog-title-foreground mouse:min-h-8">
        {/* A dialog title can be a whole question from `vis.ask`; wrap it (bounded) instead of eating it. */}
        <h2
          className="line-clamp-3 text-center font-mono text-body font-bold tracking-wide"
          title={title}
        >
          {title}
        </h2>
        {onClose && (
          <button
            type="button"
            className="absolute inset-y-0 right-0 grid min-w-9 place-items-center border-l border-dialog-title-foreground/20 text-dialog-title-foreground/70 transition-colors hover:bg-err/15 hover:text-err focus-visible:bg-err/15 focus-visible:text-err focus-visible:outline-none mouse:min-w-8"
            onClick={onClose}
            aria-label="Close dialog"
          >
            <CloseIcon />
          </button>
        )}
      </header>
      <div className="border-t border-dialog-edge">{children}</div>
      {footer && (
        <footer className="border-t border-dialog-edge bg-panel-2 px-4 py-2 font-mono text-meta text-dialog-hint">
          {footer}
        </footer>
      )}
    </section>
  );
}

/** Same Braille cadence the TUI uses, so waiting looks the same everywhere. */
const SPINNER_FRAMES = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'];

// One LITERAL class per frame: Tailwind scans source text, so a computed
// `[animation-delay:-${i}00ms]` would never be emitted. The delay is negative
// so every frame is already mid-cycle on the first paint — a positive delay
// would show all ten glyphs stacked until their turn came round.
const SPINNER_DELAYS = [
  '[animation-delay:-1000ms]',
  '[animation-delay:-900ms]',
  '[animation-delay:-800ms]',
  '[animation-delay:-700ms]',
  '[animation-delay:-600ms]',
  '[animation-delay:-500ms]',
  '[animation-delay:-400ms]',
  '[animation-delay:-300ms]',
  '[animation-delay:-200ms]',
  '[animation-delay:-100ms]',
];

/**
 * The waiting spinner: ten frames stacked in one grid cell, cross-faded by the
 * `spinner-frame` keyframe (see `index.css`).
 *
 * It carries NO timer. The JS version this replaces re-rendered ten times a
 * second and, through the `absolute inset-0` shell, forced a whole-document
 * relayout on every frame — about a fifth of the WebKit main thread on an
 * otherwise idle iOS screen. Here the box is fixed and only `opacity` moves.
 */
export function Spinner({ className = '' }: { className?: string }) {
  return (
    <span aria-hidden="true" className={`inline-grid ${className}`}>
      {SPINNER_FRAMES.map((frame, index) => (
        <span
          key={frame}
          className={`col-start-1 row-start-1 animate-spinner-frame opacity-0 motion-reduce:hidden ${SPINNER_DELAYS[index]}`}
        >
          {frame}
        </span>
      ))}
      <span className="col-start-1 row-start-1 hidden motion-reduce:block">●</span>
    </span>
  );
}

/**
 * A live count that rides INSIDE another label — a scope chip, a machine row.
 *
 * It is the SAME filled block as the unread badge, in green: the strip's two
 * numbers are one shape in two colours — `macbook \u25ae3\u25ae\u25ae4\u25ae`,
 * running then waiting — so the chip reports both without a word and without a
 * second kind of thing to learn. A filled box aligns by its own box instead of
 * by a glyph's metrics, which is why it sits centred where the earlier `\u25cf`
 * hung low and the bracketed `[3]` read lighter than the badge beside it.
 *
 * The fill is `ok-surface`, NOT `ok`. `--ok` is the app's green INK — LIVE
 * text, the 6px machine dot — and a badge poured from it is a slab twice as
 * dark as the amber block beside it, reading as a louder green than anything
 * else on the screen and carrying its digit at 5:1. `ok-surface` is the same
 * hue at the amber badge's lightness, so the two blocks are peers and the digit
 * clears 8.6:1 on paper, 13.7:1 on ink.
 *
 * Colour is the whole signal, so the word is kept for anyone who cannot see it,
 * and a count of zero renders nothing at all: an empty green box is not news.
 */
export function LiveTally({ count }: { count: number }) {
  if (count <= 0) return null;
  return (
    <span className="inline-flex items-center bg-ok-surface px-1 font-mono text-chip font-bold text-ok-foreground">
      {count}
      <span className="sr-only"> live</span>
    </span>
  );
}

/**
 * Unread is NOT a tally, it is a notification, so it must not join the live
 * count's brackets: `macbook [4] 3` puts two bare numbers side by side and
 * nothing in the chip says which is which — the reader has to remember that
 * green means live and amber means new.
 *
 * The session row already speaks this app's unread language: a FILLED amber
 * block. The chip borrows it and drops the word, because a filled box aligns by
 * its own box instead of by a glyph's metrics, and it is the only solid thing in
 * a strip of outlined chips — exactly the weight a notification wants and the
 * live count deliberately does not have.
 *
 * Nothing to report renders nothing at all, so a caller cannot paint an empty
 * badge.
 */
export function UnreadBadge({ count }: { count: number }) {
  if (count <= 0) return null;
  return (
    <span className="inline-flex items-center bg-accent px-1 font-mono text-chip font-bold text-accent-foreground">
      {count}
      <span className="sr-only"> unread</span>
    </span>
  );
}

/**
 * The band that ENDS a machine. A project boundary is a hairline; a whole
 * computer is not, so this is space — the page's own ink showing through the
 * panel, closed top and bottom by the strong rule — read before any label.
 * Render it only BETWEEN machines: the first block starts flush and a fleet of
 * one never pays it.
 */
export function MachineGap() {
  return <div className="h-3 border-y border-edge-strong bg-ink" aria-hidden="true" />;
}

/**
 * A machine header is a sticky banner: its rail and stronger boundary distinguish
 * the computer from its projects while its title and tally retain the same
 * monospace hierarchy those project headers use.
 *
 * A sticky band is composited ABOVE the scroller's overlay scrollbar, so on iOS
 * it painted over the thumb: the bar vanished behind every machine header. The
 * band therefore stops short of the right edge on phones; from `sm` up the
 * scroller reserves a real gutter and the margin is dropped.
 */
export function MachineBanner({ children }: { children: ReactNode }) {
  return (
    <header className="sticky top-0 z-10 mr-2 flex min-h-11 items-center justify-between gap-3 border-b border-edge-strong bg-panel px-3 py-2 sm:mr-0 sm:px-4">
      {children}
    </header>
  );
}

/**
 * The rail that CONTAINS a machine: one 2px line in the machine's own hue running
 * down everything it owns, banner included. A project boundary is a hairline and a
 * machine boundary is a colour change — the eye can see where `tower` ends without
 * reading a single word. Without a colour there is no rail: a fleet of one is not
 * a boundary, so a solo machine pays nothing for the concept.
 */
export function MachineRail({ color, children }: { color?: MachineColor; children: ReactNode }) {
  if (!color) return <>{children}</>;
  return <div className={`border-l-2 ${color.rail}`}>{children}</div>;
}

/**
 * The machine's hue as a solid block, worn by its banner and its scope chip, so
 * the chip you tapped and the rail you got back are visibly the same machine.
 */
export function MachineMark({ color }: { color: MachineColor }) {
  return <span className={`size-1.5 shrink-0 ${color.dot}`} aria-hidden="true" />;
}

/**
 * The verb this screen exists for, on the machine that will run it.
 *
 * It used to be the first row of the header's `⋯` menu: a tap, a popover, a read,
 * and only then the thing people do all day. So it is a BUTTON now — the Blockether
 * yellow, on every machine header, directly before the `⋯` that keeps the rarer
 * verbs — and the menu is left with the questions that actually deserve one.
 *
 * It NAMES its machine, because every header carries one and three controls all
 * labelled "New session" say nothing to a screen reader; `where` puts the project it
 * will start in on the tooltip, since the header has no room to print a path.
 *
 * `pressEffect="none"`: a machine with no project yet falls through to the folder
 * browser anchored on this button, and a transform moves the box that was measured.
 */
export function NewSessionButton({
  machine,
  where,
  disabled,
  onPress,
}: {
  machine: string;
  where?: string | null;
  disabled?: boolean;
  onPress: (anchor: HTMLElement) => void;
}) {
  return (
    <Button
      type="button"
      pressEffect="none"
      disabled={disabled}
      aria-label={`New session on ${machine}`}
      title={where ? `New session on ${machine}, in ${where}` : `New session on ${machine}`}
      className="shrink-0 whitespace-nowrap"
      onClick={(event) => onPress(event.currentTarget)}
    >
      New session
    </Button>
  );
}

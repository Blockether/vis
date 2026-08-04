import {
  forwardRef,
  type ButtonHTMLAttributes,
  type InputHTMLAttributes,
  type ReactNode,
} from 'react';

// Ref-forwarding: a button that ANCHORS something (a popover, a focus return) has
// to be measurable by its owner, and cloning the element's classes at the call site
// to get a bare <button> is how a design system drifts.
export const Button = forwardRef<
  HTMLButtonElement,
  ButtonHTMLAttributes<HTMLButtonElement> & {
    variant?: 'solid' | 'ghost' | 'quiet' | 'danger' | 'contrast';
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
    // Dark chrome for a segment that must NOT read as a second accent block (the
    // caret half of a split button). The dialog-title pair is the palette's own
    // title-bar ink + its guaranteed-legible foreground, so it tracks the theme
    // instead of hardcoding black.
    contrast: `border-dialog-title bg-dialog-title text-dialog-title-foreground hover:border-dialog-title hover:bg-dialog-title/85 ${dimmed}`,
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
      <header className="relative flex min-h-9 items-center justify-center bg-dialog-title px-12 py-1.5 text-dialog-title-foreground sm:min-h-8">
        <h2 className="truncate font-mono text-body font-bold tracking-wide">{title}</h2>
        {onClose && (
          <button
            type="button"
            className="absolute inset-y-0 right-0 grid min-w-9 place-items-center border-l border-dialog-title-foreground/20 font-mono text-title text-dialog-title-foreground/70 transition-colors hover:bg-err/15 hover:text-err focus-visible:bg-err/15 focus-visible:text-err focus-visible:outline-none sm:min-w-8"
            onClick={onClose}
            aria-label="Close dialog"
          >
            ✕
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
 * The strip used to paint the number and a `●` glyph as one text run: the glyph
 * carries its own metrics and drops below the digits' optical centre, so the
 * green pair read as one smudged token hanging low in a chip whose every other
 * part is centred. There is no room for the word "live" beside a machine name,
 * so the count goes in SQUARE brackets instead: the brackets in the host's own
 * ink, the number in green. It is nothing but text, so it sits on the label's
 * baseline by construction, and colour is what still says "live" — with the word
 * itself kept for anyone who cannot see the colour.
 *
 * Square rather than round, because the unread badge beside it is a FILLED
 * rectangle: brackets make the two counts one shape in two weights — outlined
 * box = running, filled box = waiting — instead of a round token sitting next to
 * a block. In a monospaced strip both glyphs occupy one cell, so the chip is
 * exactly as wide either way.
 */
export function LiveTally({ count }: { count: number }) {
  return (
    <span className="whitespace-nowrap">
      [<span className="font-bold text-ok">{count}</span>]
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

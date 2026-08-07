import {
  createContext,
  forwardRef,
  useContext,
  type ButtonHTMLAttributes,
  type InputHTMLAttributes,
  type ReactNode,
} from 'react';

import type { MachineColor } from '../lib/machine-colors';
import { createPortal } from 'react-dom';

import { ChevronIcon, CloseIcon, DotsIcon, DraftIcon } from './icons';

// Ref-forwarding: a button that ANCHORS something (a popover, a focus return) has
// to be measurable by its owner, and cloning the element's classes at the call site
// to get a bare <button> is how a design system drifts.
export const Button = forwardRef<
  HTMLButtonElement,
  ButtonHTMLAttributes<HTMLButtonElement> & {
    variant?: 'solid' | 'ghost' | 'quiet' | 'danger' | 'overlay';
    /**
     * Press feedback. `scale` is the default nudge; `none` is for a button that
     * ANCHORS something (a popover) or sits in a segmented group — a transform
     * moves the box the menu was measured against and makes the group breathe
     * under the finger.
     */
    pressEffect?: 'scale' | 'none';
    /**
     * `compact` is the DESKTOP rhythm of a header row: a 24px box, centred in a
     * taller row, at the meta type scale. It lives here and only here, because a
     * row where the primary is 32px and the `⋯` beside it is 24px is precisely
     * the incoherence this app was reported for. Touch is untouched — a finger
     * still gets the full box.
     */
    density?: 'default' | 'compact';
  }
>(function Button(
  { variant = 'solid', pressEffect = 'scale', density = 'default', className = '', ...props },
  ref,
) {
  // Disabled colours live PER VARIANT, not in the base class: `quiet` has to stay
  // frameless while it is busy, and a shared `disabled:border-edge` would fight it
  // on equal specificity (whoever Tailwind emits last wins).
  const dimmed = 'disabled:border-edge disabled:bg-panel-2 disabled:text-muted';
  // ONE hover system, and it only ever moves the SURFACE.
  //
  // Each variant used to invent its own, and each said something different from
  // "you are on it": `solid` FADED the amber to 85%, so the primary looked like it
  // was switching off under the cursor; `ghost` and `quiet` flared their ink and
  // their frame amber, which is more attention than a hover has earned and is a
  // second amber on a screen whose primary is already amber; `danger` poured a
  // solid red fill under `text-white`, and `--color-white` in this app is the
  // PAGE's ink (`--fg`) — #262626 on #dc2626 is 2.3:1, a control that becomes
  // unreadable exactly when the pointer is on it.
  //
  // The press already answers the finger (`active:scale`) and the keyboard already
  // has its ring, so hover is the quietest of the three: the paper changes, the ink
  // does not. `solid` has no hover at all — a filled amber slab is as arrived as a
  // control gets, and there is nothing for a hover to add.
  const styles = {
    solid: `border-accent bg-accent text-accent-foreground ${dimmed}`,
    ghost: `border-edge-strong bg-transparent text-white hover:bg-hover ${dimmed}`,
    // For a SECONDARY action sitting next to a solid primary: two bordered boxes
    // side by side read as rivals, so this one keeps the button's box (transparent
    // border, identical metrics) and NEVER draws a frame — not at rest and not on
    // hover. A frame that arrives under the pointer is a box appearing out of
    // nowhere around a glyph that was never a box; on touch there is no pointer at
    // all, so tapping the `⋯` simply boxed it and left it boxed. The surface moving
    // already says "you are on it", and the keyboard still gets its own ring.
    quiet:
      'border-transparent bg-transparent text-dialog-hint hover:bg-hover disabled:border-transparent disabled:bg-transparent disabled:text-muted',
    // The red stays INK and the fill stays a wash, exactly as `MenuItem`'s danger
    // row does — one destructive language in both.
    danger: `border-err/40 bg-err/10 text-err hover:border-err hover:bg-err/20 ${dimmed}`,
    // A control that floats over CONTENT — a thumbnail, a picture — rather than over
    // chrome. It carries its own ink because whatever is under it is not the app's
    // paper, and it is a VARIANT rather than a class at the call site because
    // `bg-transparent` and `bg-ink/80` are decided by Tailwind's emission order, never
    // by which of the two the call site happened to type last.
    overlay:
      'border-transparent bg-ink/80 text-dialog-hint hover:bg-hover hover:text-accent-ink disabled:border-transparent disabled:bg-panel-2 disabled:text-muted',
    // A split button's caret half is NOT a second variant: it is `solid` with a
    // hairline in `accent-foreground`. Dark chrome next to an amber primary reads
    // as a disabled slab, and nobody presses a control that looks switched off.
  }[variant];
  // The transform utilities are OMITTED rather than overridden: `active:scale-100`
  // and `active:scale-[0.98]` have equal specificity, so a call-site override would
  // be decided by Tailwind's emission order, not by the call site.
  const press = pressEffect === 'scale' ? 'active:scale-[0.98] disabled:active:scale-100' : '';
  // A 32px FACE with a 44px TARGET.
  //
  // The box used to BE 44px on touch, so the amber "New session" slab filled the
  // whole header band edge to edge and read as a toolbar rather than as a button —
  // on a 390px iPhone it was the loudest thing on the screen. A hit box is not a
  // paint job: the visible control is now the header's own 32px rhythm (`min-h-8`,
  // the same box the desktop `sm:` step already uses) and the missing 6px above and
  // below are restored as an invisible `::after` that the finger still lands on.
  // So Apple's 44pt target survives untouched while the ink stops shouting; the
  // `⋯` beside it shrinks by exactly the same amount, because a header that holds
  // one 32px button and one 44px button holds two different affordances.
  const scale =
    density === 'compact'
      ? 'relative h-8 self-center after:absolute after:inset-x-0 after:-top-1.5 after:-bottom-1.5 after:content-[""] mouse:h-6 mouse:min-h-6 mouse:text-meta mouse:after:content-none'
      : '';

  return (
    <button
      ref={ref}
      className={`min-h-7 rounded-none border px-2.5 py-0.5 text-meta font-bold transition-[background-color,border-color,color,opacity,transform,translate,scale,rotate] duration-150 ${press} focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 disabled:cursor-not-allowed disabled:opacity-100 disabled:shadow-none motion-reduce:transition-none sm:min-h-8 sm:px-3 sm:text-ui ${scale} ${styles} ${className}`}
      {...props}
    />
  );
});

/**
 * An icon-only control is still a BUTTON.
 *
 * A kebab, a close, a retry: they carry no word, so they used to be written by
 * hand at the call site — and the machine header's `⋯` ended up a 32px bordered
 * box while the project header's, one row below it, was a 44px borderless slab
 * with a bigger glyph. Two controls that do the same thing looked like two
 * different affordances, and neither looked like the yellow button beside them.
 *
 * So it is `Button` with its word replaced by a glyph: the same box, border,
 * focus ring, transition and desktop rhythm as every other button in the app.
 * `pressEffect="none"` because these anchor menus and sheets — a transform moves
 * the box the popover was measured against.
 */
export const IconButton = forwardRef<
  HTMLButtonElement,
  ButtonHTMLAttributes<HTMLButtonElement> & {
    /** Icon-only, so the name is not optional. */
    label: string;
    variant?: 'solid' | 'ghost' | 'quiet' | 'danger' | 'overlay';
    /** Passed through: a control over a thumbnail is not on a header's rhythm. */
    density?: 'default' | 'compact';
    /**
     * This button ENDS a list row, so it owns the row's trailing edge.
     *
     * A centred glyph in a box that stops at the gutter is not the same distance
     * from the paper's edge as the glyph that starts the row: measured on the
     * desktop list, the leading chevron's ink sat 19px inside the panel and the
     * trailing `⋯`'s ink sat 30px, because the box respected the gutter and then
     * centred a 12px glyph inside 28px of its own. The margins matched and the INK
     * did not, which is the only one of the two an eye can see.
     *
     * So the gutter moves INSIDE the button: the box runs to the paper's edge and
     * pads its glyph away from it by exactly `LIST_EDGE`. The ink is symmetric, and
     * the target grows to the edge instead of leaving a dead 16px strip beside it.
     */
    edge?: boolean;
  }
>(function IconButton(
  { label, className = '', variant = 'ghost', density = 'compact', edge, children, ...props },
  ref,
) {
  // `border-r-0`: this box ends ON the paper's own edge, and the paper already draws
  // that line. A hover frame that redraws it puts two hairlines in one pixel column.
  //
  // `h-auto self-stretch`: it ends a ROW, so its hover is the row's own height. The
  // compact scale's 32px face centred in a 44px row painted a floating band with a
  // 6px dead strip above and below it — the trash in "Manage projects" hovered short
  // of the row it belongs to. Stretching also makes the invisible `after:` reach
  // pointless, so it is dropped (`after:content-none`) rather than left to overhang.
  const box = edge
    ? 'h-auto min-w-10 justify-items-end self-stretch border-r-0 pl-0 pr-3 -mr-3 after:content-none sm:min-w-12 sm:pr-4 sm:-mr-4 mouse:min-w-10'
    : 'min-w-7 place-items-center px-0 sm:min-w-8 sm:px-0 mouse:min-w-6';
  return (
    <Button
      ref={ref}
      type="button"
      variant={variant}
      pressEffect="none"
      density={density}
      aria-label={label}
      className={`grid shrink-0 items-center ${box} ${className}`}
      {...props}
    >
      {children}
    </Button>
  );
});

/**
 * THE OVERFLOW CONTROL, and there is only one of it.
 *
 * Every `⋯` in the app is this: the machine header's, the project header's one row
 * below it, the one on an artifact tile. They kept drifting apart because each call
 * site spelled out its own box, its own glyph size and its own popup semantics —
 * different borders, a different right edge, a different height — while all three
 * mean exactly "the rarer half of what this row can do".
 *
 * `quiet` on purpose: a header is chrome, and a bordered box around a glyph reads as
 * a second rival to the yellow verb beside it. The frame arrives on hover and focus,
 * where it answers "can I press this"; `overlay` is the same control over a picture,
 * which has to bring its own ink to stay legible.
 */
export const KebabButton = forwardRef<
  HTMLButtonElement,
  ButtonHTMLAttributes<HTMLButtonElement> & {
    /** It carries no word, so it names what it acts on: `Actions for tower`. */
    label: string;
    /** Whether the menu it owns is open right now. */
    isOpen?: boolean;
    variant?: 'quiet' | 'overlay';
    density?: 'default' | 'compact';
  }
>(function KebabButton(
  { label, isOpen, variant = 'quiet', density = 'compact', className = '', ...props },
  ref,
) {
  return (
    <IconButton
      ref={ref}
      label={label}
      variant={variant}
      density={density}
      edge
      className={className}
      aria-haspopup="menu"
      aria-expanded={isOpen}
      {...props}
    >
      <DotsIcon className="size-3" />
    </IconButton>
  );
});

/**
 * THE WAY OUT, and there is only one of it.
 *
 * A dialog, an opened artifact and the artifacts sheet itself are all left the
 * same way: a close welded to the right edge of the band that titles them,
 * separated by that band's own hairline, going red only under the pointer —
 * closing is not a destructive act until you mean it. Every one of those three
 * surfaces used to spell the same forty classes out again, which is how the
 * artifacts sheet ended up wearing a bordered chip in a strip of bordered chips
 * where every other surface wears chrome.
 *
 * `tone` is the paper it sits on, because that is the only thing that differs:
 * the dark title bar of a dialog, or a panel band.
 */
export function DialogClose({
  label,
  tone = 'title',
  className = '',
  onClose,
}: {
  label: string;
  tone?: 'title' | 'panel';
  className?: string;
  onClose: () => void;
}) {
  const skin =
    tone === 'title'
      ? 'border-dialog-title-foreground/20 text-dialog-title-foreground/70'
      : 'border-dialog-edge text-dialog-hint';
  return (
    <button
      type="button"
      onClick={onClose}
      aria-label={label}
      className={`grid min-w-9 shrink-0 place-items-center border-l transition-colors duration-150 hover:bg-err/15 hover:text-err focus-visible:bg-err/15 focus-visible:text-err focus-visible:outline-none motion-reduce:transition-none mouse:min-w-8 ${skin} ${className}`}
    >
      <CloseIcon />
    </button>
  );
}

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

/**
 * ONE DESKTOP BOX for every dialog: the same height, whatever is inside it.
 *
 * A dialog that sizes itself to its content makes the scrim jump — "Manage projects"
 * stood two-thirds of the window tall while the question beside it was a strip. Above
 * `sm:` they are the same rectangle and the CONTENT scrolls inside it; below it the
 * sheet is simply the whole phone.
 */
export const DIALOG_DESKTOP_HEIGHT = 'sm:h-[min(38rem,100%)]';

/**
 * THE ONE MODAL: a FULL-SCREEN sheet on a phone, a fixed dialog over a scrim on a desktop.
 *
 * iOS taught the thumb that a surface which arrives from the bottom edge is the one
 * it can reach and flick away, so `Menu` already slid up from there while a dialog
 * with the very same job — "Manage projects" — dropped into the middle of the glass.
 * Two layers, two physics, one hand. The sheet wins below `sm:`: it takes the WHOLE
 * glass, owns both safe areas itself, and travels its own height on the way in.
 *
 * `Menu` is the other half of that contract — a sheet on a phone, a popover under
 * the control it came from. Between them they are every layer this app puts over
 * itself, so no screen writes the safe-area scrim out by hand again; two of them
 * had already drifted into two copies of the same forty characters.
 */
export function Modal({
  onDismiss,
  children,
}: {
  onDismiss: () => void;
  children: ReactNode;
}) {
  return createPortal(
    <div
      className="fixed inset-0 z-50 flex items-stretch justify-center bg-ink/85 backdrop-blur-[2px] transition-opacity duration-200 starting:opacity-0 motion-reduce:transition-none sm:items-center sm:pb-[max(1rem,env(safe-area-inset-bottom))] sm:pl-[max(1rem,env(safe-area-inset-left))] sm:pr-[max(1rem,env(safe-area-inset-right))] sm:pt-[max(1rem,env(safe-area-inset-top))]"
      role="presentation"
      onClick={onDismiss}
    >
      {/* ONE SIZE. On the phone a dialog IS the screen — full bleed, full height,
          so a list inside it gets every pixel the glass has and the verbs at its
          foot are always in the same place. From `sm:` up every dialog is the same
          box (`sm:max-w-xl`, `DIALOG_DESKTOP_HEIGHT`): a question and a file browser
          that open over the same screen used to be two different rectangles.

          The scrim is application settings' own — ink at 85% under a 2px blur, faded
          in rather than snapped on. That dialog was hand-rolled beside this one and
          was the better looking of the two, so its glass moved IN HERE and the copy
          moved out; `sm:max-w-xl` is its width, for the same reason. */}
      <div
        className={`flex w-full flex-col sm:max-w-xl ${DIALOG_DESKTOP_HEIGHT}`}
        role="presentation"
        onClick={(event) => event.stopPropagation()}
      >
        {children}
      </div>
    </div>,
    document.body,
  );
}

/**
 * THE HEADER OF EVERY SURFACE THAT OPENS OVER ANOTHER, and there is only one of it.
 *
 * There were seven, and no two agreed. Two heights (36px and 48px), two alignments
 * (a centred title in `DialogFrame` and the artifact overlay; a left title with a
 * subtitle in machine settings, application settings, the model picker and the paste
 * editor), two paddings, and four close buttons hand-built at the call site in two
 * different boxes — none of them the `DialogClose` this file says is the only way out.
 *
 * Left wins, because it is the only one of the two shapes that can hold a SUBTITLE,
 * and four of the seven needed one — the gateway a setting belongs to, the model
 * currently pinned, which pasted block is being edited. Centring also cost `px-12` of
 * dead space on both sides to clear a close button that is welded to one of them.
 *
 * The band is the list's own (`min-h-12 mouse:min-h-9`), so a dialog's header and a
 * machine's header are the same height on the same screen.
 */
export function DialogHeader({
  title,
  titleId,
  subtitle,
  closeLabel,
  onClose,
  className = '',
}: {
  title: ReactNode;
  /** For a surface labelled by `aria-labelledby` rather than `aria-label`. */
  titleId?: string;
  subtitle?: ReactNode;
  /** Names what it closes: three of these can be open over one another. */
  closeLabel?: string;
  onClose?: () => void;
  className?: string;
}) {
  return (
    <header
      className={`flex min-h-12 shrink-0 items-stretch bg-dialog-title text-dialog-title-foreground mouse:min-h-9 ${className}`}
    >
      <div className={`min-w-0 flex-1 self-center py-1.5 ${LIST_EDGE}`}>
        {/* A title can be a whole QUESTION from `vis.ask`, and a question clipped to
            one line is no longer one anybody can answer. So it wraps — bounded at
            three lines, which is the depth `HumanInputPrompt` was fixed to and pins.
            The band's height is a minimum, not a cap. */}
        <h2
          id={titleId}
          className="line-clamp-3 font-mono text-body font-bold tracking-wide"
          title={typeof title === 'string' ? title : undefined}
        >
          {title}
        </h2>
        {subtitle && (
          <p className="truncate font-mono text-meta text-dialog-title-foreground/70">
            {subtitle}
          </p>
        )}
      </div>
      {onClose && <DialogClose label={closeLabel ?? 'Close'} tone="title" onClose={onClose} />}
    </header>
  );
}

export function DialogFrame({
  title,
  subtitle,
  children,
  footer,
  onClose,
  className = '',
}: {
  title: string;
  /** The line under the title — which machine, which model, which paste. */
  subtitle?: ReactNode;
  children: ReactNode;
  footer?: ReactNode;
  onClose?: () => void;
  className?: string;
}) {
  return (
    <section
      className={`flex min-h-0 flex-1 flex-col overflow-hidden border-t-2 border-accent bg-panel pt-[env(safe-area-inset-top)] pb-[env(safe-area-inset-bottom)] shadow-none transition-[opacity,transform,translate,scale,rotate] duration-300 ease-[cubic-bezier(0.22,0.61,0.36,1)] starting:translate-y-full starting:opacity-0 motion-reduce:transition-none sm:border sm:border-dialog-edge sm:pt-0 sm:pb-0 sm:shadow-[8px_8px_0_var(--dialog-shadow)] sm:duration-200 sm:starting:translate-y-2 ${className}`}
      role="dialog"
      aria-modal="true"
      aria-label={title}
    >
      <DialogHeader title={title} subtitle={subtitle} closeLabel="Close dialog" onClose={onClose} />
      {/* A COLUMN, so a dialog that lays out its own regions gets a scrolling body and
          a docked footer. It used to be one plain scroll box: "Manage projects" put its
          own `flex-1` list and its own `New folder` / `Use project` footer inside it,
          nothing established a column, so the list grew to its content and the two verbs
          the sheet exists for scrolled off the bottom of it. */}
      <div className="flex min-h-0 flex-1 flex-col overflow-y-auto overscroll-contain border-t border-dialog-edge">
        {children}
      </div>
      {footer && (
        <footer className="shrink-0 border-t border-dialog-edge bg-panel-2 px-4 py-2 font-mono text-meta text-dialog-hint">
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
  return <div className="h-3 bg-ink" aria-hidden="true" />;
}

/**
 * ONE band, and every header in this list is it.
 *
 * Measured on a 390px iPhone: the machine banner stood 61px tall — a `py-2` of its own
 * wrapped around a 44px control — while the project header directly below it, wearing
 * the very same yellow verb and the very same `⋯`, stood 49px. Two headers that mean
 * the same thing must not be two boxes, so the height, the paper, the outgoing rule and
 * the `items-stretch` that lets a pressable half fill the band are decided HERE. A
 * control inside centres itself (`HeaderActions`); it never sets the row's height, and
 * a header never pads itself vertically around a control that is already 44px tall.
 *
 * Neither horizontal edge is spelled here: the leading one belongs to `HeaderTitle` /
 * `HeaderToggle` (a toggle's hover has to reach the edge of the screen) and the trailing
 * one to `HeaderActions`, so each edge stays one decision instead of two.
 */
/**
 * THE LADDER, and it is the only thing that says which of the three levels you are
 * looking at.
 *
 * Measured on the shipped list: the machine name and the project name were the SAME
 * PIXELS — 11px, weight 700, `#262626`, in a 36px band on `rgb(250,243,235)` — and a
 * session title one step deeper was 11px too. Three levels of ownership, one type
 * step out of the eight this app defines, and the deepest level wearing the TALLEST
 * band. The paper was supposed to carry the difference (`bg-panel` against
 * `bg-panel-2`) and cannot: `--surface` and `--panel2` are the same value in both
 * bundled palettes, so that distinction has never rendered.
 *
 * The ladder moves on THREE axes at once, because one of them alone is never enough
 * to be seen: 13px against 12px is a step you can measure and cannot notice.
 *
 *   level     type              band (touch / mouse)   paper
 *   machine   text-subhead 15   56 / 40                level-machine
 *   project   text-title   13   52 / 36                level-project
 *   session   text-ui      11   48 / 32                the page's own surface
 *
 * Two points of type per step, four pixels of band per step, and one derived step of
 * paper per level (see `--color-level-*` in `index.css`).
 *
 * The LEAF is the shortest, not the tallest. A session row used to stand 48px against
 * a 36px project band — the child bigger than the thing that contains it — which is
 * backwards however you argue it, and on a desktop it is also just a waste: the row
 * is ONE line there, so 32px holds it exactly. Touch keeps every level at 44px or
 * more, so the ladder survives a thumb.
 */
const HEADER_TYPE = {
  machine: 'text-subhead',
  project: 'text-title',
} as const;

/** The band a machine's header stands in, against the one a project's stands in. */
const HEADER_BAND = {
  machine: 'flex min-h-14 items-stretch mouse:min-h-10',
  project: 'flex min-h-13 items-stretch mouse:min-h-9',
} as const;

/**
 * Which level the header belongs to, read by the halves inside it.
 *
 * `SectionHeader` already knows its tone, so nothing below it restates one: a title
 * that took its own `tone` prop would be a second place for the answer to live, and
 * two places holding one answer is how the machine and the project came to be the
 * same 11px in the first place.
 */
const HeaderTone = createContext<'machine' | 'project'>('project');

/**
 * THE LEADING EDGE OF THE LIST, and every row in it starts here.
 *
 * Measured on a 390px iPhone before this was one value: the machine's mark began at
 * 14px, the project's disclosure at 14px but its NAME at 36px, a session's title at
 * 10px, and an opened session's usage rollup at 40px. Five leading edges down one
 * column of a list whose whole job is to show what contains what — and the session
 * titles, the deepest thing on the screen, started FURTHEST LEFT, so depth read
 * backwards. Containment is carried by the machine's rail and by the header's own
 * paper, exactly as this file's other comments claim; it is not carried by a
 * different indent per component, because that only ever produced five of them.
 */
export const LIST_EDGE = 'pl-3 sm:pl-4';

/**
 * THE INNER EDGE OF A PRESSABLE ROW, and the other half of `LIST_EDGE`.
 *
 * A row's pressable half is a HOVER SLAB: it fills the row from the leading edge up to
 * the trailing control cluster, and it PAINTS. Measured on a 390px iPhone, the session
 * row's own facts — the status badge, the timestamp — ended at 340 and the slab ended
 * at 340 too, so on hover the ink sat exactly on the boundary of its own highlight.
 * A slab needs the same air inside its trailing edge that `LIST_EDGE` gives its
 * leading one; the gap BETWEEN the slab and the cluster stays `LIST_TRAIL`'s business.
 */
export const LIST_EDGE_END = 'pr-3 sm:pr-4';

/**
 * THE LEFT EDGE OF THE LIST, worn by whatever is standing there.
 *
 * The card gives this side up entirely so that a machine's rail can BE the frame
 * rather than stand beside it: a coloured 2px line one pixel inside a grey one is
 * two lines doing one job, which is what the rail was removed for. Chrome bands wear
 * it in the edge ink; a machine block wears it in its own hue (`MachineRail`). Both
 * are 2px, and the card's right edge is 2px too, so the ink lands symmetrically
 * whichever of them is painting.
 */
export const LIST_FRAME = 'border-l-2 border-dialog-edge';

/**
 * THE TRAILING CONTROL COLUMN, and every row in the list ends in it.
 *
 * The other half of the same failure: a header's `⋯` stopped 12px short of the
 * screen while the session row's disclosure — one row below it, the same size glyph,
 * the same "there is more here" promise — ran flush to the edge. Two controls in
 * what the eye reads as one column, 12px apart, which is precisely the report that
 * some things have a margin and the chevrons beside them do not.
 *
 * So the gap in front of the cluster, the gap between its controls and where it
 * stops are decided once, HERE, and a row that ends in a control wears this.
 */
// The cluster ALWAYS owns the gutter, because what ends a row is not fixed: a
// project header drops its `⋯` while a filter is live (a group showing 3 of 40
// matches must not offer a control that deletes 40), and the amber verb becomes the
// last thing in the row. An `edge` IconButton reclaims this padding with a matching
// negative margin, so a bare GLYPH runs to the paper while a filled BOX stops at the
// gutter — and both are true whichever one happens to be last.
const LIST_TRAIL = 'flex shrink-0 items-stretch gap-2 self-stretch pl-2 pr-3 sm:pr-4';

/**
 * The leading GLYPH of a header, in a box the width of the widest one.
 *
 * A machine is marked by a 6px identity block and a project by a 14px disclosure, so
 * with each sized to its own ink the machine's name began at x=28 and the project's
 * name directly below it at x=36 — the last 8px of the same misalignment, surviving
 * inside the two components that had just been taught to share every other edge.
 * One column, and the names start together.
 */
const HEADER_GLYPH = 'grid size-3.5 shrink-0 place-items-center';

export function SectionHeader({
  tone,
  rule,
  children,
}: {
  tone: 'machine' | 'project';
  /**
   * A border-colour class for the band's OUTGOING rule, when that rule carries
   * meaning — a machine's own hue. It replaces the hairline rather than joining it:
   * a coloured line beside a grey one is the double border this list was reported
   * for, and the band only ever draws one.
   */
  rule?: string;
  children: ReactNode;
}) {
  // A machine ORGANIZES everything under it, so its band is the taller of the two,
  // the darkest paper of the three, and sticks to the top of the scroller; a project
  // is a section inside that machine and sits one step nearer the page.
  const paper =
    tone === 'machine' ? 'sticky top-0 z-10 bg-level-machine' : 'bg-level-project';
  const edge = rule ? `border-b-2 ${rule}` : 'border-b border-dialog-edge';
  return (
    <HeaderTone.Provider value={tone}>
      <header className={`${HEADER_BAND[tone]} ${edge} ${paper}`}>{children}</header>
    </HeaderTone.Provider>
  );
}

/**
 * A machine header is the sticky `SectionHeader`: its rail and hue distinguish the
 * computer from its projects while its band, its edges and its trailing cluster are
 * the same ones every project header below it wears. It fills the machine block just
 * like a session row; leaving a scrollbar-sized side margin made the machine boundary
 * look clipped and introduced a false horizontal edge on narrow screens.
 */
export function MachineBanner({ children }: { children: ReactNode }) {
  // No coloured rule here: the hue is the RAIL now, and one machine wearing its
  // colour twice in the same corner is the barcode this list was reported for.
  return <SectionHeader tone="machine">{children}</SectionHeader>;
}

/**
 * The leading half of a header that only NAMES its section: an optional mark, then the
 * name, truncated. It takes the width the trailing cluster leaves and no more.
 */
export function HeaderTitle({
  mark,
  name,
  qualifier,
  qualifierTitle,
}: {
  mark?: ReactNode;
  name: ReactNode;
  /**
   * What the name alone cannot settle — the address behind a machine's label, the
   * way a project header carries the path behind its folder name.
   *
   * All three levels of this list say the same sentence now: a NAME, then the thing
   * that tells two of them apart, then what the row reports. Rendered nothing when a
   * machine has no label of its own, because then the address IS the name and
   * printing it twice is not a hierarchy.
   */
  qualifier?: ReactNode;
  qualifierTitle?: string;
}) {
  const tone = useContext(HeaderTone);
  return (
    // The glyph centres against the LINE (`items-center`, as `HeaderToggle` does it)
    // while the name and its qualifier share a BASELINE inside it. Baseline-aligning
    // the mark alongside them drops a 10px block below the ink it belongs to.
    <span className={`flex min-w-0 flex-1 items-center gap-2 ${LIST_EDGE}`}>
      {mark && <span className={HEADER_GLYPH}>{mark}</span>}
      <span className="flex min-w-0 items-baseline gap-2">
        <span
          className={`shrink-0 truncate font-mono font-bold text-white ${qualifier ? 'max-w-[60%]' : 'min-w-0'} ${HEADER_TYPE[tone]}`}
        >
          {name}
        </span>
        {qualifier && (
          <span
            className="min-w-0 truncate font-mono text-chip text-dialog-hint"
            title={qualifierTitle}
          >
            {qualifier}
          </span>
        )}
      </span>
    </span>
  );
}

/**
 * The leading half of a header that OPENS its section: the same title voice as
 * `HeaderTitle`, plus a disclosure and the path the name alone cannot give.
 *
 * It is a BUTTON that fills the band, so the hover reaches both the screen edge and the
 * band's own rules, and it is `flex-1 min-w-0` so the NAME gets whatever the trailing
 * cluster leaves. It used to carry a fixed 160px count column inside it, which on a
 * phone left `~/vis` rendering as `~/v…`; counts report through `HeaderActions` now,
 * exactly as the machine header above it reports its own.
 */
export function HeaderToggle({
  isOpen,
  onToggle,
  name,
  path,
  pathTitle,
}: {
  isOpen: boolean;
  onToggle: () => void;
  name: ReactNode;
  path: ReactNode;
  pathTitle?: string;
}) {
  return (
    <button
      type="button"
      aria-expanded={isOpen}
      onClick={onToggle}
      className={`flex min-w-0 flex-1 items-center gap-2 py-1.5 text-left transition-colors duration-150 hover:bg-hover focus-visible:bg-hover focus-visible:outline-none motion-reduce:transition-none mouse:py-0 ${LIST_EDGE} ${LIST_EDGE_END}`}
    >
      <span className={HEADER_GLYPH}>
        <ChevronIcon open={isOpen} className="size-3.5 text-dialog-hint" />
      </span>
      {/* Name and path on ONE line, sharing a baseline. Stacked, this header stood two
          lines tall while the machine header directly above it — the same band, the
          same job — stood one, so the two never read as peers; and the path, which is
          the thing that tells two `vis` projects apart, was the smaller of two lines
          in a row already crowded by a count, a yellow verb and a `⋯`.
          The NAME holds its ground and the PATH gives way: the name is the identity
          and the path only qualifies it, so the path is the one that truncates — with
          the whole thing on `title` for the row that needs it spelled out. */}
      <span className="flex min-w-0 items-baseline gap-2">
        <span
          className={`max-w-[60%] shrink-0 truncate font-mono font-bold text-white ${HEADER_TYPE.project}`}
        >
          {name}
        </span>
        <span className="min-w-0 truncate font-mono text-chip text-dialog-hint" title={pathTitle}>
          {path}
        </span>
      </span>
    </button>
  );
}

/**
 * The trailing half of a row: what it REPORTS, then what it OFFERS.
 *
 * It owns the right edge of every row in the list — headers and session rows alike —
 * which is why no row pads that side itself. A machine header padded its own right
 * edge while the project header one row below it ended flush, so the two `⋯` that were
 * finally the same button still sat at two different distances from the same screen
 * edge; the session rows below them then ran their disclosure flush to the screen, a
 * third distance. One component decides all of it now.
 */
export function HeaderActions({ children }: { children: ReactNode }) {
  return <span className={LIST_TRAIL}>{children}</span>;
}

/**
 * "There is more inside this row", and there is only one of it.
 *
 * The sibling of `KebabButton`: where the `⋯` holds the rarer VERBS of a row, this
 * holds the rest of its FACTS — a session's usage rollup, opened in place. They are
 * the same promise in two directions, so they are the same box, in the same column,
 * with the same border-on-hover and the same focus ring; only the glyph and the
 * `aria-expanded` differ. It was a hand-built 32px strip welded to the screen edge,
 * at 40% opacity, which is why it read as a decoration rather than as the control it
 * is — and why it never lined up with the `⋯` directly above it.
 *
 * Opacity is NOT the resting state: a control that fades to 0.4 to look quiet is one
 * that fails contrast while doing it. It rests in the same hint ink as every other
 * quiet glyph in the list and answers the pointer with the button's own frame.
 */
export const RowDisclosure = forwardRef<
  HTMLButtonElement,
  Omit<ButtonHTMLAttributes<HTMLButtonElement>, 'children'> & {
    /** It carries no word, so it names its row: `Show details for <session>`. */
    label: string;
    isOpen: boolean;
  }
>(function RowDisclosure({ label, isOpen, className = '', ...props }, ref) {
  return (
    <IconButton
      ref={ref}
      label={label}
      variant="quiet"
      edge
      aria-expanded={isOpen}
      className={className}
      {...props}
    >
      <ChevronIcon open={isOpen} className="size-3.5" />
    </IconButton>
  );
});

/** A header's own quiet voice: what it counts, in the list's monospace hint ink. */
export function HeaderMeta({ children }: { children: ReactNode }) {
  return (
    <span className="flex items-center gap-2 font-mono text-chip text-dialog-hint">
      {children}
    </span>
  );
}

/**
 * A header's own count, in `HeaderMeta`'s voice.
 *
 * The NUMBER is the fact and never leaves the screen; the noun is a courtesy. A
 * project header carries a yellow verb and a `⋯` beside it, so on a phone it is
 * `isCrowded` and the noun waits for `sm` — printing "699 sessions" in a fixed 160px
 * column is what truncated that project's own name to `~/v…`. A machine header has
 * the room and says the phrase.
 *
 * A screen reader always hears the whole phrase: the visible half is decoration.
 */
export function HeaderTally({
  count,
  unit,
  isCrowded = false,
}: {
  count: number;
  unit: string;
  /**
   * This header also carries a yellow verb and a `⋯`, so on a phone the NUMBER holds
   * the line alone and the noun waits for `sm`. A header with room says the phrase.
   */
  isCrowded?: boolean;
}) {
  const noun = count === 1 ? unit : `${unit}s`;
  return (
    <>
      <span className="sr-only">
        {count} {noun}
      </span>
      <span className="whitespace-nowrap" aria-hidden="true">
        {count}
        <span className={isCrowded ? 'hidden sm:inline' : ''}> {noun}</span>
      </span>
    </>
  );
}

/**
 * "3 live", with the same pulse a live session row wears. Nothing is said when
 * nothing is running: an idle project must not print a zero next to a green dot.
 */
export function LiveCount({ count }: { count: number }) {
  if (count <= 0) return null;
  return (
    <span className="inline-flex items-center gap-1 font-bold text-ok">
      <span className="size-1.5 animate-pulse bg-ok motion-reduce:animate-none" aria-hidden="true" />
      {count} live
    </span>
  );
}

/**
 * The rail that CONTAINS a machine: 2px of its own hue down everything it owns,
 * banner included. A project boundary is a hairline and a machine boundary is a
 * colour change, so where one computer ends is seen before it is read.
 *
 * It is the card's LEFT FRAME, not a line inside it — see `LIST_FRAME`. That is the
 * whole difference from the version that had to be removed: it doubled the card's
 * own border and, being a border, pushed every railed row 2px deeper than its own
 * trailing edge stopped. Now the frame is 2px on both sides and the rail simply
 * colours the one on the left.
 */
export function MachineRail({ color, children }: { color?: MachineColor; children: ReactNode }) {
  return (
    <div className={color ? `border-l-2 ${color.rail}` : LIST_FRAME}>{children}</div>
  );
}

/**
 * The machine's hue as a solid block, worn by its banner and its scope chip, so
 * the chip you tapped and the rail you got back are visibly the same machine.
 */
export function MachineMark({ color, size = 'inline' }: { color: MachineColor; size?: 'inline' | 'banner' }) {
  // A machine's identity block used to be `size-1.5` everywhere — the same 6px square,
  // at the same size, as the LIVE / WAITING / IDLE dot on every session row beneath
  // it. One shape meaning two things, and the SMALLEST glyph marking the HIGHEST
  // level. In a banner it is the mark of a whole computer and takes the glyph column;
  // riding inside a scope chip's text it stays the 6px it has to be.
  const box = size === 'banner' ? 'size-2.5' : 'size-1.5';
  return <span className={`${box} shrink-0 ${color.dot}`} aria-hidden="true" />;
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
 *
 * `onDraft` makes it a SPLIT button. A draft is the same verb in a different place —
 * this project, copied privately — so it is not a second word-button, and it is no
 * longer a row in the machine's `⋯` two headers up, where it named a machine rather
 * than the project it actually forks. It is the amber half joined to the amber verb,
 * carrying the draft mark and nothing else: one control, one colour, one edge, and
 * the rarer half costs the width of a glyph. It exists only when the app was told to
 * offer drafts; without it this is a plain button again, exactly as it was.
 */
export function NewSessionButton({
  machine,
  where,
  disabled,
  onPress,
  onDraft,
}: {
  machine: string;
  where?: string | null;
  disabled?: boolean;
  onPress: (anchor: HTMLElement) => void;
  /** Omitted while `Offer drafts` is off — then there is no second half at all. */
  onDraft?: (anchor: HTMLElement) => void;
}) {
  const verb = (
    <Button
      type="button"
      pressEffect="none"
      density="compact"
      disabled={disabled}
      aria-label={`New session on ${machine}`}
      title={where ? `New session on ${machine}, in ${where}` : `New session on ${machine}`}
      className={`shrink-0 whitespace-nowrap${onDraft ? ' border-r-0' : ''}`}
      onClick={(event) => onPress(event.currentTarget)}
    >
      New session
    </Button>
  );
  if (!onDraft) return verb;
  return (
    <span className="flex shrink-0 items-stretch">
      {verb}
      {/* The seam is the only thing between the two halves: same fill, same height, a
          hairline in the ink they both carry. A second bordered box beside the verb
          would read as a different control doing a different thing. */}
      <Button
        type="button"
        pressEffect="none"
        density="compact"
        disabled={disabled}
        aria-label={`New session in a draft of ${where ?? 'this project'} on ${machine}`}
        title={`New session in a draft — a private copy of ${where ?? 'this project'}`}
        className="shrink-0 border-l-accent-foreground/30 px-2"
        onClick={(event) => onDraft(event.currentTarget)}
      >
        <DraftIcon className="size-4" />
      </Button>
    </span>
  );
}

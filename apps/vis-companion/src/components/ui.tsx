import {
  createContext,
  forwardRef,
  useContext,
  useRef,
  useState,
  type ButtonHTMLAttributes,
  type InputHTMLAttributes,
  type MouseEvent,
  type ReactNode,
} from 'react';

import type { MachineColor } from '../lib/machine-colors';
import type { PullPhase } from '../lib/pull-to-search';
import type { RefObject } from 'react';
import { createPortal } from 'react-dom';

import {
  ArrowDownIcon,
  CheckIcon,
  ChevronIcon,
  CloseIcon,
  CopyIcon,
  DotsIcon,
  DraftIcon,
  PlusIcon,
  ProjectsIcon,
  SearchIcon,
} from './icons';

// Ref-forwarding: a button that ANCHORS something (a popover, a focus return) has
// to be measurable by its owner, and cloning the element's classes at the call site
// to get a bare <button> is how a design system drifts.
export const Button = forwardRef<
  HTMLButtonElement,
  ButtonHTMLAttributes<HTMLButtonElement> & {
    /**
     * FOUR RANKS AND ONE CONTEXT.
     *
     * The rank is what the reader is being told about the verb, and there are only
     * ever four of those: `primary` is the thing this screen exists for, `secondary`
     * is a real control beside it, `quiet` is chrome that must not draw a frame, and
     * `danger` is destructive. They used to be spelled `solid`/`ghost`, which name
     * the PAINT rather than the rank, so a call site had to know the palette to
     * choose; `inverse` was a fifth paint-name with one call site, and one screen's
     * "Add machine" is not a rank of its own.
     *
     * `overlay` is not a rank at all: it is the same control floating over CONTENT —
     * a thumbnail, a picture — where the app's paper is not underneath, so it has to
     * bring its own. It stays a variant because a face is decided here or it is
     * decided by Tailwind's emission order at a call site, never by a `className`.
     */
    variant?: 'primary' | 'secondary' | 'quiet' | 'danger' | 'overlay' | 'remove';
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
     *
     * `panel` is a SETTINGS PANEL's own verb. The panel reports in monospace at
     * the meta scale, so the buttons under its prose speak in that voice too, in
     * a 36px box on the rhythm of the rows above them. It used to be four copies
     * of the same forty characters of `className`, two hundred lines apart in one
     * file — which is how a fifth copy gets one utility wrong and nobody sees it.
     */
    density?: 'default' | 'compact' | 'panel';
    /**
     * This button stands INSIDE a segmented run — the image viewer's `− 100% +`.
     * The middle of the run drops its side frames so the three boxes draw ONE
     * outline rather than a doubled hairline in every seam. A frame is a face, so
     * it is a prop: two `border` utilities meeting at a call site are settled by
     * Tailwind's emission order and never by which one the call site typed.
     */
    isJoined?: boolean;
  }
>(function Button(
  {
    variant = 'primary',
    pressEffect = 'scale',
    density = 'default',
    isJoined = false,
    className = '',
    ...props
  },
  ref,
) {
  // Disabled colours live PER VARIANT, not in the base class: `quiet` has to stay
  // frameless while it is busy, and a shared `disabled:border-edge` would fight it
  // on equal specificity (whoever Tailwind emits last wins).
  const dimmed = 'disabled:border-edge disabled:bg-panel-2 disabled:text-muted';
  // ONE hover system, and it only ever moves the SURFACE.
  //
  // Each variant used to invent its own, and each said something different from
  // "you are on it": `primary` FADED the amber to 85%, so the primary looked like it
  // was switching off under the cursor; `secondary` and `quiet` flared their ink and
  // their frame amber, which is more attention than a hover has earned and is a
  // second amber on a screen whose primary is already amber; `danger` poured a
  // solid red fill under `text-white`, and `--color-white` in this app is the
  // PAGE's ink (`--fg`) — #262626 on #dc2626 is 2.3:1, a control that becomes
  // unreadable exactly when the pointer is on it.
  //
  // The press already answers the finger (`active:scale`) and the keyboard already
  // has its ring, so hover is the quietest of the three: the paper changes, the ink
  // does not. `primary` has no hover at all — a filled amber slab is as arrived as a
  // control gets, and there is nothing for a hover to add.
  const styles = {
    primary: `border-accent bg-accent text-accent-foreground ${dimmed}`,
    secondary: `border-edge-strong bg-transparent text-white hover:bg-hover ${dimmed}`,
    // For a SECONDARY action sitting next to the primary: two bordered boxes
    // side by side read as rivals, so this one keeps the button's box (transparent
    // border, identical metrics) and NEVER draws a frame — not at rest and not on
    // hover. A frame that arrives under the pointer is a box appearing out of
    // nowhere around a glyph that was never a box; on touch there is no pointer at
    // all, so tapping the `⋯` simply boxed it and left it boxed. The surface moving
    // already says "you are on it", and the keyboard still gets its own ring.
    quiet:
      'border-transparent bg-transparent text-dialog-hint hover:bg-hover disabled:border-transparent disabled:bg-transparent disabled:text-muted',
    // TAKING SOMETHING AWAY, when the mark is not the ✕ — a trash can that empties a
    // project of its transcripts. THE WAY OUT ITSELF IS `CloseButton`, the app's one
    // ✕, and never this: the variant carries only the INK the two share.
    //
    // It used to borrow `quiet`, whose ink is `text-dialog-hint` (#6f6a63): on the app
    // bar that put a pale grey mark beside a query and a `Preferences` both at #262626,
    // and an eye reads a faded mark as "disabled" rather than "press me". It carries the
    // page's own ink at rest and turns red only under the pointer, exactly like
    // `CloseButton` — one destructive language, one red.
    remove:
      'border-transparent bg-transparent text-white hover:bg-err/15 hover:text-err disabled:border-transparent disabled:bg-transparent disabled:text-muted',
    // The red stays INK and the fill stays a wash, exactly as `MenuItem`'s danger
    // row does — one destructive language in both.
    danger: `border-err/40 bg-err/10 text-err hover:border-err hover:bg-err/20 ${dimmed}`,
    // A control that floats over CONTENT — a thumbnail, a picture, a note's own first
    // lines — rather than over chrome. It carries its own ink because whatever is under
    // it is not the app's paper, and it wears the same black block every other floating
    // control wears: `bg-ink/80` was ink by NAME only, and in a light theme that token
    // resolves near-white, so the glyph disappeared into the page it sat on. It is a
    // VARIANT rather than a class at the call site because two competing `bg-*` are
    // settled by Tailwind's emission order, never by which one a call site typed last.
    overlay:
      'border-transparent bg-dialog-title text-dialog-title-foreground hover:bg-accent hover:text-accent-ink disabled:border-transparent disabled:bg-panel-2 disabled:text-muted',
    // A split button's caret half is NOT a second variant: it is `primary` with a
    // hairline in `accent-foreground`.
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
  const scale = {
    default: '',
    compact:
      'relative h-8 self-center after:absolute after:inset-x-0 after:-top-1.5 after:-bottom-1.5 after:content-[""] mouse:h-6 mouse:min-h-6 mouse:text-meta mouse:after:content-none',
    panel: 'min-h-9 px-3 font-mono text-meta',
  }[density];
  const joined = isJoined ? 'border-x-0' : '';

  return (
    <button
      ref={ref}
      className={`min-h-7 rounded-none border px-2.5 py-0.5 text-meta font-bold transition-[background-color,border-color,color,opacity,transform,translate,scale,rotate] duration-150 ${press} focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 disabled:cursor-not-allowed disabled:opacity-100 disabled:shadow-none motion-reduce:transition-none sm:min-h-8 sm:px-3 sm:text-ui ${scale} ${joined} ${styles} ${className}`}
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
    variant?: 'primary' | 'secondary' | 'quiet' | 'danger' | 'overlay' | 'remove';
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
  { label, className = '', variant = 'secondary', density = 'compact', edge, children, ...props },
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
  //
  // The cancellation is spelled at EVERY density the scale pins a height at, because
  // one `h-auto` cannot outrank a `mouse:h-6` written in a variant of its own: the
  // desktop list kept a 24px disclosure at the TOP of its 32px row, its chevron four
  // pixels above the title beside it.
  const box = edge
    ? 'h-auto min-w-10 justify-items-end self-stretch border-r-0 pl-0 pr-3 -mr-3 after:content-none sm:min-w-12 sm:pr-4 sm:-mr-4 mouse:h-auto mouse:min-w-10'
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
 *
 * THE EDGE BOX BELONGS TO THE ROW VERSION ONLY. A `⋯` that ends a row reclaims that
 * row's trailing gutter (`-mr-3 sm:-mr-4`) so its ink lands on the paper's edge; an
 * overlay ends no row — it is PLACED, `top-1 right-1` on an artifact tile — and that
 * same negative margin hung the glyph outside the card it belongs to.
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
      edge={variant !== 'overlay'}
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
 * A CHIP: one small word that is ON or OFF, and there is only one of it.
 *
 * A filter over the artifacts, a toggle's three choices, which transport an MCP
 * server speaks — the same question every time, and every screen used to answer
 * it in its own hand: the artifacts strip drew `min-h-7 … text-meta`, the
 * settings choices `min-h-8 … text-chip`, the transport row a third box with no
 * hover at all. Three sizes of the same control, on two screens a tap apart.
 *
 * Selection is the app's amber, exactly as `MachineTab` spells it, and OFF is
 * the quiet frame every other resting control wears — `border-edge-strong`, the
 * same hairline `Button`'s `secondary` and `Switch`'s OFF draw. It is NOT
 * `border-edge`: that is the FIELD hairline, which always has `bg-input` under it
 * to separate the box, and on its own it measures 1.18:1 against the page. An
 * enum toggle's choices and the boolean toggle's switch stand one row apart in
 * the same settings list, so they carry one frame or they read as two ranks of
 * control. A chip that leads nowhere (a filter with nothing behind it) is
 * `disabled` and says so by fading, never by inventing a fourth face.
 */
export const Chip = forwardRef<
  HTMLButtonElement,
  ButtonHTMLAttributes<HTMLButtonElement> & { isOn?: boolean }
>(function Chip({ isOn = false, className = '', ...props }, ref) {
  return (
    <button
      ref={ref}
      type="button"
      aria-pressed={isOn}
      className={`inline-flex min-h-7 shrink-0 items-center justify-center gap-1.5 border px-2 font-mono text-meta font-bold transition-colors duration-150 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 disabled:cursor-not-allowed disabled:opacity-40 motion-reduce:transition-none mouse:min-h-6 ${
        isOn
          ? 'border-accent bg-accent text-accent-foreground'
          : 'border-edge-strong bg-transparent text-dialog-hint hover:bg-hover'
      } ${className}`}
      {...props}
    />
  );
});

/**
 * "THERE IS MORE OF THIS BELOW", and there is only one of it.
 *
 * The transcript's attachments and the artifacts sheet page the same way, so
 * they were the same promise in two faces: a 44px left-aligned bar in
 * `text-footer-muted` under the message, a 32px centred bar in `text-dialog-hint`
 * inside the sheet — and each spelled its own arrow beside its own words. The
 * arrow belongs to the control, not to the caller, and `label` is what a screen
 * reader hears ("Load 12 more artifacts") while the children are what the eye
 * reads.
 */
export function LoadMore({
  label,
  isEarlier = false,
  className = '',
  children,
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & {
  label: string;
  /**
   * The rest is ABOVE: the transcript's older turns. Same promise, same bar, the
   * arrow turned over — a second component for it would be the same forty classes
   * again with one of them different.
   */
  isEarlier?: boolean;
}) {
  return (
    <button
      type="button"
      aria-label={label}
      className={`mt-2 flex min-h-8 w-full min-w-0 items-center justify-center gap-1.5 border border-dialog-edge bg-panel px-2 font-mono text-meta text-dialog-hint transition-colors duration-150 hover:bg-hover focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 motion-reduce:transition-none mouse:min-h-7 ${className}`}
      {...props}
    >
      <ArrowDownIcon className={`size-3 opacity-70 ${isEarlier ? 'rotate-180' : ''}`} />
      <span className="min-w-0 truncate">{children}</span>
    </button>
  );
}

/**
 * COPY THIS, and there is only one of it.
 *
 * A code block's `Copy` chip and the session id beside the title are the same
 * control: press, the clipboard takes it, the chip says so for a moment and goes
 * back. Both used to own that state, that timeout, that `catch` for a webview
 * with no clipboard, and two different faces around it — and both had to keep
 * their own width so "Copied" would not shove the row.
 *
 * The chip also lives inside pressable things (a `<summary>`, a header), so it
 * ALWAYS stops the click it consumed: copying a snippet must never also toggle
 * the disclosure it sits in.
 */
export function CopyChip({
  value,
  label,
  title,
  className = '',
  children,
}: {
  /** What lands on the clipboard. */
  value: string;
  /** What the control is called: "Copy code", "Copy session id". */
  label: string;
  /** Hover text, when there is more to say than the label — the full id. */
  title?: string;
  /** Placement only; the chip's own face is fixed. */
  className?: string;
  /** What it reads at rest. */
  children: ReactNode;
}) {
  const [isCopied, setIsCopied] = useState(false);
  async function copy(event: MouseEvent<HTMLButtonElement>) {
    event.preventDefault();
    event.stopPropagation();
    try {
      await navigator.clipboard.writeText(value);
      setIsCopied(true);
      window.setTimeout(() => setIsCopied(false), 1_500);
    } catch {
      // Clipboard access can be unavailable in an untrusted mobile webview.
    }
  }
  return (
    <button
      type="button"
      onClick={copy}
      aria-label={label}
      title={title ?? label}
      // The minimum width keeps "Copy" and "Copied" the same box, so the chip
      // never jumps under the finger that just pressed it.
      className={`group inline-flex h-6 min-w-[6ch] items-center justify-center gap-1 border bg-button px-2 text-center font-mono text-chip transition-colors duration-150 hover:bg-hover focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 motion-reduce:transition-none ${
        isCopied ? 'border-ok text-ok' : 'border-dialog-edge text-button-foreground'
      } ${className}`}
    >
      {isCopied ? (
        <CheckIcon className="size-3 text-ok" />
      ) : (
        <CopyIcon className="size-3 opacity-60 transition-opacity group-hover:opacity-100" />
      )}
      <span className="min-w-0 truncate">{isCopied ? 'Copied' : children}</span>
    </button>
  );
}

/**
 * A ROW YOU PRESS, and there is only one of it.
 *
 * A provider, a model, a saved gateway, an artifact's older version, a preset to
 * sign in with: a full-width slab, its content left-aligned, that opens or picks
 * the thing it names. Five screens spelled that out five times — `min-h-12` here
 * and `min-h-11` there, `hover:bg-hover` with and without a focus paper, a frame
 * on some and none on others — so rows doing one job read as several.
 *
 * `isFramed` is the only real difference: a row standing on the page needs no
 * frame, a row standing INSIDE a card needs one. Selection is the same in both:
 * the amber edge over the raised paper, never a second colour.
 */
export const ListRow = forwardRef<
  HTMLButtonElement,
  ButtonHTMLAttributes<HTMLButtonElement> & {
    isSelected?: boolean;
    isFramed?: boolean;
  }
>(function ListRow(
  { isSelected = false, isFramed = false, className = '', ...props },
  ref,
) {
  const paper = isFramed
    ? `border ${isSelected ? 'border-accent bg-panel-2' : 'border-dialog-edge bg-panel'}`
    : isSelected
      ? 'bg-panel-2'
      : '';
  return (
    <button
      ref={ref}
      type="button"
      className={`flex min-h-12 w-full min-w-0 items-center gap-2 px-3 py-2 text-left transition-colors duration-150 hover:bg-hover focus-visible:bg-hover focus-visible:outline-none disabled:cursor-default disabled:opacity-50 disabled:hover:bg-transparent motion-reduce:transition-none ${paper} ${className}`}
      {...props}
    />
  );
});

/**
 * THE ANSWER TO A DESTRUCTIVE QUESTION, ASKED IN THE ROW ITSELF, and there is
 * only one of it.
 *
 * Deleting one session and forgetting one machine are the same moment: the row
 * is already under the thumb, the question has two answers, and hiding the list
 * behind a scrim to ask it makes a confirmation look like a destination. So the
 * confirm IS the row — the two answers split its own width and stand its full
 * height, and neither of them is a 28px target inside a dialog.
 *
 * The refusal comes FIRST and takes the focus, because the safe answer is the
 * one a mistaken thumb should land on; the committing half wears the red wash
 * `MenuItem`'s danger row and the swipe strip's `Delete` wear, so the ink that
 * means "this does not come back" is the same ink wherever it is asked.
 *
 * IT WEARS ITS OWN FRAME, AND THE COST IS ASKED INSIDE IT. The block REPLACES
 * the row it is asking about, so that row is off the screen and every edge
 * around the question was the list's own neutral divider — the same 1px rule
 * two calm rows share. Reported over a machine's providers: `Signs out of
 * OpenAI Codex…` hung under the OpenCode Go row and read as THAT row's meta
 * line, and the refusal, standing on the panel's own paper with no edge of any
 * kind, did not read as a control at all. So the group is boxed in `err-edge`
 * and the cost sentence is a PROP inside that box — one question, framed in
 * the ink it is asking in, and no call site spelling the sentence its own way.
 *
 * The question is also the group's own LABEL, for a reader who cannot see the
 * box it is asked in.
 */
export function ConfirmRow({
  question,
  cost,
  keepLabel = 'No, keep',
  confirmLabel,
  isBusy = false,
  onKeep,
  onConfirm,
}: {
  /** What is being asked, for a reader who cannot see the row: `Delete alpha?`. */
  question: string;
  /** What committing COSTS, in one sentence, standing inside the same frame. */
  cost?: ReactNode;
  /** The refusal, when keeping is not called "No, keep". */
  keepLabel?: string;
  /** The commitment, carrying its own progress while it runs: `Deleting...`. */
  confirmLabel: string;
  isBusy?: boolean;
  onKeep: () => void;
  onConfirm: () => void;
}) {
  return (
    // The box's own edge REPLACES the list rule above it, so the question is
    // bounded ONCE on every side instead of by a neutral hairline stacked on a
    // red one; the rule below it is already this box's own bottom border.
    <div
      role="group"
      aria-label={question}
      className="-mt-px border border-err-edge"
    >
      {cost !== undefined && (
        // The rule that separates the cost from its two answers belongs to the
        // SENTENCE, not to the answers: a border on that row would eat a pixel
        // of the 48px both answers owe a finger.
        <p className="border-b border-err-edge px-3 py-2 font-mono text-chip text-dialog-hint">
          {cost}
        </p>
      )}
      <div className="flex min-h-12 items-stretch mouse:min-h-8">
        <button
          type="button"
          autoFocus
          className="flex flex-1 items-center justify-center bg-panel-2 font-mono text-meta font-bold uppercase tracking-[0.08em] text-white/70 transition-colors duration-150 hover:bg-hover hover:text-white focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-accent/60 motion-reduce:transition-none"
          onClick={onKeep}
        >
          {keepLabel}
        </button>
        <button
          type="button"
          disabled={isBusy}
          className="flex flex-1 items-center justify-center border-l border-err-edge bg-err-surface font-mono text-meta font-bold uppercase tracking-[0.08em] text-err-ink transition-colors duration-150 hover:bg-err hover:text-white active:bg-err active:text-white focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-err/70 disabled:opacity-60 motion-reduce:transition-none"
          onClick={onConfirm}
        >
          {confirmLabel}
        </button>
      </div>
    </div>
  );
}

/**
 * THE WEIGHT A BAND'S NAME WEARS, pressable or not.
 *
 * `PYTHON` over a program, `RESULT` over what it printed, `THINKING` over the
 * reasoning that produced it: one word, in caps, saying what the band under it
 * holds. It is ONE weight because a row the reader cannot press must read as
 * the same kind of row as the one they can — `Disclosure` tone `step` is this
 * ink on a button, `BandLabel` is it standing on its own, and the TUI paints
 * the same names in the same weight (`render/band-label`).
 */
const BAND_NAME = 'font-extrabold tracking-[0.06em]';

/**
 * A TRACE ROW YOU EXPAND, and there is only one of it.
 *
 * A tool step's header, the THINKING band and an attachment rail's summary all
 * ask the same question — "show me the rest of this" — and each spelled its own
 * answer: three chevrons (one of them a rotated `›`), three heights, three
 * hovers, and only two of them tagged `data-disclosure-toggle`, which is what
 * the transcript uses to keep the scroll anchored when a row opens.
 *
 * `tone` is the ink of the thing it opens, because that is the only difference —
 * ink INCLUDING the slant: the reasoning a thinking band opens is set in italic,
 * so that band's own name is italic, and bold with it.
 * The height follows the pointer, never the width: 32px under a finger, the
 * tight 24px rhythm only where there is a cursor.
 */
export function Disclosure({
  isOpen,
  tone = 'muted',
  bleed = false,
  className = '',
  children,
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & {
  isOpen: boolean;
  tone?: 'step' | 'thinking' | 'muted';
  /**
   * Gives the row's own gutter back: the chevron lines its ink up with the card's
   * leading edge while the press target keeps the padding a finger needs. It is
   * the row's spacing, so it belongs to the row rather than to a `-ml-2 px-2`
   * pair spelled at a call site, where the two halves can drift apart.
   */
  bleed?: boolean;
}) {
  const ink =
    tone === 'step'
      ? `${BAND_NAME} text-accent-ink hover:bg-hover`
      : tone === 'thinking'
        ? 'font-bold italic tracking-[0.07em] text-thinking hover:text-dialog-hint-key'
        : 'text-footer-muted hover:bg-hover';
  return (
    <button
      type="button"
      data-disclosure-toggle
      aria-expanded={isOpen}
      className={`flex min-h-8 w-full min-w-0 cursor-pointer select-none items-center gap-1.5 text-left font-mono text-chip transition-colors duration-150 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 motion-reduce:transition-none mouse:min-h-6 ${bleed ? '-ml-2 px-2' : ''} ${ink} ${className}`}
      {...props}
    >
      <ChevronIcon open={isOpen} className="size-3 shrink-0 opacity-70" />
      {children}
    </button>
  );
}

/**
 * THE NAME OF A BAND THAT DOES NOT OPEN.
 *
 * A program's header when the whole program is already on screen, a result card
 * whose value carried no tally: there is nothing to disclose, so the row is not
 * a `Disclosure` — but it is the same NAME in the same weight. Without it those
 * rows said nothing at all, a chevron and a duration standing for whatever the
 * band happened to hold.
 */
export function BandLabel({
  className = '',
  children,
}: {
  className?: string;
  children: ReactNode;
}) {
  return (
    <span
      className={`select-none truncate font-mono text-chip ${BAND_NAME} text-accent-ink ${className}`}
    >
      {children}
    </span>
  );
}

/**
 * THE COUNT BESIDE A BAND'S NAME, and it never takes the name's weight.
 *
 * `+3 more`, `+8 more`: what is HIDDEN is not what the band IS, so the name
 * stays the one constant the eye can find down the column and the tally steps
 * back out of it.
 */
export function BandTally({ children }: { children: ReactNode }) {
  return <span className="font-normal tracking-normal">{children}</span>;
}

/**
 * AN OPTION YOU PICK, and there is only one of it.
 *
 * The human-input form's checkbox and its select/multiselect options are the
 * same control asking the same question — a status glyph, a label, a frame that
 * turns amber when it is the answer — and each spelled its own class list, so a
 * checkbox hovered its frame and an option did not. What differs is the GLYPH
 * (`HUMAN_INPUT_CHOICE_MARKS`: `●`/`○` for a choice of one, `[✓]`/`[ ]` for a
 * choice of any) and the ARIA the caller passes, never the face.
 */
export function ChoiceRow({
  isOn,
  mark,
  className = '',
  children,
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & {
  isOn: boolean;
  /** The status glyph, decorative: the label beside it carries the meaning. */
  mark: ReactNode;
}) {
  return (
    <button
      type="button"
      className={`flex w-full min-w-0 items-center gap-2 border px-2.5 py-1 text-left font-mono text-meta transition-colors duration-150 focus-visible:border-accent focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-accent/30 disabled:cursor-not-allowed disabled:text-muted motion-reduce:transition-none sm:text-ui ${
        isOn
          ? 'border-accent bg-hover text-accent-ink'
          : 'border-edge bg-input text-white hover:border-accent'
      } ${className}`}
      {...props}
    >
      <span aria-hidden="true">{mark}</span>
      <span className="min-w-0 truncate">{children}</span>
    </button>
  );
}

/**
 * THE WAY BACK, and there is only one of it.
 *
 * A full-screen surface that stands ON another one — a session over its list —
 * leaves by the leading half of its own title band: a stretched, notch-aware
 * column carrying one chevron. It is not an `IconButton`: an icon button is a
 * box inside a row, and this one IS the row's left edge, so it owns the safe
 * area the phone puts outside the paper and grows with the band's height.
 */
export function BackButton({
  label,
  className = '',
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & { label: string }) {
  return (
    <button
      type="button"
      aria-label={label}
      className={`grid w-[calc(2.75rem+env(safe-area-inset-left))] shrink-0 place-items-center border-r border-dialog-edge bg-dialog-title pl-[env(safe-area-inset-left)] font-mono text-subhead font-bold text-dialog-title-foreground transition-[background-color,transform,translate,scale,rotate] duration-150 hover:bg-accent-2 focus-visible:bg-accent-2 focus-visible:outline-none active:scale-[0.96] motion-reduce:transition-none mouse:w-[calc(2.5rem+env(safe-area-inset-left))] ${className}`}
      {...props}
    >
      <ChevronIcon back className="size-4" aria-hidden />
    </button>
  );
}

/**
 * A CONTROL THAT ARRIVES OVER THE CONTENT, and there is only one of it.
 *
 * Not chrome: it is laid on top of the thing it acts on — "Latest" over the
 * transcript — so it brings its own paper, its own lift and its own entrance
 * (`starting:`), and the call site decides only WHERE it lands. `Button` cannot
 * be it: a button's face is flat because it sits ON the app's paper, and one
 * that floats has to be told from the sentence it is covering.
 */
export function Pill({
  className = '',
  children,
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement>) {
  return (
    <button
      type="button"
      className={`inline-flex min-h-8 items-center gap-1.5 border border-dialog-edge bg-button px-3 font-mono text-meta font-bold text-button-foreground shadow-[4px_4px_0_var(--dialog-shadow)] transition-[opacity,transform,translate,scale,rotate,background-color] duration-150 hover:bg-hover focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 active:scale-[0.97] starting:translate-y-2 starting:opacity-0 motion-reduce:transition-none ${className}`}
      {...props}
    >
      {children}
    </button>
  );
}

/**
 * A ROW OF A COMPLETION LIST, and there is only one of it.
 *
 * `@file` and `/command` are the same gesture answered twice, and they were the
 * same forty classes written twice — including the one that matters and is easy
 * to forget: the pointer press is CANCELLED, because a completion list must not
 * take the caret out of the composer it is completing. That is behaviour, so it
 * belongs to the control and not to whoever remembers it.
 *
 * `isActive` is the keyboard's position in the list, which is why it is the
 * app's amber and not a hover: a finger and an arrow key are pointing at two
 * different rows and the reader has to be able to tell which is which.
 */
export function OptionRow({
  isActive = false,
  className = '',
  children,
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & { isActive?: boolean }) {
  return (
    <button
      type="button"
      role="option"
      aria-selected={isActive}
      onPointerDown={(event) => event.preventDefault()}
      className={`grid min-h-9 w-full gap-3 border-t border-dialog-edge px-3 py-1.5 text-left transition-colors duration-150 motion-reduce:transition-none ${
        isActive
          ? 'bg-accent text-accent-foreground'
          : 'text-dialog-foreground hover:bg-hover'
      } ${className}`}
      {...props}
    >
      {children}
    </button>
  );
}

/**
 * THE COMPOSER'S OWN CONTROLS, and there is one of them.
 *
 * Attach, dictate, send, stop: four boxes in one strip that were written four
 * times, and they had drifted apart in the only dimension a strip is read in —
 * two were 32×28 with a `mouse:` step, the send was a 32px square, the stop had
 * no rhythm at all. They also each re-spelled the same transition list and the
 * same `active:scale-[0.94]`, and none of them had a focus ring.
 *
 * `tone` is what the control MEANS, and the box follows from it: `quiet` is a
 * glyph in the strip, `send` is the verb the strip exists for, `stop` fills the
 * slot the send left, `recording` is `quiet` while it is listening. Nothing here
 * is a `className` at the call site, because a strip whose boxes disagree is
 * exactly what this replaced.
 */
export function ComposerButton({
  label,
  tone = 'quiet',
  isHolding = false,
  className = '',
  children,
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & {
  /** Icon-only, so the name is not optional. */
  label: string;
  tone?: 'quiet' | 'send' | 'stop' | 'recording' | 'voice';
  /**
   * A press-and-hold is UNDERWAY. The paper rises through the button for as
   * long as the switch takes, so the gesture reports itself while it happens
   * instead of only when it lands — the one confirmation available to an app
   * with no haptics.
   */
  isHolding?: boolean;
}) {
  const face = {
    quiet: 'h-8 w-7 text-dialog-hint hover:bg-hover hover:text-dialog-hint-key disabled:text-muted mouse:h-7 mouse:w-6',
    recording:
      'h-8 w-7 animate-pulse bg-warn-surface text-err disabled:text-muted motion-reduce:animate-none mouse:h-7 mouse:w-6',
    // The MODE, not an action: the button keeps the strip's box and changes its
    // paper, so "which microphone am I holding" is answered by the control
    // itself rather than by a badge stuck to its corner.
    voice:
      'h-8 w-7 bg-accent text-accent-foreground hover:bg-accent-2 disabled:bg-button disabled:text-muted mouse:h-7 mouse:w-6',
    send: 'size-8 border border-dialog-edge bg-dialog-title text-ui font-bold text-dialog-title-foreground hover:bg-accent-2 disabled:scale-100 disabled:bg-button disabled:text-dialog-hint mouse:size-7',
    // It stands in the send's slot, which is already the right size: taking the
    // whole of it is how the two never disagree about where the strip ends.
    stop: 'size-full border border-err bg-cancelled hover:bg-warn-surface starting:scale-90 starting:opacity-0',
  }[tone];
  return (
    <button
      type="button"
      aria-label={label}
      className={`relative grid shrink-0 place-items-center transition-[background-color,color,opacity,transform,translate,scale,rotate] duration-150 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-accent/60 active:scale-[0.94] motion-reduce:transition-none ${face} ${className}`}
      {...props}
    >
      {isHolding && (
        <span
          aria-hidden="true"
          className="pointer-events-none absolute inset-0 origin-bottom scale-y-100 bg-accent/30 transition-transform duration-[450ms] ease-linear starting:scale-y-0 motion-reduce:hidden"
        />
      )}
      <span className="relative grid place-items-center">{children}</span>
    </button>
  );
}

/**
 * WHAT THIS TURN WILL RUN AS, and there is only one of it.
 *
 * The line under the composer reports the model and the reasoning level, and
 * both are pressable: one opens the picker, one cycles. They are the same small
 * caps at the same size and they had two different hovers, so the strip read as
 * one label beside one button. `isPicker` is the only difference that survived —
 * the dotted rule under the word that OPENS something.
 */
export function MetaButton({
  isPicker = false,
  className = '',
  children,
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & { isPicker?: boolean }) {
  return (
    <button
      type="button"
      className={`px-1 py-1 text-left font-mono text-chip font-semibold uppercase tracking-[0.08em] transition-colors duration-150 hover:text-accent-ink focus-visible:text-accent-ink focus-visible:outline-none motion-reduce:transition-none ${
        isPicker
          ? 'text-dialog-hint-key underline decoration-dialog-edge decoration-1 underline-offset-4 hover:decoration-accent'
          : 'text-dialog-hint'
      } ${className}`}
      {...props}
    >
      {children}
    </button>
  );
}

/**
 * PRESSABLE PROSE, and there is only one of it.
 *
 * A queued turn you can still edit, a pasted block standing in for 40 lines:
 * text in a row that opens an editor. Neither is a button-shaped thing and
 * neither should become one — but they hovered differently (one flared its ink,
 * one moved its paper) for the same gesture. Hover moves the SURFACE here as it
 * does everywhere else; `isToken` adds the dotted rule that says this word is
 * standing in for something longer.
 */
export function TextButton({
  isToken = false,
  className = '',
  children,
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & { isToken?: boolean }) {
  return (
    <button
      type="button"
      className={`min-w-0 px-1 text-left font-mono text-ui text-dialog-foreground transition-colors duration-150 hover:bg-hover focus-visible:bg-hover focus-visible:outline-none disabled:cursor-not-allowed disabled:hover:bg-transparent motion-reduce:transition-none ${
        isToken ? 'truncate underline decoration-dotted underline-offset-2' : ''
      } ${className}`}
      {...props}
    >
      {children}
    </button>
  );
}

/**
 * ONE VALUE OF A SETTING, and there is only one of it.
 *
 * Theme, sessions per project, where a session starts: a segmented grid where
 * every cell is one of the values and exactly one of them is yours. Three
 * hand-spelled copies of it had already drifted apart in height and gap.
 *
 * It is a CELL and not a `ChoiceRow`, and the difference is real rather than
 * cosmetic: the grid draws the hairlines (`gap-px` over `bg-dialog-edge`), so a
 * cell that framed itself would double every line in the grid. A `ChoiceRow`
 * stands on its own and brings its own frame. Selection is the amber FILL here,
 * as it is on every other segmented thing in the app (`Chip`, `MachineTab`,
 * `OptionRow`), and the glyph is `dialogs/choice-mark`'s own `●`/`○` — one of
 * these is the answer, never several.
 */
export function ChoiceCell({
  title,
  sub,
  isSelected,
  className = '',
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & {
  title: string;
  /** The quiet word under the name: a theme's mode, a page size's temper. */
  sub: string;
  isSelected: boolean;
}) {
  return (
    <button
      type="button"
      aria-pressed={isSelected}
      className={`flex min-h-10 min-w-0 items-center justify-between gap-3 px-3 py-1.5 text-left transition-[background-color,color,transform,translate,scale,rotate] duration-150 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-accent active:scale-[0.99] disabled:opacity-45 motion-reduce:transition-none mouse:min-h-9 ${
        isSelected ? 'bg-accent text-accent-foreground' : 'bg-input text-white hover:bg-hover'
      } ${className}`}
      {...props}
    >
      <span className="min-w-0">
        <span className="block truncate font-mono text-ui font-bold">{title}</span>
        <span className="block truncate font-mono text-chip uppercase tracking-wider opacity-65">
          {sub}
        </span>
      </span>
      <span className="shrink-0 font-mono text-meta font-black" aria-hidden="true">
        {isSelected ? '●' : '○'}
      </span>
    </button>
  );
}

/**
 * ON OR OFF, and there is only one of it.
 *
 * A feature toggle is a WORD, not a sliding knob: `ON`/`OFF` in the same mono
 * the rest of the app is set in, amber when it is on. It reports its own work
 * (`isBusy` → `··`, `aria-busy`) because a setting is a round trip to a gateway
 * and a control that snaps back a second later without saying why is a bug
 * report. `role="switch"` and `aria-checked` are the control's, not the caller's.
 *
 * OFF WEARS THE RESTING FRAME, because a switched-off toggle had NO BOX AT ALL.
 * It drew `border-transparent` over `bg-panel-2`, and `--panel2` is the same value
 * as `--surface` in both bundled palettes (#faf3eb light, #161820 dark): the fill
 * that was supposed to be the control's own paper measures 1.00:1 against the row
 * it sits in, and the border was transparent by name — so a settings row ended in
 * a grey word floating on the page, reported as "these off buttons are not visible
 * at all: why no border?". The frame is `border-edge-strong` (1.38:1 light,
 * 1.40:1 dark), the hairline `Button`'s `secondary` already draws — the button
 * this one stands beside on an MCP server row — and the fill is dropped, so the
 * switch reads on whatever paper it is put on rather than carrying a step that
 * has never once rendered. ON keeps the amber slab and names its own frame, so
 * the two faces differ in colour and never in box.
 *
 * The hover moves the SURFACE and not the ink, exactly as `Button` spells it: the
 * ink used to jump from `text-dialog-hint` to `text-white` under the pointer,
 * which is the app's one hover system saying two things at once.
 */
export function Switch({
  label,
  isOn,
  isBusy,
  className = '',
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & {
  /** Icon-only in effect — `ON` is not a name — so the name is not optional. */
  label: string;
  isOn: boolean;
  isBusy?: boolean;
}) {
  return (
    <button
      type="button"
      role="switch"
      aria-label={`${label}: ${isOn ? 'on' : 'off'}`}
      aria-checked={isOn}
      aria-busy={isBusy}
      className={`mt-0.5 inline-flex h-8 w-[3.25rem] shrink-0 items-center justify-center border font-mono text-chip font-black tracking-[0.08em] transition-colors duration-150 ease-out focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 active:scale-[0.97] disabled:opacity-45 motion-reduce:transition-none motion-reduce:active:scale-100 mouse:h-6 ${
        isOn
          ? 'border-accent bg-accent text-accent-foreground'
          : 'border-edge-strong bg-transparent text-dialog-hint hover:bg-hover'
      } ${className}`}
      {...props}
    >
      <span aria-hidden className={isBusy ? 'animate-pulse' : ''}>
        {isBusy ? '··' : isOn ? 'ON' : 'OFF'}
      </span>
    </button>
  );
}

/**
 * IS THIS DEVICE CONNECTED TO THIS MACHINE — the answer, and the verb that
 * changes it.
 *
 * The notifications panel used to answer an OPERATOR's question instead: it
 * listed every push token the gateway holds, so one iPhone reinstalled three
 * times stood in it four times under four masked tokens, and the reader's own
 * question — am I connected? — survived only as the verb on a button. Reported
 * as: same device, four entries, and no way to just see whether alerts arrive.
 *
 * THE ROW IS THE VERB, AND NOTHING ELSE. The control used to be `Switch`, whose
 * entire face is the state it is ALREADY in, standing under two lines that had
 * just said it: `Not connected`, then `<machine> will not alert this device.`,
 * then a box reading `OFF` — the same no three times over. Dropping the switch
 * for a verb left the two lines behind, and they were reported again: the panel
 * is too big, I want one Connect/Disconnect button there. `Disconnect` can only
 * stand where this device IS connected, so the button's own word carries the
 * state and the panel's title already names the machine. `Switch` still belongs
 * to a setting this device owns outright (a feature toggle, an MCP server) where
 * the press IS the new state; this one is a round trip to a machine that can
 * refuse, and a round trip is a verb.
 *
 * The words live in this component rather than at each call site because native
 * APNs/FCM and Web Push are two transports for ONE question and must never
 * become two vocabularies.
 */
export function NotifyConnectionRow({
  machine,
  isOn,
  isBusy = false,
  isChecking = false,
  disabled = false,
  onClick,
}: {
  /** The paired machine this row speaks for; it names itself in the sentence. */
  machine: string;
  isOn: boolean;
  isBusy?: boolean;
  /** Before the first answer lands, what this device is registered for is unknown. */
  isChecking?: boolean;
  disabled?: boolean;
  onClick: () => void;
}) {
  // ONE CONTROL, AND IT IS THE VERB. Reported over the settings dialog: the row
  // carried a verdict line, a sentence naming the machine and the button — three
  // ways to say the same yes or no, in a panel already titled with that machine.
  // `Disconnect` can only stand where this device IS connected, so the button's
  // own word is the state; only the not-yet-answered case needs saying, and it
  // says it on the button.
  const label = isChecking
    ? 'Checking…'
    : isBusy
      ? isOn
        ? 'Disconnecting…'
        : 'Connecting…'
      : isOn
        ? 'Disconnect'
        : 'Connect';
  const isWaiting = isBusy || isChecking;
  const action = isChecking
    ? `Asking ${machine} whether this device is registered`
    : isOn
      ? `Disconnect notifications from ${machine}`
      : `Connect notifications from ${machine}`;

  return (
    // FULL WIDTH ON A PHONE. A lone verb hugging the right edge of a full-bleed
    // panel reads as a leftover of a row that lost its text; the thumb is also
    // nowhere near it. Below `sm:` the button IS the row.
    <div className="flex items-center justify-end px-3 py-2">
      {/* Connecting is the invitation and wears the amber; disconnecting is the
          way out of something already working and never shouts to be taken. */}
      <Button
        variant={isOn ? 'secondary' : 'primary'}
        density="panel"
        aria-label={action}
        aria-busy={isWaiting}
        disabled={disabled}
        onClick={onClick}
        className="w-full justify-center sm:w-auto sm:shrink-0"
      >
        <span className={isWaiting ? 'animate-pulse' : ''}>{label}</span>
      </Button>
    </div>
  );
}

/**
 * THE ✕, AND THERE IS EXACTLY ONE OF IT.
 *
 * Eight surfaces were left by three different buttons in five different boxes,
 * measured on an iPhone 14 with the shipped stylesheet: a 36×48 band-ender welded
 * to a dialog title, the same button at 36×44 on a menu band, again at 36×44
 * carrying its own black paper on the artifacts strip, a 28×28 chip-ender on a
 * queued turn, 28×28 with a different hairline on a pasted block, 28×32 hung over
 * an attachment thumbnail, and a 40×30 one inside the fleet search. Three ink
 * rules and four hairlines between them. They had already been made one MARK; they
 * were never one BUTTON, and an eye reads the box before it reads the stroke.
 *
 * So there is one, and everything about its face is decided here:
 *
 * - TWO SQUARES, AND THE COMPONENT PICKS WHICH — never the call site. A ✕ is either
 *   the CELL THAT ENDS A BAND (`isBand`: 48×48, 36×36 for a mouse, stretched to the
 *   band's own height so the hairline that welds it runs the whole band) or the MARK
 *   INSIDE ANOTHER CONTROL (32×32, `mouse:size-6`, centred — a composer chip, a
 *   queued row, the fleet search's Clear). Both are squares, both are hung at the
 *   trailing edge where a way out is looked for, and neither is chosen at a call
 *   site, so the five arbitrary boxes this replaced cannot come back. The band cell
 *   is one box and not one per band, because every band that hosts it stands at the
 *   same height (`min-h-12`, `mouse:min-h-9`): the dialog title and the menu heading
 *   alike. The 32px mark used to end those bands too, and it was the wrong box for
 *   a band twice over — a square floating in the middle of a 48px row with its
 *   hairline stopping 8px short at both ends, and a 32×32 target for the one gesture
 *   that leaves a screen, under the app's own 44px minimum.
 * - THE INK OF THE SURFACE IT STANDS ON (`text-current`), and that same ink at 20%
 *   for the hairline that welds it. A band declares its foreground once — a dialog
 *   title bar `text-dialog-title-foreground`, a menu's accent heading, a panel strip
 *   the page ink it inherits anyway — and the way out inherits the same token, so one
 *   entry moves both the words and the ✕. Spelling the page's own `--fg` here instead
 *   put a #f3f4f6 mark on the #ffc420 heading of `blockether-dark` at 1.5:1 while that
 *   band's own words were #0f1117; measured on the live menu, the ✕ disagreed with its
 *   band in five of the six shipped themes and agreed in one by coincidence.
 * - RED ONLY UNDER THE POINTER, and as a wash rather than a fill: closing is not a
 *   destructive act until you mean it. It is the same red `MenuItem`'s danger row
 *   uses, so there is one destructive language in the app.
 *
 * WHERE it sits is the call site's only business: `className` may POSITION it (the
 * attachment chip hangs it on the chip's right edge) and nothing else. There is no
 * tone, no size and no hairline prop, because those three were the whole difference
 * between the eight buttons this replaces; `isBand` is not a size but a PLACE — it
 * says this ✕ is a band's own last cell rather than a mark inside another control.
 */
export function CloseButton({
  label,
  isBand = false,
  className = '',
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & {
  /** Icon-only, so the name is not optional: "Close artifacts", "Remove notes.md". */
  label: string;
  /**
   * This ✕ IS THE BAND'S LAST CELL — a dialog title, a menu heading — rather than a
   * mark inside another control. It fills the band's height and stands as wide as
   * that band is tall, so the way out of a screen is a square you can hit and its
   * hairline is the band's own full-height edge.
   */
  isBand?: boolean;
}) {
  return (
    <button
      type="button"
      aria-label={label}
      title={label}
      className={`grid shrink-0 place-items-center border-l border-current/20 text-current transition-colors duration-150 hover:bg-err/15 hover:text-err focus-visible:bg-err/15 focus-visible:text-err focus-visible:outline-none disabled:cursor-not-allowed disabled:hover:bg-transparent disabled:hover:text-current motion-reduce:transition-none ${
        isBand
          ? 'w-12 self-stretch mouse:w-9'
          : 'size-8 self-center mouse:size-6'
      } ${className}`}
      {...props}
    >
      <CloseIcon />
    </button>
  );
}

/**
 * A VERB IN A BAND, and there is only one of it.
 *
 * `CloseButton isBand` is the cell that ENDS a band; this is that same cell with a
 * WORD in it, for a band that also offers something — re-poll this fleet, go where
 * the accounts live. It fills the band's height, is welded by the band's own
 * hairline, and takes the band's ink, so the trailing end of a title bar reads as
 * one run of cells rather than as buttons parked on a heading.
 *
 * It is not `Button`: a bordered box on a title band claims a rank chrome has not
 * earned, and a 32px face centred in a 48px band is a smaller target than the ✕ one
 * hairline away from it — two boxes for the two things a band can do.
 */
export function BandButton({
  className = '',
  children,
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement>) {
  return (
    <button
      type="button"
      className={`grid shrink-0 place-items-center self-stretch whitespace-nowrap border-l border-current/20 px-3 font-mono text-meta font-bold text-current transition-colors duration-150 hover:bg-current/10 focus-visible:bg-current/10 focus-visible:outline-none disabled:cursor-not-allowed disabled:opacity-60 disabled:hover:bg-transparent motion-reduce:transition-none sm:px-4 sm:text-ui mouse:px-3 mouse:text-meta ${className}`}
      {...props}
    >
      {children}
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

/**
 * The app bar's fleet-wide search.
 *
 * A FIELD is a control, so it wears the vocabulary's own face: flat corners, the
 * app's own border and its type step. It used to be a hand-rolled `<label
 * className="… h-8 rounded border … bg-input">`: a white-filled, rounded slab on
 * paper that carries no other box at rest, so the quietest thing on the bar was
 * also the loudest.
 *
 * Resting it is PAPER — border only, no fill — and the input surface plus the ring
 * arrive with the caret, which is the same rule the terminal already follows
 * (`t/input-field-bg` active, flat at rest). The Clear glyph is the field's own, so
 * emptying the query returns the caret rather than dropping focus onto the document.
 *
 * IT LEADS WITH THE GLASS. Every guideline for a header search says the same two
 * things — keep the text field OPEN in the bar, and mark it with a magnifying glass —
 * so the mark sits inside the box, before the caret, in the hint's own ink. Without it
 * the widest framed box on the screen announced nothing about itself.
 *
 * IT WEARS THE BAR'S OWN FACE AND CARRIES THE FINGER'S TARGET AROUND IT. The bar is
 * a 32px rhythm (`Button`, 24px for a mouse), and a framed box 12px taller than every
 * control beside it is the loudest thing on the row — which is exactly what `h-11`
 * looked like. So the face is `h-8 mouse:h-6`, and the 44px touch rectangle arrives
 * the way `Button` already does it: as invisible slop. It is split into TWO strips,
 * above and below, rather than one overlay, because an overlay across the FACE would
 * hand every press to the `<label>` and a press inside the text must place a caret
 * where it landed. A mouse gets no strips at all — they would only eat the row above.
 *
 * BOTH OF ITS INKS SIT AT THE SAME INSET. Clear is the app's one `CloseButton`, and the
 * field gives back the inset it would otherwise sit inside (`-me-3 sm:-me-4`, exactly the
 * `px-3 sm:px-4` it pads with), so the square runs to the field's own border and centres
 * its mark there. Centred in its own 28px box INSIDE that inset, the ✕ used to stop about
 * 20px short of the border while the placeholder started 10px in, and an eye reads that
 * asymmetry as a control that missed its corner.
 */
export const SearchField = forwardRef<
  HTMLInputElement,
  {
    value: string;
    onValue: (value: string) => void;
    /** Spoken name; the placeholder is the promise, this is the label. */
    label: string;
    placeholder?: string;
    /** POSITION only (`w-full`, `flex-1`, an order); the face belongs here. */
    className?: string;
  }
>(function SearchField({ value, onValue, label, placeholder, className = '' }, ref) {
  const own = useRef<HTMLInputElement | null>(null);
  return (
    <label
      className={`relative flex h-8 min-w-0 items-center gap-2 self-center rounded-none border border-edge-strong bg-transparent px-3 transition-[background-color,border-color,box-shadow] duration-150 before:absolute before:inset-x-0 before:-top-1.5 before:h-1.5 before:content-[''] after:absolute after:inset-x-0 after:-bottom-1.5 after:h-1.5 after:content-[''] focus-within:border-accent focus-within:bg-input focus-within:ring-1 focus-within:ring-accent/30 motion-reduce:transition-none mouse:h-6 mouse:before:content-none mouse:after:content-none sm:px-4 ${className}`}
    >
      <SearchIcon className="size-3.5 shrink-0 text-dialog-hint" />
      {/* A SEARCH field, so the phone offers its own search key and stops correcting:
          a machine name, a project folder and a session title are not prose. The
          platform's own cancel button is hidden because this field already has one,
          and two clear controls in one box is one too many. */}
      <input
        ref={(node) => {
          own.current = node;
          if (typeof ref === 'function') ref(node);
          else if (ref) (ref as RefObject<HTMLInputElement | null>).current = node;
        }}
        value={value}
        onChange={(event) => onValue(event.target.value)}
        type="search"
        enterKeyHint="search"
        autoCorrect="off"
        autoCapitalize="none"
        spellCheck={false}
        className="min-w-0 flex-1 appearance-none bg-transparent font-mono text-ui text-white outline-none placeholder:text-dialog-hint mouse:text-meta [&::-webkit-search-cancel-button]:hidden"
        placeholder={placeholder}
        aria-label={label}
      />
      {value ? (
        <CloseButton
          label="Clear search"
          className="-me-3 sm:-me-4"
          onClick={() => {
            onValue('');
            own.current?.focus();
          }}
        />
      ) : null}
    </label>
  );
});

/**
 * WHAT A PULL AT THE TOP OF THE LIST IS ABOUT TO DO.
 *
 * The band the search page arrives behind, riding down over the first header of
 * the list while a finger drags it (`lib/pull-to-search`). It is the whole
 * report this gesture gets: with no haptics on this app, a pull that showed
 * nothing until the finger lifted would be a gesture nobody could learn.
 *
 * It says what the NEXT thing the hand does will mean — `Pull to search` while
 * the pull is short of the threshold, `Release to search` once lifting would
 * open the page — so the reader is never told about a state, only about their
 * own next move. Armed, it takes the accent, because the one moment worth
 * seeing out of the corner of an eye is the moment the gesture becomes real.
 *
 * It is `HEADER_BAND`'s own height and paper: it stands in for the band it
 * covers rather than adding a second, thinner strip above it, and it is
 * decoration for a touch gesture, so it is never announced and never pressed.
 */
export function PullToSearchHint({ phase }: { phase: PullPhase }) {
  const isShown = phase !== 'none';
  return (
    <div
      aria-hidden="true"
      className={`pointer-events-none absolute inset-x-0 top-0 z-20 flex min-h-13 items-center justify-center gap-2 border-b border-dialog-edge bg-level-project font-mono text-meta transition-[translate,opacity] duration-150 motion-reduce:transition-none mouse:min-h-9 ${
        isShown ? 'translate-y-0 opacity-100' : '-translate-y-full opacity-0'
      } ${phase === 'armed' ? 'text-accent' : 'text-dialog-hint'}`}
    >
      <SearchIcon className="size-3.5 shrink-0" />
      {phase === 'armed' ? 'Release to search' : 'Pull to search'}
    </div>
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
/**
 * WHICH sheet a `DialogFrame` is standing in — the one thing it cannot see about
 * itself. A `fit` sheet stops at its content and is welded to the BOTTOM edge, so
 * there is no notch above it to clear: padding its top with the safe-area inset
 * hangs 47 dead pixels of panel paper over the title on every iPhone, on the very
 * dialog whose whole point is to take no more height than it needs. Default
 * `false`, so a frame opened outside `Modal` keeps clearing the notch.
 */
const IsFitSheet = createContext(false);

/**
 * WHERE A LAYER THAT COVERS THE APP IS MOUNTED, AND HOW IT IS POSITIONED.
 *
 * The native iOS keyboard pins the app SHELL — not the layout viewport — to the
 * visible glass (`useVisualViewportShell`), and the same is true of a mobile web
 * browser, whose `dvh` never subtracts a keyboard. A layer portalled to
 * `document.body` therefore keeps the full glass height and leaves everything at
 * its bottom edge, focused fields included, underneath the keyboard.
 *
 * So every full-screen layer mounts INSIDE the shell and is `absolute` in it;
 * `fixed` on the body is only the fallback for a mount taken before the shell
 * exists. `Modal` and the opened document (`DocArtifact`) are both that layer,
 * which is why the rule is one function rather than two copies of a selector.
 */
export function overlayLayer(): {
  host: HTMLElement;
  position: 'absolute' | 'fixed';
} {
  const host =
    document.querySelector<HTMLElement>('[data-viewport-shell]') ?? document.body;
  return { host, position: host === document.body ? 'fixed' : 'absolute' };
}

export function Modal({
  onDismiss,
  size = 'full',
  children,
}: {
  onDismiss: () => void;
  /**
   * `full` is the screen: a list, a browser, anything that wants every pixel.
   *
   * `fit` is a QUESTION — "Delete this session?" is two lines and two verbs, and
   * taking the whole phone for it makes a confirmation look like a destination.
   * It rides up from the bottom edge like the full sheet does, but only as tall as
   * what it holds, and on the desktop it is the same box without the fixed height.
   *
   * `wide` is SETTINGS, and settings only: the one dialog that stands two columns
   * beside each other (this device, and the machines) rather than asking one
   * question. Same scrim, same physics, same height — only the width differs, and
   * below `sm:` it is the identical full-bleed sheet, where the columns stack.
   */
  size?: 'full' | 'fit' | 'wide';
  children: ReactNode;
}) {
  const { host: portalHost, position } = overlayLayer();

  return createPortal(
    <div
      className={`${position} inset-0 z-50 flex justify-center bg-ink/85 backdrop-blur-[2px] transition-opacity duration-200 starting:opacity-0 motion-reduce:transition-none sm:items-center sm:pb-[max(1rem,env(safe-area-inset-bottom))] sm:pl-[max(1rem,env(safe-area-inset-left))] sm:pr-[max(1rem,env(safe-area-inset-right))] sm:pt-[max(1rem,env(safe-area-inset-top))] ${
        size === 'fit' ? 'items-end' : 'items-stretch'
      }`}
      role="presentation"
      onClick={onDismiss}
    >
      {/* ONE SIZE. On the phone a full dialog IS the screen — full bleed, full height,
          so a list inside it gets every pixel the glass has and the verbs at its
          foot are always in the same place. From `sm:` up every dialog that asks one
          question is the same box (`sm:max-w-xl`, `DIALOG_DESKTOP_HEIGHT`): a question
          and a file browser that open over the same screen used to be two different
          rectangles.

          A `fit` dialog is one exception, and it is a SIZE rather than a second
          modal: same scrim, same physics, same box — it simply stops at its content.
          `wide` is the other, and it is a LAYOUT rather than a mood: settings stands
          two columns wide, and 36rem split in half is two columns of nothing.

          The scrim is settings' own — ink at 85% under a 2px blur, faded
          in rather than snapped on. That dialog was hand-rolled beside this one and
          was the better looking of the two, so its glass moved IN HERE and the copy
          moved out; `sm:max-w-xl` is its width, for the same reason. */}
      <div
        className={`flex w-full flex-col ${
          size === 'wide' ? 'sm:max-w-4xl' : 'sm:max-w-xl'
        } ${size === 'fit' ? 'max-h-full sm:h-auto' : DIALOG_DESKTOP_HEIGHT}`}
        role="presentation"
        onClick={(event) => event.stopPropagation()}
      >
        <IsFitSheet.Provider value={size === 'fit'}>{children}</IsFitSheet.Provider>
      </div>
    </div>,
    portalHost,
  );
}

/**
 * THE HEADER OF EVERY SURFACE THAT OPENS OVER ANOTHER, and there is only one of it.
 *
 * There were seven, and no two agreed. Two heights (36px and 48px), two alignments
 * (a centred title in `DialogFrame` and the artifact overlay; a left title with a
 * subtitle in machine settings, application settings, the model picker and the paste
 * editor), two paddings, and four close buttons hand-built at the call site in two
 * different boxes — none of them the `CloseButton` this file says is the only way out.
 *
 * Left wins, because it is the only one of the two shapes that can hold a SUBTITLE,
 * and four of the seven needed one — the gateway a setting belongs to, the model
 * currently pinned, which pasted block is being edited. Centring also cost `px-12` of
 * dead space on both sides to clear a close button that is welded to one of them.
 *
 * The band is the list's own (`min-h-12 mouse:min-h-9`), so a dialog's header and a
 * machine's header are the same height on the same screen.
 */
/**
 * The way out as ONE value: the handler and the name that goes with it, or nothing
 * at all. A surface whose own `onClose` is optional spreads this, so the pair can
 * never come apart on the way down — an unnamed X is a screen reader saying
 * "button" and nothing else.
 */
export function closeWith(
  onClose: (() => void) | undefined,
  label: string,
):
  | { onClose: () => void; closeLabel: string }
  | { onClose?: undefined; closeLabel?: undefined } {
  return onClose ? { onClose, closeLabel: label } : {};
}

export function DialogHeader({
  title,
  titleId,
  subtitle,
  actions,
  closeLabel,
  onClose,
  isUnderNotch = false,
  isStacked = false,
  className = '',
}: {
  title: ReactNode;
  /** For a surface labelled by `aria-labelledby` rather than `aria-label`. */
  titleId?: string;
  subtitle?: ReactNode;
  /**
   * What this band OFFERS, standing between the name and the way out: `BandButton`
   * cells, and nothing else. A dialog's own verbs used to be a docked footer under
   * the body — the model picker's `Refresh` and `Manage providers` sat a screenful
   * of empty panel below the last row they act on. In the band they are cells of
   * the same run the ✕ ends, so all three are the band's height and one hairline
   * apart.
   */
  actions?: ReactNode;
  /** The way out. Both halves travel together — see the union below. */
  /**
   * This band is the TOP of the screen, so it clears the notch itself. The desktop
   * has no inset to clear and drops the padding again.
   *
   * THE INSET STANDS ABOVE THE BAND'S OWN ROW, NEVER INSIDE IT (`box-content`).
   * `min-h-12` is a BORDER-BOX minimum, so the safe-area padding was SUBTRACTED from
   * the band: measured on the image viewer at 390px with a 47px top inset, the band
   * came out 77px instead of 47+48, its row collapsed to the 30px the title happened
   * to need, and `CloseButton isBand` — which stretches to that row — shipped 48x30
   * instead of the 48x48 square it is on every other header, under the app's own
   * 44px minimum for the one gesture that leaves a screen. Reported as the heading
   * having the wrong height and the close being a different box from everywhere else.
   */
  isUnderNotch?: boolean;
  /**
   * This band opens OVER another dialog's band, so it draws the hairline that
   * tells the two of them apart — an artifact opened inside the artifacts sheet.
   */
  isStacked?: boolean;
  /** Placement only; the band's own face is fixed. */
  className?: string;
} & (
  | {
      onClose: () => void;
      /**
       * The way out is icon-only, so its name is not optional, and three of these
       * bands can stand over one another: a plain "Close" names all three the same
       * and a screen reader cannot tell the human which one it is on. Say what it
       * closes — "Close model picker", "Close report.pdf".
       */
      closeLabel: string;
    }
  | { onClose?: undefined; closeLabel?: undefined }
)) {
  return (
    <header
      className={`flex min-h-12 shrink-0 items-stretch bg-dialog-title text-dialog-title-foreground mouse:min-h-9 ${
        isUnderNotch ? 'box-content pt-[env(safe-area-inset-top)] sm:pt-0' : ''
      } ${isStacked ? 'border-t border-dialog-title-foreground/20' : ''} ${className}`}
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
      {/* What the band OFFERS, before the way out and in cells of the band's own:
          welded by the same hairline, standing the band's full height. */}
      {actions}
      {/* The two travel together by type; the second test is what TypeScript needs
          to see it, because a destructured union does not narrow on its own. */}
      {onClose && closeLabel && (
        <CloseButton isBand label={closeLabel} onClick={onClose} />
      )}
    </header>
  );
}

export function DialogFrame({
  title,
  subtitle,
  actions,
  children,
  footer,
  onClose,
  closeLabel,
  className = '',
}: {
  title: string;
  /** The line under the title — which machine, which model, which paste. */
  subtitle?: ReactNode;
  /**
   * What this band OFFERS, standing between the name and the way out: `BandButton`
   * cells, and nothing else. A dialog's own verbs used to be a docked footer under
   * the body — the model picker's `Refresh` and `Manage providers` sat a screenful
   * of empty panel below the last row they act on. In the band they are cells of
   * the same run the ✕ ends, so all three are the band's height and one hairline
   * apart.
   */
  actions?: ReactNode;
  children: ReactNode;
  footer?: ReactNode;
  onClose?: () => void;
  /**
   * What LEAVING does, when it is not simply "Close <title>": the human-input
   * dialog's way out CANCELS the request it is asking about.
   */
  closeLabel?: string;
  className?: string;
}) {
  // A sheet that stops at its content starts halfway down the glass, so the notch
  // is not above it and the top inset is dead space (`IsFitSheet`).
  const isFitSheet = useContext(IsFitSheet);
  return (
    <section
      className={`flex min-h-0 flex-1 flex-col overflow-hidden border-t-2 border-accent bg-panel ${
        isFitSheet ? '' : 'pt-[env(safe-area-inset-top)]'
      } pb-[env(safe-area-inset-bottom)] shadow-none transition-[opacity,transform,translate,scale,rotate] duration-300 ease-[cubic-bezier(0.22,0.61,0.36,1)] starting:translate-y-full starting:opacity-0 motion-reduce:transition-none sm:border sm:border-dialog-edge sm:pt-0 sm:pb-0 sm:shadow-[8px_8px_0_var(--dialog-shadow)] sm:duration-200 sm:starting:translate-y-2 ${className}`}
      role="dialog"
      aria-modal="true"
      aria-label={title}
    >
      {/* A dialog knows its own title, so it is never told the name of its own way
          out — five surfaces used to hand a screen reader the same two words. */}
      <DialogHeader
        title={title}
        subtitle={subtitle}
        actions={actions}
        {...closeWith(onClose, closeLabel ?? `Close ${title}`)}
      />
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
export function Spinner({
  tone = 'inherit',
  className = '',
}: {
  /**
   * `accent` is the app's waiting ink: a spinner that reports work in the amber
   * the rest of the screen uses for it. `inherit` takes the ink of the line it
   * rides, which is what a spinner inside a sentence wants.
   */
  tone?: 'inherit' | 'accent';
  /** Placement only; the frames' own face is fixed. */
  className?: string;
}) {
  const ink = tone === 'accent' ? 'text-accent-ink' : '';
  return (
    <span aria-hidden="true" className={`inline-grid ${ink} ${className}`}>
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
 * THE MACHINE'S NAME IS A TAG IN ITS OWN HUE, and this is that tag's face.
 *
 * A machine is named exactly once on this screen — in the chrome above the list, which
 * is also where it is renamed — so the tag has to be one face worn by both the resting
 * name and the field it becomes (`EditableName`), or the header changes shape when a
 * caret arrives. It is a FACE rather than a component for that reason alone.
 *
 * A full-bleed bar of hue was a second loud band competing with the project header
 * under it, and it only answered "whose" for as long as it stayed on screen. The colour
 * that ANSWERS ownership is the machine's spine (`MachineRail`, 2px down every project
 * and session it owns); the name only has to NAME, so it wears the hue on its own
 * leading edge and nothing else does.
 *
 * THE HUE IS THE TAG'S EDGE, NEVER ITS FILL. A hue at L 0.62 filled behind a word can
 * only carry dark ink — the page's own paper on top of it is a 3.2:1 name — so the one
 * word that says which computer you are looking at was black type on orange paper, the
 * only black ink on a screen where every other word is the page's. The tag is the
 * page's own paper and ink now, with 2px of hue down its leading side: the SAME rule
 * `MachineRail` paints under it, at the same width and on the same side, so the name
 * and the block it heads are one machine and the word keeps ordinary ink's contrast.
 */
export function machineTagFace(color?: MachineColor): string {
  // `w-fit` so the tag hugs the NAME: a tag as wide as the column it sits in
  // is a bar again, which is the exact thing the spine replaced.
  return `w-fit max-w-full truncate border-l-2 bg-level-machine px-1.5 font-mono text-ui font-bold text-white ${color ? color.rail : 'border-dialog-edge'}`;
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
 * `HeaderTitle` (a header's hover has to reach the edge of the screen) and the trailing
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
 *   project   text-title   13   52 / 36                level-project
 *   session   text-ui      11   48 / 32                the page's own surface
 *
 * Two points of type per step, four pixels of band per step, and one derived step of
 * paper per level (see `--color-level-*` in `index.css`).
 *
 * There is no machine step any more: the machine is SELECTION (a chip in the scope
 * strip, the title of the chrome above the list), not a band, so a second header tone
 * one hairline from this one could only ever read as the same thing said twice.
 *
 * The LEAF is the shortest, not the tallest. A session row used to stand 48px against
 * a 36px project band — the child bigger than the thing that contains it — which is
 * backwards however you argue it, and on a desktop it is also just a waste: the row
 * is ONE line there, so 32px holds it exactly. Touch keeps every level at 44px or
 * more, so the ladder survives a thumb.
 */
const HEADER_TYPE = 'text-title';

/** The band every header in the list stands in. It sticks; nothing above it does. */
const HEADER_BAND =
  'flex min-h-13 items-stretch mouse:min-h-9 sticky top-0 z-10 bg-level-project';

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
 * RUNNING PROSE, and the app has exactly ONE rule for it.
 *
 * Justification is a WIDTH trade: word-spacing is the only slack a justified line
 * has, so it is only safe when the breaker is given enough stops — hence
 * `hyphens-auto` with a 6/3/3 limit and `text-pretty` ride WITH `text-justify`
 * and are never spelled apart from it. Settings used to justify without
 * hyphenation in three places while the chat justified with it, which is the same
 * paragraph set two different ways on one screen. This mirrors the TUI's single
 * justifier (`markdown-layout/justify-line-runs` → lanterna `justifyLine`).
 *
 * A caller that owns an UNBREAKABLE atom it cannot scope `break-all` to (one raw
 * text run: the user bubble) takes `PROSE_RAGGED` instead — same typography, flush
 * left — rather than dropping the whole rule or re-spelling half of it.
 */
export const PROSE_RAGGED =
  'hyphens-auto [hyphenate-limit-chars:6_3_3] text-pretty text-left';

export const PROSE =
  'hyphens-auto [hyphenate-limit-chars:6_3_3] text-pretty text-justify';

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
//
// It adds NO gutter in front of itself: the first control already carries its 44px hit
// box as padding around a 14px mark, so a `pl-2` on top of the slab's own `pr-3` put
// 34px between a session's `IDLE` and the `›` that follows it while the same `›` sat
// 13px from the paper on its other side. `gap-2` still separates two controls from
// each other, which is the only distance this cluster has to invent.
const LIST_TRAIL = 'flex shrink-0 items-stretch gap-2 self-stretch pr-3 sm:pr-4';

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
  rule,
  children,
}: {
  /**
   * A border-colour class for the band's OUTGOING rule, when that rule carries
   * meaning. It replaces the hairline rather than joining it: a coloured line beside
   * a grey one is the double border this list was reported for, and the band only
   * ever draws one.
   */
  rule?: string;
  children: ReactNode;
}) {
  const edge = rule ? `border-b-2 ${rule}` : 'border-b border-dialog-edge';
  return <header className={`${HEADER_BAND} ${edge}`}>{children}</header>;
}

/**
 * THE SHELF a section hangs under its own header: what the section REPORTS, and
 * how it is WALKED.
 *
 * Reported ("there is no visual differentiation between the paging"): the pager
 * was painted at the FOOT of a group's rows, in the rows' own paper, closing with
 * the same hairline every row draws — and the element directly after it was the
 * next project's header. So the strip that walks `vis` looked like the last row of
 * `vis` or the first row of `vis-companion`, and nothing on the screen said which.
 * A control that belongs to a HEADING has to stand on the heading's paper: the
 * shelf is the header's own `bg-level-project`, half a band tall, and it closes the
 * pair with one hairline instead of drawing a second one mid-list.
 *
 * It STICKS to the underside of the band it belongs to — `top-13`, exactly
 * `HEADER_BAND`'s `min-h-13`, and `mouse:top-9` for the compact one — so header and
 * shelf travel as one object: at row 40 of a 794-session project the reader can
 * still see which project they are in AND jump to page 12 without scrolling back.
 * It is one step BEHIND the band in z-order (`z-9` against `z-10`), because when a
 * group scrolls out its own shelf must pass UNDER the next header rather than over
 * it.
 *
 * It is ONE LINE on a phone, and it stays one line at every page. It used to wrap:
 * a 100-page pager asks for 304px of steps and numbers, the count beside it 115px,
 * and a 430px screen has 400 — so stepping from page 1 to page 4 opened the window,
 * broke the row in two and grew a STICKY strip from 41px to 59px under the thumb
 * that pressed it, with the count marooned beside 300px of empty paper. The pager
 * carries a phone form of fixed width for exactly this reason (see `Pager`), so the
 * shelf holds its height; `flex-wrap` remains for the honest case, a count so long
 * it cannot share the line, because a truncated count is not an option.
 *
 * The count lives here rather than in the header's trailing cluster for the same
 * reason the qualifier moved under the name: measured on a 320px screen, `699
 * sessions`, `3 live`, the amber verb and the `⋯` took the cluster's width first
 * and left the project NAME — the thing the reader came for — 24px wide.
 */
export function SectionShelf({ children }: { children: ReactNode }) {
  return (
    <div
      className={`sticky top-13 z-9 flex min-h-9 flex-wrap items-center justify-between gap-x-3 gap-y-1 border-b border-dialog-edge bg-level-project py-1 mouse:top-9 mouse:min-h-8 ${LIST_EDGE} ${LIST_EDGE_END}`}
    >
      {children}
    </div>
  );
}

/**
 * THE TROUGH between two sections, and the only thing between them that is not a
 * line.
 *
 * Reported ("there is no visual differentiation between the projects and it all
 * looks like kind of the same thing"): the last row of one project and the header
 * of the next were separated by a single hairline, the same hairline that separates
 * two sessions inside one project — one pixel of grey asked to mean both "next row"
 * and "different repository". A boundary that matters is seen before it is read, so
 * it is 8px of the machine's own paper (`bg-level-machine`, `L 0.905` against the
 * card's `0.96`): each project reads as a slab standing ON its machine's rail, and
 * the rail's hue runs through the gap because the machine owns both sides of it.
 *
 * It is 8px and not a margin: a gap collapses, a slab does not, and this one has to
 * PAINT to be a boundary at all.
 */
export function SectionGap() {
  return <div aria-hidden="true" className="h-2 bg-level-machine" />;
}

/**
 * A name that edits IN PLACE, and does not move when it does.
 *
 * The resting name and the field it becomes are the same box: the same class list,
 * and a field stripped of the browser defaults the FACE does not speak (`border-0`, no
 * ring, no native appearance) — paper and padding belong to the face, or a name that is
 * a coloured tag loses its colour the moment a caret arrives — sized by `size` in
 * CHARACTERS — the header is a mono
 * face, so one character is one column and the field is exactly as wide as the word it
 * replaced. Anything width-guessing (a `w-full` field, a measured span) shifts the
 * qualifier beside it the moment the caret arrives, which is the jump this exists to
 * refuse.
 *
 * Enter commits, Escape restores, and leaving commits too — a phone keyboard is
 * dismissed far more often than Enter is pressed.
 */
export function EditableName({
  value,
  label,
  face,
  onCommit,
}: {
  value: string;
  label: string;
  /**
   * The TYPE the name is set in — required, and spelled `face` rather than
   * `className` because it is not a call site positioning a control: this field
   * has to read as the heading it replaces, in the heading's own ink, or the
   * screen changes shape the moment a caret arrives.
   */
  face: string;
  onCommit: (name: string) => void;
}) {
  const [draft, setDraft] = useState<string | null>(null);
  if (draft === null)
    return (
      <button
        type="button"
        aria-label={label}
        title={label}
        onClick={() => setDraft(value)}
        className={`${face} text-left hover:underline focus-visible:outline-none focus-visible:underline`}
      >
        {value}
      </button>
    );
  const commit = () => {
    setDraft(null);
    if (draft.trim() !== value) onCommit(draft.trim());
  };
  return (
    <input
      autoFocus
      aria-label={label}
      value={draft}
      size={Math.max(draft.length, 1)}
      onChange={(event) => setDraft(event.target.value)}
      onBlur={commit}
      onKeyDown={(event) => {
        if (event.key === 'Enter') commit();
        if (event.key === 'Escape') setDraft(null);
      }}
      className={`${face} appearance-none border-0 focus:outline-none`}
    />
  );
}

/**
 * The leading half of a header that only NAMES its section: an optional mark, then the
 * name, and under the name whatever qualifies it. It takes the width the trailing
 * cluster leaves and no more, and it spends that width on one thing at a time.
 */
export function HeaderTitle({
  mark,
  name,
  qualifier,
  qualifierTitle,
  onRename,
  renameLabel,
}: {
  mark?: ReactNode;
  name: ReactNode;
  /**
   * Makes the NAME itself the rename control: press it, type, Enter saves and
   * Escape puts it back. A machine's name is the one thing on this band a human
   * owns, and sending them into a settings screen to change a word is a trip.
   */
  onRename?: (name: string) => void;
  /** What the pressable name is called to a screen reader. */
  renameLabel?: string;
  /**
   * What the name alone cannot settle — the path behind a project's folder name,
   * the address behind a machine's label. It rides UNDER the name, never beside it.
   *
   * Beside the name it was the half that lost: the header's trailing cluster takes
   * its width first, so a qualifier shared what was left of ONE line with the name
   * and truncated mid-token — `~/vis/apps/vis-c…` on a 390px phone, and the name
   * itself capped at 60% of that remainder to make room. The two say different
   * things, so they are two lines: the name reads first, the path reads whole
   * under it, and neither is rationed against the other. Rendered nothing when a
   * machine has no label of its own, because then the address IS the name and
   * printing it twice is not a hierarchy.
   */
  qualifier?: ReactNode;
  qualifierTitle?: string;
}) {
  return (
    // The glyph centres against the STACK (`items-center`), which is the whole block
    // it marks — a fold owns both lines of the name it folds. Baseline-aligning the
    // mark alongside them drops a 10px block below the ink it belongs to.
    <span className={`flex min-w-0 flex-1 items-center gap-2 ${LIST_EDGE}`}>
      {/* The column is RESERVED, marked or not: the machine header wears a hue
          block here and the project header below it wears nothing, and a column
          that only exists when it is filled put the machine's name at x=36 and
          the project's at x=14 on a 390px iPhone — the deeper row starting
          further left, which is hierarchy read backwards. */}
      <span className={HEADER_GLYPH}>{mark}</span>
      {/* `items-start` keeps each line as wide as its own ink and no wider: the
          name stays a word-sized press target rather than a full-width one, and
          both lines still truncate, because shrink-to-fit is capped by the column
          the trailing cluster leaves. */}
      <span className="flex min-w-0 flex-col items-start">
        {onRename ? (
          <EditableName
            face={`min-w-0 truncate bg-transparent p-0 font-mono font-bold text-white ${HEADER_TYPE}`}
            label={renameLabel ?? 'Rename'}
            value={typeof name === 'string' ? name : ''}
            onCommit={onRename}
          />
        ) : (
          <span
            className={`min-w-0 truncate font-mono font-bold text-white ${HEADER_TYPE}`}
          >
            {name}
          </span>
        )}
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
 * The leading half of a PROJECT header, and the whole of it is the disclosure.
 *
 * A project is the level a reader scopes by, so it is the level that folds: with
 * four checkouts on one machine the screen is otherwise a scroll through work
 * nobody asked about. The mark column `HeaderTitle` already reserves carries the
 * chevron, so the name stays on the one leading edge every row in this list shares
 * — the disclosure buys a glyph, never an indent.
 *
 * It is a `HeaderTitle` inside a pressable, not a button beside one: the whole
 * naming half answers the thumb, and the trailing cluster (`HeaderActions`) keeps
 * its own controls, so "New session" is never swallowed by the fold.
 */
export function ProjectCrumb({
  name,
  qualifier,
  qualifierTitle,
  isOpen,
  onToggle,
  label,
}: {
  name: ReactNode;
  qualifier?: ReactNode;
  qualifierTitle?: string;
  isOpen: boolean;
  onToggle: () => void;
  /** What the fold is called to a screen reader: `Collapse vis`. */
  label: string;
}) {
  return (
    <button
      type="button"
      aria-expanded={isOpen}
      aria-label={label}
      onClick={onToggle}
      className="flex min-w-0 flex-1 items-center text-left transition-colors duration-150 hover:bg-hover focus-visible:bg-hover focus-visible:outline-none motion-reduce:transition-none"
    >
      <HeaderTitle
        mark={<ChevronIcon open={isOpen} className="size-3.5 text-dialog-hint" />}
        name={name}
        qualifier={qualifier}
        qualifierTitle={qualifierTitle}
      />
    </button>
  );
}

/**
 * The page numbers a pager PAINTS: always the first, the last, and a window
 * around the current one, with a gap marker (`null`) wherever the run breaks.
 *
 * A pair of steps can only ever walk: reaching page 5 of 73 cost four taps and
 * page 40 was unreachable in practice. Numbers make the jump one tap — but 73 of
 * them do not fit on a 390px phone, so the strip is windowed, and both ends stay
 * pinned because "back to the start" and "the oldest sessions" are the two jumps
 * a reader actually asks for.
 */
export function pageWindow(page: number, pageCount: number, span = 1): (number | null)[] {
  const shown = new Set<number>([1, pageCount]);
  for (let n = page - span; n <= page + span; n += 1) {
    if (n >= 1 && n <= pageCount) shown.add(n);
  }
  // A gap marker that hides exactly ONE page is a lie that costs a tap: print the
  // number instead, which also keeps the strip's width from jumping by a whole
  // cell as the reader walks it.
  for (const n of [...shown]) {
    if (shown.has(n + 2)) shown.add(n + 1);
  }
  const numbers = [...shown].sort((a, b) => a - b);
  const out: (number | null)[] = [];
  numbers.forEach((n, index) => {
    const previous = numbers[index - 1];
    if (previous !== undefined && n - previous > 1) out.push(null);
    out.push(n);
  });
  return out;
}

/**
 * THE PAGER, and there is only one of it.
 *
 * A project's history is walked a PAGE at a time, and the page is cut by the
 * gateway — never by hiding rows a client already downloaded. "Show more" grew
 * one endless column that could only ever get longer, could not be walked
 * backwards, and left the reader with no idea how much history there was; a
 * disclosure chevron on the header hid the whole project behind a tap for the
 * same reason. A page number answers both: where you are, and how much there is.
 *
 * It is NOT a band of its own: it rides on the group's `SectionShelf`, under the
 * project header and on the header's paper. Painted at the FOOT of the rows it was
 * indistinguishable from one — a `border-t border-dialog-edge` strip at the end of
 * a group, one hairline above the next project's header — so `1 2 … 80 ›` read
 * either as the last session of `vis` or as the first thing in `vis-companion`,
 * which is the report this shelf exists for. The shelf also STICKS, so the control
 * that walks a 794-session project is still on screen at row 40 of that project.
 *
 * What the pager owns is the steps at its ends and what stands between them: from
 * `sm` up the NUMBERS, every one of them pressable, so page 5 of 73 is ONE tap and
 * not four; on a phone, where the numbers cannot share a line with the group's
 * count, the position itself (`4 / 80`) at a width that never changes. The paper,
 * the two list edges and the closing hairline belong to the shelf. Below one page it
 * renders nothing at all — a pager for a project with four sessions is a control
 * that can never be pressed, and its shelf then carries only the group's count.
 *
 * A step that cannot be taken is not painted. It used to render disabled, so page
 * one wore a `<` that answered nothing and the eye still had to check it.
 *
 * But a control that DISAPPEARS must not move the one beside it: with the band
 * centred, stepping off page one dropped a `<` into the strip and slid `>` left
 * under the finger already on it, so the third tap landed on a number — walking
 * the list by tapping `>` was impossible. So each step owns a FIXED slot at its
 * end of the band, holding its width whether or not it is painted, and only the
 * numbers between them breathe. `>` is at the same x on every page.
 */
export function Pager({
  page,
  pageCount,
  onPage,
  label,
}: {
  /** 1-based, so it reads the way it is printed. */
  page: number;
  pageCount: number;
  onPage: (page: number) => void;
  /** What is being paged, for the screen reader: "vis sessions". */
  label: string;
}) {
  if (pageCount <= 1) return null;
  // `invisible` rather than absent: the slot keeps its exact box, so nothing on the
  // band moves when the step arrives or leaves. Nothing is painted, nothing is
  // announced, nothing is focusable.
  const step = (to: number, isBack: boolean) => {
    const can = to >= 1 && to <= pageCount;
    return (
      <IconButton
        label={isBack ? 'Previous page' : 'Next page'}
        variant="quiet"
        onClick={() => onPage(to)}
        className={can ? '' : 'invisible'}
        aria-hidden={can ? undefined : true}
        tabIndex={can ? undefined : -1}
      >
        <ChevronIcon back={isBack} className="size-3" />
      </IconButton>
    );
  };
  return (
    <nav
      aria-label={`Pages of ${label}`}
      // On a phone the cluster is exactly as wide as its own content and never grows
      // (`shrink-0`, no basis to negotiate); from `sm` up it takes the shelf's trailing
      // end. The shelf is a flex row that WRAPS, and a pager that asks for 304px there
      // is a pager that pushes the group's count onto a line of its own — see the two
      // forms below.
      className="flex min-w-0 shrink-0 justify-end sm:grow"
    >
      {/* The pager is LIVE: pressing a step or a number changes nothing else on the
          shelf, so without this a screen reader hears silence after the press. It is
          the ONE voice of the position — both visible forms below are drawn from it,
          and neither is announced a second time. */}
      <span aria-live="polite" className="sr-only">
        Page {page} of {pageCount}
      </span>
      {/* The shelf runs the width of the list; the CONTROL does not. Steps pinned to
          the paper's two edges put `<` and `>` 360px apart on a phone, so paging is a
          two-handed reach and no thumb can rest between them — you cannot tap `>`
          twice without moving. The cluster is held at the shelf's trailing end
          instead, which puts the two steps a thumb's width from the numbers they
          belong to and in the column every other trailing control already uses.

          It is sized by its CONTENT, and `>` is what that buys: `>` ends the cluster,
          the cluster ends at the shelf's trailing edge, so `>` is at the same x on
          every page of every project and the window can only breathe to the LEFT. A
          capped box (`w-full max-w-[19rem]`) promised the same thing and did not keep
          it — measured at 768px, `1 2 3 4 5 … 80` needs 319px of a 304px cap, and
          because a flex item cannot shrink below its own content the box simply
          overflowed: on page 4, and only on page 4, `>` sat 15px right of where it
          sits on every other page, outside the trailing column it shares with the
          `⋯` of the rows below. The step the list is walked with does not move. */}
      <div className="flex items-center gap-1">
        {step(page - 1, true)}
        {/* THE PHONE FORM: the position itself, printed between the two steps.

            Reported from a phone, with a screenshot: on page 4 of a 798-session
            project the shelf was two lines with a hole in it — the count alone on the
            first beside 300px of empty paper, the numbers alone on the second — and
            it CHANGED HEIGHT as the reader paged. Measured at 430px: page 1 asks for
            277px and fits beside the 115px count on one 41px line, page 4 opens the
            window to `1 2 3 4 5 … 80`, asks for 304px, and the shelf wraps to 59px.
            A sticky strip grew 18px under the thumb that had just pressed it.

            No phone line holds both: at 390px the count and the widest window want
            431px of a 362px line. The numbers are the half that can be said in fewer
            characters, so below `sm` they become `4 / 80` — 56px, the SAME 56px on
            every page of every project, so the shelf keeps one line and one height
            for the whole walk. `<` and `>` still step, and the strip comes back whole
            at `sm`, where a number is a tap and not a squeeze between two others. */}
        <span
          aria-hidden="true"
          className="min-w-14 px-1 text-center font-mono text-meta text-dialog-hint tabular-nums sm:hidden"
        >
          {page} / {pageCount}
        </span>
        <span className="hidden flex-1 items-center justify-center gap-1 sm:flex">
          {pageWindow(page, pageCount).map((entry, index) =>
            entry === null ? (
              <span
                key={`gap-${index}`}
                aria-hidden
                className="px-1 font-mono text-chip text-dialog-hint"
              >
                &#8230;
              </span>
            ) : (
              <Button
                key={entry}
                variant={entry === page ? 'primary' : 'quiet'}
                density="compact"
                aria-label={`Page ${entry}`}
                aria-current={entry === page ? 'page' : undefined}
                onClick={() => onPage(entry)}
                className="min-w-7 px-1 font-mono tabular-nums sm:min-w-8 sm:px-1.5"
              >
                {entry}
              </Button>
            ),
          )}
        </span>
        {step(page + 1, false)}
      </div>
    </nav>
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
 * A count is a NUMBER AND ITS NOUN, on every screen. A bare `725` over a list of
 * rows says nothing about what was counted, and the phone is exactly where the
 * reader has the least context to supply it from — so the noun is never dropped to
 * win back width. What gives way instead is the project's own name, which
 * truncates with the full path on its `title`.
 */
export function HeaderTally({ count, unit }: { count: number; unit: string }) {
  const noun = count === 1 ? unit : `${unit}s`;
  return (
    <span className="whitespace-nowrap">
      {count} {noun}
    </span>
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
 * THE SPINE OF A MACHINE: 2px of its own hue down everything it owns, banner
 * included. A project boundary is a hairline and a machine boundary is a colour
 * change, so where one computer ends is seen before it is read — and, unlike the
 * band at the top of the block, the spine is still on screen once the reader has
 * scrolled past it, which is the whole reason ownership lives here.
 *
 * It is the card's LEFT FRAME, not a line inside it — see `LIST_FRAME`. That is the
 * whole difference from the version that had to be removed: it doubled the card's
 * own border and, being a border, pushed every railed row deeper than its own
 * trailing edge stopped. Now one border paints the left side and a machine simply
 * COLOURS it — same 2px as the grey frame it falls back to, and as the leading edge of
 * the tag that names it, so nothing on this side of the list is wider than the rest.
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
export function MachineMark({
  color,
  size = 'inline',
  isHollow,
}: {
  color: MachineColor;
  size?: 'inline' | 'banner';
  /**
   * The machine is not answering: the SAME hue, drained to an outline.
   *
   * A machine that is down keeps its identity — it is still that computer, and
   * still that colour — so its mark is not recoloured and not removed, it is
   * emptied. Nothing is behind it, which is exactly what the block says.
   */
  isHollow?: boolean;
}) {
  // A machine's identity block used to be `size-1.5` everywhere — the same 6px square,
  // at the same size, as the LIVE / WAITING / IDLE dot on every session row beneath
  // it. One shape meaning two things, and the SMALLEST glyph marking the HIGHEST
  // level. In a banner it is the mark of a whole computer and takes the glyph column;
  // riding inside a scope chip's text it stays the 6px it has to be.
  const box = size === 'banner' ? 'size-2.5' : 'size-1.5';
  const face = isHollow ? `border ${color.rail}` : color.dot;
  return <span className={`${box} shrink-0 ${face}`} aria-hidden="true" />;
}

/**
 * THE FLEET SWITCHER IS ONE OBJECT, NOT A ROW OF COMPETING BOXES.
 *
 * A machine tab is a STATE (one of them is always on, and it stays on after the
 * finger lifts); `Add machine` beside it is a VERB. They used to wear the same
 * species — a bordered chip next to a filled button — so the row read as "here are
 * three things you can do" when it says "you are here, and here is one thing to do".
 *
 * So the machines share ONE track and nothing inside it is bordered: the chosen
 * machine is a raised paper tile, the rest are hint ink on the track's own fill.
 * The row then holds exactly two objects, the switch and the verb, and they are
 * told apart by fill logic rather than by colour.
 *
 * `All` IS THE FIRST TILE, and it exists only above a FLEET. It is the same kind of
 * STATE as a machine tab — pressing it answers "which machine" with "every one of
 * them", and the list under it is one named section per machine, each on its own rail
 * — so it shares the track rather than standing beside it as a control of another
 * species. It wears no hue block: the hues it stands for are the sections themselves.
 * A fleet of one never renders it, because "every machine" and "this machine" would
 * hand back the same list under two names.
 *
 * The track is the BUTTON's box from the outside — 2px of padding around a 28px
 * tile is exactly the 32px every control on this row stands at, with no frame of its
 * own: the duller fill IS the track, and a border around it would only re-draw the
 * edge the fill already has, and `mouse:` takes both
 * down together to 24px — so the switch and `Add machine` share one baseline
 * whatever the pointer. Overflow scrolls INSIDE the clipped track, so a fleet of
 * six never widens the row or pushes the verb off the trailing edge.
 *
 * The corners are SQUARE. This screen's paper is a stack of square bands — machine
 * card, project header, session rows — and a pill-cornered track floating above them
 * was the only such shape on the page.
 */
export function MachineSwitcher({ children }: { children: ReactNode }) {
  return (
    <div className="flex min-w-0 shrink items-center gap-0.5 overflow-x-auto bg-level-machine p-0.5">
      {children}
    </div>
  );
}

/**
 * One machine inside the switcher's track. Selection is a RAISED TILE — the page's
 * own paper lifted out of the track — never a border and never the accent: amber is
 * this product's verb colour, and a selected tab painted in it reads as a button
 * that will do something when you press it. Square, like the track that holds it.
 *
 * News is a HIGHLIGHT, not a tally. A machine tab carried two numbers (live, unread)
 * and the reader had to learn a colour code to tell them apart; what a tab has to say
 * is "something happened over here", so unread is one amber mark and bold ink. The
 * exact count belongs to the session rows that own it.
 *
 * A MACHINE THAT IS NOT ANSWERING IS NOT A PLACE TO GO, SO ITS TILE IS A VERB.
 *
 * It used to be a tab like any other, in the same ink and the same weight, wearing
 * the word "offline" — the only label on this strip that GREW when its machine got
 * worse — and pressing it scoped the whole screen to a machine with nothing to show.
 * A dead machine is now DRAINED: hollow hue, hint ink, never the raised tile, no word
 * at all, and it is dropped from `All` so nothing under the switch belongs to it.
 *
 * The one thing a dead machine can still do is come back, so that is what its tile
 * does: `isDown` makes the press a RETRY of that machine and nothing else. It is no
 * longer a state, so it carries no `aria-pressed`; the caller gives it the verb's own
 * `label` ("Reconnect to tower") and puts the machine's name and the transport's own
 * reason in `title`, which is where a 6px block cannot speak.
 *
 * `note` is what that press is doing, spoken in the tile that was pressed —
 * "reconnecting...", then the failure if it came back dead. It exists only after a
 * press: a fleet's dead machines say nothing until they are asked, and the failure
 * is a word rather than a state, so the caller takes it back off the tile.
 *
 * A FAILURE IS THE ONE WORD HERE THAT IS NOT A NAME, so `isNoteError` prints it in
 * error ink. In the tile's own hint ink it read as more chrome on a strip made of
 * chrome, and the press that went nowhere looked like a press that did nothing.
 */
export function MachineTab({
  isOn,
  hasUnread,
  isDown,
  note,
  isNoteError,
  label,
  title,
  onClick,
  children,
}: {
  isOn: boolean;
  hasUnread?: boolean;
  /** Not answering: drained face, never the scope, and the press is a retry. */
  isDown?: boolean;
  /** The word this tile earned by being pressed, and only then. */
  note?: string | null;
  /** That word is a FAILURE: error ink, because a quiet failure reads as chrome. */
  isNoteError?: boolean;
  /** The verb's accessible name, when the press is no longer "show me this machine". */
  label?: string;
  title?: string;
  onClick: () => void;
  children: ReactNode;
}) {
  return (
    <button
      type="button"
      aria-pressed={isDown ? undefined : isOn}
      aria-label={label}
      title={title}
      // The tile answers its own press, so it is the live region: `reconnecting...`
      // and what came back are read out where the finger already is.
      aria-live={isDown ? 'polite' : undefined}
      onClick={onClick}
      className={`inline-flex h-7 shrink-0 items-center gap-1.5 px-2 font-mono text-meta transition-colors duration-150 motion-reduce:transition-none mouse:h-5 ${
        isDown
          ? 'text-dialog-hint/60 hover:text-dialog-hint'
          : isOn
            ? 'bg-panel font-bold text-white shadow-sm'
            : hasUnread
              ? 'font-bold text-white'
              : 'text-dialog-hint hover:text-white'
      }`}
    >
      {children}
      {note && (
        <span className={isNoteError ? 'text-err' : 'opacity-80'}>{note}</span>
      )}
      {hasUnread && !isDown && (
        <span className="inline-block size-1.5 shrink-0 bg-accent">
          <span className="sr-only">unread</span>
        </span>
      )}
    </button>
  );
}

/**
 * The verb this screen exists for, spelled as the ONE PLUS on the screen.
 *
 * It used to be the first row of the header's `⋯` menu: a tap, a popover, a read,
 * and only then the thing people do all day. So it is a BUTTON now — the Blockether
 * yellow, on every project header, directly before the `⋯` that keeps the rarer
 * verbs — and the menu is left with the questions that actually deserve one.
 *
 * IT IS A MARK, NOT A WORD, because it repeats. A project header appears once per
 * repository, so a fleet with nine checkouts painted nine amber paragraphs of the
 * same eleven characters: measured on a 390px frame the word cost 83px of a row
 * whose name and path had 221px to share, and the ninth one said nothing the first
 * had not. The mark is 37px, the name column takes the 46px back, and the rank is
 * untouched — same amber, same compact box, same 44px touch target.
 *
 * The plus therefore means START A SESSION HERE and nothing else on this screen: the
 * machine's control wears the folder it opens (`MachineProjectsButton`), the draft
 * half wears a fork inside that folder, and no glyph is used for two meanings.
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
  busyLabel,
  onPress,
  onDraft,
}: {
  machine: string;
  where?: string | null;
  disabled?: boolean;
  /**
   * The work THIS button started, spoken inside it: "Creating...", "Forking...".
   *
   * A busy word parked beside the control said the screen was busy without saying
   * which of a fleet's headers had been pressed; the verb that was pressed is the
   * only honest place for it, so the button wears its own progress and refuses a
   * second press while it does.
   *
   * A MARK HAS NOWHERE TO HOLD THAT WORD, so while the create runs this control is
   * the word button it used to be — for exactly as long as the create runs, on the
   * one header that was pressed, which is the only row that has anything to say.
   */
  busyLabel?: string | null;
  onPress: (anchor: HTMLElement) => void;
  /** Omitted when the surface has no draft question — then there is no second half. */
  onDraft?: (anchor: HTMLElement) => void;
}) {
  const isBusy = Boolean(busyLabel);
  const label = `New session on ${machine}`;
  const title = where ? `New session on ${machine}, in ${where}` : label;
  const verb = isBusy ? (
    <Button
      type="button"
      pressEffect="none"
      density="compact"
      disabled
      aria-live="polite"
      aria-label={label}
      title={title}
      className={`shrink-0 whitespace-nowrap${onDraft ? ' border-r-0' : ''}`}
      onClick={(event) => onPress(event.currentTarget)}
    >
      {busyLabel}
    </Button>
  ) : (
    <IconButton
      variant="primary"
      disabled={disabled}
      aria-live="polite"
      label={label}
      title={title}
      className={onDraft ? 'border-r-0' : ''}
      onClick={(event) => onPress(event.currentTarget)}
    >
      <PlusIcon className="size-4" />
    </IconButton>
  );
  if (!onDraft) return verb;
  return (
    <span className="flex shrink-0 items-stretch">
      {verb}
      {/* The seam is the only thing between the two halves: same fill, same height, a
          hairline in the ink they both carry. A second bordered box beside the verb
          would read as a different control doing a different thing. */}
      <IconButton
        variant="primary"
        disabled={disabled || isBusy}
        label={`New session in a draft of ${where ?? 'this project'} on ${machine}`}
        title={`New session in a draft — a private copy of ${where ?? 'this project'}`}
        className="border-l-accent-foreground/30"
        onClick={(event) => onDraft(event.currentTarget)}
      >
        <DraftIcon className="size-4" />
      </IconButton>
    </span>
  );
}


/**
 * A MACHINE'S PROJECTS: the inventory of one gateway, opened from the thing that
 * names that gateway.
 *
 * IT IS NOT A CREATE, and for a long time it said it was. `openManageProjects` opens
 * `ManageProjectsSheet` on `Projects · <machine>` — choose the machine's current
 * project, remove one, or take the `New project…` at its foot — so a control spelled
 * "New project" promised the last of the three things behind it, and a plus on the
 * band would have promised the same plus one row below meant a session. So it wears
 * the NOUN it opens: the app's one mark for a place on disk, the same folder a
 * project row carries, with the fork inside it left to mean a copy of that place.
 *
 * It stands in two places — above the card while the list is scoped to one machine,
 * and on each machine's band in the All view — and it NAMES its machine, because in
 * the fleet view several of them are on screen at once.
 *
 * `face="word"` is for the one seam where the mark cannot teach itself: a machine
 * with nothing under its band has no project row on screen to say what the folder
 * means, so its empty body carries the word instead of a glyph with no example.
 *
 * `pressEffect="none"`: the sheet it opens is anchored on this button's measured box,
 * and a transform moves the box that was measured.
 */
export function MachineProjectsButton({
  machine,
  face = 'mark',
  onPress,
}: {
  machine: string;
  face?: 'mark' | 'word';
  onPress: (anchor: HTMLElement) => void;
}) {
  const label = `Projects on ${machine}`;
  const title = `Projects on ${machine} — choose one, add one, remove one`;
  if (face === 'word')
    return (
      <Button
        type="button"
        variant="primary"
        density="compact"
        pressEffect="none"
        className="shrink-0 whitespace-nowrap"
        aria-label={label}
        title={title}
        onClick={(event) => onPress(event.currentTarget)}
      >
        Projects
      </Button>
    );
  return (
    <IconButton
      variant="primary"
      label={label}
      title={title}
      onClick={(event) => onPress(event.currentTarget)}
    >
      <ProjectsIcon className="size-4" />
    </IconButton>
  );
}

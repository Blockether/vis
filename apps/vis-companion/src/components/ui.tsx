/**
 * THE CONTROL VOCABULARY. Every pressable thing in the app is one of these, and a
 * screen COMPOSES them — a screen never paints a control of its own.
 *
 * How a control is made here, in order:
 *
 * 1. WAIT FOR THE SECOND CALL SITE. One screen's button is that screen's button.
 *    At the second one it moves in here, in the same commit, with both call sites
 *    switched over — a control invented for a single caller is a guess about the
 *    next one, and the guess is what goes stale.
 * 2. THE PROPS NAME A STATE, NEVER A PAINT: `isOn`, `isSelected`, `isBusy`,
 *    `tone`, `density`, `variant`. A caller may not hand paint down — the only
 *    `className` a control accepts POSITIONS it (`flex-1`, `shrink-0`, a grid
 *    cell) and the whole-tree scan in `ui.test.tsx` fails when a paint utility
 *    arrives at one of these components.
 * 3. TOKENS ONLY. Tailwind v4 utilities over the tokens in `index.css`: no
 *    component CSS, no inline styles, no hex, and no raw radius — the ladder is
 *    `rounded-chip | rounded-control | rounded-field | rounded-panel`.
 * 4. TWO FACES, AND ONLY TWO. `sm:` answers "is there room" and owns layout;
 *    only `mouse:` may make a control tighter (44px under a finger, 28px under a
 *    pointer). A control has no other responsive behaviour.
 * 5. IT SAYS ITS OWN NAME. Anything without a visible word takes a `label` and
 *    wears it as `aria-label`, and a state is `aria-pressed` / `aria-expanded` /
 *    `aria-current` — never a colour alone, which a screen reader cannot see and
 *    a colour-blind reader cannot tell apart.
 * 6. IT SHIPS WITH A STORY, in the same commit. `ui.stories.tsx` holds the
 *    vocabulary; a control that needs DATA to say anything gets
 *    `<Component>.stories.tsx` beside it, and the data is `dev/story-data.ts`.
 *    Draw the states that can break it — long name, zero, busy, failed — because
 *    that gallery is where the design is LOOKED at. The suite pins behaviour and
 *    the tree-wide rules; it never pins a size, a spacing or a token.
 */
import {
  createContext,
  forwardRef,
  useContext,
  useEffect,
  useId,
  useRef,
  useState,
  type ButtonHTMLAttributes,
  type InputHTMLAttributes,
  type MouseEvent,
  type PointerEvent,
  type ReactNode,
} from 'react';

import { createPortal } from 'react-dom';

import {
  CheckIcon,
  ChevronIcon,
  CloseIcon,
  CopyIcon,
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
    /**
     * A WORD SETS A CONTROL'S WIDTH; A MARK DOES NOT.
     *
     * `box` is the button with a word in it — the word measures it, so it is a
     * rounded rectangle on the control rung. `disc` is the same button with its
     * word replaced by a glyph: nothing inside it has a width to earn, so it is a
     * 32px circle (28px under a pointer), which is the box `CloseButton isBand`
     * has drawn since it was written and nothing else in the app copied.
     *
     * It is a PROP rather than a `className` because the two shapes disagree about
     * `px-*` and `rounded-*`, and two utilities of equal specificity are settled by
     * Tailwind's emission order and never by the call site: `IconButton` asked for
     * `px-0` here for as long as it has existed and lost every time. Measured on the
     * shipping app before this prop: every icon-only control was a 38x32 rectangle
     * around a 16px mark, and 42x24 under a pointer — a face 1.75 times as wide as
     * it was tall, on a row of boxes that were 36, 38 and 42 wide.
     */
    shape?: 'box' | 'disc';
  }
>(function Button(
  {
    variant = 'primary',
    pressEffect = 'scale',
    density = 'default',
    isJoined = false,
    shape = 'box',
    className = '',
    disabled = false,
    onClick,
    onPointerDown,
    onPointerUp,
    ...props
  },
  ref,
) {
  const tapPress = useTapPress(onClick, disabled, onPointerDown, onPointerUp);
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
  // THE DISC IS THE BOX THAT NEVER LEARNED A WORD. It keeps the header's own 32px
  // rhythm and the 6px of invisible reach above and below it that makes the 44px
  // finger target, and under a pointer it is 28px — square, where the rectangle it
  // replaces was 24px tall and 42px wide: under the 28px floor on the one side an
  // eye checks against the control standing beside it.
  const frame =
    shape === 'disc'
      ? 'relative grid size-8 self-center place-items-center rounded-full after:absolute after:inset-x-0 after:-top-1.5 after:-bottom-1.5 after:content-[""] mouse:size-7 mouse:after:content-none'
      : `min-h-7 rounded-control px-2.5 py-0.5 sm:min-h-8 sm:px-3 sm:text-ui ${scale}`;

  return (
    <button
      ref={ref}
      disabled={disabled}
      className={`border text-meta font-bold transition-[background-color,border-color,color,opacity,transform,translate,scale,rotate] duration-150 ${press} focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 disabled:cursor-not-allowed disabled:opacity-100 disabled:shadow-none motion-reduce:transition-none ${frame} ${joined} ${styles} ${className}`}
      {...tapPress}
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
 * AND IT IS A DISC. A word is what makes a control wide and a mark is not, so the box
 * around a mark is square — and a square control's honest corner is the circle, the
 * one this app already drew on the way out of a dialog and nowhere else. Reported
 * (paraphrased: line the borders and the icons up, and make them round): the bar's
 * glass and cog, the strip's folder mark, the band's plus and the pager's two steps
 * were five rectangles at three different widths around marks at three different
 * sizes, every one of them wider than it was tall.
 *
 * A control that ENDS a row keeps the row's cell instead (`edge`): it is not a box
 * standing on the page, it is that row's trailing edge, and a circle cannot stretch
 * to a row's height.
 *
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
    /**
     * This is the row's ONE trailing action, so its column is the same 48px touch /
     * 36px mouse cell as the `CloseButton` that ends the sheet above it. A row with a
     * cluster of disclosure controls keeps the narrower gutter geometry instead.
     */
    fullCell?: boolean;
  }
>(function IconButton(
  { label, className = '', variant = 'secondary', density = 'compact', edge, fullCell, children, ...props },
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
    ? `h-auto justify-items-end self-stretch border-r-0 pl-0 pr-3 -mr-3 after:content-none sm:pr-4 sm:-mr-4 mouse:h-auto ${
        fullCell ? 'w-12 mouse:w-9' : 'min-w-10 sm:min-w-12 mouse:min-w-10'
      }`
    : 'place-items-center';
  return (
    <Button
      ref={ref}
      type="button"
      variant={variant}
      pressEffect="none"
      shape={edge ? 'box' : 'disc'}
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
 * "THERE IS MORE OF THIS", and there is only one of it.
 *
 * A RULE WITH THE WORDS STANDING IN IT, because what is hidden is a CUT: the
 * line is where the content stops and the words say how much stopped there. It
 * used to be a boxed bar wearing an arrow, while the activity band spelled the
 * same promise three other ways — a chevron, a bare `+2 more lines`, a
 * guillemet — so one fact wore four marks and none of them was the shape of a
 * cut. The TUI draws this rule too (`more-rule` in
 * `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/render.clj`),
 * so both surfaces say it the same way.
 *
 * `label` is what a screen reader hears ("Load 12 more artifacts") while the
 * children are what the eye reads. With no `onClick` nothing CAN be loaded —
 * the rest is already gone — so it is a rule and not a button, and it reports
 * the count instead of offering to show it.
 */
export function LoadMore({
  label,
  tone = 'muted',
  className = '',
  children,
  onClick,
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & {
  label: string;
  /**
   * The rule takes the paper it lies on: on a failure's own surface the muted
   * ink measures 3.1:1, so there the line and the words are the error's.
   */
  tone?: 'muted' | 'error';
}) {
  const ink = tone === 'error' ? 'text-err-ink' : 'text-dialog-hint';
  const line = tone === 'error' ? 'bg-err-edge' : 'bg-dialog-edge';
  const shape = `mt-1.5 flex min-h-6 w-full min-w-0 items-center gap-2 font-mono text-meta ${ink} ${className}`;
  const inside = (
    <>
      <span aria-hidden="true" className={`h-px min-w-3 flex-1 ${line}`} />
      <span className="min-w-0 truncate">{children}</span>
      <span aria-hidden="true" className={`h-px min-w-3 flex-1 ${line}`} />
    </>
  );
  if (!onClick) {
    return (
      <div className={shape} aria-label={label}>
        {inside}
      </div>
    );
  }
  return (
    <button
      type="button"
      aria-label={label}
      onClick={onClick}
      className={`${shape} transition-colors duration-150 hover:bg-hover focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 motion-reduce:transition-none`}
      {...props}
    >
      {inside}
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
  density = 'default',
  className = '',
  children,
}: {
  /** What lands on the clipboard. */
  value: string;
  /** What the control is called: "Copy code", "Copy session id". */
  label: string;
  /** Hover text, when there is more to say than the label — the full id. */
  title?: string;
  /**
   * `compact` is a SCREEN HEADER's rhythm — the one `Button` already spells: a 32px
   * face on touch, 24px under a pointer, and Apple's 44px target restored as invisible
   * slop rather than as paint. The default belongs to a CARD's own band, which is 32px
   * tall and centres a 24px chip, so a chip that grew there would stack padding on top
   * of its own height.
   */
  density?: 'default' | 'compact';
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
  // ONE control, two rhythms. On a card's band it keeps its own paper, because that band
  // is content and the chip is the only control on it. A SCREEN's band is CHROME: the
  // chip drops its paper for the page's ink and joins the row of quiet controls every
  // other header in the app already wears.
  //
  // The minimum width keeps "Copy" and "Copied" the same box, so the chip never jumps
  // under the finger that just pressed it — but only while the word is THERE. Where a
  // phone header has room for the mark alone, the box goes square instead of holding a
  // word's worth of air open beside the control next to it.
  const face =
    density === 'compact'
      ? `relative h-8 min-w-8 border-transparent bg-transparent text-ui after:absolute after:inset-x-0 after:-top-1.5 after:-bottom-1.5 after:content-[""] mouse:h-6 mouse:min-w-6 mouse:text-meta mouse:after:content-none sm:min-w-[6ch] ${
          isCopied ? 'text-ok' : 'text-white'
        }`
      : `h-6 min-w-[6ch] bg-button text-chip ${
          isCopied ? 'border-ok text-ok' : 'border-dialog-edge text-button-foreground'
        }`;
  return (
    <button
      type="button"
      onClick={copy}
      aria-label={label}
      title={title ?? label}
      className={`group inline-flex items-center justify-center gap-1 rounded-control border px-2 text-center font-mono transition-colors duration-150 hover:bg-hover focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 motion-reduce:transition-none ${face} ${className}`}
    >
      {isCopied ? (
        <CheckIcon className="size-3 text-ok" />
      ) : (
        <CopyIcon className="size-3 opacity-60 transition-opacity group-hover:opacity-100" />
      )}
      <span
        className={`min-w-0 truncate ${density === 'compact' ? 'hidden sm:inline' : ''}`}
      >
        {isCopied ? 'Copied' : children}
      </span>
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
 * Framing and pointer density are the two real differences. A row standing on
 * the page needs no frame; a row inside a card needs one. `compact` spends less
 * height only when a mouse is present, so a touch row keeps its full target.
 * Selection stays the amber edge over raised paper in every form.
 */
export const ListRow = forwardRef<
  HTMLButtonElement,
  ButtonHTMLAttributes<HTMLButtonElement> & {
    isSelected?: boolean;
    isFramed?: boolean;
    density?: 'regular' | 'compact';
  }
>(function ListRow(
  {
    isSelected = false,
    isFramed = false,
    density = 'regular',
    className = '',
    ...props
  },
  ref,
) {
  const paper = isFramed
    ? `border ${isSelected ? 'border-accent bg-panel-2' : 'border-dialog-edge bg-panel'}`
    : isSelected
      ? 'bg-panel-2'
      : '';
  const spacing = density === 'compact' ? 'mouse:min-h-8 mouse:py-0' : '';
  return (
    <button
      ref={ref}
      type="button"
      className={`flex min-h-12 w-full min-w-0 items-center gap-2 px-3 py-2 text-left transition-colors duration-150 hover:bg-hover focus-visible:bg-hover focus-visible:outline-none disabled:cursor-default disabled:opacity-50 disabled:hover:bg-transparent motion-reduce:transition-none ${paper} ${spacing} ${className}`}
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
 * kind, did not read as a control at all. So a non-sizing overlay boxes the
 * group in `err-edge` — a layout border made the confirmation taller than the
 * row it replaces — and the cost sentence is a PROP inside that box.
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
    // The overlay REPLACES the list rule above it without contributing pixels
    // of its own. A layout border made this confirmation taller than the calm
    // row it replaces even though both answer buttons already owed 48px.
    <div
      role="group"
      aria-label={question}
      className="relative after:pointer-events-none after:absolute after:-top-px after:inset-x-0 after:bottom-0 after:border after:border-err-edge"
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
          className="flex flex-1 items-center justify-center bg-panel-2 font-mono text-meta font-bold uppercase tracking-[0.08em] text-dialog-hint transition-colors duration-150 hover:bg-hover hover:text-fg focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-accent/60 motion-reduce:transition-none"
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
 *
 * `chronology` is the odd one because what it opens is not a band at all: it is
 * one step of a run, a sentence in the transcript's own result ink, so the row
 * keeps the weight it is read at and only the chevron says it opens.
 *
 * A `caption` also stops at its own words (`w-auto`): every other tone fills the row
 * it shares, but a caption owns the space above a block, and a press target running
 * the whole width of that block would open the text from empty paper.
 * `caption` is the name of a block that stands OUTSIDE it, over its top-left
 * corner: caps at chip size in hint ink, the caption this app already writes over
 * a field. A framed transport is the loud thing on that row, so its own name steps
 * off the frame instead of competing inside it.
 *
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
  tone?: 'step' | 'thinking' | 'muted' | 'caption' | 'branch' | 'chronology';
  /**
   * Gives the row's own gutter back: the chevron lines its ink up with the card's
   * leading edge while the press target keeps the padding a finger needs. It is
   * the row's spacing, so it belongs to the row rather than to a `-ml-2 px-2`
   * pair spelled at a call site, where the two halves can drift apart.
   *
   * The paper reaches LEFT and stays flush RIGHT, so the width grows back exactly
   * what the margin took: a fill that began at the chevron's own ink read as a
   * row with nothing in front of it, and one that then stopped eight pixels short
   * of the column it sits in read as a row missing its last eight pixels.
   */
  bleed?: boolean;
}) {
  const ink =
    tone === 'step'
      ? `${BAND_NAME} text-accent-ink hover:bg-hover`
      : tone === 'thinking'
        ? 'font-bold italic tracking-[0.07em] text-thinking hover:text-dialog-hint-key'
        : tone === 'caption'
          ? 'uppercase tracking-[0.08em] text-dialog-hint hover:text-accent-ink'
          : tone === 'branch'
            ? 'font-bold text-white hover:bg-hover'
            : tone === 'chronology'
              ? 'text-code-result hover:bg-hover'
              : 'text-footer-muted hover:bg-hover';
  return (
    <button
      type="button"
      data-disclosure-toggle
      aria-expanded={isOpen}
      className={`flex min-h-8 min-w-0 cursor-pointer select-none items-center gap-1.5 text-left font-mono ${tone === 'branch' ? 'text-ui' : 'text-chip'} transition-colors duration-150 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 motion-reduce:transition-none mouse:min-h-6 ${tone === 'caption' ? 'w-auto' : bleed ? 'w-[calc(100%_+_0.5rem)]' : 'w-full'} ${bleed ? '-ml-2 px-2' : ''} ${ink} ${className}`}
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
 *
 * It is INK on the band's own paper, never a plate. `--dialog-title` IS the accent in
 * `blockether-dark`, so a filled way-out painted a yellow block into the navigation
 * bar — above the title it leads, and beside a composer send wearing that same token,
 * which is two filled accents on one screen and a bar outranking its own screen. A bar
 * carries navigation, and navigation is a glyph in the page's ink.
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
      className={`grid w-[calc(2.75rem+env(safe-area-inset-left))] shrink-0 place-items-center pl-[env(safe-area-inset-left)] text-white transition-[background-color,transform,translate,scale,rotate] duration-150 hover:bg-hover focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-accent/60 active:scale-[0.96] motion-reduce:transition-none mouse:w-[calc(2.5rem+env(safe-area-inset-left))] ${className}`}
      {...props}
    >
      <ChevronIcon back className="size-4" aria-hidden />
    </button>
  );
}


/**
 * A ROW OF A COMPLETION LIST, and there is only one of it.
 *
 * `@file` and `/command` are the same gesture answered twice, and they were the
 * same forty classes written twice — including the one that matters and is easy
 * to forget: the mousedown default is CANCELLED, because a completion list must
 * not take the caret out of the composer it is completing. That is behaviour, so
 * it belongs to the control and not to whoever remembers it.
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
      onMouseDown={(event) => event.preventDefault()}
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
 * A TAP IS THE PRESS, because iOS does not always finish one as a `click`.
 *
 * WKWebView turns a touch into a click through its own synthetic-click path,
 * and that path can decide the tap was a hover and dispatch none — reported as
 * "in a new session the send does nothing; tap the top so the keyboard hides,
 * tap send again and it works". The composer stood above the keyboard the whole
 * time and the button flashed under the finger, so the touch reached this
 * control and only the click was missing. Pointer events are raised from the
 * touch itself, so the press is read there and the click that may or may not
 * follow the SAME gesture is swallowed. A mouse and a keyboard still arrive as
 * a click; a finger that slid off the control still releases nothing, which is
 * exactly what a click would have done.
 */
function useTapPress(
  onPress: ((event: MouseEvent<HTMLButtonElement>) => void) | undefined,
  isDisabled: boolean,
  onPointerDown?: (event: PointerEvent<HTMLButtonElement>) => void,
  onPointerUp?: (event: PointerEvent<HTMLButtonElement>) => void,
) {
  const gesture = useRef<'idle' | 'down' | 'pressed'>('idle');
  const isOver = (event: PointerEvent<HTMLButtonElement>) => {
    const box = event.currentTarget.getBoundingClientRect();
    return (
      event.clientX >= box.left &&
      event.clientX <= box.right &&
      event.clientY >= box.top &&
      event.clientY <= box.bottom
    );
  };
  return {
    onPointerDown: (event: PointerEvent<HTMLButtonElement>) => {
      onPointerDown?.(event);
      gesture.current = 'down';
    },
    onPointerUp: (event: PointerEvent<HTMLButtonElement>) => {
      onPointerUp?.(event);
      // A release that did not start here is no press at all: `click` only
      // fires when one element saw both halves of the gesture.
      if (gesture.current !== 'down') return;
      // Anything refused here falls back to the click, if one comes.
      gesture.current = 'idle';
      if (isDisabled || event.button !== 0 || event.defaultPrevented) return;
      // A touch is captured by whatever was pressed, so the coordinates are the
      // only thing that says the finger left the control before it lifted.
      if (!isOver(event)) return;
      gesture.current = 'pressed';
      onPress?.(event);
    },
    onClick: (event: MouseEvent<HTMLButtonElement>) => {
      const isEcho = gesture.current === 'pressed';
      gesture.current = 'idle';
      if (isEcho) return;
      onPress?.(event);
    },
  };
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
 * glyph in the strip, `send` is the verb the strip exists for, `stop` is the same
 * disc replacing send in its reserved slot, and `recording` is `quiet` while it is
 * listening. Nothing here is a `className` at the call site, because a strip whose
 * boxes disagree is exactly what this replaced.
 *
 * The PRESS itself is `useTapPress` above: this strip is tapped with the
 * keyboard up more than anything else in the app, and on iOS a tap is not
 * reliably a `click`.
 */
export function ComposerButton({
  label,
  tone = 'quiet',
  surface = 'strip',
  isHolding = false,
  className = '',
  children,
  disabled = false,
  onClick,
  onPointerDown,
  onPointerUp,
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & {
  /** Icon-only, so the name is not optional. */
  label: string;
  tone?: 'quiet' | 'send' | 'stop' | 'recording' | 'voice';
  /** A lone control over content keeps the platform's full finger target. */
  surface?: 'strip' | 'overlay';
  /**
   * A press-and-hold is UNDERWAY. The paper rises through the button for as
   * long as the switch takes, so the gesture reports itself while it happens
   * instead of only when it lands — the one confirmation available to an app
   * with no haptics.
   */
  isHolding?: boolean;
}) {
  const press = useTapPress(onClick, disabled, onPointerDown, onPointerUp);
  const overlayFrame =
    surface === 'overlay'
      ? 'size-11 border border-dialog-edge shadow-[4px_4px_0_var(--dialog-shadow)]'
      : '';
  const face = {
    quiet: 'h-8 w-7 text-dialog-hint hover:bg-hover hover:text-dialog-hint-key disabled:text-muted mouse:h-7 mouse:w-6',
    recording: `${
      surface === 'overlay' ? overlayFrame : 'h-8 w-7 mouse:h-7 mouse:w-6'
    } animate-pulse bg-warn-surface text-err disabled:text-muted motion-reduce:animate-none`,
    // The MODE, not an action: the button keeps the strip's box and changes its
    // paper, so "which microphone am I holding" is answered by the control
    // itself rather than by a badge stuck to its corner.
    voice: `${
      surface === 'overlay' ? overlayFrame : 'h-8 w-7 mouse:h-7 mouse:w-6'
    } bg-accent text-accent-foreground hover:bg-accent-2 disabled:bg-button disabled:text-muted`,
    // NOTHING TO SEND is a control with no PAPER, not a dimmed arrow on paper.
    // Dimming kept the filled square and greyed the mark inside it: hint ink on
    // button paper measures 1.96:1 in blockether-light, under the 3:1 floor, so
    // the reader could not see the thing being greyed. The paper is what leaves
    // — the state is a SHAPE — and the arrow, now on the field's own paper,
    // measures 5.27:1.
    send: 'size-8 rounded-full border border-dialog-edge bg-dialog-title text-dialog-title-foreground hover:bg-accent-2 disabled:scale-100 disabled:border-transparent disabled:bg-transparent disabled:text-dialog-hint mouse:size-7',
    // It REPLACES send in the reserved slot, so it owns the same disc itself.
    // Depending on the parent's dimensions made the gallery stretch it across a row.
    stop: 'size-8 rounded-full border border-err bg-cancelled hover:bg-warn-surface starting:scale-90 starting:opacity-0 mouse:size-7',
  }[tone];
  return (
    <button
      type="button"
      aria-label={label}
      disabled={disabled}
      {...press}
      className={`relative grid shrink-0 place-items-center overflow-hidden ${tone === 'send' || tone === 'stop' ? '' : 'rounded-control'} transition-[background-color,color,opacity,transform,translate,scale,rotate] duration-150 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-accent/60 active:scale-[0.94] motion-reduce:transition-none ${face} ${className}`}
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
 *
 * A chip that reports a LEVEL leads with its mark, so the row lays its children
 * out itself: `inline-flex` with one gap, and the caller passes a word.
 */
export function MetaButton({
  isPicker = false,
  className = '',
  children,
  disabled = false,
  onClick,
  onPointerDown,
  onPointerUp,
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & { isPicker?: boolean }) {
  const press = useTapPress(onClick, disabled, onPointerDown, onPointerUp);
  return (
    <button
      type="button"
      disabled={disabled}
      {...press}
      className={`inline-flex items-center gap-1 px-1 py-1 text-left font-mono text-chip font-semibold uppercase tracking-[0.08em] transition-colors duration-150 hover:text-accent-ink focus-visible:text-accent-ink focus-visible:outline-none motion-reduce:transition-none ${
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
 * WHETHER THE CHOICES AROUND THIS ONE STAND INSIDE A NESTED CLUSTER.
 *
 * `SettingsChoiceGroup` owns the depth of what it holds, so a cell ASKS the cluster it
 * stands in instead of taking a prop every call site would have to keep in step with the
 * group it is already written inside.
 */
const IsNestedChoice = createContext(false);
/**
 * ONE VALUE OF A SETTING, and there is only one of it.
 *
 * Theme, where a session starts: a segmented grid where
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
 *
 * A LEAF SPENDS ONE LINE. Reported over the open TTS panel: ten full-bleed bars of
 * the same height, each carrying the same two lines of capitals, read as a shutter
 * rather than a list — and the choice that OWNED the voices under it weighed exactly
 * as much as its own children. A cell with nothing nested beneath it takes `isLeaf`:
 * the name leads, its quiet meta trails on the same line, and the row gives back a
 * line of height. The two-line stack is what a cell keeps when it owns the cluster
 * that follows, and when it is one column of a segmented grid (speech rate), where
 * there is no width for a trailing meta.
 *
 * A NAME CAN BE THE WHOLE FACT, and then the cell takes no `sub` at all. Every theme is
 * called `Blockether Light`, `Solarized Dark`, `Vis Light`, so a trailing `light`/`dark`
 * restated the last word of its own row down the whole list and answered nothing. Left
 * out, the row is the name and its mark, and the mark takes the edge the meta held.
 *
 * DEPTH IS NOT THE CELL'S OWN DECISION. Inside a nested `SettingsChoiceGroup` its content
 * steps one notch right — the second channel that draws the tree — while the row itself
 * stays full-bleed, because the row is a thumb target and the panel edge is where a thumb
 * lands.
 */
export function ChoiceCell({
  title,
  sub,
  isSelected,
  isLeaf = false,
  showSelectionMark = true,
  leadingAction,
  className = '',
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & {
  title: string;
  /**
   * The quiet word beside or under the name — a voice's language, a rate's word. Left out when
   * the name already carries it.
   */
  sub?: string;
  isSelected: boolean;
  /** Nothing nests under this choice: one line, with `sub` trailing instead of stacked. */
  isLeaf?: boolean;
  /** Hide the choice glyph when an adjacent action occupies its trailing place. */
  showSelectionMark?: boolean;
  /**
   * An independent icon action that sits before the value's name, never after it. Its compact
   * fixed-width cell ends at the grid's dark hairline; the choice owns the breathing room beyond it.
   */
  leadingAction?: {
    label: string;
    icon: ReactNode;
    disabled?: boolean;
    onClick: () => void;
  };
}) {
  const isNested = useContext(IsNestedChoice);
  const choice = (
    <button
      type="button"
      aria-pressed={isSelected}
      className={`flex min-w-0 items-center gap-3 text-left transition-[background-color,color,transform,translate,scale,rotate] duration-150 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-accent active:scale-[0.99] disabled:opacity-45 motion-reduce:transition-none ${
        isNested
          ? leadingAction
            ? 'pl-3 pr-3'
            : 'pl-6 pr-3'
          : leadingAction
            ? 'pl-3 pr-3'
            : 'px-3'
      } ${
        isLeaf ? 'min-h-9 mouse:min-h-8' : 'min-h-10 justify-between py-1.5 mouse:min-h-9'
      } ${
        isSelected ? 'bg-accent text-accent-foreground' : 'bg-input text-white hover:bg-hover'
      } ${className}`}
      {...props}
    >
      {isLeaf ? (
        <>
          <span className="min-w-0 truncate font-mono text-ui font-bold">{title}</span>
          {sub && (
            <span className="ml-auto min-w-0 truncate font-mono text-chip">{sub}</span>
          )}
        </>
      ) : (
        <span className="min-w-0">
          <span className="block truncate font-mono text-ui font-bold">{title}</span>
          {sub && (
            <span className="block truncate font-mono text-chip uppercase tracking-wider">
              {sub}
            </span>
          )}
        </span>
      )}
      {showSelectionMark && (
        <span
          className={`shrink-0 font-mono text-meta font-black${sub ? '' : ' ml-auto'}`}
          aria-hidden="true"
        >
          {isSelected ? '●' : '○'}
        </span>
      )}
    </button>
  );
  if (!leadingAction) return choice;

  return (
    <div className="grid min-w-0 grid-cols-[2.5rem_minmax(0,1fr)]">
      <button
        type="button"
        aria-label={leadingAction.label}
        disabled={leadingAction.disabled}
        onClick={leadingAction.onClick}
        className={`grid min-h-9 place-items-center border-r border-dialog-edge transition-[background-color,color,transform,translate,scale,rotate] duration-150 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-accent active:scale-[0.99] disabled:opacity-45 motion-reduce:transition-none mouse:min-h-8 ${
          isSelected ? 'bg-accent text-accent-foreground' : 'bg-input text-white hover:bg-hover'
        }`}
      >
        {leadingAction.icon}
      </button>
      {choice}
    </div>
  );
}

/**
 * ONE ENGINE ROW WITH TWO INDEPENDENT ACTIONS: choose it, or inspect its settings.
 *
 * Selection and disclosure used to be one accidental action: choosing an engine exposed
 * its children, while an unselected engine had no way to show its own catalogue. The row
 * is one visual surface split by one hairline, but it remains two keyboard targets. Both
 * halves keep the selected fill so the chevron never looks detached from its owner.
 */
export function SettingsChoiceDisclosure({
  title,
  sub,
  isSelected,
  isOpen,
  controls,
  onSelect,
  onToggle,
}: {
  title: string;
  sub: string;
  isSelected: boolean;
  isOpen: boolean;
  controls: string;
  onSelect: () => void;
  onToggle: () => void;
}) {
  return (
    <div className="grid grid-cols-[minmax(0,1fr)_2.5rem]">
      <ChoiceCell title={title} sub={sub} isSelected={isSelected} onClick={onSelect} />
      <button
        type="button"
        aria-label={`Settings for ${title}`}
        aria-expanded={isOpen}
        aria-controls={controls}
        onClick={onToggle}
        className={`grid min-h-10 w-10 place-items-center border-l transition-[background-color,color] duration-150 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-accent active:bg-accent-2 motion-reduce:transition-none mouse:min-h-9 ${
          isSelected
            ? 'border-accent-foreground/30 bg-accent text-accent-foreground'
            : 'border-dialog-edge bg-input text-dialog-hint hover:bg-hover hover:text-white'
        }`}
      >
        <ChevronIcon open={isOpen} className="size-3 shrink-0" />
      </button>
    </div>
  );
}

/**
 * A NAMED CLUSTER OF SETTINGS CHOICES, distinct from its neighbouring clusters.
 *
 * Depth is DRAWN, never spent as empty space. A nested cluster used to indent its whole
 * body, so its rows stood a step in from the left while still reaching the right edge: a
 * pale gutter down one side that nothing closed at the bottom. Rows stay full-bleed because
 * the hairlines belong to the parent grid. The heading and, through `IsNestedChoice`, every
 * cell's content move one notch right while the rows remain full-width thumb targets.
 *
 * The heading stays on the panel's paper rather than becoming a filled band. Its lower
 * hairline separates the name from the controls, while the nested cluster's top rule, left
 * rail and full-width foot make every boundary explicit. All use the panel's structural ink;
 * amber belongs to selection and to the top-level heading notch, never to nesting.
 */
export function SettingsChoiceGroup({
  label,
  isNested = false,
  children,
}: {
  label: string;
  isNested?: boolean;
  children: ReactNode;
}) {
  const headingId = useId();
  return (
    <section
      role="group"
      aria-labelledby={headingId}
      className={
        isNested
          ? 'min-w-0 border-l-2 border-t border-dialog-edge bg-panel'
          : 'min-w-0'
      }
    >
      <header
        className={`flex min-h-6 items-center bg-panel pb-1.5 pt-3 ${
          isNested ? 'border-b border-dialog-edge pl-6 pr-3' : 'px-3'
        }`}
      >
        <h4
          id={headingId}
          className="font-mono text-chip font-bold uppercase tracking-[0.12em] text-dialog-hint"
        >
          {label}
        </h4>
      </header>
      <IsNestedChoice.Provider value={isNested}>{children}</IsNestedChoice.Provider>
      {isNested && <div className="h-px w-full bg-dialog-edge" />}
    </section>
  );
}
/**
 * A SETTINGS DIRECTION THAT OPENS its concrete choices.
 *
 * The row keeps the setting name and current value visible while closed, and the one
 * trailing chevron names the hidden list. It is a whole-row target because ASR and TTS
 * are destinations, not tiny icon actions beside inert copy.
 */
export function SettingsDisclosure({
  label,
  value,
  isOpen,
  className = '',
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & {
  label: string;
  value: string;
  isOpen: boolean;
}) {
  return (
    <button
      type="button"
      aria-expanded={isOpen}
      className={`flex min-h-12 w-full min-w-0 items-center gap-3 px-3 py-2 text-left transition-colors duration-150 hover:bg-hover focus-visible:bg-hover focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-accent motion-reduce:transition-none mouse:min-h-10 ${className}`}
      {...props}
    >
      <span className="min-w-0 flex-1">
        <span className="block font-mono text-ui font-black uppercase tracking-[0.08em] text-white">
          {label}
        </span>
        <span className="block truncate font-mono text-chip text-dialog-hint">{value}</span>
      </span>
      <ChevronIcon open={isOpen} className="size-3 shrink-0 text-dialog-hint" />
    </button>
  );
}

/**
 * ON OR OFF, AND THE KNOB IS WHERE THE ANSWER IS.
 *
 * A feature toggle used to be a WORD — `ON`/`OFF` set in the same mono as the rest
 * of the app, amber when on. Reported over this dialog (paraphrased: these have to
 * be real toggles, the modern kind with the little circle in them, it is the most
 * natural thing there is): a switch is the one control a reader recognises before
 * reading it, and a settings column that spells its state out in type puts a second
 * mono word on every row to argue with the label it belongs to. So the state is
 * POSITION — knob left, knob right — and the colour only agrees with it, which is
 * also how it keeps its meaning without hue.
 *
 * 46x28 on touch and 40x24 under a pointer, the design skill's own settings figures,
 * with the 44px finger target arriving as invisible reach above and below (32px on a
 * pointer, the floor) rather than as paint.
 *
 * OFF WEARS THE RESTING FRAME, because a switched-off toggle had NO BOX AT ALL.
 * It drew `border-transparent` over `bg-panel-2`, and `--panel2` is the same value
 * as `--surface` in both bundled palettes (#faf3eb light, #161820 dark): the fill
 * that was supposed to be the control's own paper measures 1.00:1 against the row
 * it sits in, and the border was transparent by name — so a settings row ended in
 * a grey word floating on the page, reported as "these off buttons are not visible
 * at all: why no border?". The track keeps `border-edge-strong` (1.38:1 light,
 * 1.40:1 dark), the hairline `Button`'s `secondary` already draws, and the knob is
 * that same ink, so an off switch is a filled circle on the row's own paper.
 *
 * It reports its own work — the knob pulses and `aria-busy` says so — because a
 * setting is a round trip to a gateway and a control that snaps back a second later
 * without saying why is a bug report. `role="switch"` and `aria-checked` are the
 * control's, not the caller's.
 */
export function Switch({
  label,
  isOn,
  isBusy,
  className = '',
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & {
  /** A knob is not a name, so the name is not optional. */
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
      className={`relative inline-flex h-7 w-[2.875rem] shrink-0 items-center rounded-full border p-0.5 transition-colors duration-150 ease-out after:absolute after:inset-x-0 after:-top-2 after:-bottom-2 after:content-[""] focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 disabled:opacity-45 motion-reduce:transition-none mouse:h-6 mouse:w-10 mouse:after:-top-1 mouse:after:-bottom-1 ${
        isOn ? 'border-accent bg-accent' : 'border-dialog-hint bg-transparent hover:bg-hover'
      } ${className}`}
      {...props}
    >
      <span
        aria-hidden
        className={`size-[1.375rem] rounded-full transition-transform duration-150 ease-out mouse:size-[1.125rem] ${
          isOn
            ? 'translate-x-[1.125rem] bg-accent-foreground mouse:translate-x-4'
            : 'translate-x-0 bg-dialog-hint'
        } ${isBusy ? 'animate-pulse motion-reduce:animate-none' : ''}`}
      />
    </button>
  );
}

/**
 * IS THIS DEVICE CONNECTED TO THIS MACHINE — the answer, and the verb that changes
 * it, in the one control at the end of the panel's own band.
 *
 * The notifications panel used to answer an OPERATOR's question instead: it
 * listed every push token the gateway holds, so one iPhone reinstalled three
 * times stood in it four times under four masked tokens, and the reader's own
 * question — am I connected? — survived only as the verb on a button. Reported
 * as: same device, four entries, and no way to just see whether alerts arrive.
 *
 * THE CONTROL IS THE VERB, AND NOTHING ELSE. It used to be `Switch`, whose entire
 * face is the state it is ALREADY in, standing under two lines that had just said
 * it: `Not connected`, then `<machine> will not alert this device.`, then a box
 * reading `OFF` — the same no three times over. Dropping the switch for a verb left
 * the two lines behind, and they were reported again: the panel is too big, I want
 * one Connect/Disconnect button there. `Switch` still belongs to a setting this
 * device owns outright (a feature toggle, an MCP server) where the press IS the new
 * state; this one is a round trip to a machine that can refuse, and a round trip is
 * a verb.
 *
 * THEN THE MARK BECAME THE SWITCH. Reported over the same panel (paraphrased: make
 * these buttons circles with icons and put them in the headers, the settings are too
 * big) the word became a bell in the band's trailing cell; reported again over that
 * bell (paraphrased: drop the address beside it — notifications are simply on or off,
 * so that should be a toggle too), the bell became the control every other on/off
 * setting in this dialog already wears. The round trip did not go away, it is spoken
 * by the same `isBusy` every gateway-backed toggle here uses: the knob pulses while
 * the machine is being asked. The sentence survives as the pointer's `title` —
 * `Connect notifications from <machine>` — and the machine's name rides the switch's
 * own accessible name, so the band no longer prints an address a reader already read
 * three rows above.
 */
export function NotifyConnectionSwitch({
  machine,
  isOn,
  isBusy = false,
  isChecking = false,
  disabled = false,
  onClick,
}: {
  /** The paired machine this control speaks for; it names itself in the sentence. */
  machine: string;
  isOn: boolean;
  isBusy?: boolean;
  /** Before the first answer lands, what this device is registered for is unknown. */
  isChecking?: boolean;
  disabled?: boolean;
  onClick: () => void;
}) {
  const isWaiting = isBusy || isChecking;
  // The pointer gets the whole sentence, because a switch says WHICH WAY it will go
  // and not what that means; a screen reader gets the setting's name and its state
  // from the control itself.
  const action = isChecking
    ? `Asking ${machine} whether this device is registered`
    : isOn
      ? `Disconnect notifications from ${machine}`
      : `Connect notifications from ${machine}`;
  return (
    <Switch
      label={`Notifications from ${machine}`}
      isOn={isOn}
      isBusy={isWaiting}
      title={action}
      disabled={disabled}
      onClick={onClick}
    />
  );
}

/**
 * THE ✕, AND THERE IS EXACTLY ONE OF IT.
 *
 * Eight surfaces were left by three different buttons in five different boxes,
 * measured on an iPhone 14 with the shipped stylesheet. They had already been made
 * one MARK; they were never one BUTTON, and an eye reads the face before it reads the
 * stroke.
 *
 * So there is one, and everything about its face is decided here:
 *
 * - TARGET AND FACE ARE DIFFERENT. A ✕ either ends a band (`isBand`) with the band's
 *   full 48×48 target (36×36 for a mouse), stands alone at the end of a row
 *   (`isStandalone`) with a 32px circular face, or sits inside another control as a
 *   32px mark (`mouse:size-6`). The band target carries a 32px circular FACE, 28px for
 *   a mouse: the visible control rides the header rhythm while the invisible room
 *   around it keeps the way out comfortably hittable. A wrapped title can make the
 *   band taller; the target still stretches with it while its face stays round.
 * - A BAND CLOSE IS A COMPACT BRANDED DISC, NOT A SECOND VERB. The two Blockether
 *   palettes carry the SAME amber pair, mirrored: on Light's dark title paper the face is
 *   an amber fill with dark ink, and on Dark's amber title band it is that pair swapped —
 *   an ink fill with an amber mark. Leaving Dark's face transparent made the way out the
 *   faintest thing on a screen whose Share is a filled amber block: one hairline in the
 *   band's own colour. Every other palette keeps the hairline face, which is the only one
 *   its quieter band can afford. This one fixed circle may coexist with Share or Download
 *   because it identifies the way out rather than offering a competing act; it never
 *   licenses another filled label in chrome. Hover deepens the disc — amber to its darker
 *   step, ink to the theme's hover paper — and never paints a square across the band's
 *   last cell.
 * - A close inside another control keeps the established hairline and red intent wash.
 *   Its parent already supplies the face, so wrapping that mark in another circle would
 *   be a box inside a box.
 *
 * WHERE it sits is the call site's only business: `className` may POSITION it (the
 * attachment chip hangs it on the chip's right edge) and nothing else. `isBand` and
 * `isStandalone` name PLACES — a band's last target or a row action without a parent
 * face — rather than caller-selected paint.
 */
export function CloseButton({
  label,
  isBand = false,
  isStandalone = false,
  className = '',
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & {
  /** Icon-only, so the name is not optional: "Close artifacts", "Remove notes.md". */
  label: string;
  /**
   * This ✕ IS THE BAND'S LAST TARGET — a dialog title, a menu heading — rather than
   * a mark inside another control. It fills the band's height; the compact circular
   * face inside it stays on the control rhythm instead of inflating to the touch target.
   */
  isBand?: boolean;
  /** This ✕ ends a row without another control supplying its face. */
  isStandalone?: boolean;
}) {
  const mark = <CloseIcon />;
  return (
    <button
      type="button"
      aria-label={label}
      title={label}
      className={`grid shrink-0 place-items-center text-current motion-reduce:transition-none ${
        isBand
          ? 'group w-12 self-stretch transition-opacity duration-150 focus-visible:outline-none disabled:pointer-events-none disabled:cursor-not-allowed disabled:opacity-40 mouse:w-9'
          : isStandalone
            ? 'size-8 self-center rounded-full border border-current/20 transition-[background-color,color,transform] duration-150 hover:bg-err/15 hover:text-err focus-visible:bg-err/15 focus-visible:text-err focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 active:scale-[0.94] disabled:cursor-not-allowed disabled:opacity-40 mouse:size-7'
            : 'size-8 self-center border-l border-current/20 transition-colors duration-150 hover:bg-err/15 hover:text-err focus-visible:bg-err/15 focus-visible:text-err focus-visible:outline-none disabled:cursor-not-allowed disabled:hover:bg-transparent disabled:hover:text-current mouse:size-6'
      } ${className}`}
      {...props}
    >
      {isBand ? (
        <span className="grid size-8 place-items-center rounded-full border border-current transition-[background-color,box-shadow,transform] duration-150 blockether-light:bg-accent blockether-light:text-accent-foreground blockether-dark:bg-accent-foreground blockether-dark:text-accent group-hover:bg-current/15 blockether-light:group-hover:bg-accent-2 blockether-dark:group-hover:bg-hover group-focus-visible:ring-2 group-focus-visible:ring-current/60 blockether-dark:group-focus-visible:ring-accent-foreground/60 group-active:scale-[0.94] motion-reduce:transition-none mouse:size-7">
          {mark}
        </span>
      ) : (
        mark
      )}
    </button>
  );
}

/**
 * A VERB IN A BAND, and there is only one of it.
 *
 * `CloseButton isBand` is the cell that ENDS a band; this is the same cell for what
 * the band also OFFERS. A domain verb earns its word. Universal chrome can replace
 * that word with a mark by supplying `label`; the label names it, the cell becomes
 * square, and the mark stays optically centred instead of inheriting the text nudge.
 *
 * It is not `Button`: a bordered box on a title band claims a rank chrome has not
 * earned, and a smaller face inside the band gives a finger less target than the X
 * one hairline away. The cell fills the band and is welded by its own left rule.
 *
 * A CELL WITH SOMETHING TO COMMIT WEARS THE ACCENT (`isPrimary`). The accent exists
 * only while the commit is live; a disabled cell drops it instead of dimming a false
 * promise.
 */
export function BandButton({
  className = '',
  isPrimary = false,
  label,
  children,
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & {
  /** This cell COMMITS something, and wears the accent while it has something to commit. */
  isPrimary?: boolean;
  /** The accessible name of an icon-only cell; providing it also gives the mark a square target. */
  label?: string;
}) {
  const isLive = isPrimary && !props.disabled;
  const isIconOnly = Boolean(label);
  return (
    <button
      type="button"
      className={`grid shrink-0 place-items-center self-stretch whitespace-nowrap border-l border-current/20 font-mono text-meta font-bold transition-colors duration-150 focus-visible:outline-none disabled:cursor-not-allowed disabled:opacity-60 disabled:hover:bg-transparent motion-reduce:transition-none sm:text-ui mouse:text-meta ${
        isIconOnly ? 'w-12 px-0 mouse:w-9' : 'px-3 sm:px-4 mouse:px-3'
      } ${
        isLive
          ? 'bg-accent text-accent-foreground hover:bg-accent-2 focus-visible:bg-accent-2'
          : 'text-current hover:bg-current/10 focus-visible:bg-current/10'
      } ${className}`}
      aria-label={label}
      title={label}
      {...props}
    >
      <span className={isIconOnly ? '' : 'translate-y-px'}>{children}</span>
    </button>
  );
}

export const Input = forwardRef<HTMLInputElement, InputHTMLAttributes<HTMLInputElement>>(
  function Input({ className = '', ...props }, ref) {
    // A masked field's dots sit shoulder-to-shoulder at this type step, so a
    // typed key cannot be counted; the tracking is what breathes between them.
    const masked = props.type === 'password' ? 'tracking-[0.15em]' : '';
    return (
      <input
        ref={ref}
        className={`min-h-7 w-full rounded-control border border-edge bg-input px-2.5 py-0.5 font-mono text-meta text-white transition-[border-color,box-shadow] duration-150 placeholder:text-dialog-hint focus:border-accent focus:outline-none focus:ring-1 focus:ring-accent/30 motion-reduce:transition-none sm:min-h-8 sm:px-3 sm:text-ui ${masked} ${className}`}
        {...props}
      />
    );
  },
);

/**
 * A SHORT STATE MESSAGE, with one optional title band and one way out.
 *
 * Plain banners remain compact prose. A titled banner owns its two-line hierarchy and
 * padding here, while its dismiss control becomes the trailing band cell: no call site
 * can strand the close mark inside spare left/right padding or put the detail back beside
 * the title. `neutral` is for information that is neither success, warning nor failure.
 */
export function Banner({
  kind,
  title,
  dismiss,
  children,
}: {
  kind: 'neutral' | 'ok' | 'warn' | 'err';
  title?: ReactNode;
  dismiss?: { label: string; onClick: () => void };
  children: ReactNode;
}) {
  const colors = {
    neutral: 'border-edge-strong bg-level-project text-footer-strong',
    ok: 'border-ok/50 bg-ok/10 text-ok',
    warn: 'border-warn-strong/60 bg-warn-surface text-warn',
    err: 'border-err/50 bg-err/10 text-err',
  }[kind];
  const isTitled = title !== undefined;

  return (
    <div
      className={`border font-mono text-body ${colors} ${
        isTitled ? 'flex min-h-12 items-stretch p-0' : 'px-3 py-2'
      }`}
      role="status"
    >
      {isTitled ? (
        <>
          <div className="min-w-0 flex-1 px-3 py-1">
            <div className="block text-title font-bold">{title}</div>
            <div className="block truncate text-body text-footer-strong">{children}</div>
          </div>
          {dismiss ? (
            <CloseButton isBand label={dismiss.label} onClick={dismiss.onClick} />
          ) : null}
        </>
      ) : (
        children
      )}
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
const DIALOG_DESKTOP_HEIGHT = 'sm:h-[min(38rem,100%)]';

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
 * machine's header are the same height on the same screen. The dialog frame owns every
 * outer radius. The band's lower edge stays square and full width so the scrolling body
 * meets it on one exact seam; rounding that edge exposed two wedges of body paper and
 * made the band read as a capsule laid over a second surface.
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
      role="presentation"
      className={`flex min-h-12 shrink-0 items-stretch overflow-hidden bg-dialog-title text-dialog-title-foreground mouse:min-h-9 ${
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
          <p className="truncate font-mono text-meta text-dialog-title-foreground">
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

/**
 * AN OPENED ARTIFACT IS THE WHOLE SCREEN, and there is one of it.
 *
 * A document opened from the transcript and a settled RUN opened from the row
 * beside it are the same gesture — so they are the same screen: the band names
 * the artifact, reports what it IS under that name, and the way out is the
 * band's own cell. The body under it takes every remaining pixel and owns its
 * own scrolling.
 *
 * IT IS THE VIEWPORT-PINNED LAYER, NOT THE GLASS. It mounts where `Modal`
 * mounts (`overlayLayer`), `absolute` in the shell the keyboard driver pins, so
 * a raised keyboard cannot bury a field at its bottom edge. Escape is the way
 * out a keyboard has, and it is here rather than at a call site so every
 * artifact answers that key the same.
 */
export function OverlayScreen({
  title,
  subtitle,
  actions,
  onClose,
  children,
}: {
  title: string;
  /** What the band REPORTS about the artifact under its name. */
  subtitle?: ReactNode;
  /** The artifact's own verbs, as cells of this band before the ✕. */
  actions?: ReactNode;
  onClose: () => void;
  children: ReactNode;
}) {
  useEffect(() => {
    function onKey(event: KeyboardEvent) {
      if (event.key === 'Escape') onClose();
    }
    window.addEventListener('keydown', onKey);
    return () => window.removeEventListener('keydown', onKey);
  }, [onClose]);

  const { position } = overlayLayer();
  return (
    <div
      className={`${position} inset-0 z-50 flex h-full min-h-0 min-w-0 flex-col overflow-hidden overscroll-contain bg-panel pt-[env(safe-area-inset-top)]`}
    >
      <DialogHeader
        title={title}
        subtitle={subtitle}
        actions={actions}
        closeLabel={`Close ${title}`}
        onClose={onClose}
      />
      <div className="flex min-h-0 min-w-0 flex-1 flex-col">{children}</div>
    </div>
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
      className={`flex min-h-0 flex-1 flex-col overflow-hidden rounded-t-panel border-t-2 border-accent bg-panel ${
        isFitSheet ? '' : 'pt-[env(safe-area-inset-top)]'
      } pb-[env(safe-area-inset-bottom)] shadow-none transition-[opacity,transform,translate,scale,rotate] duration-300 ease-[cubic-bezier(0.22,0.61,0.36,1)] starting:translate-y-full starting:opacity-0 motion-reduce:transition-none sm:rounded-panel sm:border sm:border-dialog-edge sm:pt-0 sm:pb-0 sm:shadow-[8px_8px_0_var(--dialog-shadow)] sm:duration-200 sm:starting:translate-y-2 ${className}`}
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

/** The shared leading inset for dialog bands and navigator rows. */
export const LIST_EDGE = 'pl-3 sm:pl-4';
/**
 * RUNNING PROSE, and the app has exactly ONE rule for it.
 *
 * The transcript is one reading column: reasoning, answers, speech, and the user's
 * own text align both edges. Inline code remains an atomic left-aligned box inside
 * that column, while hyphenation and pretty wrapping moderate the spaces between
 * ordinary words on narrow phones.
 */
export const PROSE =
  'hyphens-auto [hyphenate-limit-chars:6_3_3] text-pretty text-justify';

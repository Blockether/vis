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
 *    component CSS, no inline styles, no hex. Surfaces and controls use square
 *    corners (`rounded-none`), including switches, chips, fields and sheets.
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
  type HTMLAttributes,
  type InputHTMLAttributes,
  type MouseEvent,
  type PointerEvent,
  type ReactNode,
} from 'react';

import { createPortal } from 'react-dom';
import * as SelectPrimitive from '@radix-ui/react-select';
import viewSchema from '../../../../packages/vis-contract/resources/vis-contract/schema/view.json';

import { AlertIcon, CheckIcon, ChevronIcon, CloseIcon, CopyIcon, SidebarIcon } from './icons';

/**
 * Shared reading roles for settings, lists and forms. Size, weight and quiet ink
 * belong to the role; callers control placement only. ONE STEP FOR BOTH FACES: a
 * heading, a label and an option each took a step UP under a finger, so the sheet
 * that reads as a column on a desktop read as a poster on a phone — a provider's
 * whole name claimed the width of the screen and clipped mid-word. Only a
 * control's BOX is tighter under a pointer; the words are the same size on both,
 * and descriptions and metadata never shrink below their readable steps.
 * Use inherited ink inside a selected control or a semantic status container.
 */
export function Text({
  variant,
  as: Tag = 'span',
  tone = 'default',
  className = '',
  ...props
}: HTMLAttributes<HTMLElement> & {
  variant: 'heading' | 'section' | 'label' | 'option' | 'description' | 'meta';
  as?: 'span' | 'p' | 'h3' | 'h4';
  tone?: 'default' | 'inherit';
}) {
  const role = {
    heading: 'text-title font-semibold',
    section: 'text-body font-semibold',
    label: 'text-title font-medium',
    option: 'text-title font-normal',
    description: 'text-body font-normal',
    meta: 'text-ui font-normal',
  }[variant];
  const ink =
    tone === 'inherit'
      ? 'text-inherit'
      : variant === 'description' || variant === 'meta'
        ? 'text-dialog-hint'
        : 'text-white';
  return (
    <Tag
      {...props}
      className={`font-mono normal-case tracking-normal ${role} ${ink} ${className}`}
    />
  );
}

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
     * Text buttons share a 32px touch face and a 28px pointer face.
     * Invisible reach preserves a 44px touch target; owners leave at least 8px
     * between adjacent targets. `compact` centres the button in a header and
     * uses metadata type under a pointer. `panel` keeps fixed horizontal padding.
     */
    density?: 'default' | 'compact' | 'panel';
  }
>(function Button(
  {
    variant = 'primary',
    pressEffect = 'scale',
    density = 'default',
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
  // Disabled colours belong to each text-button variant.
  const dimmed = 'disabled:border-edge disabled:bg-panel-2 disabled:text-muted';
  // Desktop hover changes foreground only. Base/selected surfaces, press feedback
  // and keyboard focus are independent states. Filled primary and inverse controls
  // keep their contrast pair when no stronger foreground is available.
  const styles = {
    primary: `border-accent bg-accent text-accent-foreground ${dimmed}`,
    secondary: `border-edge-strong bg-transparent text-white enabled:hover:text-accent-ink ${dimmed}`,
    // Quiet text actions stay frameless beside the primary.
    quiet:
      'border-transparent bg-transparent text-dialog-hint enabled:hover:text-white disabled:border-transparent disabled:bg-transparent disabled:text-muted',
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
      'border-transparent bg-transparent text-white enabled:hover:text-err-ink disabled:border-transparent disabled:bg-transparent disabled:text-muted',
    // The red stays INK and the fill stays a wash, exactly as `MenuItem`'s danger
    // row does — one destructive language in both.
    danger: `border-err/40 bg-err/10 text-err enabled:hover:text-err-ink ${dimmed}`,
    // A control that floats over CONTENT — a thumbnail, a picture, a note's own first
    // lines — rather than over chrome. It carries its own ink because whatever is under
    // it is not the app's paper, and it wears the same black block every other floating
    // control wears: `bg-ink/80` was ink by NAME only, and in a light theme that token
    // resolves near-white, so the glyph disappeared into the page it sat on. It is a
    // VARIANT rather than a class at the call site because two competing `bg-*` are
    // settled by Tailwind's emission order, never by which one a call site typed last.
    overlay:
      'border-transparent bg-dialog-title text-dialog-title-foreground disabled:border-transparent disabled:bg-panel-2 disabled:text-muted',
    // A split button's caret half is NOT a second variant: it is `primary` with a
    // hairline in `accent-foreground`.
  }[variant];
  // The transform utilities are OMITTED rather than overridden: `active:scale-100`
  // and `active:scale-[0.98]` have equal specificity, so a call-site override would
  // be decided by Tailwind's emission order, not by the call site.
  const press = pressEffect === 'scale' ? 'active:scale-[0.98] disabled:active:scale-100' : '';
  // The pseudo-element extends the padding box (inside the 1px border) to 44px.
  const touchReach =
    'relative after:absolute after:inset-x-0 after:-top-[7px] after:-bottom-[7px] after:content-[""] mouse:after:content-none';
  const scale = {
    default: `${touchReach} min-h-8 px-2.5 text-ui sm:px-3 mouse:min-h-7`,
    compact: `${touchReach} h-8 min-h-8 px-2.5 self-center text-ui sm:px-3 mouse:h-7 mouse:min-h-7 mouse:text-meta`,
    panel: `${touchReach} min-h-8 px-3 font-mono text-ui mouse:min-h-7`,
  }[density];
  const frame = `rounded-none py-0.5 ${scale}`;

  return (
    <button
      ref={ref}
      disabled={disabled}
      className={`border text-meta font-bold transition-[background-color,border-color,color,opacity,transform,translate,scale,rotate] duration-150 ${press} focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 disabled:cursor-not-allowed disabled:opacity-100 motion-reduce:transition-none ${frame} ${styles} ${className}`}
      {...tapPress}
      {...props}
    />
  );
});

// Icon controls never draw an enclosing border, ring or circular face. Keyboard
// focus adds an underline instead; the hit target remains independent of the glyph.
const iconControlClass =
  'relative border-0 rounded-none focus-visible:outline-none before:pointer-events-none before:absolute focus-visible:before:inset-x-1 focus-visible:before:bottom-0 focus-visible:before:h-0.5 focus-visible:before:bg-current focus-visible:before:content-[""]';

/**
 * A named, borderless icon action. Variants change intent and ink, not framing.
 * The 32px / 28px layout box keeps a 44px touch target through invisible reach,
 * and its centered mark rides the same trailing rail a row's menu mark rides.
 * Over-content controls retain a square backing for contrast, never a circle.
 */
export const IconButton = forwardRef<
  HTMLButtonElement,
  ButtonHTMLAttributes<HTMLButtonElement> & {
    /** Icon-only, so the name is not optional. */
    label: string;
    variant?: 'primary' | 'secondary' | 'quiet' | 'danger' | 'overlay' | 'remove';
    /**
     * Passed through: a control over a thumbnail is not on a header's rhythm.
     * `band` is the mark that stands BESIDE a header's facts instead of on its
     * trailing rail — a pager's steps — so under a pointer it takes the band's
     * 24px step and reads as chrome rather than as a box. Touch is unchanged.
     */
    density?: 'default' | 'compact' | 'band';
  }
>(function IconButton(
  {
    label,
    className = '',
    variant = 'secondary',
    density = 'compact',
    children,
    disabled = false,
    onClick,
    onPointerDown,
    onPointerUp,
    ...props
  },
  ref,
) {
  const tapPress = useTapPress(onClick, disabled, onPointerDown, onPointerUp);
  // ONE BOX, ONE RAIL. A mark is read by its center, and the trailing rail the eye
  // follows down a list is its row menus': a row keeps its gutter (`pr-3 sm:pr-4`)
  // and then this same 32px/28px box, so a header mark that keeps the box centers
  // on that rail at every width and pointer. A glyph pinned to the paper's edge
  // instead stood half a box inside it — the settings headers' add marks, reported
  // over that dialog as a cross that did not sit on the dots below it. `band` is the
  // one step off that rail: navigation that stands beside a header's facts, never a
  // row's own mark, so nothing lines up under it to be missed.
  const box = `size-8 self-center place-items-center after:absolute after:-inset-1.5 after:content-[""] mouse:after:content-none ${
    density === 'band' ? 'mouse:size-6' : 'mouse:size-7'
  }`;
  const ink = {
    primary: 'bg-transparent text-accent-ink enabled:hover:text-white',
    secondary: 'bg-transparent text-white enabled:hover:text-accent-ink',
    quiet: 'bg-transparent text-dialog-hint enabled:hover:text-white',
    danger: 'bg-transparent text-err-ink',
    remove: 'bg-transparent text-white enabled:hover:text-err-ink',
    overlay: 'bg-dialog-title text-dialog-title-foreground',
  }[variant];
  return (
    <button
      ref={ref}
      type="button"
      aria-label={label}
      disabled={disabled}
      className={`${iconControlClass} grid shrink-0 items-center font-bold transition-colors duration-150 disabled:cursor-not-allowed disabled:text-muted motion-reduce:transition-none ${density === 'default' ? 'text-ui' : 'text-ui mouse:text-meta'} ${box} ${ink} ${className}`}
      {...tapPress}
      {...props}
    >
      {children}
    </button>
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
      className={`relative inline-flex min-h-8 min-w-11 shrink-0 items-center justify-center gap-1.5 border px-2 font-mono text-ui font-bold transition-colors duration-150 after:absolute after:inset-x-0 after:-top-[7px] after:-bottom-[7px] after:content-[""] focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 disabled:cursor-not-allowed disabled:opacity-40 motion-reduce:transition-none mouse:min-h-7 mouse:min-w-7 mouse:text-meta mouse:after:content-none ${
        isOn
          ? 'border-accent bg-accent text-accent-foreground'
          : 'border-edge-strong bg-transparent text-dialog-hint enabled:hover:text-white'
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
 * `apps/vis-tui/src/com/blockether/vis/tui/render.clj`), so both surfaces say it
 * the same way.
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
  const target = onClick ? 'min-h-11 mouse:min-h-7' : 'min-h-6';
  const shape = `mt-1.5 flex ${target} w-full min-w-0 items-center gap-2 font-mono text-meta ${ink} ${className}`;
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
      className={`${shape} transition-colors duration-150 enabled:hover:text-white focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 motion-reduce:transition-none`}
      {...props}
    >
      {inside}
    </button>
  );
}

/**
 * Copies a value without activating the surrounding header or disclosure.
 * Without children, uses the app's quiet icon button and a checkmark on success.
 * Children retain a visible value, such as a session id.
 */
export function CopyChip({
  value,
  label,
  title,
  onError,
  density = 'default',
  edge = false,
  className = '',
  children,
}: {
  /** Text to copy, or a lazy source cancelled when the control unmounts. */
  value: string | ((signal: AbortSignal) => Promise<string>);
  /** What the control is called: "Copy code", "Copy session id". */
  label: string;
  /** Hover text, when there is more to say than the label — the full id. */
  title?: string;
  /** Show copy failures beside the owning content; an empty message clears the previous failure. */
  onError?: (message: string) => void;
  /**
   * Both contexts share the 32px touch / 28px pointer face and 44px touch reach.
   * `compact` joins screen-header chrome; the default labels a value on a card.
   */
  density?: 'default' | 'compact';
  /** Execution-band copy: fixed trailing inset, with touch reach into the outer gutter. */
  edge?: boolean;
  /** Placement only; the chip's own face is fixed. */
  className?: string;
  /** Visible value at rest; omit for an icon-only copy action. */
  children?: ReactNode;
}) {
  const [isCopied, setIsCopied] = useState(false);
  const [isCopying, setIsCopying] = useState(false);
  const [error, setError] = useState('');
  const pending = useRef<AbortController | null>(null);
  const reset = useRef<number | undefined>(undefined);
  useEffect(
    () => () => {
      pending.current?.abort();
      pending.current = null;
      window.clearTimeout(reset.current);
    },
    [],
  );
  async function copy(event: MouseEvent<HTMLButtonElement>) {
    event.preventDefault();
    event.stopPropagation();
    if (pending.current) return;
    const controller = new AbortController();
    pending.current = controller;
    window.clearTimeout(reset.current);
    setIsCopied(false);
    setIsCopying(true);
    setError('');
    onError?.('');
    try {
      if (typeof value === 'string') {
        await navigator.clipboard.writeText(value);
      } else {
        if (typeof ClipboardItem !== 'undefined' && navigator.clipboard?.write) {
          // WebKit needs write() during the press, before retained history finishes loading.
          const blob = Promise.resolve().then(async () => {
            const result = await value(controller.signal);
            controller.signal.throwIfAborted();
            return new Blob([result], { type: 'text/plain' });
          });
          const write = async () =>
            navigator.clipboard.write([new ClipboardItem({ 'text/plain': blob })]);
          await Promise.all([blob, write()]);
        } else {
          const text = await value(controller.signal);
          controller.signal.throwIfAborted();
          await navigator.clipboard.writeText(text);
        }
      }
      if (controller.signal.aborted) return;
      setIsCopied(true);
      reset.current = window.setTimeout(() => setIsCopied(false), 1_500);
    } catch (cause) {
      if (!controller.signal.aborted) {
        const message = cause instanceof Error ? cause.message : 'Clipboard unavailable.';
        setError(message);
        onError?.(message);
        controller.abort();
      }
    } finally {
      if (pending.current === controller) {
        pending.current = null;
        setIsCopying(false);
      }
    }
  }
  const tapPress = useTapPress(copy, isCopying);
  const statusLabel = isCopying
    ? 'Copying…'
    : isCopied
      ? 'Copied'
      : error
        ? 'Copy failed. Try again.'
        : label;
  const statusTitle = error || (isCopying || isCopied ? statusLabel : (title ?? label));
  const failure =
    error && !onError ? (
      <span role="alert" className="sr-only">
        {error} Try again.
      </span>
    ) : null;
  if (children === undefined) {
    const icon = isCopying ? (
      <Spinner />
    ) : isCopied ? (
      <CheckIcon className="size-3 text-ok" />
    ) : error ? (
      <AlertIcon className="size-3 text-err-ink" />
    ) : (
      <CopyIcon className="size-3" />
    );
    if (edge) {
      // Reserve less space before the glyph; keep the 44px touch target in the outer gutter.
      return (
        <button
          type="button"
          aria-label={statusLabel}
          title={statusTitle}
          aria-busy={isCopying || undefined}
          disabled={isCopying}
          {...tapPress}
          className={`${iconControlClass} grid h-auto w-8 shrink-0 self-stretch items-center justify-items-end bg-transparent text-dialog-hint transition-colors duration-150 enabled:hover:text-white pl-0 pr-3 -mr-3 after:absolute after:top-0 after:bottom-0 after:left-0 after:-right-3 after:content-[""] motion-reduce:transition-none sm:w-9 sm:pl-0 sm:pr-4 sm:-mr-4 sm:after:-right-2 mouse:h-auto mouse:w-7 mouse:after:content-none ${className}`}
        >
          {icon}
          {failure}
        </button>
      );
    }
    return (
      <IconButton
        label={statusLabel}
        title={statusTitle}
        aria-busy={isCopying || undefined}
        disabled={isCopying}
        variant="quiet"
        density={density}
        onClick={copy}
        className={className}
      >
        {icon}
        {failure}
      </IconButton>
    );
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
      ? `min-w-11 border-transparent bg-transparent mouse:min-w-7 sm:min-w-[6ch] ${
          isCopied ? 'text-ok' : 'text-white enabled:hover:text-accent-ink'
        }`
      : `min-w-[6ch] bg-button ${
          isCopied ? 'border-ok text-ok' : 'border-dialog-edge text-button-foreground'
        }`;
  return (
    <button
      type="button"
      onClick={copy}
      aria-label={statusLabel}
      title={statusTitle}
      aria-busy={isCopying || undefined}
      disabled={isCopying}
      className={`relative inline-flex h-8 items-center justify-center gap-1 rounded-none border px-2 text-center font-mono text-ui transition-colors duration-150 after:absolute after:inset-x-0 after:-top-[7px] after:-bottom-[7px] after:content-[""] focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 motion-reduce:transition-none mouse:h-7 mouse:text-meta mouse:after:content-none ${face} ${className}`}
    >
      {isCopying ? (
        <Spinner />
      ) : isCopied ? (
        <CheckIcon className="size-3 text-ok" />
      ) : error ? (
        <AlertIcon className="size-3 text-err-ink" />
      ) : (
        <CopyIcon className="size-3 opacity-60" />
      )}
      <span className={`min-w-0 truncate ${density === 'compact' ? 'hidden sm:inline' : ''}`}>
        {isCopying ? 'Copying…' : isCopied ? 'Copied' : children}
      </span>
      {failure}
    </button>
  );
}

/**
 * A full-width row that opens or selects a provider, model, gateway, artifact
 * version or preset. Hover changes the inherited foreground, never its surface.
 *
 * Framing and density are the two real differences. A row standing on the page
 * needs no frame; a row inside a card needs one. `compact` is a 36px band under
 * touch and 32px under a pointer; invisible slop restores the touch target to 44px
 * without making the paper taller. Selection stays the amber edge over raised paper
 * in every form.
 *
 * `inset` chooses the gutter a row keeps: the dialog's own rail — the same one its
 * title band, its section bands and its choice cells stand on — or the live view's
 * inset, so a row inside that view lines up with the cells above and below it.
 */
export const ListRow = forwardRef<
  HTMLButtonElement,
  ButtonHTMLAttributes<HTMLButtonElement> & {
    isSelected?: boolean;
    isFramed?: boolean;
    density?: 'regular' | 'compact';
    inset?: 'regular' | 'live-view';
  }
>(function ListRow(
  {
    isSelected = false,
    isFramed = false,
    density = 'regular',
    inset = 'regular',
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
  const spacing =
    density === 'compact'
      ? 'relative min-h-9 py-0.5 after:absolute after:inset-x-0 after:-inset-y-1 after:content-[""] mouse:min-h-8 mouse:py-0 mouse:after:content-none'
      : 'min-h-12 py-2';
  const gutter = inset === 'live-view' ? 'px-(--live-view-inset)' : 'px-3 sm:px-4';
  return (
    <button
      ref={ref}
      type="button"
      className={`flex w-full min-w-0 items-center gap-2 ${gutter} text-left transition-colors duration-150 enabled:hover:text-accent-ink focus-visible:bg-hover focus-visible:outline-none disabled:cursor-default disabled:opacity-50 motion-reduce:transition-none ${paper} ${spacing} ${className}`}
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
 * IT STANDS WHAT THE ROW STOOD. The two answers own a 48px floor, and a floor is
 * not a row's height: a session row that stacks its metadata under the title
 * measures 52px on a phone, so the question replacing it left the list four
 * pixels shorter and everything below it jumped the moment it was asked. The row
 * hands over what it MEASURED as it asked — `rowHeight` — and the block takes
 * that as its own minimum, the answers splitting whatever height it leaves. A
 * cost sentence can still make the block taller, because that sentence has to
 * stand somewhere; nothing makes it shorter than the row it replaced.
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
  rowHeight,
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
  /** What the row this replaces STOOD, in px, so the list keeps its own height. */
  rowHeight?: number;
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
      className="relative flex flex-col after:pointer-events-none after:absolute after:-top-px after:inset-x-0 after:bottom-0 after:border after:border-err-edge"
      style={rowHeight === undefined ? undefined : { minHeight: rowHeight }}
    >
      {cost !== undefined && (
        // The rule that separates the cost from its two answers belongs to the
        // SENTENCE, not to the answers: a border on that row would eat a pixel
        // of the 48px both answers owe a finger.
        <p className="border-b border-err-edge px-3 py-2 font-mono text-chip text-dialog-hint">
          {cost}
        </p>
      )}
      <div className="flex min-h-12 flex-1 items-stretch mouse:min-h-8">
        <button
          type="button"
          autoFocus
          className="flex flex-1 items-center justify-center bg-panel-2 font-mono text-meta font-bold uppercase tracking-[0.08em] text-dialog-hint transition-colors duration-150 enabled:hover:text-fg focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-accent/60 motion-reduce:transition-none"
          onClick={onKeep}
        >
          {keepLabel}
        </button>
        <button
          type="button"
          disabled={isBusy}
          className="flex flex-1 items-center justify-center border-l border-err-edge bg-err-surface font-mono text-meta font-bold uppercase tracking-[0.08em] text-err-ink transition-colors duration-150 active:bg-err active:text-white focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-err/70 disabled:opacity-60 motion-reduce:transition-none"
          onClick={onConfirm}
        >
          {confirmLabel}
        </button>
      </div>
    </div>
  );
}

/**
 * Execution names share one semibold weight, whether static or pressable.
 * Font size and primary ink match transcript text; counts stay secondary.
 */
const BAND_NAME = 'font-semibold tracking-[0.06em]';

/** Open an execution in a transient screen; unlike Disclosure, never folds content. */
export function ExecutionAction({
  className = '',
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement>) {
  return (
    <button
      type="button"
      className={`flex min-h-11 w-full min-w-0 items-center gap-1.5 text-left font-mono text-ui text-code-result transition-colors duration-150 enabled:hover:text-accent-ink focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 motion-reduce:transition-none mouse:min-h-7 ${className}`}
      {...props}
    />
  );
}

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
 * so that band's own name is italic, and bold with it. Its size is the size of
 * `BandLabel`: THINKING, CODE and ACTIVITY head one trace at one text size, as the
 * TUI paints all three through the same bold band label.
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
 * Execution and Thinking rows keep the chevron immediately after the NAME, never in a
 * reserved leading column and never at a row edge: what the row adds to that name — a
 * count, a summary, an elapsed time — follows the chevron in `tally`, so the mark that
 * opens the row stands where its words begin. Execution bands are 44px on touch, 28px
 * with a pointer.
 * Compact operation rows use a 24px face with invisible reach to those targets.
 * Place compact rows in an isolated container: the reach stays behind visible
 * controls and content, so adjacent rows never intercept each other's faces.
 * Other bands retain their existing 32px / 24px rhythm.
 */
export function Disclosure({
  isOpen,
  tone = 'muted',
  bleed = false,
  inlineChevron = false,
  tally,
  density = 'default',
  className = '',
  children,
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & {
  isOpen: boolean;
  tone?: 'step' | 'thinking' | 'muted' | 'caption' | 'branch' | 'chronology' | 'execution';
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
  /** Keep the chevron immediately after the label, not at either row edge. */
  inlineChevron?: boolean;
  /** Optional metadata after the chevron, so hiding it never shifts the label or icon. */
  tally?: ReactNode;
  /** Compact operation rows or comfortable 44px touch / 28px mouse controls. */
  density?: 'default' | 'compact' | 'comfortable';
}) {
  const ink =
    tone === 'step'
      ? `${BAND_NAME} text-white enabled:hover:text-accent-ink`
      : tone === 'thinking'
        ? 'font-bold italic tracking-[0.07em] text-thinking enabled:hover:text-dialog-hint-key'
        : tone === 'caption'
          ? 'uppercase tracking-[0.08em] text-dialog-hint enabled:hover:text-accent-ink'
          : tone === 'branch'
            ? 'font-bold text-white enabled:hover:text-accent-ink'
            : tone === 'chronology' || tone === 'execution'
              ? 'text-code-result enabled:hover:text-accent-ink'
              : 'text-footer-muted enabled:hover:text-accent-ink';
  const size =
    density === 'compact'
      ? 'relative min-h-6 text-ui mouse:text-meta after:absolute after:inset-x-0 after:-inset-y-2.5 after:-z-10 after:content-[""] mouse:after:-inset-y-0.5'
      : density === 'comfortable' || tone === 'execution'
        ? 'min-h-11 text-ui mouse:min-h-7'
        : `min-h-8 mouse:min-h-6 ${tone === 'step' || tone === 'branch' || tone === 'thinking' ? 'text-ui' : 'text-chip'}`;
  return (
    <button
      type="button"
      data-disclosure-toggle
      aria-expanded={isOpen}
      className={`flex min-w-0 cursor-pointer select-none items-center gap-1.5 text-left font-mono ${size} transition-colors duration-150 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 motion-reduce:transition-none ${tone === 'caption' ? 'w-auto' : bleed ? 'w-[calc(100%_+_0.5rem)]' : 'w-full'} ${bleed ? '-ml-2 px-2' : ''} ${ink} ${className}`}
      {...props}
    >
      {!inlineChevron && tone !== 'execution' && tone !== 'thinking' && (
        <ChevronIcon open={isOpen} className="size-3 shrink-0 opacity-70" />
      )}
      {children}
      {(inlineChevron || tone === 'execution' || tone === 'thinking') && (
        <ChevronIcon open={isOpen} className="size-3 shrink-0" />
      )}
      {tally}
    </button>
  );
}

/**
 * Transcript-sized execution name, shared by static and collapsible bands.
 * Named failure and interruption states retain their semantic ink.
 */
export function BandLabel({
  className = '',
  tone = 'default',
  weight = 'name',
  children,
}: {
  className?: string;
  /** Only failure and interruption labels override the primary text color. */
  tone?: 'default' | 'err' | 'hint';
  /**
   * `state` is the word a band ENDS on — LIVE, while the run is going — rather than
   * the name it opens with. It stands in the full weight of the verb beside it and
   * underlines under the pointer as that verb does, so the end of the row reads as
   * one line of type instead of a label wedged against a control.
   */
  weight?: 'name' | 'state';
  children: ReactNode;
}) {
  const ink = tone === 'err' ? 'text-err' : tone === 'hint' ? 'text-dialog-hint' : 'text-white';
  const face = weight === 'state' ? 'font-bold tracking-[0.06em] hover:underline' : BAND_NAME;
  return (
    <span className={`select-none truncate font-mono text-ui ${face} ${ink} ${className}`}>
      {children}
    </span>
  );
}

/**
 * Secondary metadata beside a band name: regular weight and readable touch sizing.
 */
export function BandTally({
  children,
  placement = 'inline',
}: {
  children: ReactNode;
  placement?: 'inline' | 'trailing';
}) {
  return (
    <span
      className={`font-mono font-normal tracking-normal text-ui tabular-nums text-dialog-hint mouse:text-meta ${placement === 'trailing' ? 'ml-auto min-w-0 break-words text-right' : ''}`}
    >
      {children}
    </span>
  );
}

/**
 * A checkbox or select option with a decorative mark and a wrapping label.
 *
 * The caller owns selection and ARIA semantics. The shared face keeps a 44px
 * touch target (28px with a mouse); long labels grow the row rather than truncate.
 * `HUMAN_INPUT_CHOICE_MARKS` supplies the exclusive or inclusive choice glyph.
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
      className={`flex min-h-11 w-full min-w-0 items-center gap-2 border px-2.5 py-1 text-left font-mono text-ui transition-colors duration-150 focus-visible:border-accent focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-accent/30 disabled:cursor-not-allowed disabled:text-muted motion-reduce:transition-none mouse:min-h-7 ${
        isOn
          ? 'border-accent bg-hover text-accent-ink'
          : 'border-edge bg-input text-white enabled:hover:text-accent-ink'
      } ${className}`}
      {...props}
    >
      <span aria-hidden="true" className="shrink-0">
        {mark}
      </span>
      <span className="min-w-0 whitespace-normal [overflow-wrap:anywhere]">{children}</span>
    </button>
  );
}

/**
 * Shared layout for Ask fields and Live nodes. Columns stack in source order;
 * rows fit equal-width columns of at least 12rem with a shared 12px gap, wrapping
 * when their container is too narrow. Below 12rem, one column fits the space.
 *
 * Each nested layout measures its own space through CSS grid, not the viewport.
 * This component owns no label, fieldset, disclosure, focus or value state.
 */
export function ViewLayout({
  direction = 'column',
  className = '',
  children,
  ...props
}: HTMLAttributes<HTMLDivElement> & { direction?: 'row' | 'column' }) {
  return (
    <div
      {...props}
      data-view-layout={direction}
      className={`grid min-w-0 items-start gap-3 [overflow-wrap:anywhere] [&>*]:min-w-0 ${
        direction === 'row'
          ? 'grid-cols-[repeat(auto-fit,minmax(min(100%,12rem),1fr))]'
          : 'grid-cols-1'
      } ${className}`}
    >
      {children}
    </div>
  );
}

/** Shared section typography; callers keep plain text or inline Markdown semantics. */
export function ViewHeading({
  level = 3,
  children,
}: {
  level?: 1 | 2 | 3 | 4 | 5 | 6;
  children: ReactNode;
}) {
  const Heading = `h${level}` as 'h1' | 'h2' | 'h3' | 'h4' | 'h5' | 'h6';
  return (
    <Heading
      className={`font-mono font-bold text-white ${level === 1 ? 'text-head' : level === 2 ? 'text-subhead' : 'text-title'}`}
    >
      {children}
    </Heading>
  );
}

/** Readable body text for either kind of view; parsing belongs to the caller. */
export function ViewParagraph({ children }: { children: ReactNode }) {
  return <p className={`font-mono text-body text-white ${PROSE}`}>{children}</p>;
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
const bandEdgeClass = `${iconControlClass} grid w-[calc(2.75rem+env(safe-area-inset-left))] shrink-0 place-items-center bg-transparent pl-[env(safe-area-inset-left)] text-white transition-[color,transform,translate,scale,rotate] duration-150 enabled:hover:text-accent-ink active:scale-[0.96] motion-reduce:transition-none mouse:w-[calc(2.5rem+env(safe-area-inset-left))]`;

export function BackButton({
  label,
  className = '',
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & { label: string }) {
  return (
    <button type="button" aria-label={label} className={`${bandEdgeClass} ${className}`} {...props}>
      <ChevronIcon back className="size-4" aria-hidden />
    </button>
  );
}

/**
 * THE DESK'S WAY BACK TO THE LIST, and it stands where the phone's arrow does.
 *
 * On a desk the session list is a column beside the transcript, so a session has
 * nothing to go back to — but the column can be put away to read wide, and this is
 * the ONE way to it. It wears `BackButton`'s own leading column with a panel glyph
 * in it, so it sits at the seam between the list and the pane it toggles and keeps
 * its place whether the list is up or away. In the app bar, beside the mark, it read
 * as a piece of the logo; at the pane's edge it reads as what it is.
 */
export function SidebarToggle({
  isShown,
  className = '',
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & { isShown: boolean }) {
  const label = isShown ? 'Hide the session list' : 'Show the session list';
  return (
    <button
      type="button"
      aria-label={label}
      title={label}
      aria-expanded={isShown}
      className={`${bandEdgeClass} ${className}`}
      {...props}
    >
      <SidebarIcon className="size-4" aria-hidden />
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
          : 'text-dialog-foreground enabled:hover:text-accent-ink'
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
 * Every tone is a bare glyph in the same 32px / 28px layout box. Intent changes
 * the ink, never an enclosing border or circular fill. Overlays keep a full
 * 44px target; strip controls carry invisible reach for touch — 44px tall, the
 * strip's own padding, and one pitch wide: the 32px box plus the strip's 4px
 * gap, so adjacent reaches tile instead of swallowing each other's press.
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
  const frame =
    surface === 'overlay'
      ? 'size-11'
      : 'size-8 after:absolute after:left-1/2 after:top-1/2 after:h-11 after:w-9 after:-translate-x-1/2 after:-translate-y-1/2 after:content-[""] mouse:size-7 mouse:after:content-none';
  const face = {
    quiet: 'text-dialog-hint enabled:hover:text-dialog-hint-key disabled:text-muted',
    recording: 'animate-pulse text-err-ink disabled:text-muted motion-reduce:animate-none',
    voice: 'text-accent-ink enabled:hover:text-white disabled:text-muted',
    send: 'text-accent-ink enabled:hover:text-white disabled:scale-100 disabled:text-muted',
    stop: 'text-err-ink starting:scale-90 starting:opacity-0',
  }[tone];
  return (
    <button
      type="button"
      aria-label={label}
      disabled={disabled}
      {...press}
      className={`${iconControlClass} grid shrink-0 place-items-center bg-transparent transition-[background-color,color,opacity,transform,translate,scale,rotate] duration-150 active:scale-[0.94] motion-reduce:transition-none ${frame} ${face} ${className}`}
      {...props}
    >
      {isHolding && (
        <span aria-hidden="true" className="pointer-events-none absolute inset-0 overflow-hidden">
          <span className="absolute inset-0 origin-bottom scale-y-100 bg-accent/30 transition-transform duration-[450ms] ease-linear starting:scale-y-0 motion-reduce:hidden" />
        </span>
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
 * Compact response controls tighten desktop tracking and icons without reducing
 * label size or press targets. `isFlush` drops the leading pad, so a button that
 * follows inline text stands against it instead of a step away from it.
 */
export function MetaButton({
  isPicker = false,
  isFlush = false,
  density = 'default',
  className = '',
  children,
  disabled = false,
  onClick,
  onPointerDown,
  onPointerUp,
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & {
  isPicker?: boolean;
  density?: 'default' | 'compact';
  isFlush?: boolean;
}) {
  const press = useTapPress(onClick, disabled, onPointerDown, onPointerUp);
  return (
    <button
      type="button"
      disabled={disabled}
      {...press}
      className={`relative inline-flex min-h-8 min-w-11 items-center gap-1 ${isFlush ? 'pr-1 pl-0' : 'px-1'} py-1 text-left font-mono text-meta font-semibold uppercase tracking-normal transition-colors duration-150 after:absolute after:inset-x-0 after:-top-1.5 after:-bottom-1.5 after:content-[""] enabled:hover:text-accent-ink focus-visible:text-accent-ink focus-visible:outline-none motion-reduce:transition-none mouse:min-h-7 mouse:min-w-7 mouse:after:content-none ${
        density === 'compact'
          ? 'mouse:tracking-normal mouse:[&>svg]:size-2.5'
          : 'mouse:tracking-[0.08em]'
      } ${
        isPicker
          ? 'text-dialog-hint-key underline decoration-dialog-edge decoration-1 underline-offset-4 enabled:hover:decoration-accent'
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
 * A queued turn or a pasted block that opens an editor. Hover changes only the
 * foreground. `isToken` adds a dotted underline for text standing in for more.
 *
 * `isBand` is the same prose ENDING AN EXECUTION BAND: the verb that stops a run,
 * sharing its line with the state word after it. It takes the band's weight, caps
 * and tracking, and wears no face at all — a box inside a line of text reads as a
 * box — so it says it is pressable by underlining itself under the pointer. The
 * 44px touch reach lives in a pseudo-element, leaving the row its own height.
 */
export function TextButton({
  isToken = false,
  isBand = false,
  className = '',
  children,
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & { isToken?: boolean; isBand?: boolean }) {
  const face = isBand
    ? 'relative select-none whitespace-nowrap font-bold uppercase tracking-[0.06em] text-white after:absolute after:inset-x-0 after:-inset-y-3.5 after:content-[""] enabled:hover:underline focus-visible:underline disabled:opacity-60 mouse:after:content-none'
    : `px-1 text-dialog-foreground enabled:hover:text-accent-ink focus-visible:bg-hover ${
        isToken ? 'truncate underline decoration-dotted underline-offset-2' : ''
      }`;
  return (
    <button
      type="button"
      className={`min-w-0 text-left font-mono text-ui transition-colors duration-150 focus-visible:outline-none disabled:cursor-not-allowed motion-reduce:transition-none ${face} ${className}`}
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
  variant = 'cell',
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
  /** List rows share the panel's paper and heading inset; cells retain the input surface. */
  variant?: 'cell' | 'list';
  /**
   * An independent, borderless icon action before the value's name. Its compact
   * fixed-width cell preserves the choice's breathing room without a dividing rule.
   */
  leadingAction?: {
    label: string;
    icon: ReactNode;
    disabled?: boolean;
    onClick: () => void;
  };
}) {
  const isNested = useContext(IsNestedChoice);
  const fill = isSelected
    ? 'bg-accent text-accent-foreground'
    : `${variant === 'list' ? 'bg-panel' : 'bg-input'} text-white enabled:hover:text-accent-ink`;
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
            : variant === 'list'
              ? 'px-3 sm:px-4'
              : 'px-3'
      } ${isLeaf ? 'min-h-11 mouse:min-h-8' : 'min-h-11 justify-between py-2 mouse:min-h-9'} ${fill} ${className}`}
      {...props}
    >
      {isLeaf ? (
        <>
          <Text variant="option" tone="inherit" className="min-w-0 truncate">
            {title}
          </Text>
          {sub && (
            <Text
              variant="meta"
              tone={isSelected ? 'inherit' : 'default'}
              className="ml-auto min-w-0 truncate"
            >
              {sub}
            </Text>
          )}
        </>
      ) : (
        <span className="min-w-0">
          <Text variant="option" tone="inherit" className="block truncate">
            {title}
          </Text>
          {sub && (
            <Text
              variant="meta"
              tone={isSelected ? 'inherit' : 'default'}
              className="block truncate"
            >
              {sub}
            </Text>
          )}
        </span>
      )}
      {showSelectionMark && (
        <span className={`shrink-0 font-mono text-ui${sub ? '' : ' ml-auto'}`} aria-hidden="true">
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
        className={`${iconControlClass} grid min-h-11 place-items-center transition-[background-color,color,transform,translate,scale,rotate] duration-150 active:scale-[0.99] disabled:opacity-45 motion-reduce:transition-none mouse:min-h-8 ${fill}`}
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
 * is one visual surface with two keyboard targets and no border around the chevron.
 * Both halves keep the selected fill so the chevron stays part of its owner's row.
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
    <div className="grid grid-cols-[minmax(0,1fr)_2.75rem] mouse:grid-cols-[minmax(0,1fr)_2.5rem]">
      <ChoiceCell title={title} sub={sub} isSelected={isSelected} onClick={onSelect} />
      <button
        type="button"
        aria-label={`Settings for ${title}`}
        aria-expanded={isOpen}
        aria-controls={controls}
        onClick={onToggle}
        className={`${iconControlClass} grid min-h-11 w-11 place-items-center transition-[background-color,color] duration-150 active:bg-accent-2 motion-reduce:transition-none mouse:min-h-9 mouse:w-10 ${
          isSelected
            ? 'bg-accent text-accent-foreground'
            : 'bg-input text-dialog-hint enabled:hover:text-white'
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
      className={isNested ? 'min-w-0 border-l-2 border-t border-dialog-edge bg-panel' : 'min-w-0'}
    >
      <header
        className={`flex min-h-6 items-center bg-panel pb-1.5 pt-3 ${
          isNested ? 'border-b border-dialog-edge pl-6 pr-3' : 'px-3'
        }`}
      >
        <Text as="h4" variant="section" id={headingId}>
          {label}
        </Text>
      </header>
      <IsNestedChoice.Provider value={isNested}>{children}</IsNestedChoice.Provider>
      {isNested && <div className="h-px w-full bg-dialog-edge" />}
    </section>
  );
}

/**
 * One centered row for a settings heading and its trailing control.
 *
 * THE TRAILING MARK ENDS WHERE THE HEADING STARTS. The action used to stand centred in a
 * fixed 48px cell INSIDE the band's own gutter, which parked the add mark, the switch and
 * the disclosure chevron 16px further in than the chevron on every row below — one column
 * of controls reading as three, reported from a phone over the settings dialog. The slot
 * ends on the gutter now, and a control keeps its touch target by growing INTO that gutter
 * (`IconButton`'s `edge`) rather than by pushing its ink off the rail.
 *
 * A disclosure makes the whole header the button.
 */
export function SettingsHeader({
  children,
  action,
  disclosure,
}: {
  children: ReactNode;
  action?: ReactNode;
  disclosure?: {
    isOpen: boolean;
    onToggle: () => void;
    label: string;
  } | null;
}) {
  const layout = 'flex min-h-11 min-w-0 items-center gap-3 px-3 py-1 sm:px-4 mouse:min-h-10';
  const content = (
    <>
      {children}
      {/* The band's full height keeps a switch's cell tall, and `justify-end` stops
          every action on the gutter. An add mark keeps the standard icon box, so its
          mark centers on the rail the rows' menu marks below center on; a switch and
          a bare chevron end their own right edge on the gutter. The chevron is the
          size it is on a row: one mark, one meaning, one size. */}
      <span className="-my-1 flex shrink-0 items-center justify-end self-stretch empty:hidden">
        {disclosure ? <ChevronIcon open={disclosure.isOpen} className="size-3" /> : action}
      </span>
    </>
  );
  return disclosure ? (
    <button
      type="button"
      aria-label={disclosure.label}
      aria-expanded={disclosure.isOpen}
      onClick={disclosure.onToggle}
      className={`${layout} w-full text-left transition-colors duration-150 enabled:hover:text-accent-ink focus-visible:bg-hover focus-visible:outline-none motion-reduce:transition-none`}
    >
      {content}
    </button>
  ) : (
    <div className={layout}>{content}</div>
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
      className={`flex min-h-12 w-full min-w-0 items-center gap-3 px-3 py-2 text-left transition-colors duration-150 enabled:hover:text-accent-ink focus-visible:bg-hover focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-accent motion-reduce:transition-none mouse:min-h-10 ${className}`}
      {...props}
    >
      <span className="min-w-0 flex-1">
        <Text variant="label" tone="inherit" className="block">
          {label}
        </Text>
        <Text variant="meta" className="block truncate">
          {value}
        </Text>
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
 * 46x28 on touch and 40x24 under a pointer, with an invisible hit area providing
 * a 44px touch target and a 32px pointer target.
 *
 * Both states keep the theme foreground outline, so the accent fill never
 * hides the track boundary. The off knob uses the quieter hint foreground.
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
      className={`relative inline-flex h-7 w-[2.875rem] shrink-0 items-center rounded-none border p-0.5 transition-colors duration-150 ease-out after:absolute after:inset-x-0 after:-top-2 after:-bottom-2 after:content-[""] focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 disabled:opacity-45 motion-reduce:transition-none mouse:h-6 mouse:w-10 mouse:after:-top-1 mouse:after:-bottom-1 ${
        isOn ? 'border-white bg-accent' : 'border-white bg-transparent'
      } ${className}`}
      {...props}
    >
      <span
        aria-hidden
        className={`size-[1.375rem] rounded-none transition-transform duration-150 ease-out mouse:size-[1.125rem] ${
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
 * The shared close/remove mark, without a border or a filled face in any theme.
 * A band close keeps its full-height trailing cell and ends its mark on the dialog's own
 * rail, so the ✕, the section bands beneath it and every row chevron share one trailing
 * edge. Compact row and attachment removes keep their placement and extend their touch
 * reach invisibly. Keyboard focus uses the same underline as other icon controls.
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
  /** This close action fills the band's final cell, including wrapped titles. */
  isBand?: boolean;
  /** This close action ends a compact row rather than a title band. */
  isStandalone?: boolean;
}) {
  const mark = <CloseIcon className={isStandalone ? 'size-2.5' : undefined} />;
  return (
    <button
      type="button"
      aria-label={label}
      title={label}
      className={`${iconControlClass} grid shrink-0 items-center bg-transparent text-current transition-colors duration-150 disabled:cursor-not-allowed disabled:opacity-40 motion-reduce:transition-none ${
        isBand
          ? 'w-12 justify-items-end self-stretch pr-3 sm:pr-4 mouse:w-9'
          : 'size-8 justify-items-center self-center after:absolute after:left-1/2 after:top-1/2 after:size-11 after:-translate-x-1/2 after:-translate-y-1/2 after:content-[""] mouse:size-7 mouse:after:content-none'
      } ${className}`}
      {...props}
    >
      {mark}
    </button>
  );
}

/**
 * A VERB IN A BAND, and there is only one of it.
 *
 * Text actions fill the band and keep their dividing rule. Supplying `label`
 * makes the action a bare icon in a full-height, borderless touch target.
 * Keyboard focus underlines it; primary intent never adds a circle.
 *
 * A CELL WITH SOMETHING TO COMMIT WEARS THE ACCENT (`isPrimary`). The accent exists
 * only while the commit is live; a disabled cell drops it instead of dimming a false
 * promise.
 */
export function BandButton({
  className = '',
  isFirst = false,
  isPrimary = false,
  label,
  children,
  ...props
}: ButtonHTMLAttributes<HTMLButtonElement> & {
  /** This cell starts an open action run instead of continuing a ruled one. */
  isFirst?: boolean;
  /** This cell COMMITS something, and wears the accent while it has something to commit. */
  isPrimary?: boolean;
  /** The accessible name of an icon-only action. */
  label?: string;
}) {
  const isLive = isPrimary && !props.disabled;
  const isIconOnly = Boolean(label);
  return (
    <button
      type="button"
      className={`grid shrink-0 place-items-center self-stretch whitespace-nowrap font-mono text-meta font-bold focus-visible:outline-none disabled:cursor-not-allowed disabled:opacity-60 sm:text-ui mouse:text-meta ${
        isIconOnly
          ? `${iconControlClass} group w-12 bg-transparent px-0 text-current mouse:w-9`
          : `${isFirst ? '' : 'border-l border-current/20'} px-3 transition-colors duration-150 motion-reduce:transition-none sm:px-4 mouse:px-3 ${
              isLive
                ? 'bg-accent text-accent-foreground focus-visible:bg-accent-2'
                : 'text-current focus-visible:bg-current/10'
            }`
      } ${className}`}
      aria-label={label}
      title={label}
      {...props}
    >
      <span
        className={
          isIconOnly
            ? 'pointer-events-none grid size-8 place-items-center transition-transform duration-150 group-active:scale-[0.94] group-disabled:scale-100 motion-reduce:transition-none mouse:size-7'
            : 'translate-y-px'
        }
      >
        {children}
      </span>
    </button>
  );
}

/**
 * A single-line form field: one 32px touch / 28px pointer face in every location.
 * The wrapper extends touch reach to 44px without intercepting native caret placement.
 * Optional icon/action slots share this geometry; className only positions the field.
 * Chat composers and inline title editing keep their separate controls.
 */
export const Input = forwardRef<
  HTMLInputElement,
  InputHTMLAttributes<HTMLInputElement> & { icon?: ReactNode; action?: ReactNode }
>(function Input({ className = '', icon, action, ...props }, ref) {
  // Keep password mask dots distinct without changing plain-text spacing.
  const masked = props.type === 'password' ? 'tracking-[0.15em]' : '';
  return (
    <span
      className={`relative block w-full min-w-0 self-center before:absolute before:inset-x-0 before:-top-1.5 before:h-1.5 before:content-[''] after:absolute after:inset-x-0 after:-bottom-1.5 after:h-1.5 after:content-[''] mouse:before:content-none mouse:after:content-none ${className}`}
      onPointerDown={(event) => {
        // Only the outer hit strips focus here; native text and action presses pass through.
        if (event.target === event.currentTarget) {
          event.preventDefault();
          event.currentTarget.querySelector('input')?.focus();
        }
      }}
    >
      <input
        ref={ref}
        className={`block h-8 w-full rounded-none border border-edge bg-input py-0.5 font-mono text-ui text-white transition-[border-color,box-shadow] duration-150 placeholder:text-dialog-hint focus:border-accent focus:outline-none focus:ring-1 focus:ring-accent/30 motion-reduce:transition-none mouse:h-7 ${icon ? 'pl-8 sm:pl-9' : 'pl-2.5 sm:pl-3'} ${action ? 'pr-10 mouse:pr-8 [&::-webkit-search-cancel-button]:hidden' : 'pr-2.5 sm:pr-3'} ${masked}`}
        {...props}
      />
      {icon && (
        <span
          aria-hidden="true"
          className="pointer-events-none absolute inset-y-0 left-2.5 flex items-center text-dialog-hint sm:left-3"
        >
          {icon}
        </span>
      )}
      {action && (
        <span className="absolute inset-y-0 right-0 flex items-center text-white">{action}</span>
      )}
    </span>
  );
});

/** Make the modal picker the only interactive branch; restore only attributes we own. */
function isolateSelectContent(node: HTMLDivElement | null) {
  if (!node?.isConnected) return;
  const changed: Element[] = [];
  for (let branch: Element = node; branch.parentElement; branch = branch.parentElement) {
    for (const sibling of branch.parentElement.children) {
      if (
        sibling !== branch &&
        !sibling.hasAttribute('inert') &&
        !sibling.matches('[aria-live]') &&
        !sibling.querySelector('[aria-live]')
      ) {
        sibling.setAttribute('inert', '');
        changed.push(sibling);
      }
    }
    if (branch.parentElement === node.ownerDocument.body) break;
  }
  return () => changed.forEach((element) => element.removeAttribute('inert'));
}

/**
 * A single choice with app-owned faces and a portalled, collision-aware listbox.
 * Radix owns keyboard navigation, typeahead, touch scrolling and focus return.
 * Arrows explore; Enter/Space commits; Escape cancels without dismissing a parent.
 * While open, Tab stays in the picker (the primitive's native-select convention).
 * Callers own the saved value; rejected saves never replace it optimistically here.
 */
export const Select = forwardRef<
  HTMLButtonElement,
  Pick<
    ButtonHTMLAttributes<HTMLButtonElement>,
    'id' | 'aria-label' | 'aria-labelledby' | 'aria-describedby' | 'aria-busy' | 'disabled' | 'className'
  > & {
    value: string;
    onValueChange: (value: string) => void;
    options: readonly { value: string; label: string; disabled?: boolean }[];
  }
>(function Select({ value, onValueChange, options, disabled = false, className = '', ...props }, ref) {
  const [open, setOpen] = useState(false);
  const generatedId = useId();
  const triggerId = props.id ?? generatedId;
  const unavailable = disabled || options.length === 0;
  const selected = options.find((option) => option.value === value);
  const label = options.length === 0 ? 'No options available' : (selected?.label ?? value);
  // An empty string is a real choice (Unassigned, No group), not Radix's placeholder.
  // Prefix every value so the mapping is reversible and cannot collide with caller data.
  const prefix = 'option:';
  if (unavailable && open) setOpen(false);

  return (
    <SelectPrimitive.Root
      value={`${prefix}${value}`}
      onValueChange={(next) => onValueChange(next.slice(prefix.length))}
      open={open && !unavailable}
      onOpenChange={setOpen}
      disabled={unavailable}
    >
      <SelectPrimitive.Trigger
        {...props}
        id={triggerId}
        ref={ref}
        title={label}
        className={`relative inline-flex h-8 min-w-11 max-w-full items-center justify-between gap-2 self-center rounded-none border border-edge bg-input px-2.5 font-mono text-ui text-white after:absolute after:inset-x-0 after:-inset-y-1.5 after:content-[''] enabled:hover:text-accent-ink focus-visible:border-accent focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-accent/30 disabled:cursor-not-allowed disabled:text-muted data-[state=open]:border-accent mouse:h-7 mouse:after:content-none ${className}`}
      >
        <span className="min-w-0 truncate text-left">
          <SelectPrimitive.Value>{label}</SelectPrimitive.Value>
        </span>
        <SelectPrimitive.Icon asChild>
          <ChevronIcon open className={`size-3 shrink-0 ${open && !unavailable ? 'rotate-180' : ''}`} />
        </SelectPrimitive.Icon>
      </SelectPrimitive.Trigger>
      <SelectPrimitive.Portal container={open ? overlayLayer().host : undefined}>
        <SelectPrimitive.Content
          position="popper"
          ref={isolateSelectContent}
          sideOffset={8}
          collisionPadding={12}
          aria-label={props['aria-label']}
          aria-labelledby={props['aria-labelledby'] ?? (props['aria-label'] ? undefined : triggerId)}
          className="z-[60] flex max-h-[min(20rem,var(--radix-select-content-available-height))] min-w-[var(--radix-select-trigger-width)] max-w-[min(24rem,calc(100vw-1.5rem))] flex-col overflow-hidden rounded-none border border-dialog-edge bg-panel font-mono text-ui text-white shadow-float"
          onKeyDown={(event) => {
            // Do not let the surrounding dialog interpret a picker key as its own.
            event.stopPropagation();
            if (event.key === 'Escape') {
              event.preventDefault();
              setOpen(false);
            }
          }}
        >
          <SelectPrimitive.ScrollUpButton className="flex min-h-11 shrink-0 items-center justify-center text-dialog-hint mouse:min-h-7">
            <ChevronIcon open className="size-3 rotate-180" />
          </SelectPrimitive.ScrollUpButton>
          <SelectPrimitive.Viewport className="min-h-0 p-1">
            {options.map((option) => (
              <SelectPrimitive.Item
                key={option.value}
                value={`${prefix}${option.value}`}
                disabled={option.disabled}
                textValue={option.label}
                aria-selected={option.value === value}
                onPointerMove={(event) => {
                  // Hover changes ink only; keep the keyboard focus indicator in place.
                  if (event.pointerType === 'mouse') event.preventDefault();
                }}
                className="flex min-h-11 cursor-default select-none items-center gap-2 rounded-none px-2 py-1.5 outline-none data-[state=checked]:bg-panel-2 data-[disabled]:text-muted data-[disabled]:pointer-events-none [&:not([data-disabled])]:hover:text-accent-ink focus-visible:ring-1 focus-visible:ring-inset focus-visible:ring-accent mouse:min-h-7"
              >
                <SelectPrimitive.ItemText className="min-w-0 flex-1 break-words">
                  {option.label}
                </SelectPrimitive.ItemText>
                <span className="ml-auto flex size-3 shrink-0 items-center justify-center">
                  <SelectPrimitive.ItemIndicator>
                    <CheckIcon className="size-3" />
                  </SelectPrimitive.ItemIndicator>
                </span>
              </SelectPrimitive.Item>
            ))}
          </SelectPrimitive.Viewport>
          <SelectPrimitive.ScrollDownButton className="flex min-h-11 shrink-0 items-center justify-center text-dialog-hint mouse:min-h-7">
            <ChevronIcon open className="size-3" />
          </SelectPrimitive.ScrollDownButton>
        </SelectPrimitive.Content>
      </SelectPrimitive.Portal>
    </SelectPrimitive.Root>
  );
});

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
    ok: 'border-ok/50 bg-ok/10 text-footer-strong',
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
          {dismiss ? <CloseButton isBand label={dismiss.label} onClick={dismiss.onClick} /> : null}
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
 * `sm:` they are the same rectangle and the CONTENT scrolls inside it; below it a
 * `full` sheet is simply the whole phone.
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
 * WHETHER THE SHEET AROUND A `DialogFrame` STOPS AT ITS CONTENT — the one thing the
 * frame cannot see about itself. Such a sheet is welded to the BOTTOM edge and starts
 * partway down the glass, so there is no notch above it to clear: padding its top with
 * the safe-area inset hangs 47 dead pixels of panel paper over the title on every
 * iPhone. Default `false`, so a frame opened outside `Modal`, and the full-bleed sheet
 * that IS the whole phone, keep clearing the notch.
 */
const IsContentSheet = createContext(false);

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
  const host = document.querySelector<HTMLElement>('[data-viewport-shell]') ?? document.body;
  return { host, position: host === document.body ? 'fixed' : 'absolute' };
}

/**
 * WHERE A LAYER THAT COVERS ONLY THE OPEN SESSION MOUNTS.
 *
 * A run is the session's own work, and the desk beside it is not part of the
 * question: on a wide window the app layer dimmed the session list and the
 * header along with the transcript, so one opened run took the whole screen.
 * The session screen marks its own positioned root (`data-session-surface`),
 * and a layer that belongs to that session stands `absolute` INSIDE it — scrim,
 * box and all — leaving everything outside the pane legible and clickable.
 *
 * That root is itself inside the shell, so such a layer keeps the keyboard
 * behaviour `overlayLayer` exists for. With no session on screen there is
 * nothing to stand in, and the app shell is the fallback.
 */
export function sessionLayer(): {
  host: HTMLElement;
  position: 'absolute' | 'fixed';
} {
  const host = document.querySelector<HTMLElement>('[data-session-surface]');
  return host ? { host, position: 'absolute' } : overlayLayer();
}

export function Modal({
  onDismiss,
  size = 'full',
  within = 'app',
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
   * question. Same scrim, same physics, same desktop box — only the width differs.
   * Below `sm:` the columns stack and, like `fit`, it stops at its content.
   */
  size?: 'full' | 'fit' | 'wide';
  /**
   * WHAT THE DIALOG STANDS OVER. `app` is every dialog that belongs to the whole
   * application — settings, the model picker, a confirmation — and it covers the
   * shell.
   *
   * `session` is a dialog about ONE session, and it stands in that session's pane
   * instead. On a desk that is a list and a transcript side by side, an opened run
   * has no business dimming the list beside it. Inside that pane it takes every pixel:
   * the pane is already the bound the desktop box exists to impose.
   */
  within?: 'app' | 'session';
  children: ReactNode;
}) {
  const { host: portalHost, position } = within === 'session' ? sessionLayer() : overlayLayer();
  const dismissOnClick = useRef(false);
  // ONLY `full` PAPERS THE WHOLE PHONE. Reported over settings on an iPhone: with the
  // application fold closed and three machines listed, two thirds of the glass below the
  // last row was blank panel. A sheet that stops at its content rises from the bottom
  // edge instead, and its ceiling is the glass minus the notch it never stands under.
  const stopsAtContent = size !== 'full';
  // A DIALOG THAT BELONGS TO ONE SESSION TAKES THAT SESSION WHOLE. The desktop box keeps a
  // question from papering the window, but a session layer is already bounded by the pane it
  // stands in, so the same cap left an opened run as a small window in the middle of a pane it
  // could have had. Reported: opening a live run should fill the session it belongs to.
  const fillsPane = within === 'session' && !stopsAtContent;
  const boxHeight = stopsAtContent
    ? `max-h-[calc(100%-env(safe-area-inset-top))] ${size === 'fit' ? 'sm:h-auto' : DIALOG_DESKTOP_HEIGHT}`
    : fillsPane
      ? 'sm:h-full'
      : DIALOG_DESKTOP_HEIGHT;
  const desktopWidth = size === 'wide' ? 'sm:max-w-4xl mouse:max-w-6xl' : 'sm:max-w-xl';
  const boxWidth = fillsPane ? 'sm:max-w-none' : desktopWidth;

  return createPortal(
    <div
      className={`${position} inset-0 z-50 flex justify-center transition-opacity duration-200 starting:opacity-0 motion-reduce:transition-none sm:items-center sm:pb-[max(1rem,env(safe-area-inset-bottom))] sm:pl-[max(1rem,env(safe-area-inset-left))] sm:pr-[max(1rem,env(safe-area-inset-right))] sm:pt-[max(1rem,env(safe-area-inset-top))] ${
        stopsAtContent ? 'items-end' : 'items-stretch'
      }`}
      role="presentation"
      onPointerDown={(event) => {
        dismissOnClick.current = event.target === event.currentTarget;
      }}
      onClick={(event) => {
        if (dismissOnClick.current && event.target === event.currentTarget) onDismiss();
        dismissOnClick.current = false;
      }}
    >
      {/* ONE SIZE. On the phone a full dialog IS the screen — full bleed, full height,
          so a list inside it gets every pixel the glass has and the verbs at its
          foot are always in the same place. From `sm:` up every dialog that asks one
          question is the same box (`sm:max-w-xl`, `DIALOG_DESKTOP_HEIGHT`): a question
          and a file browser that open over the same screen used to be two different
          rectangles.

          A `fit` dialog is one exception, and it is a SIZE rather than a second
          modal: same scrim, same physics, same box — it simply stops at its content.
          Its ceiling is the glass MINUS the notch: a sheet that stops at its content
          clears no notch itself (`IsContentSheet`), so one grown to its cap — fifteen
          provider presets — would otherwise stand its title under the clock.
          `wide` is the other, and it is a LAYOUT rather than a mood: settings stands
          two columns wide, and 36rem split in half is two columns of nothing. It stops
          at its content on the phone as well, where a short fleet left the glass below
           the last row as blank paper.

           A dialog that stands in ONE SESSION is the third, and it is a PLACE rather than
           a size: its layer is the session pane, not the window, so the box takes all of
           it. The question rectangle inside a pane is a small window in the middle of one.

          The scrim is settings' own — its width (`sm:max-w-xl`) and its fade, which
          arrives rather than snaps on. What it no longer carries is PAINT. Reported,
          with settings open over a live session: every arriving message made the picker
          inside the dialog flicker. A `backdrop-filter` re-rasterises everything it
          covers and everything stacked above it whenever the page beneath changes, and
          a streaming transcript changes on nearly every frame — so the blur dragged the
          dialog and its open lists through the transcript's repaint. The ink wash went
          with it, by the same call: a dialog stands OVER the application without
          painting on it, and every box carries its own paper.
          `ui.conventions.test.ts` holds the layer clear. */}
      <div
        className={`flex w-full flex-col ${boxWidth} ${boxHeight}`}
        role="presentation"
        onClick={(event) => event.stopPropagation()}
      >
        <IsContentSheet.Provider value={stopsAtContent}>{children}</IsContentSheet.Provider>
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
 * machine's header are the same height on the same screen. The frame and band have
 * square corners; the lower edge stays full width so the scrolling body meets it
 * on one exact seam.
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
): { onClose: () => void; closeLabel: string } | { onClose?: undefined; closeLabel?: undefined } {
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
          className="line-clamp-3 font-mono text-head font-semibold tracking-normal"
          title={typeof title === 'string' ? title : undefined}
        >
          {title}
        </h2>
        {subtitle && (
          <Text as="p" variant="meta" tone="inherit" className="truncate">
            {subtitle}
          </Text>
        )}
      </div>
      {/* What the band OFFERS, before the way out and in cells of the band's own:
          welded by the same hairline, standing the band's full height. */}
      {actions}
      {/* The two travel together by type; the second test is what TypeScript needs
          to see it, because a destructured union does not narrow on its own. */}
      {onClose && closeLabel && <CloseButton isBand label={closeLabel} onClick={onClose} />}
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
  // A sheet that stops at its content starts partway down the glass, so the notch
  // is not above it and the top inset is dead space (`IsContentSheet`).
  const isContentSheet = useContext(IsContentSheet);
  return (
    <section
      className={`flex min-h-0 flex-1 flex-col overflow-hidden rounded-none border-t-2 border-accent bg-panel ${
        isContentSheet ? '' : 'pt-[env(safe-area-inset-top)]'
      } pb-[env(safe-area-inset-bottom)] transition-[opacity,transform,translate,scale,rotate] duration-300 ease-[cubic-bezier(0.22,0.61,0.36,1)] starting:translate-y-full starting:opacity-0 motion-reduce:transition-none sm:border sm:border-dialog-edge sm:pt-0 sm:pb-0 sm:shadow-float sm:duration-200 sm:starting:translate-y-2 ${className}`}
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

/** Shared frame vocabulary; the app and terminal use the same cadence. */
const SPINNER_FRAMES: Readonly<Record<string, readonly string[]>> = Object.fromEntries(
  viewSchema.$defs.spinner_variant.oneOf.map((variant) => [variant.const, variant['x-vis-frames']]),
);

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
  variant = 'braille',
  className = '',
}: {
  /**
   * `accent` is the app's waiting ink: a spinner that reports work in the amber
   * the rest of the screen uses for it. `inherit` takes the ink of the line it
   * rides, which is what a spinner inside a sentence wants.
   */
  tone?: 'inherit' | 'accent';
  variant?: 'braille' | 'dots' | 'line' | 'pulse';
  /** Placement only; the frames' own face is fixed. */
  className?: string;
}) {
  const ink = tone === 'accent' ? 'text-accent-ink' : '';
  return (
    <span aria-hidden="true" className={`inline-grid ${ink} ${className}`}>
      {SPINNER_FRAMES[variant].map((frame, index) => (
        <span
          key={index}
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
 * own text read flush left with a ragged right edge, the way every other chat
 * surface reads. Inline code remains an atomic left-aligned box inside that column,
 * while hyphenation and pretty wrapping keep the rag even on narrow phones.
 */
export const PROSE = 'hyphens-auto [hyphenate-limit-chars:6_3_3] text-pretty';

/**
 * THE ICON SET — every mark the app draws, in one grammar, in one file.
 *
 * The app used to say "icon" with characters it borrowed from the terminal:
 * `✕` for close, `▾`/`▸` for a disclosure, `▶` for a video, `↓` for "load more",
 * `↗` for a link. A glyph is TYPE, not an icon — it is whatever weight, cap
 * height and side bearing the font happens to ship, it does not align to the
 * label beside it, it does not scale with the control, it cannot take a stroke,
 * and on a phone it renders in whatever fallback face the OS picked. Three
 * icons were already drawn properly (the composer's camera, picture and
 * microphone); the rest are here now, and nothing in `src/**` paints a glyph as
 * a control mark again — `icons.test.tsx` reads the shipped source and fails.
 *
 * ONE grammar, so a strip of them looks like a set: a 24-unit grid, `size-3.5`
 * (the composer's own size), `none` fill, `currentColor` stroke at 1.8, rounded
 * joins, and `aria-hidden` because an icon inside a labelled control is
 * decoration.
 *
 * That size is a FLOOR, not a default argument: `className` carries colour,
 * spacing and motion far more often than a size, and a default parameter is
 * REPLACED by whatever the caller passes — which is how the transcript's paste
 * disclosure shipped an `<svg>` with no width at all and grew to the width of
 * the bubble. `size-3.5` is applied unless the caller's own classes name a size.
 *
 * ONE SIZE, and the viewBox is not it. `0 0 24 24` sizes the CANVAS: a paperclip
 * drawn corner to corner and a cross drawn across the middle third are the same
 * box and two visibly different icons in the same row — which is exactly how the
 * clip ended up half again as big as the `✕` beside it. So every mark here is
 * measured and drawn to the same INK: centred on (12, 12), inside a live area of
 * 4–20, at an optical size of 13.5 units — by AREA, so a wide camera and a tall
 * microphone weigh the same; by its long axis for a thin mark like the chevron,
 * which can only be as big as it is tall. Solid and diagonal marks (play, clip,
 * pencil) are drawn a touch smaller because they read heavier at the same box,
 * and no mark's longest side passes 15. `icons.test.tsx` walks every path and
 * fails on a mark that drifts out of that band. `fill`/`stroke` are presentation
 * ATTRIBUTES, so a Tailwind class on the same element (`fill-accent`) still
 * wins — that is how the starred star wears the brand yellow.
 *
 * Deliberately NOT here: `●`/`○`/`[✓]`, which are the cross-channel choice
 * marks the TUI paints too (see `HUMAN_INPUT_CHOICE_MARKS`), and the spinner's
 * Braille cadence. Those are the same characters on purpose.
 */
import type { ReactNode } from "react";

/** One class list from parts, so an absent one leaves no hole behind. */
const classes = (...parts: (string | false | undefined)[]) =>
  parts.filter(Boolean).join(" ").replace(/\s+/g, " ").trim();

/** Does this class list already say how big the mark is? */
const NAMES_A_SIZE = /(?:^|\s)(?:size|[hw])-/;

function Icon({
  children,
  className = "",
}: {
  children: ReactNode;
  className?: string;
}) {
  return (
    <svg
      viewBox="0 0 24 24"
      className={classes(
        "shrink-0",
        !NAMES_A_SIZE.test(className) && "size-3.5",
        className,
      )}
      fill="none"
      stroke="currentColor"
      strokeWidth="1.8"
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true"
    >
      {children}
    </svg>
  );
}

/**
 * The mark of an ATTACHMENT. The clip is DIAGONAL: at 14px an upright one
 * closes into a blob, and the slant keeps both openings wider than the stroke.
 */
export function ClipIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M18.42 11.71l-6.88 6.88a4.19 4.19 0 0 1 -5.96 -5.96l7.21 -7.21a2.77 2.77 0 0 1 3.94 3.94l-7.21 7.21a1.43 1.43 0 0 1 -2.01 -2.01l6.63 -6.63" />
    </Icon>
  );
}

/** The way out of a dialog. Two strokes, not a `✕` set in the body font. */
export function CloseIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M5.25 5.25l13.5 13.5M18.75 5.25l-13.5 13.5" />
    </Icon>
  );
}

/**
 * THE MARK OF SEARCH, and the one convention nobody gets to be creative about: a
 * schematic magnifying glass, INSIDE the open field and leading it. A bare framed box
 * on the bar reads as "some input"; the glass says what typing here does before a
 * placeholder is read. It is ink, not a control — the field's own label speaks it.
 */
export function SearchIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <circle cx="10.5" cy="10.5" r="6.25" />
      <path d="M15.25 15.25l4.5 4.5" />
    </Icon>
  );
}

/**
 * A disclosure. ONE icon in two states: it points right when closed and turns a
 * quarter clockwise when open, so opening a section is a MOVE the eye follows
 * rather than one character being swapped for a different character.
 *
 * `back` is the third state, and it exists because the two controls in this app that
 * mean "return to where you came from" — the session header's back button and a
 * menu's `MenuBack` band — both drew this glyph pointing FORWARD. A chevron is a
 * direction before it is a decoration: an arrow aimed at the rest of the app,
 * labelled "Back to sessions", tells the eye the opposite of what the button does.
 * It is a rotation rather than a second path so the two can never drift in stroke,
 * cap or weight.
 */
export function ChevronIcon({
  open = false,
  back = false,
  className,
}: {
  open?: boolean;
  /** Points the way OUT — a back button, a step retraced. Ignored while `open`. */
  back?: boolean;
  className?: string;
}) {
  return (
    <Icon
      className={classes(
        "transition-transform duration-150 motion-reduce:transition-none",
        open ? "rotate-90" : back && "rotate-180",
        className,
      )}
    >
      <path d="M8.62 5.25l6.75 6.75l-6.75 6.75" />
    </Icon>
  );
}

/** Play. Solid, because a hollow triangle at thumbnail size reads as a flaw. */
export function PlayIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M7.04 4.71l10.72 7.29l-10.72 7.29z" fill="currentColor" />
    </Icon>
  );
}

/** Stop — a solid square paired with Play's solid triangle. */
export function StopIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <rect x="5.5" y="5.5" width="13" height="13" fill="currentColor" />
    </Icon>
  );
}
/** Pause — the same solid weight as play, because they are one control. */
export function PauseIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <rect x="6.6" y="4.71" width="3" height="14.58" fill="currentColor" />
      <rect x="14.4" y="4.71" width="3" height="14.58" fill="currentColor" />
    </Icon>
  );
}

/** More of the same, below: what "Load 12 more" does to the list. */
export function ArrowDownIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M12 4.84v13.79M5.64 12.8l6.36 6.36l6.36 -6.36" />
    </Icon>
  );
}

/** Out of here — a citation, a link, a thing that opens elsewhere. */
export function ArrowOutIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M5.5 18.5l13 -13M7.94 5.5H18.5v10.56" />
    </Icon>
  );
}

/** Add. Rotated 45° by its caller, it is the same stroke saying "close". */
export function PlusIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M12 5.25v13.5M5.25 12h13.5" strokeLinecap="square" />
    </Icon>
  );
}

/** Take a photo. */
export function CameraIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <path
        d="M4.5 8.25h3.33l1.25 -1.67h5.83L16.17 8.25h3.33v9.17H4.5z"
        strokeLinecap="square"
      />
      <circle cx="12" cy="12.42" r="2.67" />
    </Icon>
  );
}

/** A picture already taken. */
export function ImageIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M4.5 6.38h15v11.25H4.5z" strokeLinecap="square" />
      <path
        d="M4.5 14.81l4.22 -4.22l2.81 2.81l3.28 -3.28L19.5 14.81"
        strokeLinecap="square"
      />
      <circle cx="9.19" cy="9.66" r="1.12" />
    </Icon>
  );
}

/** Dictation. */
export function MicIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <rect x="9.5" y="4.5" width="5" height="9.17" rx="2.5" />
      <path
        d="M6.58 11.58a5.42 5.42 0 0 0 10.83 0M12 17v2.5M9.08 19.5h5.83"
        strokeLinecap="square"
      />
    </Icon>
  );
}

/**
 * VOICE CONVERSATION — the microphone that answers back. Dictation's mic writes
 * into the box and stops; this one sends what you said and reads the reply
 * aloud, so it is the same capsule with the reply coming out of it. Two marks,
 * because one control now carries two modes and the mode has to be visible
 * without a word beside it.
 */
export function VoiceLoopIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <rect x="8" y="4.8" width="4.6" height="8.6" rx="2.3" />
      <path d="M6 11.4a4.3 4.3 0 0 0 8.6 0" strokeLinecap="square" />
      <path d="M10.3 15.7V18.8M8 18.8h4.6" strokeLinecap="square" />
      <path d="M15.2 10.6a1.9 1.9 0 0 1 0 2.8" />
      <path d="M17.4 8.8a4 4 0 0 1 0 6.4" />
    </Icon>
  );
}

/** Rename. */
export function PencilIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M15.02 5.5l3.48 3.48L9.56 17.92L5.5 18.5l0.58 -4.06z" />
      <path d="M13.39 7.12l3.48 3.48" />
    </Icon>
  );
}

/** Delete. The row swipe's own mark, and the only way "delete" is ever drawn. */
export function TrashIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M5.35 7.33h13.31M9.82 7.33V5.15h4.35v2.18M7.14 7.33l0.9 11.52h7.93l0.9 -11.52" />
      <path d="M10.21 10.27v5.63M13.79 10.27v5.63" />
    </Icon>
  );
}

/**
 * The favorite mark. Filled is "starred": the BODY is an amber FILL, so it wears
 * the brand yellow (`accent`, #ffc420) and never the brown ink a text glyph
 * needs. Yellow on yellow paper is not a mark, though — #ffc420 on the light
 * theme's #faf3eb measures 1.45:1, under the 3:1 a graphic owes, so a starred
 * row carried a glyph the eye slid straight off. The EDGE carries the contrast
 * the fill cannot: `accent-ink` (light #7a4a00, 6.8:1 on that paper; dark
 * #fde68a on panel) draws the star's outline at 0.9px on a 12px mark. The shape
 * is legible, the colour is still the brand yellow.
 *
 * The un-starred outline stays adaptive (`stroke-current`) so it reads among the
 * other action icons; the fill alone is too quiet to spot at a glance in a list,
 * so the swipe action still shows both states.
 */
export function StarIcon({
  filled = false,
  className,
}: {
  filled?: boolean;
  className?: string;
}) {
  return (
    <Icon
      className={classes(
        filled ? "fill-accent stroke-accent-ink" : "fill-none stroke-current",
        className,
      )}
    >
      <path d="M12 5.17l2.2 4.51l4.98 0.69l-3.59 3.47l0.81 4.98L12 16.51l-4.4 2.31l0.81 -4.98l-3.59 -3.47l4.98 -0.69z" />
    </Icon>
  );
}

/** Which way a table is sorted, and that it can be sorted at all. */
export function SortIcon({
  dir,
  className,
}: {
  dir?: "asc" | "desc";
  className?: string;
}) {
  if (dir === "asc") {
    return (
      <Icon className={className}>
        <path d="M12 19.03V4.97M5.51 11.46l6.49 -6.49l6.49 6.49" />
      </Icon>
    );
  }
  if (dir === "desc") {
    return (
      <Icon className={className}>
        <path d="M12 4.97v14.05M5.51 12.54l6.49 6.49l6.49 -6.49" />
      </Icon>
    );
  }
  return (
    <Icon className={className}>
      <path d="M8.93 8.93l3.07 -3.68l3.07 3.68M8.93 15.07l3.07 3.68l3.07 -3.68" />
    </Icon>
  );
}

/**
 * WHERE a machine answers: a globe, because the choice this opens is a ROUTE —
 * tailnet, LAN or loopback — and not a name or a rank.
 */
export function AddressIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M12 4a8 8 0 1 0 0 16a8 8 0 1 0 0 -16M4 12h16M12 4c2.2 2.2 3.2 5 3.2 8s-1 5.8 -3.2 8c-2.2 -2.2 -3.2 -5 -3.2 -8s1 -5.8 3.2 -8" />
    </Icon>
  );
}
/**
 * A project: a folder, drawn with the same flat corners as the rest of this set
 * rather than the rounded folder every icon set ships. A project in this app IS a
 * directory on a machine, so the shape is not decoration — it is the one mark that
 * separates "a place on disk" from a document.
 */
export function ProjectsIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M4.66 6.4h5.18l1.6 2.14h7.9v9.06H4.66z" />
    </Icon>
  );
}


/** A fork of a conversation: one trunk with a branch leaving it. */
export function ForkIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <circle cx="8" cy="6" r="1.7" />
      <circle cx="8" cy="18" r="1.7" />
      <circle cx="16" cy="6" r="1.7" />
      <path d="M8 7.7v8.6" />
      <path d="M16 7.7v1.5c0 2.4-8 1.4-8 3.9" />
    </Icon>
  );
}

export function SettingsIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M10.59 7.06L10.9 5.06A7.03 7.03 0 0 1 13.1 5.06L13.41 7.06A5.14 5.14 0 0 1 15.57 8.3L17.46 7.58A7.03 7.03 0 0 1 18.56 9.48L16.99 10.76A5.14 5.14 0 0 1 16.99 13.24L18.56 14.52A7.03 7.03 0 0 1 17.46 16.42L15.57 15.7A5.14 5.14 0 0 1 13.41 16.94L13.1 18.94A7.03 7.03 0 0 1 10.9 18.94L10.59 16.94A5.14 5.14 0 0 1 8.43 15.7L6.54 16.42A7.03 7.03 0 0 1 5.44 14.52L7.01 13.24A5.14 5.14 0 0 1 7.01 10.76L5.44 9.48A7.03 7.03 0 0 1 6.54 7.58L8.43 8.3A5.14 5.14 0 0 1 10.59 7.06Z" />
      <circle cx="12" cy="12" r="2.06" />
    </Icon>
  );
}

/**
 * Something did not arrive: a picture that failed to fetch, a tile whose bytes
 * never came. A bang inside the same triangle the warning banners use, so the
 * failed state of a thumbnail is recognisable at 14px without a word.
 */
export function AlertIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M12 5.84L4.61 18.16h14.79L12 5.84z" />
      <path d="M12 10.36v3.29" />
      <path d="M12 16.11h0.01" />
    </Icon>
  );
}

/**
 * Overflow — "more actions." Three filled dots on the baseline, so a menu
 * trigger reads the same weight at every size the way a strip of stroked icons
 * does. The ⋯ glyph this replaces is a MATH symbol (U+22EF): it sits on the
 * cap-height line, never the optical centre, and at small sizes it is a dash.
 */
export function DotsIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <circle cx="7" cy="12" r="1.5" fill="currentColor" stroke="none" />
      <circle cx="12" cy="12" r="1.5" fill="currentColor" stroke="none" />
      <circle cx="17" cy="12" r="1.5" fill="currentColor" stroke="none" />
    </Icon>
  );
}

/**
 * COPY. Two sheets, one over the other — the mark every clipboard control in
 * the world wears, so it needs no label to be understood.
 *
 * It replaces the `#` that used to lead the session id: `#` says "this is an
 * identifier", never "press me and it lands on your clipboard", and a chip whose
 * whole verb is copying must SHOW that verb.
 */
export function CopyIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M15 9.75V6.75a1.5 1.5 0 0 0 -1.5 -1.5H6.75A1.5 1.5 0 0 0 5.25 6.75v6.5a1.5 1.5 0 0 0 1.5 1.5h3" />
      <rect x="9.75" y="9.75" width="9" height="9" rx="1.5" />
    </Icon>
  );
}

/** DONE. The tick that answers a press — the copy chip's second face. */
export function CheckIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M4.75 12.75l5 5L19.25 5.75" />
    </Icon>
  );
}

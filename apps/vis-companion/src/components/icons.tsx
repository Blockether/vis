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
 * (the composer's own size — pass `className` for a bigger one), `none` fill,
 * `currentColor` stroke at 1.8, rounded joins, and `aria-hidden` because an icon
 * inside a labelled control is decoration. `fill`/`stroke` are presentation
 * ATTRIBUTES, so a Tailwind class on the same element (`fill-accent`) still
 * wins — that is how the starred star wears the brand yellow.
 *
 * Deliberately NOT here: `●`/`○`/`[✓]`, which are the cross-channel choice
 * marks the TUI paints too (see `HUMAN_INPUT_CHOICE_MARKS`), and the spinner's
 * Braille cadence. Those are the same characters on purpose.
 */
import type { ReactNode } from "react";

function Icon({
  children,
  className = "size-3.5",
}: {
  children: ReactNode;
  className?: string;
}) {
  return (
    <svg
      viewBox="0 0 24 24"
      className={`shrink-0 ${className}`}
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
      <path d="M20 11.5l-8.2 8.2a5 5 0 0 1-7.1-7.1l8.6-8.6a3.3 3.3 0 0 1 4.7 4.7l-8.6 8.6a1.7 1.7 0 0 1-2.4-2.4l7.9-7.9" />
    </Icon>
  );
}

/** The way out of a dialog. Two strokes, not a `✕` set in the body font. */
export function CloseIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M6.5 6.5l11 11M17.5 6.5l-11 11" />
    </Icon>
  );
}

/**
 * A disclosure. ONE icon in two states: it points right when closed and turns a
 * quarter clockwise when open, so opening a section is a MOVE the eye follows
 * rather than one character being swapped for a different character.
 */
export function ChevronIcon({
  open = false,
  className = "size-3.5",
}: {
  open?: boolean;
  className?: string;
}) {
  return (
    <Icon
      className={`transition-transform duration-150 motion-reduce:transition-none ${
        open ? "rotate-90" : ""
      } ${className}`}
    >
      <path d="M9.5 5.5l6.5 6.5-6.5 6.5" />
    </Icon>
  );
}

/** Play. Solid, because a hollow triangle at thumbnail size reads as a flaw. */
export function PlayIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M8.5 5.2l10 6.8-10 6.8z" fill="currentColor" />
    </Icon>
  );
}

/** More of the same, below: what "Load 12 more" does to the list. */
export function ArrowDownIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M12 5v13M6 12.5l6 6 6-6" />
    </Icon>
  );
}

/** Out of here — a citation, a link, a thing that opens elsewhere. */
export function ArrowOutIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M8 16l8-8M9.5 8H16v6.5" />
    </Icon>
  );
}

/** Add. Rotated 45° by its caller, it is the same stroke saying "close". */
export function PlusIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M12 5v14M5 12h14" strokeLinecap="square" />
    </Icon>
  );
}

/** Take a photo. */
export function CameraIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M3 8h4l1.5-2h7L17 8h4v11H3z" strokeLinecap="square" />
      <circle cx="12" cy="13" r="3.2" />
    </Icon>
  );
}

/** A picture already taken. */
export function ImageIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M4 6h16v12H4z" strokeLinecap="square" />
      <path d="M4 15l4.5-4.5 3 3 3.5-3.5L20 15" strokeLinecap="square" />
      <circle cx="9" cy="9.5" r="1.2" />
    </Icon>
  );
}

/** Dictation. */
export function MicIcon({ className }: { className?: string }) {
  return (
    <Icon className={className}>
      <rect x="9" y="3" width="6" height="11" rx="3" />
      <path
        d="M5.5 11.5a6.5 6.5 0 0 0 13 0M12 18v3M8.5 21h7"
        strokeLinecap="square"
      />
    </Icon>
  );
}

/** Rename. */
export function PencilIcon({ className = "size-4" }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M15.9 3.6l4.5 4.5L8.85 19.65 3.6 20.4l.75-5.25z" />
      <path d="M13.8 5.7l4.5 4.5" />
    </Icon>
  );
}

/** Delete. The row swipe's own mark, and the only way "delete" is ever drawn. */
export function TrashIcon({ className = "size-4" }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M4.2 6.45h15.6M9.45 6.45V3.9h5.1v2.55M6.3 6.45l1.05 13.5h9.3l1.05-13.5" />
      <path d="M9.9 9.9v6.6M14.1 9.9v6.6" />
    </Icon>
  );
}

/**
 * The favorite mark. Filled is "starred": a filled star is an amber FILL, so it
 * wears the brand yellow (`accent`, #ffc420), not the legible amber ink a text
 * glyph would need. The outline stays adaptive (`stroke-current`) so it reads
 * among the other action icons; the fill alone is too quiet to spot at a glance
 * in a list, so the swipe action still shows both states.
 */
export function StarIcon({
  filled = false,
  className = "size-4",
}: {
  filled?: boolean;
  className?: string;
}) {
  return (
    <Icon
      className={`${
        filled ? "fill-accent stroke-accent" : "fill-none stroke-current"
      } ${className}`}
    >
      <path d="M12 2.85l2.85 5.85 6.45.9-4.65 4.5 1.05 6.45L12 17.55l-5.7 3 1.05-6.45-4.65-4.5 6.45-.9z" />
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
        <path d="M12 19V6M6 12l6-6 6 6" />
      </Icon>
    );
  }
  if (dir === "desc") {
    return (
      <Icon className={className}>
        <path d="M12 5v13M6 12l6 6 6-6" />
      </Icon>
    );
  }
  return (
    <Icon className={className}>
      <path d="M8 9.5l2.5-3 2.5 3M8 14.5l2.5 3 2.5-3" />
    </Icon>
  );
}

/**
 * The tab bar and the header cog. One box each, on the same grid as everything
 * above: the nav used to keep its own 20-unit set, which meant the app had two
 * icon grammars and a control could only match one of them.
 */
export function TranscriptsIcon({
  className = "size-4",
}: {
  className?: string;
}) {
  return (
    <Icon className={className}>
      <path d="M4.2 5.4h15.6v13.2H4.2zM7.8 9h8.4M7.8 12h8.4M7.8 15h4.8" />
    </Icon>
  );
}

export function MachinesIcon({ className = "size-4" }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M4.8 5.4h14.4v4.8H4.8zM4.8 13.8h14.4v4.8H4.8z" />
      <path d="M7.8 7.8h.01M7.8 16.2h.01" />
    </Icon>
  );
}

export function SettingsIcon({ className = "size-4" }: { className?: string }) {
  return (
    <Icon className={className}>
      <path d="M10.02 5.08 L10.46 2.28 A9.84 9.84 0 0 1 13.54 2.28 L13.98 5.08 A7.2 7.2 0 0 1 17 6.82 L19.64 5.81 A9.84 9.84 0 0 1 21.19 8.47 L18.98 10.26 A7.2 7.2 0 0 1 18.98 13.74 L21.19 15.53 A9.84 9.84 0 0 1 19.64 18.19 L17 17.18 A7.2 7.2 0 0 1 13.98 18.92 L13.54 21.72 A9.84 9.84 0 0 1 10.46 21.72 L10.02 18.92 A7.2 7.2 0 0 1 7 17.18 L4.36 18.19 A9.84 9.84 0 0 1 2.81 15.53 L5.02 13.74 A7.2 7.2 0 0 1 5.02 10.26 L2.81 8.47 A9.84 9.84 0 0 1 4.36 5.81 L7 6.82 A7.2 7.2 0 0 1 10.02 5.08 Z" />
      <circle cx="12" cy="12" r="2.88" />
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
      <path d="M12 4.5L3 19.5h18L12 4.5z" />
      <path d="M12 10v4" />
      <path d="M12 17h.01" />
    </Icon>
  );
}

/**
 * THE ICON SET — every mark the app draws, in one grammar, from one library.
 *
 * The marks are LUCIDE (`lucide-react`, ISC). It is the set this file spent
 * twenty-seven functions re-inventing by hand: the same 24-unit grid, `none`
 * fill, `currentColor` stroke, rounded caps and joins — drawn and maintained by
 * the people who ship 1 700 of them, one ES module per icon so the bundle
 * carries only what is imported, under a licence that asks for no attribution
 * in the UI and no NOTICE file. NOT SF Symbols: those are the native marks on
 * Apple platforms and licensed for Apple platforms alone, so the Android and web
 * builds could never draw them — one set that renders identically on all three
 * beats a set that is legal on one.
 *
 * What this file owns is the VOCABULARY, and that is the whole reason it still
 * exists: the app's NAME for each mark (`ClipIcon`, not `Paperclip`), the size
 * floor, and the states a mark has — a chevron that turns, a star that fills, a
 * sort that points. A screen names the job; the library draws it. Nothing else
 * in `src/**` imports `lucide-react`, and nothing else draws an `<svg>`;
 * `icons.test.tsx` reads the shipped source and fails.
 *
 * The app used to say "icon" with characters it borrowed from the terminal:
 * `✕` for close, `▾`/`▸` for a disclosure, `▶` for a video, `↓` for "load more",
 * `↗` for a link. A glyph is TYPE, not an icon — it is whatever weight, cap
 * height and side bearing the font happens to ship, it does not align to the
 * label beside it, it does not scale with the control, it cannot take a stroke,
 * and on a phone it renders in whatever fallback face the OS picked. Nothing in
 * `src/**` paints a glyph as a control mark.
 *
 * ONE BOX, AND THE TYPE DECIDES IT. Inter and JetBrains Mono measure the same
 * here (cap 0.719em, stem 0.0875em), so one table serves both: `text-head` 17px
 * takes an 18px mark, `text-title` 13px takes `size-3.5` (14px), `text-ui` 11px
 * and `text-chip` 9px take `size-3` (12px). Lucide's stroke of 2 lands at
 * 1.50 / 1.17 / 1.00px in those boxes — within 4% of the stem of the type beside
 * it at every step. So the BOX is what scales and the stroke is never touched;
 * the hand-drawn set's 1.8 read 8% lighter than its own labels.
 *
 * That size is a FLOOR, not a default argument: `className` carries colour,
 * spacing and motion far more often than a size, and a default parameter is
 * REPLACED by whatever the caller passes — which is how the transcript's paste
 * disclosure shipped an `<svg>` with no width at all and grew to the width of
 * the bubble. `size-3.5` is applied unless the caller's own classes name a size.
 *
 * ONE COLUMN, ONE SHAPE. Where marks stack — the live view's status column —
 * they are one 20-unit ring with a different interior (`circle-check`,
 * `circle-x`, `circle-dot`, `circle-dashed`, `circle-slash`, `circle-alert`), so
 * fifty rows keep one rhythm and no row's mark outweighs its neighbour's. Ink is
 * what the eye measures, not the box: `check` is 16 × 11 of it and
 * `circle-dashed` is 20 × 20, and a column that mixes them looks ragged at the
 * same `size-3`.
 *
 * Deliberately NOT here: `●`/`○`/`[✓]`, which are the cross-channel choice
 * marks the TUI paints too (see `HUMAN_INPUT_CHOICE_MARKS`), and the spinner's
 * Braille cadence. Those are the same characters on purpose — a terminal cell
 * holds a character, so the TUI mirrors these mark names in glyphs and an SVG
 * never travels the other way.
 */
import {
  AlignLeft,
  ArrowDown,
  ArrowUp,
  ArrowUpRight,
  Camera,
  ChartNoAxesColumn,
  Check,
  ChevronRight,
  ChevronsUpDown,
  CircleAlert,
  CircleCheck,
  CircleDashed,
  CircleDot,
  CircleSlash,
  CircleX,
  Copy,
  Download,
  Ellipsis,
  Folder,
  GitFork,
  Globe,
  Image,
  type LucideIcon,
  Mic,
  MicVocal,
  Paperclip,
  Pause,
  Pencil,
  Play,
  Plus,
  Search,
  Settings,
  Square,
  Star,
  Trash2,
  TriangleAlert,
  X,
  Zap,
} from "lucide-react";

/** One class list from parts, so an absent one leaves no hole behind. */
const classes = (...parts: (string | false | undefined)[]) =>
  parts.filter(Boolean).join(" ").replace(/\s+/g, " ").trim();

/** Does this class list already say how big the mark is? */
const NAMES_A_SIZE = /(?:^|\s)(?:size|[hw])-/;

/**
 * Every mark on the same terms: this app's size floor, and `aria-hidden`,
 * because an icon inside an already-labelled control is decoration. Everything
 * else — grid, fill, stroke, caps, joins — is the library's, untouched.
 */
function Mark({
  icon: Drawn,
  className,
}: {
  icon: LucideIcon;
  className?: string;
}) {
  return (
    <Drawn
      className={classes(
        "shrink-0",
        !NAMES_A_SIZE.test(className ?? "") && "size-3.5",
        className,
      )}
      aria-hidden="true"
    />
  );
}

/**
 * A MARK SITS ON THE BASELINE, and its box is taller than the cap beside it: a
 * 12px ring centres its ink 6px above the baseline, while 11px type centres its
 * caps 3.9px above it. Two pixels down is the difference — measured, and the same
 * two at every step this app sets a mark inside (`text-meta` 10, `text-ui` 11,
 * `text-body` 12); the reason a mark that is geometrically right still reads a
 * hair high until it is moved.
 *
 * Only a mark sharing a line WITH TYPE takes it. A mark that is a control's whole
 * face is centred by its own box and must never be nudged.
 */
export const MARK_NUDGE = "translate-y-[2px]";
/** The mark of an ATTACHMENT. */
export function ClipIcon({ className }: { className?: string }) {
  return <Mark icon={Paperclip} className={className} />;
}

/** The way out of a dialog. Two strokes, not a `✕` set in the body font. */
export function CloseIcon({ className }: { className?: string }) {
  return <Mark icon={X} className={className} />;
}

/**
 * THE MARK OF SEARCH, and the one convention nobody gets to be creative about: a
 * schematic magnifying glass, INSIDE the open field and leading it. A bare framed box
 * on the bar reads as "some input"; the glass says what typing here does before a
 * placeholder is read. It is ink, not a control — the field's own label speaks it.
 */
export function SearchIcon({ className }: { className?: string }) {
  return <Mark icon={Search} className={className} />;
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
 * It is a rotation rather than a second mark so the three can never drift in
 * stroke, cap or weight.
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
    <Mark
      icon={ChevronRight}
      className={classes(
        "transition-transform duration-150 motion-reduce:transition-none",
        open ? "rotate-90" : back && "rotate-180",
        className,
      )}
    />
  );
}

/** Play. Solid, because a hollow triangle at thumbnail size reads as a flaw. */
export function PlayIcon({ className }: { className?: string }) {
  return <Mark icon={Play} className={classes("fill-current", className)} />;
}

/** Stop — a solid square paired with Play's solid triangle. */
export function StopIcon({ className }: { className?: string }) {
  return <Mark icon={Square} className={classes("fill-current", className)} />;
}

/** Pause — the same solid weight as play, because they are one control. */
export function PauseIcon({ className }: { className?: string }) {
  return <Mark icon={Pause} className={classes("fill-current", className)} />;
}

/** More of the same, below: what "Load 12 more" does to the list. */
export function ArrowDownIcon({ className }: { className?: string }) {
  return <Mark icon={ArrowDown} className={className} />;
}

/** Bring one not-yet-local thing down onto this machine. */
export function DownloadIcon({ className }: { className?: string }) {
  return <Mark icon={Download} className={className} />;
}

/** Out of here — a citation, a link, a thing that opens elsewhere. */
export function ArrowOutIcon({ className }: { className?: string }) {
  return <Mark icon={ArrowUpRight} className={className} />;
}

/**
 * SEND — named for the act, not the direction. This is the same `ArrowUp` the
 * sort header points with, and it is deliberately not called `ArrowUpIcon`: the
 * composer's primary control says "send", and a screen that names the job can
 * never end up drawing two marks for one verb. The character `↑` stood here
 * long after every other mark had moved — the control this app presses most,
 * wearing the body font's weight beside icons drawn at stroke 2.
 */
export function SendIcon({ className }: { className?: string }) {
  return <Mark icon={ArrowUp} className={className} />;
}
/** Add. Rotated 45° by its caller, it is the same stroke saying "close". */
export function PlusIcon({ className }: { className?: string }) {
  return <Mark icon={Plus} className={className} />;
}

/** Take a photo. */
export function CameraIcon({ className }: { className?: string }) {
  return <Mark icon={Camera} className={className} />;
}

/** A picture already taken. */
export function ImageIcon({ className }: { className?: string }) {
  return <Mark icon={Image} className={className} />;
}

/** Dictation. */
export function MicIcon({ className }: { className?: string }) {
  return <Mark icon={Mic} className={className} />;
}

/**
 * VOICE CONVERSATION — the microphone that answers back. Dictation's mic writes
 * into the box and stops; this one sends what you said and reads the reply
 * aloud, so it is the same capsule with the reply coming out of it. One control
 * now carries two modes, and the mode has to be visible without a word beside it.
 */
export function VoiceLoopIcon({ className }: { className?: string }) {
  return <Mark icon={MicVocal} className={className} />;
}

/** Rename. */
export function PencilIcon({ className }: { className?: string }) {
  return <Mark icon={Pencil} className={className} />;
}

/** Delete. The row swipe's own mark, and the only way "delete" is ever drawn. */
export function TrashIcon({ className }: { className?: string }) {
  return <Mark icon={Trash2} className={className} />;
}

/**
 * The favorite mark. Filled is "starred": the BODY is an amber FILL, so it wears
 * the brand yellow (`accent`, #ffc420) and never the brown ink a text glyph
 * needs. Yellow on yellow paper is not a mark, though — #ffc420 on the light
 * theme's #faf3eb measures 1.45:1, under the 3:1 a graphic owes, so a starred
 * row carried a glyph the eye slid straight off. The EDGE carries the contrast
 * the fill cannot: `accent-ink` (light #7a4a00, 6.8:1 on that paper; dark
 * #fde68a on panel). The shape is legible, the colour is still the brand yellow.
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
    <Mark
      icon={Star}
      className={classes(
        filled ? "fill-accent stroke-accent-ink" : "fill-none stroke-current",
        className,
      )}
    />
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
  if (dir === "asc") return <Mark icon={ArrowUp} className={className} />;
  if (dir === "desc") return <Mark icon={ArrowDown} className={className} />;
  return <Mark icon={ChevronsUpDown} className={className} />;
}

/**
 * WHERE a machine answers: a globe, because the choice this opens is a ROUTE —
 * tailnet, LAN or loopback — and not a name or a rank.
 */
export function AddressIcon({ className }: { className?: string }) {
  return <Mark icon={Globe} className={className} />;
}

/**
 * A project: a folder, because a project in this app IS a directory on a
 * machine, and the shape is what separates "a place on disk" from a document.
 */
export function ProjectsIcon({ className }: { className?: string }) {
  return <Mark icon={Folder} className={className} />;
}

/** A fork of a conversation: one trunk with a branch leaving it. */
export function ForkIcon({ className }: { className?: string }) {
  return <Mark icon={GitFork} className={className} />;
}

export function SettingsIcon({ className }: { className?: string }) {
  return <Mark icon={Settings} className={className} />;
}

/**
 * REASONING EFFORT, as a LEVEL — three bars of growing height, because the fact
 * behind this chip is a RANK and bars are the one shape that says a rank without
 * a word. A brain, a diamond and a gauge were drawn and looked at first: at the
 * 12px a `text-chip` line allows, each collapses into one grey blob; three bars
 * keep their count.
 */
export function ReasoningIcon({ className }: { className?: string }) {
  return <Mark icon={ChartNoAxesColumn} className={className} />;
}

/**
 * VERBOSITY — lines of prose, ragged right: how MUCH the answer says. Bars stand
 * up and lines lie down, so the two level chips never read as one mark repeated.
 */
export function VerbosityIcon({ className }: { className?: string }) {
  return <Mark icon={AlignLeft} className={className} />;
}

/** FAST — the low-latency mode. The bolt is that word in every product. */
export function FastIcon({ className }: { className?: string }) {
  return <Mark icon={Zap} className={className} />;
}

/**
 * Something did not arrive: a picture that failed to fetch, a tile whose bytes
 * never came. A bang inside the same triangle the warning banners use, so the
 * failed state of a thumbnail is recognisable at 14px without a word.
 */
export function AlertIcon({ className }: { className?: string }) {
  return <Mark icon={TriangleAlert} className={className} />;
}

/**
 * Overflow — "more actions." Three dots on the optical centre, so a menu trigger
 * reads the same weight at every size the way a strip of stroked icons does. The
 * `⋯` glyph this replaces is a MATH symbol (U+22EF): it sits on the cap-height
 * line, never the centre, and at small sizes it is a dash.
 */
export function DotsIcon({ className }: { className?: string }) {
  return <Mark icon={Ellipsis} className={className} />;
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
  return <Mark icon={Copy} className={className} />;
}

/** DONE. The tick that answers a press — the copy chip's second face. */
export function CheckIcon({ className }: { className?: string }) {
  return <Mark icon={Check} className={className} />;
}

/**
 * THE STATUS COLUMN — one ring, five interiors. These six are a SET and are used
 * as one: a live view paints a column of them, and the column is legible because
 * every mark is the same 20-unit circle and only what sits inside it changes.
 * They are the only marks in this file chosen for each other rather than for the
 * control they lead, and the TUI paints their glyph twins in the same order.
 */
export function CircleCheckIcon({ className }: { className?: string }) {
  return <Mark icon={CircleCheck} className={className} />;
}

/** Failed — the ring with the cross the close control uses. */
export function CircleXIcon({ className }: { className?: string }) {
  return <Mark icon={CircleX} className={className} />;
}

/** Running, now: the ring with something alive at its centre. */
export function CircleDotIcon({ className }: { className?: string }) {
  return <Mark icon={CircleDot} className={className} />;
}

/** Queued — the ring drawn as a dashed outline, because nothing has happened yet. */
export function CircleDashedIcon({ className }: { className?: string }) {
  return <Mark icon={CircleDashed} className={className} />;
}

/** Skipped or cancelled: the ring, struck through. Not a failure — a non-event. */
export function CircleSlashIcon({ className }: { className?: string }) {
  return <Mark icon={CircleSlash} className={className} />;
}

/** A warning that belongs in the status column, where the triangle would not fit the ring. */
export function CircleAlertIcon({ className }: { className?: string }) {
  return <Mark icon={CircleAlert} className={className} />;
}

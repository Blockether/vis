/**
 * App icon vocabulary backed only by Lucide. Screens import these semantic names,
 * never `lucide-react`, raw SVGs or font glyphs as controls. Icon boxes scale with
 * adjacent type while Lucide stroke width stays unchanged; status columns use the
 * shared circular family. Terminal choice marks and the text spinner remain text.
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
  Crop,
  Download,
  Ellipsis,
  Eraser,
  Folder,
  GitFork,
  Globe,
  Image,
  LoaderCircle,
  type LucideIcon,
  Mic,
  MicVocal,
  PanelLeft,
  Paperclip,
  Pause,
  Pencil,
  Play,
  Plus,
  RefreshCw,
  Search,
  Settings,
  Share2,
  Square,
  Star,
  Trash2,
  TriangleAlert,
  Undo2,
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
  fill,
}: {
  icon: LucideIcon;
  className?: string;
  fill?: "none" | "currentColor";
}) {
  return (
    <Drawn
      className={classes(
        "shrink-0",
        !NAMES_A_SIZE.test(className ?? "") && "size-3.5",
        className,
      )}
      fill={fill}
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

/** Remove every stroke from the current drawing. */
export function ClearIcon({ className }: { className?: string }) {
  return <Mark icon={Eraser} className={className} />;
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

/** Take back the most recent drawing stroke. */
export function UndoIcon({ className }: { className?: string }) {
  return <Mark icon={Undo2} className={className} />;
}
/** Draw directly on a picture. */
export function DrawIcon({ className }: { className?: string }) {
  return <Mark icon={Pencil} className={className} />;
}

/** Rename. */
export function PencilIcon({ className }: { className?: string }) {
  return <Mark icon={Pencil} className={className} />;
}

/** Keep only the part of a picture inside the frame. */
export function TrimIcon({ className }: { className?: string }) {
  return <Mark icon={Crop} className={className} />;
}

/** Delete. The row swipe's own mark, and the only way "delete" is ever drawn. */
export function TrashIcon({ className }: { className?: string }) {
  return <Mark icon={Trash2} className={className} />;
}

/**
 * The favorite mark. Filled is "starred": the BODY carries `fill=currentColor`
 * itself so a webview cannot drop the state with a missing utility; `text-accent`
 * gives that body the brand yellow (#ffc420). Yellow on yellow paper is not a mark,
 * though — #ffc420 on the light theme's #faf3eb measures 1.45:1, under the 3:1 a
 * graphic owes. The EDGE carries the contrast the fill cannot: `accent-ink` (light
 * #7a4a00, 6.8:1 on that paper; dark #fde68a on panel). The shape is legible, the
 * colour is still the brand yellow.
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
      fill={filled ? "currentColor" : "none"}
      className={classes(
        filled ? "text-accent stroke-accent-ink" : "stroke-current",
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

/** Work in flight inside a graphical control: one open circle that turns. */
export function LoadingIcon({ className }: { className?: string }) {
  return (
    <Mark
      icon={LoaderCircle}
      className={classes("animate-spin motion-reduce:animate-none", className)}
    />
  );
}

/** Re-read a live collection. The same arrows turn while that read is in flight. */
export function RefreshIcon({
  isBusy = false,
  className,
}: {
  isBusy?: boolean;
  className?: string;
}) {
  return (
    <Mark
      icon={RefreshCw}
      className={classes(
        isBusy && "animate-spin motion-reduce:animate-none",
        className,
      )}
    />
  );
}
export function SettingsIcon({ className }: { className?: string }) {
  return <Mark icon={Settings} className={className} />;
}

/** The desk's sidebar, as the one glyph every desktop app uses for putting it away. */
export function SidebarIcon({ className }: { className?: string }) {
  return <Mark icon={PanelLeft} className={className} />;
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

/** Hand a picture to the platform share sheet. */
export function ShareIcon({ className }: { className?: string }) {
  return <Mark icon={Share2} className={className} />;
}

/** DONE. The tick that answers a press — the copy chip's second face. */
export function CheckIcon({ className }: { className?: string }) {
  return <Mark icon={Check} className={className} />;
}

/**
 * THE STATUS COLUMN — one ring, six interiors. These six are a SET and are used
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

/**
 * Stopped before it finished — cancelled, or interrupted. The ring struck
 * through, because the work is neither done nor wrong and a cross would call it
 * a failure the reader then has to go and disprove.
 */
export function CircleSlashIcon({ className }: { className?: string }) {
  return <Mark icon={CircleSlash} className={className} />;
}
/** A warning that belongs in the status column, where the triangle would not fit the ring. */
export function CircleAlertIcon({ className }: { className?: string }) {
  return <Mark icon={CircleAlert} className={className} />;
}

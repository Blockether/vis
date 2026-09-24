/**
 * The session navigator's feature vocabulary. These pieces belong to the session list,
 * not to the app-wide control vocabulary in `ui.tsx`.
 */
import {
  Fragment,
  forwardRef,
  useRef,
  useState,
  type ButtonHTMLAttributes,
  type InputHTMLAttributes,
  type ReactNode,
  type Ref,
} from 'react';

import type { PullPhase } from '../lib/pull-to-search';
import { ChevronIcon, LoadingIcon, PlusIcon, ProjectsIcon, SearchIcon } from './icons';
import { IconButton, overlayLayer } from './ui';

const HEADER_TYPE = 'text-title';

/**
 * The sticky project band owns its top and bottom hairlines in every fold state.
 * Session rows draw only their internal separators, so the closing edge is never doubled.
 */
const HEADER_BAND =
  'min-h-13 items-stretch mouse:min-h-12 z-10 border-y border-project-header-border [--dialog-hint:var(--footer-strong)]';

/**
 * The session list's pull gesture takes over the app bar with the action a release would take.
 * Its overlay frame clips the band at the safe-area edge: a touch at the top writes an inline
 * -100% translation before we know whether it is a pull or a normal scroll, which would
 * otherwise override the idle offset and expose a strip above the app bar.
 */
export function PullToSearchHint({ phase, ref }: { phase: PullPhase; ref?: Ref<HTMLDivElement> }) {
  const isShown = phase !== 'none';
  const isArmed = phase === 'armed';
  const { position } = overlayLayer();
  return (
    <div
      aria-hidden="true"
      className={`pointer-events-none ${position} inset-x-0 top-[env(safe-area-inset-top)] z-40 h-12 overflow-hidden`}
    >
      <div
        ref={ref}
        className={`flex h-full items-center justify-center gap-2 border-b border-dialog-edge font-mono text-meta transition-[translate] duration-150 motion-reduce:transition-none ${
          isShown ? 'translate-y-0' : '-translate-y-full'
        } ${isArmed ? 'bg-accent-surface text-accent-ink' : 'bg-level-project text-dialog-hint'}`}
      >
        <SearchIcon
          className={`size-3.5 shrink-0 transition-transform duration-150 motion-reduce:transition-none ${
            isArmed ? 'scale-125' : 'scale-100'
          }`}
        />
        {isArmed ? 'Release to search' : 'Pull to search'}
      </div>
    </div>
  );
}

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

/** Shared trailing controls keep their full hit targets, with roughly 16px to the icon. */
// Account for the icon's inset within its 32px touch / 28px mouse target rather
// than adding a second full gutter outside the button.
//
// THE STEP BETWEEN TWO SLOTS IS THE INSET THAT ENDS THE RAIL. A header keeps both of its
// marks in this one cluster, so the gap is what separates them; a row keeps its disclosure
// here and hands its `⋯` to the cell just outside, so this cluster's own right padding is
// what separates THOSE two. The two numbers have to be one number, or a header's plus
// stands a couple of pixels off the disclosure of every row beneath it.
const LIST_TRAIL = 'flex shrink-0 items-stretch gap-2 self-stretch pr-2 mouse:gap-2.5 mouse:pr-2.5';
/**
 * The same rail, centred through a header's band instead of stretched down a row: a
 * header's own trailing cluster, and the inset every mark down that column shares.
 */
export const HEADER_TRAIL =
  'flex shrink-0 items-center gap-2 self-stretch pr-2 mouse:gap-2.5 mouse:pr-2.5';

/** Machine and project headers align their identity and disclosure marks. */
export const LIST_MARK = 'grid size-3.5 shrink-0 place-items-center';

/**
 * One heading, standing in the band that carries the boundary (`HEADER_BAND`).
 *
 * Its border remains the same in every fold state. In light mode, the chevron alone
 * shows which projects are open; other themes also tint an expanded header. Every
 * session is in a project, so the list has ONE kind of section.
 *
 * A SECTION'S OWN CONTROLS OWN ITS TRAILING EDGE, AND A PAGER IS NOT ONE OF THEM.
 * Reported over the project header while the pager stood on that edge: a paged project
 * put its plus and its menu in the middle of the band while an unpaged one kept them
 * flush right, so the same two buttons sat at two distances from the same screen edge.
 * The steps stand over the SET they move instead — the bands over a project's groups,
 * the page over its sessions (`SetHeader`) — which leaves this band one shape, and the
 * row menu last on it, paged or not.
 */
export function SectionHeader({
  children,
  isExpanded = false,
}: {
  children: ReactNode;
  isExpanded?: boolean;
}) {
  return (
    <header
      className={`${HEADER_BAND} sticky top-0 flex ${
        isExpanded
          ? 'bg-project-header-active [--hover:color-mix(in_srgb,var(--fg)_4%,var(--color-project-header-active))]'
          : 'bg-project-header [--hover:color-mix(in_srgb,var(--fg)_4%,var(--color-project-header))]'
      } mouse:focus-within:bg-hover mouse:has-[[aria-haspopup=dialog][aria-expanded=true]]:bg-hover`}
    >
      {children}
    </header>
  );
}

/**
 * Separate machines more strongly than projects. Project groups use a compact gap
 * and a heading rule; machines retain their larger, unpainted break.
 */
export function MachineGap() {
  return <div aria-hidden="true" className="h-8" />;
}

/**
 * The bare field underneath every name edited directly on a navigator face.
 *
 * It removes only browser furniture. The caller supplies the name's existing type face,
 * while `fit` says whether sibling ink must stay beside a word-sized field or the title owns
 * the whole track. This became shared when session titles joined header names: two hand-stripped
 * inputs would be two subtly different definitions of direct editing.
 */
export const EditableNameField = forwardRef<
  HTMLInputElement,
  Omit<InputHTMLAttributes<HTMLInputElement>, 'className'> & {
    face: string;
    fit?: 'content' | 'track';
  }
>(function EditableNameField({ face, fit = 'content', ...props }, ref) {
  return (
    <input
      ref={ref}
      className={`${face} appearance-none border-0 bg-transparent p-0 focus:outline-none ${
        fit === 'track' ? 'min-w-0 w-full' : ''
      }`}
      {...props}
    />
  );
});

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
        className={`${face} text-left mouse:hover:text-accent-ink focus-visible:outline-none focus-visible:underline`}
      >
        {value}
      </button>
    );
  const commit = () => {
    setDraft(null);
    if (draft.trim() !== value) onCommit(draft.trim());
  };
  return (
    <EditableNameField
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
      face={face}
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
    <span className="flex min-w-0 flex-1 items-center gap-2 overflow-hidden pl-4">
      {/* The column is RESERVED, marked or not: the machine header wears a hue
          block here and the project header below it wears nothing, and a column
          that only exists when it is filled put the machine's name at x=36 and
          the project's at x=14 on a 390px iPhone — the deeper row starting
          further left, which is hierarchy read backwards. */}
      <span className={LIST_MARK}>{mark}</span>
      {/* The column takes exactly what the trailing cluster leaves. `items-start` keeps
          each line as wide as its own ink and no wider, while `max-w-[100%]` caps that
          ink at the column so both lines truncate before the controls. */}
      <span className="flex min-w-0 flex-1 flex-col items-start overflow-hidden">
        {onRename ? (
          <EditableName
            face={`max-w-[100%] min-w-0 truncate bg-transparent p-0 font-bold text-white ${HEADER_TYPE}`}
            label={renameLabel ?? 'Rename'}
            value={typeof name === 'string' ? name : ''}
            onCommit={onRename}
          />
        ) : (
          <span className={`max-w-[100%] min-w-0 truncate font-bold text-white ${HEADER_TYPE}`}>
            {name}
          </span>
        )}
        {qualifier && (
          <span
            className="max-w-[100%] min-w-0 truncate font-mono text-ui text-dialog-hint mouse:text-meta"
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
 * A project's name and counts share a stable two-line band.
 * Pagination lives outside this identity block so it cannot stretch either line.
 */
export function ProjectCrumb({
  name,
  qualifier,
  qualifierTitle,
  disclosure,
}: {
  name: ReactNode;
  qualifier?: ReactNode;
  qualifierTitle?: string;
  /** The fold to expose, or null when this project has no session list. */
  disclosure: {
    isOpen: boolean;
    onToggle: () => void;
    /** What the fold is called to a screen reader: `Collapse vis`. */
    label: string;
  } | null;
}) {
  return (
    <span
      className="grid min-w-0 flex-1 grid-cols-[auto_minmax(0,1fr)] items-center gap-x-0 pl-4"
    >
      {disclosure && (
        <button
          type="button"
          aria-expanded={disclosure.isOpen}
          aria-label={disclosure.label}
          onClick={disclosure.onToggle}
          className="col-span-2 col-start-1 row-span-2 row-start-1 -ml-4 self-stretch focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-inset focus-visible:ring-white"
        />
      )}
      {/* The mark belongs in the left gutter; the reserved slot still aligns project and machine names. */}
      <span className={`pointer-events-none col-start-1 row-span-2 row-start-1 ${LIST_MARK}`}>
        {disclosure && (
          <ChevronIcon open={disclosure.isOpen} className="size-3.5 -translate-x-1.5 text-dialog-hint" />
        )}
      </span>
      <span
        className={`pointer-events-none col-start-2 row-start-1 min-w-0 truncate self-end font-bold text-white ${HEADER_TYPE}`}
      >
        {name}
      </span>
      {qualifier && (
        // ONE LEVEL UP, THE SAME QUIET VOICE: under a pointer this caption stands at the
        // step a group band counts in, so a project's own total reads as a caption under
        // its name (reported from the sessions list: it was bold and a step too large).
        <span
          className="pointer-events-none col-start-2 row-start-2 min-w-0 truncate self-start font-mono text-ui text-dialog-hint mouse:text-chip"
          title={qualifierTitle}
        >
          {qualifier}
        </span>
      )}
    </span>
  );
}

/**
 * Compact navigation over ONE set — a project's sessions, or its wall of bands:
 * previous, current / total, next.
 * Edit the current number to jump; Enter or blur commits, Escape restores it.
 * Under a pointer the whole cluster steps onto the band's 24px rhythm and its
 * metadata type, so it reads as header chrome; touch keeps its 44px targets.
 * The counter reserves its final width and centers its ink so neither arrow moves.
 * Disabled navigation keeps its place and its width, so a set that cannot move right
 * now does not move its steps.
 */
export function Pager({
  page,
  pageCount,
  onPage,
  label,
  disabled = false,
}: {
  /** 1-based, so it reads the way it is printed. */
  page: number;
  pageCount: number;
  onPage: (page: number) => void;
  /** What is being paged, for the screen reader: "vis sessions". */
  label: string;
  disabled?: boolean;
}) {
  const [draft, setDraft] = useState<string | null>(null);
  const cancelled = useRef(false);
  if (disabled && draft !== null) setDraft(null);
  if (pageCount <= 1) return null;
  const step = (target: number, isBack: boolean) => (
    <IconButton
      variant="quiet"
      density="band"
      label={isBack ? 'Previous page' : 'Next page'}
      onClick={() => onPage(target)}
      disabled={disabled || target < 1 || target > pageCount}
    >
      <ChevronIcon
        back={isBack}
        className={`mx-auto size-3 ${isBack ? 'translate-x-1' : '-translate-x-1'} mouse:translate-x-0`}
      />
    </IconButton>
  );
  return (
    <nav
      aria-label={`Pages of ${label}`}
      aria-disabled={disabled || undefined}
      className="flex shrink-0 items-center justify-end gap-2 whitespace-nowrap mouse:gap-1"
    >
      <span aria-live="polite" className="sr-only">
        Page {page} of {pageCount}
      </span>
      {step(page - 1, true)}
      <label
        className={`group grid min-h-11 min-w-11 items-center font-mono text-ui tabular-nums mouse:min-h-6 mouse:min-w-6 mouse:text-meta ${disabled ? 'cursor-not-allowed text-muted' : 'cursor-text text-white'}`}
      >
        <span aria-hidden="true" className="invisible col-start-1 row-start-1">
          {pageCount}/{pageCount}
        </span>
        <span className="col-start-1 row-start-1 flex min-w-0 items-center justify-center">
          <span className="relative">
            <span aria-hidden="true" className="invisible">
              {(draft ?? String(page)).slice(0, String(pageCount).length) || '0'}
            </span>
            <EditableNameField
              aria-label="Current page"
              aria-description={`Enter a page from 1 to ${pageCount}`}
              inputMode="numeric"
              enterKeyHint="go"
              autoComplete="off"
              spellCheck={false}
              fit="track"
              disabled={disabled}
              value={draft ?? String(page)}
              onFocus={(event) => event.currentTarget.select()}
              onClick={(event) => event.currentTarget.select()}
              onChange={(event) => setDraft(event.currentTarget.value)}
              onBlur={(event) => {
                const value = event.currentTarget.value.trim();
                setDraft(null);
                if (disabled || cancelled.current) {
                  cancelled.current = false;
                  return;
                }
                if (!/^\d+$/.test(value)) return;
                const target = Math.min(pageCount, Math.max(1, Number(value)));
                if (target !== page) onPage(target);
              }}
              onKeyDown={(event) => {
                if (event.nativeEvent.isComposing) return;
                if (event.key === 'Enter' || event.key === 'Escape') {
                  event.preventDefault();
                  event.stopPropagation();
                  cancelled.current = event.key === 'Escape';
                  event.currentTarget.blur();
                }
              }}
              face="absolute inset-0 text-center focus:underline focus:underline-offset-2 disabled:cursor-not-allowed mouse:group-hover:enabled:not-focus:text-accent-ink"
            />
          </span>
          <span aria-hidden="true">/{pageCount}</span>
        </span>
      </label>
      {step(page + 1, false)}
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
 * third distance. One component decides all of it now, on every header and every row:
 * the cluster keeps the trailing edge even where a pager stands, and the pager takes
 * the cell just inside it.
 *
 * A row stretches its trailing target through its height. A header instead centres its
 * compact controls through the full band, so neither is pinned to the title's first line.
 */
export function HeaderActions({
  children,
  align = 'stretch',
}: {
  children: ReactNode;
  align?: 'center' | 'stretch';
}) {
  return <span className={align === 'center' ? HEADER_TRAIL : LIST_TRAIL}>{children}</span>;
}

/**
 * "There is more inside this row", and there is only one of it.
 *
 * The permanent trailing control uses the same compact disc as the project's +, and
 * stands where the project's plus does: one slot inside the trailing edge, which the
 * desktop action menu holds on every header and every row. Project and session
 * controls keep the same two columns regardless of how many actions they offer.
 * Quiet ink and an explicit expanded state keep it readable without hover.
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
      aria-expanded={isOpen}
      className={className}
      {...props}
    >
      <ChevronIcon open={isOpen} className="size-3.5" />
    </IconButton>
  );
});

/**
 * The count beside a set or group label. Group bands use the small, muted voice; set
 * headings use a larger foreground voice to remain legible on their tinted or neutral
 * surface. `HeaderTally` has no step or weight of its own.
 *
 * IT OPENS WITH THE DOT THAT JOINS THE COUNT TO ITS LABEL, the same joint a project header
 * already sets between its total and the states beside it. Reported from a screenshot of the
 * list: that header read `1679 Sessions · 1 live` while the bands under it ran `GROUPS 2
 * Groups` together, so the count stood as a second label instead of answering the first.
 */
export function HeaderMeta({
  children,
  variant = 'band',
}: {
  children: ReactNode;
  variant?: 'band' | 'set';
}) {
  return (
    <span
      className={`flex items-center gap-2 font-mono font-bold ${variant === 'set' ? 'text-meta text-white' : 'text-chip text-dialog-hint'}`}
    >
      {/* Hidden from a reader who HEARS the line: the dot is punctuation between two runs of
          text, and "Groups dot 2 groups" is not what the eye is being told. */}
      <span aria-hidden>·</span>
      {children}
    </span>
  );
}

/**
 * A header's own count, standing beside the word over the set it counts (`SetHeader`'s
 * label) — `GROUPS · 2 Groups`. THE LABEL SHOUTS AND THE COUNT ANSWERS: the label names the
 * set and is set in small caps, while the count is a phrase about that set, so it is set in
 * title case and reads as words rather than as a second label beside the first.
 *
 * IT CARRIES NEITHER STEP NOR WEIGHT OF ITS OWN: the header line sets both. In a group
 * band that line is the small `HeaderMeta`; a named set has a larger label and count. In
 * a project header it is `ProjectCrumb`'s qualifier, shared with the states and an
 * arrival so the whole line stays on the total's baseline.
 *
 * A count is a NUMBER AND ITS NOUN, on every screen. A bare `725` over a list of
 * rows says nothing about what was counted, and the phone is exactly where the
 * reader has the least context to supply it from — so the noun is never dropped to
 * win back width. What gives way instead is the project's own name, which
 * truncates with the full path on its `title` — or, where a caller's own row is
 * fuller than its column, the whole count: `className` positions this span, so a
 * header may let the total ellipsise and keep an arrival beside it whole.
 */
export function HeaderTally({
  count,
  unit,
  className = '',
}: {
  count: number;
  unit: string;
  className?: string;
}) {
  const noun = count === 1 ? unit : `${unit}s`;
  return (
    <span className={`whitespace-nowrap font-mono tracking-[0.08em] capitalize ${className}`}>
      {count} {noun}
    </span>
  );
}

/**
 * Actionable project states, separated from the total and from one another.
 * Waiting-for-input is removed from LIVE because one session cannot claim two states.
 */
export function ProjectStatusCounts({
  live,
  awaiting = 0,
  unread = 0,
}: {
  live: number;
  awaiting?: number;
  unread?: number;
}) {
  const running = Math.max(0, live - awaiting);
  const statuses = [
    running > 0
      ? {
          label: `${running} live`,
          tone: 'text-white',
          dot: 'animate-pulse bg-ok motion-reduce:animate-none',
        }
      : null,
    awaiting > 0
      ? {
          label: `${awaiting} needs input`,
          // Neutral ink stays readable on both the project band and its hover.
          // The colored dot and label preserve the status distinction.
          tone: 'text-white',
          dot: 'animate-pulse bg-warn-strong motion-reduce:animate-none',
        }
      : null,
    unread > 0 ? { label: `${unread} new`, tone: 'text-accent-ink', dot: 'bg-accent' } : null,
  ].filter((status): status is NonNullable<typeof status> => status !== null);

  // INLINE, NOT `inline-flex`: the qualifier line ellipsizes when a phone's band
  // runs out of room, and `text-overflow` only elides TEXT — an atomic inline box
  // is dropped whole, so a status built as one painted `1 live ·▪` and no ellipsis.
  return statuses.map((status) => (
    <Fragment key={status.label}>
      {/* On a list under 28rem the dots close up: the phone's band holds the count,
          the live pulse and the amber demand beside a pager and a verb, and at 440px
          those three facts wanted 250px of the 238px left — `1 needs input` lost
          its last word to an ellipsis. Eight pixels a side was the difference. */}
      <span aria-hidden className="mx-2 @max-md:mx-1">
        ·
      </span>
      <span className={`whitespace-nowrap font-bold ${status.tone}`}>
        <span
          className={`mr-1 inline-block size-1.5 align-[0.05em] ${status.dot}`}
          aria-hidden="true"
        />
        {status.label}
      </span>
    </Fragment>
  ));
}

/**
 * One scrollable machine-state track plus a separate add action. `All` is the first tile
 * only for a fleet; selection and unread activity paint the tile, while overflow stays
 * inside the track.
 */
export function MachineSwitcher({ children }: { children: ReactNode }) {
  return (
    <div className="flex min-w-0 shrink items-center gap-0.5 overflow-x-auto rounded-none bg-level-machine p-0.5">
      {children}
    </div>
  );
}

/**
 * Machine state tile. Unread activity fills the tile rather than adding a glyph. A
 * machine known down becomes a retry action with no pressed state or unread tint.
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
  /** Not answering: retry instead of selecting; an error-toned name replaces the removed mark. */
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
      className={`inline-flex h-7 shrink-0 items-center gap-1.5 rounded-none px-2 font-mono text-meta transition-colors duration-150 focus-visible:outline-2 focus-visible:outline-offset-2 motion-reduce:transition-none mouse:h-5 ${
        isDown
          ? 'text-err-ink hover:text-err-ink focus-visible:outline-err-ink'
          : hasUnread
            ? `bg-machine-unread font-bold text-white focus-visible:outline-white ${isOn ? 'ring-1 ring-inset ring-white' : ''}`
            : isOn
              ? 'bg-accent-surface font-bold text-accent-ink ring-1 ring-inset ring-accent-ink focus-visible:outline-accent-ink'
              : 'text-dialog-hint hover:text-white focus-visible:outline-white'
      }`}
    >
      {children}
      {note && <span className={isNoteError ? 'text-err' : 'opacity-80'}>{note}</span>}
      {hasUnread && !isDown && <span className="sr-only">unread</span>}
    </button>
  );
}

/**
 * The verb of one project: start a session in it.
 *
 * Repeated project actions use neutral ink without a border or a circular fill.
 * The compact 32px layout box keeps a 44px touch target through `IconButton`.
 * `where` stays in the tooltip and `machine` in the accessible name, unless the
 * button belongs to a `group`: a band's plus is named for the band it starts in, so the
 * plus on the loose set's own header stays a different control. Creation replaces the
 * plus with a spinner without changing the header's width.
 *
 * It shares the list's trailing rail: before the menu on a group's band, or at the
 * right edge of the Sessions strip after its page controls. The layout box stays the
 * same in both places.
 */
export function NewSessionButton({
  machine,
  where,
  group,
  disabled,
  isBusy = false,
  onPress,
}: {
  machine: string;
  where?: string | null;
  /** The session group this plus starts INSIDE, when it stands on a group's band. */
  group?: string;
  disabled?: boolean;
  isBusy?: boolean;
  onPress: (anchor: HTMLElement) => void;
}) {
  const label = group ? `New session in ${group}` : `New session on ${machine}`;
  const title = group
    ? `${label}, on ${machine}`
    : where
      ? `New session on ${machine}, in ${where}`
      : label;
  return (
    <IconButton
      disabled={disabled || isBusy}
      aria-busy={isBusy || undefined}
      aria-live="polite"
      label={label}
      title={title}
      onClick={(event) => onPress(event.currentTarget)}
    >
      {isBusy ? <LoadingIcon className="size-4" /> : <PlusIcon className="size-4" />}
    </IconButton>
  );
}

/**
 * A MACHINE'S PROJECTS: the inventory of one gateway, opened from the thing that
 * names that gateway.
 *
 * IT IS NOT A CREATE, and for a long time it said it was. `openManageProjects` opens
 * `ManageProjectsSheet` on `Projects` — choose the machine's current
 * project, remove one, or take the `New project…` at its foot — so a control spelled
 * "New project" promised the last of the three things behind it, and a plus on the
 * band would have promised the same plus one row below meant a session. So it wears
 * the NOUN it opens: the app's one mark for a place on disk, the same folder a
 * project row carries, with the fork inside it left to mean a copy of that place.
 *
 * Project navigation uses a neutral outline beside the machine switcher.
 * The footer keeps its unframed quiet variant. Both name the machine for
 * assistive technology.
 *
 * `pressEffect="none"`: the sheet it opens is anchored on this button's measured box,
 * and a transform moves the box that was measured.
 */
export function MachineProjectsButton({
  machine,
  isQuiet,
  onPress,
}: {
  machine: string;
  /** Use the unframed variant in a footer. */
  isQuiet?: boolean;
  onPress: (anchor: HTMLElement) => void;
}) {
  const label = `Projects on ${machine}`;
  const title = `Projects on ${machine} — choose one, add one, remove one`;
  return (
    <IconButton
      variant={isQuiet ? 'quiet' : 'secondary'}
      label={label}
      title={title}
      onClick={(event) => onPress(event.currentTarget)}
    >
      <ProjectsIcon className="size-4" />
    </IconButton>
  );
}

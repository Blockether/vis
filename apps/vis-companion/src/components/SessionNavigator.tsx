/**
 * The session navigator's feature vocabulary. These pieces belong to the session list,
 * not to the app-wide control vocabulary in `ui.tsx`.
 */
import {
  Fragment,
  forwardRef,
  useState,
  type ButtonHTMLAttributes,
  type ReactNode,
  type Ref,
} from 'react';

import type { MachineColor } from '../lib/machine-colors';
import type { PullPhase } from '../lib/pull-to-search';
import { ChevronIcon, PlusIcon, ProjectsIcon, SearchIcon } from './icons';
import { Button, IconButton, LIST_EDGE } from './ui';

const HEADER_TYPE = 'text-title';

/**
 * The band every header in the list stands in. It sticks; nothing above it does.
 *
 * Both heights are that stack plus air, minus the rule the band sends out — 52 - 2 -
 * 34 and 48 - 2 - 34 — so whatever the stack carries decides them. That is exactly
 * how the pointer column reached zero while it still spelled 36.
 */
const HEADER_BAND =
  'flex min-h-13 items-stretch mouse:min-h-12 sticky top-0 z-10 bg-level-project';

/** The session list's pull gesture reports the action a release would take. */
export function PullToSearchHint({ phase, ref }: { phase: PullPhase; ref?: Ref<HTMLDivElement> }) {
  const isShown = phase !== 'none';
  const isArmed = phase === 'armed';
  return (
    <div
      ref={ref}
      aria-hidden="true"
      className={`pointer-events-none absolute inset-x-0 top-0 z-20 flex min-h-13 items-center justify-center gap-2 border-b border-dialog-edge font-mono text-meta transition-[translate] duration-150 motion-reduce:transition-none mouse:min-h-12 ${
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
 * THE MARK COLUMN, and every band AND every row in the list reserves it.
 *
 * A machine is marked by a 6px identity block and a project by a 14px disclosure, so
 * with each sized to its own ink the machine's name began at x=28 and the project's
 * name directly below it at x=36 — the last 8px of the same misalignment, surviving
 * inside the two components that had just been taught to share every other edge.
 * One column, and the names start together.
 *
 * A session row carries no mark at all and reserves it anyway, empty. Measured on the
 * desk: a project's name stood at x=302 and the titles of the rows it heads at x=279,
 * so the heading and the things it heads sat on two different left edges — the last
 * one of the five this file was written to collapse. A column that exists only where
 * something fills it is not a column.
 */
export const LIST_MARK = 'grid size-3.5 shrink-0 place-items-center';

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
 * THE TROUGH between two sections, and the only thing between them that is not a
 * line.
 *
 * Reported ("there is no visual differentiation between the projects and it all
 * looks like kind of the same thing"): the last row of one project and the header
 * of the next were separated by a single hairline, the same hairline that separates
 * two sessions inside one project — one pixel of grey asked to mean both "next row"
 * and "different repository". A boundary that matters is seen before it is read, so
 * it is 8px of the machine's own paper (`bg-level-machine`, `L 0.905` against the
 * card's `0.96`): each project reads as a slab cut from the paper its machine owns, and
 * above a fleet that machine's hue runs down the side of the gap, because it owns both
 * sides of it.
 *
 * It is 8px and not a margin: a gap collapses, a slab does not, and this one has to
 * PAINT to be a boundary at all. On a desk it paints the PAGE instead: the projects
 * it separates stand there as sheets on that page rather than as slabs cut out of
 * one card, and the paper between two objects belongs to neither of them.
 */
export function SectionGap() {
  return <div aria-hidden="true" className="h-2 bg-level-machine sm:bg-page" />;
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
      <span className={LIST_MARK}>{mark}</span>
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
 * The leading half of a PROJECT header.
 *
 * A project holding sessions is the level a reader scopes by, so it is the level that
 * folds: with four checkouts on one machine the screen is otherwise a scroll through
 * work nobody asked about. The mark column `HeaderTitle` already reserves carries the
 * chevron, so the name stays on the one leading edge every row in this list shares —
 * the disclosure buys a glyph, never an indent.
 *
 * A persisted project with no sessions still names where New session will start, but
 * has no list to reveal. `disclosure={null}` renders that name as a plain HeaderTitle:
 * no button, no expanded state, and no chevron pointing at nothing.
 *
 * When present, the disclosure is a `HeaderTitle` inside a pressable, not a button
 * beside one: the whole naming half answers the thumb, and the trailing cluster
 * (`HeaderActions`) keeps its own controls, so "New session" is never swallowed by
 * the fold.
 *
 * Its qualifier line carries what the project HOLDS as well as where it lives. The
 * count stood on a shelf of its own under this band, so one heading was two papers,
 * two hairlines and two sticky boxes; it cannot move into the trailing cluster
 * either, because measured at 320px the count, the live pulse and the amber verb
 * take that cluster's width first and leave the NAME 24px.
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
  const title = (
    <HeaderTitle
      mark={
        disclosure ? (
          <ChevronIcon open={disclosure.isOpen} className="size-3.5 text-dialog-hint" />
        ) : undefined
      }
      name={name}
      qualifier={qualifier}
      qualifierTitle={qualifierTitle}
    />
  );
  if (!disclosure) return title;

  return (
    <button
      type="button"
      aria-expanded={disclosure.isOpen}
      aria-label={disclosure.label}
      onClick={disclosure.onToggle}
      className="flex min-w-0 flex-1 items-center text-left transition-colors duration-150 hover:bg-hover focus-visible:bg-hover focus-visible:outline-none motion-reduce:transition-none"
    >
      {title}
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
 * It is NOT a band of its own: it is a cluster in the project band's own trailing
 * column, beside the verb that band offers. It rode on a `SectionShelf` hung under
 * that band — a second paper, a second hairline and a second sticky layer for one
 * heading — and before that it was painted at the FOOT of the rows, a `border-t
 * border-dialog-edge` strip on the rows' own paper one hairline above the next
 * project's header, so `1 2 … 80 ›` read either as the last session of `vis` or as
 * the first thing in `vis-companion`. In the band it travels with the name it walks:
 * at row 40 of a 794-session project the reader still sees which project they are in
 * and can still jump to page 12, because the band itself sticks.
 *
 * What the pager owns is the steps at its ends and what stands between them: from
 * `sm` up the NUMBERS, every one of them pressable, so page 5 of 73 is ONE tap and
 * not four; on a phone, where no line holds the numbers beside a project's name,
 * the position itself (`4 / 80`) at a width that never changes. The paper, the
 * list's trailing edge and the band's closing rule belong to the band. Below one
 * page it renders nothing at all — a pager for a project with four sessions is a
 * control that can never be pressed.
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
        <ChevronIcon back={isBack} className="size-4" />
      </IconButton>
    );
  };
  return (
    <nav
      aria-label={`Pages of ${label}`}
      // The cluster is exactly as wide as its own content and never grows
      // (`shrink-0`, no basis to negotiate): it stands in the band's trailing column
      // beside the verb, and a control that negotiated for width there would take it
      // from the project's own name — see the two forms below.
      className="flex min-w-0 shrink-0 justify-end"
    >
      {/* The pager is LIVE: pressing a step or a number changes nothing else on the
          shelf, so without this a screen reader hears silence after the press. It is
          the ONE voice of the position — both visible forms below are drawn from it,
          and neither is announced a second time. */}
      <span aria-live="polite" className="sr-only">
        Page {page} of {pageCount}
      </span>
      {/* The band runs the width of the list; the CONTROL does not. Steps pinned to
          the paper's two edges put `<` and `>` 360px apart on a phone, so paging is a
          two-handed reach and no thumb can rest between them — you cannot tap `>`
          twice without moving. The cluster is held in the band's trailing column
          instead, which puts the two steps a thumb's width from the numbers they
          belong to and in the column every other trailing control already uses.

          It is sized by its CONTENT, and `>` is what that buys: `>` ends the cluster,
          the cluster ends where the band's verb begins, so `>` is at the same x on
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
            every page of every project, so the band holds one height for the whole
            walk. `<` and `>` still step, and the strip comes back whole
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
      ? { label: `${running} live`, tone: 'text-ok', dot: 'animate-pulse bg-ok motion-reduce:animate-none' }
      : null,
    awaiting > 0
      ? {
          label: `${awaiting} needs input`,
          tone: 'text-warn-strong',
          dot: 'animate-pulse bg-warn-strong motion-reduce:animate-none',
        }
      : null,
    unread > 0 ? { label: `${unread} new`, tone: 'text-accent-ink', dot: 'bg-accent' } : null,
  ].filter((status): status is NonNullable<typeof status> => status !== null);

  return statuses.map((status) => (
    <Fragment key={status.label}>
      <span aria-hidden>·</span>
      <span className={`inline-flex items-center gap-1 whitespace-nowrap font-bold ${status.tone}`}>
        <span className={`size-1.5 ${status.dot}`} aria-hidden="true" />
        {status.label}
      </span>
    </Fragment>
  ));
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
 * THE CORNERS ARE THE ONES ITS NEIGHBOUR WEARS. The first report on this row —
 * "definitely there should be no rounded corners" — was about a pill floating over a
 * stack of square bands, and the answer to it squared a CONTROL along with the
 * planes. This is a segmented switch standing beside a disc: track and tiles are
 * capsules, so every painted box on the strip is a 32px face with round ends and the
 * row reads as one rhythm instead of a rectangle next to a circle. The bands under it
 * stay square, because they are still planes.
 */
export function MachineSwitcher({ children }: { children: ReactNode }) {
  return (
    <div className="flex min-w-0 shrink items-center gap-0.5 overflow-x-auto rounded-full bg-level-machine p-0.5">
      {children}
    </div>
  );
}

/**
 * One machine inside the switcher's track. Selection is a RAISED TILE — the page's
 * own paper lifted out of the track — never a border and never the accent: amber is
 * this product's verb colour, and a selected tab painted in it reads as a button
 * that will do something when you press it. A capsule, like the track that holds it.
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
      className={`inline-flex h-7 shrink-0 items-center gap-1.5 rounded-full px-2 font-mono text-meta transition-colors duration-150 motion-reduce:transition-none mouse:h-5 ${
        isDown
          ? 'text-dialog-hint hover:text-white'
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
 * The primary verb of the session list: start one session in this project.
 *
 * A project header repeats, so the resting control is one yellow plus rather
 * than the same phrase on every row. The face stays on the compact 32px header
 * rhythm while `Button` preserves a 44px touch target outside the painted box.
 * `where` remains in the tooltip and `machine` in the accessible name.
 */
export function NewSessionButton({
  machine,
  where,
  disabled,
  busyLabel,
  onPress,
}: {
  machine: string;
  where?: string | null;
  disabled?: boolean;
  /** The work this button started, spoken inside the pressed control. */
  busyLabel?: string | null;
  onPress: (anchor: HTMLElement) => void;
}) {
  const label = `New session on ${machine}`;
  const title = where ? `New session on ${machine}, in ${where}` : label;
  if (busyLabel) {
    return (
      <Button
        type="button"
        pressEffect="none"
        density="compact"
        disabled
        aria-live="polite"
        aria-label={label}
        title={title}
        className="shrink-0 whitespace-nowrap"
        onClick={(event) => onPress(event.currentTarget)}
      >
        {busyLabel}
      </Button>
    );
  }
  return (
    <IconButton
      variant="primary"
      density="compact"
      disabled={disabled}
      aria-live="polite"
      label={label}
      title={title}
      onClick={(event) => onPress(event.currentTarget)}
    >
      <PlusIcon className="size-4" />
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
 * IT HAS THREE PLACES TO STAND AND ONE OF THEM IS QUIET. Over the card while the
 * list is scoped to one machine, in the desk rail's PROJECTS caption while there is
 * a rail, and in the desk footer while there is not — and a footer is 24px of
 * `text-chip` ink, where an amber block would outrank every word beside it and
 * become the screen's second filled accent. It names its machine for assistive
 * technology wherever it stands.
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
  /** It is standing in a footer, so it is ink rather than the amber fill. */
  isQuiet?: boolean;
  onPress: (anchor: HTMLElement) => void;
}) {
  const label = `Projects on ${machine}`;
  const title = `Projects on ${machine} — choose one, add one, remove one`;
  return (
    <IconButton
      variant={isQuiet ? 'quiet' : 'primary'}
      label={label}
      title={title}
      onClick={(event) => onPress(event.currentTarget)}
    >
      <ProjectsIcon className="size-4" />
    </IconButton>
  );
}

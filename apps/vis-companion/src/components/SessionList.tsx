import { memo, useCallback, useEffect, useState } from "react";

import { Banner, ConfirmRow, LIST_EDGE } from "./ui";
import {
  HeaderActions,
  HeaderTitle,
  LIST_EDGE_END,
  LIST_MARK,
  RowDisclosure,
  SectionHeader,
} from "./SessionNavigator";
import { SwipeActions } from "./SwipeActions";
import { ForkIcon, PencilIcon, StarIcon, TrashIcon } from "./icons";
import { GatewayClient, type SessionMatch } from "../lib/gateway";
import type { GatewayConn, Session, SessionUsage } from "../lib/types";
import {
  draftMessageHasUnsent,
  type DraftMessage,
} from "../lib/draft-messages";
import type { PendingAttachment } from "../lib/attachments";
import { unreadTurnCount, useReadMarks } from "../lib/unread";
import { isFavorite } from "../lib/favorites";
import { sessionIsLive, sessionNeedsInput, timeLabel } from "../lib/fleet";

// Same frames as the session transcript's spinner and the TUI's
// `paint-content-loading!` — one vocabulary for "working" across the product.
// Two placeholder projects with ragged title widths: an even grid reads as a
// rendered table, a ragged one reads as text that has not arrived yet.
const SKELETON_GROUPS = [
  ["w-3/5", "w-2/5", "w-1/2"],
  ["w-1/2", "w-2/3"],
];

// How long the row's disclosure takes to open or close. It is duplicated by the
// `duration-200` utilities below on purpose: the class drives the paint, this
// number only decides when the panel may leave the tree.
const STATS_MOTION_MS = 200;

export const SessionRow = memo(function SessionRow({
  session,
  draft,
  conn,
  match,
  needle,
  onOpen,
  onRename,
  onFork,
  onDelete,
  onToggleStar,
  isConfirmingDelete,
  deleteBusy,
  deleteError,
  onConfirmDelete,
  onCancelDelete,
}: {
  session: Session;
  /** This device's unsent composer content for the session; EMPTY when there is none. */
  draft: DraftMessage;
  conn: GatewayConn;
  match: SessionMatch | null;
  needle: string;
  onOpen: (
    conn: GatewayConn,
    sid: string,
    fresh?: boolean,
  ) => void | Promise<void>;
  onRename: (session: Session) => void;
  /** Opens the fork question under the very cell that was pressed. */
  onFork: (session: Session, anchor: HTMLElement) => void;
  onDelete: (session: Session) => void;
  onToggleStar: (session: Session) => void;
  /** This row IS the confirm: it asks in place instead of behind a dialog. */
  isConfirmingDelete: boolean;
  deleteBusy: boolean;
  deleteError: string | null;
  onConfirmDelete: () => void;
  onCancelDelete: () => void;
}) {
  const status = statusLabel(session);
  const timestamp = session.modified_at ?? session.created_at;
  // DIRTY: this device is holding composer content nobody has sent — words, a
  // picture, a file. When the session has no title of its own, that content names
  // the row, which otherwise reads "Untitled session" with nothing on screen to
  // say why it is worth opening.
  const hasUnsent = draftMessageHasUnsent(draft);
  const title =
    session.title?.trim() ||
    (hasUnsent
      ? firstLine(draft.text) || attachmentSummary(draft.attachments)
      : "") ||
    "Untitled session";
  const live = sessionIsLive(session);
  const turns = Number(session.turn_count ?? 0);
  // Turns that finished while this session was closed: the one thing a relative
  // timestamp cannot announce. The row SUBSCRIBES to the marks: the list stays
  // mounted behind the transcript, and this component is memoised over row objects
  // an unchanged poll returns identical — so without the subscription the badge of
  // the session you just read stayed on screen until something else moved.
  useReadMarks();
  const unread = unreadTurnCount(session);
  // The right chevron is a real DISCLOSURE, not decoration: it opens this
  // session's usage rollup in place. It stays a sibling of the open-session
  // button, never nested inside it, so "tell me more" cannot navigate away.
  const [statsOpen, setStatsOpen] = useState(false);
  // The rollup FETCHES on mount and aborts on unmount, so it cannot simply stay
  // mounted while closed. Dropping it on the same commit that closes the row
  // would pull the content out from under the collapse, so it outlives
  // `statsOpen` by exactly one transition and is only then let go.
  const [statsMounted, setStatsMounted] = useState(false);
  useEffect(() => {
    if (statsOpen || !statsMounted) return;
    const timer = window.setTimeout(
      () => setStatsMounted(false),
      STATS_MOTION_MS,
    );
    return () => window.clearTimeout(timer);
  }, [statsOpen, statsMounted]);
  // Mount and open in ONE commit. The grid wrapper never leaves the tree, so
  // 0fr -> 1fr is a transition on a persistent element — no `@starting-style`,
  // which WebKit applies a frame late to freshly inserted nodes.
  const toggleStats = useCallback(() => {
    setStatsMounted(true);
    setStatsOpen((open) => !open);
  }, []);
  // The star is the GATEWAY's, and the row is holding the only copy of it there is:
  // `favorite_rank`, straight off this session. No device-side store can disagree
  // with it — which is what used to leave one screen starred and another plain.
  const isStarred = isFavorite(session);
  // Where the row GOES when this flips — the pinned band at the top of the project,
  // on page one — belongs to the group that pages it, so `ProjectGroup` owns the
  // follow. This is only the mark and the strip's verb.
  const toggleStar = useCallback(
    () => onToggleStar(session),
    [onToggleStar, session],
  );

  return (
    <div className="[&+&]:border-t [&+&]:border-dialog-edge">
      {/* The confirm IS the row (`ConfirmRow`, shared with machine and project
          removal). Only renaming needs a dialog because its answer is a field. */}
      {isConfirmingDelete ? (
        <ConfirmRow
          question={`Delete ${title}?`}
          confirmLabel={deleteBusy ? "Deleting..." : "Yes, delete"}
          isBusy={deleteBusy}
          onKeep={onCancelDelete}
          onConfirm={onConfirmDelete}
        />
      ) : (
        <SwipeActions
          label={title}
          actions={[
            {
              key: "favorite",
              label: isStarred ? "Unstar" : "Star",
              icon: <StarIcon filled={isStarred} className="size-4" />,
              // The one action on the strip that is not a neutral verb: it wears the
              // same brand yellow as the mark it leaves on the row.
              tone: "accent",
              onSelect: toggleStar,
            },
            {
              key: "rename",
              label: "Rename",
              icon: <PencilIcon className="size-4" />,
              onSelect: () => onRename(session),
            },
            {
              key: "fork",
              // The strip is 72px wide, so the caption is the one word; the whole
              // sentence lives in `name` for a reader who cannot see the row.
              label: "Fork",
              name: `Fork ${title}`,
              icon: <ForkIcon className="size-4" />,
              // Forking COPIES — it takes nothing away from the row it starts on —
              // so it stays a neutral verb beside Rename, never the red one.
              onSelect: (anchor) => onFork(session, anchor),
            },
            {
              key: "delete",
              label: "Delete",
              icon: <TrashIcon className="size-4" />,
              tone: "danger",
              onSelect: () => onDelete(session),
            },
          ]}
        >
          {/* THE ROW IS ONE SLAB, and the hover tint is ITS colour, not the open
          button's. The button stops where the disclosure begins, so a pointer
          crossing the row lit 948px of a 991px row and left the last 43 in plain
          paper — the trailing strip of the very row under the cursor. */}
          <div className="group flex items-stretch transition-colors duration-150 hover:bg-hover motion-reduce:transition-none">
            <button
              type="button"
              className={`flex min-h-12 min-w-0 flex-1 items-center gap-2 py-1.5 text-left transition-colors duration-150 active:bg-hover focus-visible:bg-hover focus-visible:outline-none motion-reduce:transition-none mouse:min-h-8 mouse:py-1 ${LIST_EDGE} ${LIST_EDGE_END}`}
              data-session-id={session.id}
              onClick={() => void onOpen(conn, session.id)}
            >
              {/* THE MARK COLUMN, EMPTY — and reserved for exactly that reason. The
            project band above these rows spends it on its fold, so a row that
            skipped it started its title 23px to the LEFT of the name that heads
            it. See `LIST_MARK`. */}
              <span className={LIST_MARK} aria-hidden="true" />
              {/* One row of facts, laid out twice from ONE dom order.
            A phone stacks it: what the session IS on the first line, what it has DONE
            on the second, each line's own trailing fact right-aligned against it.
            From `sm:` up there is room for the whole sentence on one line, and the
            facts stop floating: the wrapper below turns to `contents` so its children
            become grid items of the row itself, and id / turns / status / time land on
            FIXED tracks. That is the difference between a list and a phone list
            stretched to 1400px, where a title sat at x=56 and its own status badge at
            x=1325 with nothing between them to carry the eye across.
            Each fixed track is its own content's width: the status one holds its fixed
            favorite slot BEFORE the dot plus the longest label. `INPUT NEEDED` measures
            83px, so the whole cluster owns 7.25rem and neither a title nor a missing star
            can move it. The id track pays for its 8 hex characters inside 4.5rem instead
            of charging that width to the title. */}
              <span className="grid min-w-0 flex-1 grid-cols-[minmax(0,1fr)_auto_auto] items-center gap-x-3 gap-y-1 sm:grid-cols-[minmax(0,1fr)_5.5rem_4.5rem_4.5rem_7.25rem_6rem] sm:gap-y-0">
                {/* The NAME, and nothing but the name. The badges used to ride inside this
              cell, so every row started its flags at a different x — the longer the
              title, the further right its `NEW` — and a long title pushed them off
              the line entirely. They have their own column now. */}
                <span className="col-start-1 row-start-1 flex min-w-0 items-center gap-1.5 sm:col-start-auto sm:row-start-auto">
                  {/* The LEAF, and the smallest name on the screen: 15 / 13 / 10 down the
                machine -> project -> session ladder. It stays the strongest thing in
                its OWN row — semibold, full ink, against the hint-grey `text-chip`
                facts beside it — so shrinking it costs the scan nothing. */}
                  <span
                    className={`min-w-0 truncate font-mono text-meta font-semibold ${
                      session.title?.trim() ? "text-white" : "text-white/45"
                    }`}
                  >
                    {title}
                  </span>
                </span>
                {/* Unread and unsent-message flags share one aligned column. */}
                <span className="col-start-2 row-start-1 flex min-w-0 items-center justify-end gap-1.5 font-mono text-chip sm:col-start-auto sm:row-start-auto">
                  {unread > 0 && (
                    <span className="shrink-0 bg-accent px-1 font-mono text-chip font-bold uppercase tracking-[0.08em] text-accent-foreground">
                      {unread > 1 ? `${unread} new` : "new"}
                    </span>
                  )}
                  {hasUnsent && (
                    <span
                      className="shrink-0 border border-warn-strong px-1 font-mono text-chip font-bold uppercase tracking-[0.08em] text-warn-strong"
                      title="Unsent message waiting in this session's composer"
                    >
                      dirty
                    </span>
                  )}
                </span>
                {/* `sm:contents` is what lets one dom order be two layouts: on a phone this
              is a single line of prose under the title, and from `sm:` up it dissolves
              so that the id and the turn count become columns in their own right. */}
                {/* One rank, one ink: hierarchy is carried by SIZE (title 12px vs meta 10px),
              never by transparency — an id at 55% ink beside a `·` at 40% beside a full
              hint made one 9px line carry three different inks and none of them readable.
              A COUNT ends on its track's edge, not where its own digits run out:
              left-aligned, `9 turns` stopped 6px short of the `20 turns` one row below
              it, and a column of numbers that does not end together is not a column. */}
                <span className="col-start-1 row-start-2 flex min-w-0 items-center gap-x-2 font-mono text-meta text-dialog-hint sm:contents">
                  <span className="truncate tabular-nums">
                    {shortId(session.id)}
                  </span>
                  <span className="sm:hidden" aria-hidden="true">
                    ·
                  </span>
                  <span className="whitespace-nowrap font-mono text-meta text-dialog-hint tabular-nums sm:justify-self-end">
                    {turns} {turns === 1 ? "turn" : "turns"}
                  </span>
                </span>
                <span
                  data-session-status
                  className={`col-start-3 row-start-1 inline-flex shrink-0 items-center gap-1 justify-self-end font-mono text-chip font-bold tracking-[0.08em] sm:col-start-auto sm:row-start-auto sm:justify-self-start ${statusTone(session)}`}
                >
                  <span
                    data-session-favorite-slot
                    className="inline-flex size-3 shrink-0 items-center justify-center"
                  >
                    {isStarred && (
                      <>
                        <StarIcon filled className="size-3" />
                        <span className="sr-only">Favorite</span>
                      </>
                    )}
                  </span>
                  <span
                    data-session-status-dot
                    aria-hidden="true"
                    className={`size-1.5 shrink-0 ${statusDot(session)} ${live ? "animate-pulse motion-reduce:animate-none" : ""}`}
                  />
                  <span>{status}</span>
                </span>
                <span
                  className="col-start-2 col-end-4 row-start-2 justify-self-end whitespace-nowrap font-mono text-meta text-dialog-hint tabular-nums sm:col-start-auto sm:col-end-auto sm:row-start-auto"
                  title={formatExact(timestamp)}
                >
                  {timeLabel(timestamp)}
                </span>
              </span>
            </button>
            {/* The same box, the same column and the same right edge as the `⋯` in the
            project header directly above: both promise "there is more here", so
            neither is allowed its own geometry. */}
            <HeaderActions>
              <RowDisclosure
                isOpen={statsOpen}
                label={`${statsOpen ? "Hide" : "Show"} details for ${title}`}
                onClick={toggleStats}
              />
            </HeaderActions>
          </div>
        </SwipeActions>
      )}
      {isConfirmingDelete && deleteError && (
        <div className="px-3 pb-2">
          <Banner kind="err">{deleteError}</Banner>
        </div>
      )}
      {/* Height eases through a 0fr -> 1fr grid track: the one pure-CSS way to
          animate to CONTENT height without measuring it, and unlike a mount it
          plays in BOTH directions. The inner clip keeps the rollup from
          spilling over the next row while the track is still closing. */}
      <div
        aria-hidden={!statsOpen}
        className={`grid transition-[grid-template-rows] duration-200 ease-[cubic-bezier(0.22,0.61,0.36,1)] motion-reduce:transition-none ${
          statsOpen ? "grid-rows-[1fr]" : "grid-rows-[0fr]"
        }`}
      >
        <div className="overflow-hidden">
          <div
            className={`transition-[opacity,translate] duration-200 ease-[cubic-bezier(0.22,0.61,0.36,1)] motion-reduce:transition-none ${
              statsOpen ? "opacity-100" : "-translate-y-1 opacity-0"
            }`}
          >
            {statsMounted && <SessionStats session={session} conn={conn} />}
          </div>
        </div>
      </div>
      {match && <MatchPreview match={match} needle={needle} />}
    </div>
  );
});

// The expanded half of a session row: everything the list cannot afford to
// carry for every session at once. It is fetched HERE only when the row opens,
// and aborted if the row closes first; the gateway memoizes decoded tool tallies.
function SessionStats({
  session,
  conn,
}: {
  session: Session;
  conn: GatewayConn;
}) {
  const [usage, setUsage] = useState<SessionUsage | null>(null);
  const [phase, setPhase] = useState<"loading" | "ready" | "error">("loading");

  useEffect(() => {
    const controller = new AbortController();
    setPhase("loading");
    new GatewayClient(conn)
      .sessionUsage(session.id, controller.signal)
      .then((next) => {
        if (controller.signal.aborted) return;
        setUsage(next);
        setPhase("ready");
      })
      .catch(() => {
        if (!controller.signal.aborted) setPhase("error");
      });
    return () => controller.abort();
  }, [conn, session.id]);

  return <SessionStatsPanel session={session} usage={usage} phase={phase} />;
}

/** The deterministic paint surface behind the row's on-demand usage read. */
export function SessionStatsPanel({
  session,
  usage,
  phase,
}: {
  session: Session;
  usage: SessionUsage | null;
  phase: "loading" | "ready" | "error";
}) {
  const cacheReadShare = usage?.cache_read_share_percent;
  const reuseCoverage = usage?.reusable_prefix_coverage_percent;
  // The coverage number is only as good as the calls it was measured on, so the
  // card prints its sample beside it: a bold percentage over an undisclosed
  // denominator is the trick this pair exists to refuse.
  const reuseSamples = usage?.prompt_cache_sample_count;
  const reuseIsEstimated = (usage?.prompt_cache_estimated_sample_count ?? 0) > 0;

  return (
    <div
      className={`border-t border-dialog-edge bg-panel-2 py-2.5 ${LIST_EDGE} ${LIST_EDGE_END}`}
    >
      {phase === "loading" && (
        <p className="font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
          Reading usage…
        </p>
      )}
      {phase === "error" && (
        <p className="font-mono text-chip uppercase tracking-[0.08em] text-warn-strong">
          Usage unavailable
        </p>
      )}
      {phase === "ready" && !usage && (
        <p className="font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
          No turns yet
        </p>
      )}
      {phase === "ready" && usage && (
        <>
          <dl className="grid grid-cols-4 gap-x-3 gap-y-2">
            <Stat label="Turns" value={compactCount(usage.turn_count)} />
            <Stat label="Iters" value={compactCount(usage.iteration_count)} />
            <Stat label="Tools" value={compactCount(usage.tool_call_count)} />
            <Stat label="Folds" value={compactCount(usage.fold_count)} />
            <Stat label="In" value={compactCount(usage.input_tokens)} />
            <Stat label="Out" value={compactCount(usage.output_tokens)} />
            <Stat label="Cost" value={formatUsd(usage.cost_usd)} />
          </dl>
          <dl
            aria-label="Prompt cache"
            className="mt-2.5 grid grid-cols-2 gap-3 border-t border-dialog-edge/40 pt-2"
          >
            <CacheStat
              label="Cached input"
              value={
                typeof cacheReadShare === "number"
                  ? `${Math.round(cacheReadShare)}%`
                  : "—"
              }
              explanation="Share of all input served from provider cache"
            />
            <CacheStat
              label="Reuse coverage"
              value={
                typeof reuseCoverage === "number"
                  ? `${reuseIsEstimated ? "≈" : ""}${Math.round(reuseCoverage)}%`
                  : "—"
              }
              explanation={
                typeof reuseSamples === "number"
                  ? `${reuseIsEstimated ? "Estimated share" : "Share"} of reusable prior input recovered from cache · ${compactCount(reuseSamples)} of ${compactCount(usage.iteration_count)} calls`
                  : `${reuseIsEstimated ? "Estimated share" : "Share"} of reusable prior input recovered from cache`
              }
            />
          </dl>
          <dl className="mt-2.5 flex flex-wrap items-baseline gap-x-3 gap-y-1 border-t border-dialog-edge/40 pt-2">
            {/* `/usage` names the model the session actually RAN on, but only
                once a turn has finished stamping it — a live session's newest
                turn has none. The pin (list row) and the state's root model are
                the standing answers, so fall back to those instead of a dash. */}
            <Meta
              label="Model"
              value={
                usage.model || session.model_pref?.model || session.model || "—"
              }
              title={usage.provider || session.model_pref?.provider}
            />
            <Meta
              label="Active"
              value={formatDuration(usage.duration_ms)}
              title="Time spent inside turns"
            />
          </dl>
        </>
      )}
    </div>
  );
}

function Stat({ label, value }: { label: string; value: string }) {
  return (
    <div className="min-w-0">
      <dt className="truncate font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
        {label}
      </dt>
      <dd className="truncate font-mono text-chip font-bold tabular-nums text-white">
        {value}
      </dd>
    </div>
  );
}

function CacheStat({
  label,
  value,
  explanation,
}: {
  label: string;
  value: string;
  explanation: string;
}) {
  return (
    <div className="min-w-0">
      <dt className="truncate font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
        {label}
      </dt>
      <dd className="truncate font-mono text-chip font-bold tabular-nums text-white">
        {value}
      </dd>
      <dd className="mt-0.5 text-balance font-mono text-chip text-dialog-hint">
        {explanation}
      </dd>
    </div>
  );
}

// The grid above answers "how much"; this row answers "of what, for how long".
// It reuses the grid's dim-key/strong-value grammar so the two facts read as
// labelled data instead of one faint unlabelled sentence.
function Meta({
  label,
  value,
  title,
}: {
  label: string;
  value: string;
  title?: string;
}) {
  return (
    <div className="flex min-w-0 items-baseline gap-1.5" title={title}>
      <dt className="shrink-0 font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
        {label}
      </dt>
      <dd className="min-w-0 truncate font-mono text-meta font-bold text-white">
        {value}
      </dd>
    </div>
  );
}

function compactCount(value?: number): string {
  const n = Number(value ?? 0);
  if (!Number.isFinite(n)) return "—";
  if (n < 1_000) return String(n);
  if (n < 1_000_000) return `${(n / 1_000).toFixed(n < 10_000 ? 1 : 0)}k`;
  if (n < 1_000_000_000)
    return `${(n / 1_000_000).toFixed(n < 10_000_000 ? 1 : 0)}M`;
  return `${(n / 1_000_000_000).toFixed(1)}B`;
}

// Sub-cent totals must not read as "$0.00" — a session that cost something is
// never free.
function formatUsd(value?: number): string {
  const n = Number(value ?? 0);
  if (!Number.isFinite(n) || n <= 0) return "$0";
  if (n < 0.01) return "<$0.01";
  if (n < 1_000) return `$${n.toFixed(2)}`;
  return `$${Math.round(n).toLocaleString()}`;
}

function formatDuration(value?: number): string {
  const ms = Number(value ?? 0);
  if (!Number.isFinite(ms) || ms <= 0) return "0s";
  const seconds = Math.round(ms / 1_000);
  if (seconds < 60) return `${seconds}s`;
  const minutes = seconds / 60;
  if (minutes < 60) return `${Math.round(minutes)}m`;
  const hours = minutes / 60;
  return hours < 24 ? `${hours.toFixed(1)}h` : `${(hours / 24).toFixed(1)}d`;
}

// The list has nothing to paint yet. Two rules keep this honest:
//
// 1. Colour: the previous skeleton was drawn in panel tints — and `--panel2`
//    EQUALS `--surface` in the shipped themes (light: both #faf3eb), so it
//    rendered as invisible boxes. Bars use `--color-muted`, a mid grey that
//    separates from every gateway surface, and the Braille spinner says it in
//    words — the same one the transcript and the TUI use.
// 2. GEOMETRY: a placeholder that is not the exact height of the thing it
//    stands for makes the whole list jump when data lands. The skeleton mirrors
//    the compact 36px mouse project header and the session rows' density steps.
//    Each bar is centred inside an INVISIBLE glyph of the real type step, so its
//    line box — not a bare decorative bar — determines the same vertical rhythm.
// 3. ONE OWNER PER SEAM. The filter owns the line above the list, each skeleton
//    header owns the line below itself, sibling rows own their internal lines,
//    and the rows wrapper closes the group. No top border or negative margin is
//    needed, so loading and loaded states cannot stack adjacent rules.
function SkeletonBar({
  type,
  width,
  baz,
  tone,
}: {
  type: string;
  width: string;
  baz: string;
  tone: string;
}) {
  return (
    <span className={`grid ${width}`}>
      <span className={`col-start-1 row-start-1 invisible font-mono ${type}`}>
        &nbsp;
      </span>
      <span className={`col-start-1 row-start-1 self-center ${baz} ${tone}`} />
    </span>
  );
}

export function NavigatorSkeleton() {
  return (
    <div role="status" aria-live="polite" aria-label="Loading sessions">
      <div
        className="animate-pulse motion-reduce:animate-none"
        aria-hidden="true"
      >
        {SKELETON_GROUPS.map((rows, group) => (
          <div key={group}>
            {/* The list's OWN header band, so a loading screen can never stand at a
                different height from the screen it turns into. */}
            <SectionHeader rule="border-accent">
              {/* Two lines, because the header it stands in for is two: a name over
                  the path that qualifies it. A one-line skeleton grew by a line the
                  moment data landed, which is a layout jump on every cold open — so
                  the bars go through the same `name`/`qualifier` slots the real
                  header uses rather than a hand-stacked pair. */}
              <HeaderTitle
                name={
                  <SkeletonBar
                    type="text-title"
                    width="w-28"
                    baz="h-2.5"
                    tone="bg-muted/40"
                  />
                }
                qualifier={
                  <SkeletonBar
                    type="text-chip"
                    width="w-40"
                    baz="h-1.5"
                    tone="bg-muted/20"
                  />
                }
              />
              <HeaderActions>
                <SkeletonBar
                  type="text-chip"
                  width="w-14"
                  baz="h-1.5"
                  tone="bg-muted/25"
                />
              </HeaderActions>
            </SectionHeader>
            {/* Mirrors `SessionRow` — the SAME grid, the same leading edge, the same
                trailing column — because a skeleton that stands anywhere else is a
                layout jump the user pays for on every cold open. It used to carry an
                `invisible` chevron and `px-3`, indenting its bars 32px in a list whose
                real rows started their titles at 8px. */}
            <div>
              {rows.map((width, row) => (
                <div
                  key={row}
                  className={`flex min-h-12 w-full items-center py-1.5 [&+&]:border-t [&+&]:border-dialog-edge mouse:min-h-8 mouse:py-1 ${LIST_EDGE}`}
                >
                  <span className="grid min-w-0 flex-1 grid-cols-[minmax(0,1fr)_auto_auto] items-center gap-x-3 gap-y-1 sm:grid-cols-[minmax(0,1fr)_5.5rem_4.5rem_4.5rem_7.25rem_6rem] sm:gap-y-0">
                    <span className="col-start-1 row-start-1 sm:col-start-auto sm:row-start-auto">
                      <SkeletonBar
                        type="text-meta"
                        width={width}
                        baz="h-2.5"
                        tone="bg-muted/30"
                      />
                    </span>
                    <span className="col-start-1 row-start-2 flex items-center gap-x-2 sm:contents">
                      <SkeletonBar
                        type="text-chip"
                        width="w-14"
                        baz="h-1.5"
                        tone="bg-muted/20"
                      />
                      <SkeletonBar
                        type="text-chip"
                        width="w-10"
                        baz="h-1.5"
                        tone="bg-muted/20"
                      />
                    </span>
                    {/* The flag column a real row keeps for `NEW` / `dirty`.
                        Nothing is loading in it, but the track has to exist or the
                        columns shift the moment the rows arrive. */}
                    <span className="col-start-2 row-start-1 sm:col-start-auto sm:row-start-auto" />
                    <span className="col-start-3 row-start-1 justify-self-end sm:col-start-auto sm:row-start-auto sm:justify-self-start">
                      <SkeletonBar
                        type="text-chip"
                        width="w-12"
                        baz="h-1.5"
                        tone="bg-muted/25"
                      />
                    </span>
                    <span className="col-start-3 row-start-2 justify-self-end sm:col-start-auto sm:row-start-auto sm:justify-self-start">
                      <SkeletonBar
                        type="text-chip"
                        width="w-12"
                        baz="h-1.5"
                        tone="bg-muted/20"
                      />
                    </span>
                  </span>
                  <HeaderActions>
                    <span className="w-7 sm:w-8 mouse:w-6" />
                  </HeaderActions>
                </div>
              ))}
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}

export function shortId(id: string): string {
  return id.split("-")[0]?.slice(0, 8) || id.slice(0, 8);
}

function statusLabel(session: Session): string {
  // The DEMAND outranks liveness: a parked run is still live, and "LIVE" is
  // exactly what made the row look like it was getting on with it.
  if (sessionNeedsInput(session)) return "INPUT NEEDED";
  if (sessionIsLive(session)) return "LIVE";
  if (session.status === "suspended") return "WAITING";
  return "IDLE";
}

function statusTone(session: Session): string {
  if (sessionNeedsInput(session)) return "text-warn-strong";
  if (sessionIsLive(session)) return "text-ok";
  if (session.status === "suspended") return "text-warn-strong";
  return "text-dialog-hint";
}

function statusDot(session: Session): string {
  if (sessionNeedsInput(session))
    return "animate-pulse bg-warn-strong motion-reduce:animate-none";
  if (sessionIsLive(session))
    return "animate-pulse bg-ok motion-reduce:animate-none";
  if (session.status === "suspended") return "bg-warn-strong";
  return "border border-dialog-hint";
}

export function draftSearchText(draft: DraftMessage | undefined): string {
  if (!draft) return "";
  return [
    draft.text,
    ...draft.attachments.map((attachment) => attachment.filename),
  ]
    .join(" ")
    .toLowerCase();
}

export function sessionSearchText(session: Session): string {
  return [
    session.title,
    session.id,
    session.project_name,
    session.workspace?.label,
    session.workspace?.root,
    session.status,
    sessionNeedsInput(session) ? "input needed waiting human" : "",
    sessionIsLive(session) ? "live running" : "idle",
  ]
    .filter(Boolean)
    .join(" ")
    .toLowerCase();
}

function dateMillis(value?: string): number {
  if (!value) return 0;
  const millis = new Date(value).getTime();
  return Number.isFinite(millis) ? millis : 0;
}

function formatExact(value?: string): string {
  const millis = dateMillis(value);
  return millis ? new Date(millis).toLocaleString() : "";
}

// Search hits stay subordinate to their session: compact transcript rows, not cards.
function MatchPreview({
  match,
  needle,
}: {
  match: SessionMatch;
  needle: string;
}) {
  const rows =
    match.hits.length > 0
      ? match.hits
      : [
          {
            side: "request" as const,
            snippet: match.requestSnippet?.trim() ?? "",
            at: null,
          },
          {
            side: "reply" as const,
            snippet: match.replySnippet?.trim() ?? "",
            at: null,
          },
        ].filter((h) => h.snippet.length > 0);
  if (rows.length === 0) return null;
  return (
    <div
      className={`border-t border-dialog-edge bg-ink/30 py-1.5 ${LIST_EDGE} ${LIST_EDGE_END}`}
    >
      <div className="divide-y divide-dialog-edge/70">
        {rows.map((hit, index) => (
          <div
            key={`${hit.side}-${hit.at ?? index}`}
            className="grid grid-cols-[2.5rem_minmax(0,1fr)] gap-2 py-1.5"
          >
            <span
              className={`font-mono text-meta font-bold ${
                hit.side === "request" ? "text-you-role" : "text-vis-role"
              }`}
            >
              {hit.side === "request" ? "You" : "Vis"}
            </span>
            <p className="line-clamp-2 whitespace-pre-wrap break-words font-mono text-ui text-dialog-foreground">
              {highlightNeedle(hit.snippet, needle)}
            </p>
          </div>
        ))}
      </div>
    </div>
  );
}

// Splits `text` on the (case-insensitive) needle and wraps each hit in a
// contrast <mark> that reads on both rail colors.
function highlightNeedle(text: string, needle: string) {
  if (!needle) return text;
  const parts = text.split(new RegExp(`(${escapeRegExp(needle)})`, "ig"));
  return parts.map((part, index) =>
    part.toLowerCase() === needle.toLowerCase() && part.length > 0 ? (
      <mark
        key={index}
        className="rounded-[2px] bg-accent/30 px-0.5 font-bold text-white"
      >
        {part}
      </mark>
    ) : (
      <span key={index}>{part}</span>
    ),
  );
}

function escapeRegExp(value: string): string {
  return value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

// The first line of an unsent message, short enough to sit on one row. A dirty
// session has no other name, and a wall of pasted text must not become one.
export function firstLine(text: string): string {
  const line = text.split("\n", 1)[0]?.trim() ?? "";
  return line.length > 80 ? `${line.slice(0, 79)}\u2026` : line;
}

// An unsent message can be nothing but a picture. Then the attachment names the
// row, because "Untitled session" says nothing about what is waiting in it.
function attachmentSummary(attachments: PendingAttachment[]): string {
  const first = attachments[0];
  if (!first) return "";
  const name = firstLine(first.filename) || first.media_type;
  return attachments.length > 1 ? `${name} +${attachments.length - 1}` : name;
}

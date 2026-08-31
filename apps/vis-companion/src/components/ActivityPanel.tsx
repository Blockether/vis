import { useState } from "react";
import { InlineMarkdown, Markdown } from "./ChatContent";
import { Disclosure, LoadMore } from "./ui";
import type {
  ActivityDiffEvidence,
  ActivityProjection,
  ActivityResource,
  ActivityRow,
  ActivityTextEvidence,
  ActivityTextFormat,
} from "../lib/activity";
import { workspaceRelativePath } from "../lib/path";
import { useWorkspaceRoots } from "../lib/workspace-roots";

/** `2 more files`, `3 more steps` — what a rule holds back, counted and named. */
function moreCount(n: number, noun: string) {
  return `${n} more ${noun}${n === 1 ? "" : "s"}`;
}

/**
 * ACTIVITY, PAINTED WHERE IT BELONGS — inside the form that produced it.
 *
 * This is NOT a Live View, and it is a separate component for the same reason
 * it is a separate wire shape: a Live View is a surface a run OPENS, addressed
 * by its own id, closed when the run ends; Activity is a field OF one form,
 * replaced whole while the form runs and settled with it. They share a
 * transport and nothing else, so they change on their own schedules.
 *
 * The panel takes the projection itself, never a view: protocol 7 stopped
 * shipping Activity as a classified view addressed from a distance by an
 * anchor, so there is no record to unwrap and no classification to branch on.
 * One slot paints both states — the same component under the same key while the
 * form runs and once it has landed — which is what stops the transcript from
 * swapping a live surface for an artifact under the reader.
 *
 * ONE SPINE, NOT TWO. The chronology hangs on the TURN's own line — the same
 * hairline the invocation's ring stands on — and every step reaches it with a
 * tick of its own: the line is TIME, a mark on it is the moment the call
 * happened, and the tick is the step arriving. A second rail beside that one, or
 * a bracket closing the group, is a second border repeating what the named row
 * above it already said.
 *
 * A STEP IS INERT. Its mark is a 9px ring on the branch — never an icon, never
 * a state colour — because the run's states are already in the words, and a
 * column of glyphs is what made the old rail read as a list of unrelated chips.
 * The one colour on this axis is the red of a failure. A step says its verb in
 * the app's own voice, keeps its object in hint ink beside it with no separator
 * between them, and prints its duration in the machine's hand at the right
 * margin, so the eye reads WHAT HAPPENED down one column and HOW LONG down
 * another.
 *
 * What a step LEFT sits under it in the machine's own hand: the paths it read,
 * the card of what it changed, the head of the error that stopped it. Only the
 * patch itself opens, because only the patch is long.
 */

/**
 * THE VERB A STEP WEARS, one for each state a reader can tell apart.
 *
 * A settled verb over a step that FAILED is a lie printed in bold: the row said
 * "Patched" directly above its own evidence saying the patch was refused. So an
 * operation carries three words — what it is doing, what it did, and what it
 * failed to do. A cancelled step keeps the first: the act was under way and
 * never landed, which is exactly what the running word says.
 */
const ACTIVITY_VERBS: Record<
  string,
  readonly [running: string, settled: string, failed: string]
> = {
  grep: ["Searching", "Searched", "Search failed"],
  ls: ["Listing", "Listed", "List failed"],
  cat: ["Reading", "Read", "Read failed"],
  patch: ["Patching", "Patched", "Patch refused"],
  shell: ["Running", "Ran", "Command failed"],
  run_tests: ["Running tests", "Ran tests", "Tests failed"],
  lint_code: ["Linting", "Linted", "Lint failed"],
  format_code: ["Formatting", "Formatted", "Format failed"],
  repl_eval: ["Evaluating", "Evaluated", "Eval failed"],
  // What a code block did to the tree with its own hands. The change is already past
  // when it is reported, and its head says how much of the tree moved at once.
  change: ["Changing", "Changed", "Change failed"],
  mkdir: ["Creating", "Created", "Create failed"],
  write: ["Writing", "Wrote", "Write failed"],
  copy: ["Copying", "Copied", "Copy failed"],
  move: ["Moving", "Moved", "Move failed"],
  link: ["Linking", "Linked", "Link failed"],
  delete: ["Deleting", "Deleted", "Delete failed"],
};

function formatActivityDuration(value?: number): string | null {
  if (value == null || !Number.isFinite(value) || value <= 0) return null;
  const milliseconds = Math.trunc(value);
  if (milliseconds < 1_000) return `${milliseconds}ms`;
  if (milliseconds < 60_000) return `${(milliseconds / 1_000).toFixed(1)}s`;
  const minutes = Math.floor(milliseconds / 60_000);
  return `${minutes}m ${Math.floor((milliseconds % 60_000) / 1_000)}s`;
}

/** What the call was ABOUT, with the operation's own name never repeated back. */
function activityStepObject(row: ActivityRow): string {
  const summary = row.summary.trim();
  if (
    (row.presenter === "shell" || row.operation.toLowerCase() === "shell") &&
    summary.startsWith("running: ")
  ) {
    return summary.slice("running: ".length);
  }
  return summary.toLowerCase() === row.operation.trim().toLowerCase()
    ? ""
    : summary;
}

function activityRowSummary(row: ActivityRow): string {
  const object = activityStepObject(row);
  return (row.presenter === "shell" ||
    row.operation.toLowerCase() === "shell") &&
    row.summary.trim().startsWith("running: ")
    ? `cmd: ${object}`
    : object;
}

/** The step's own sentence: a verb the reader knows, then what it was applied to. */
function activityStepLead(row: ActivityRow): string {
  const verb = ACTIVITY_VERBS[row.operation.trim().toLowerCase()];
  if (!verb) return row.operation.trim();
  return verb[row.state === "failed" ? 2 : row.state === "succeeded" ? 1 : 0];
}

/** What one step reads as end to end: the verb, then the thing it was applied to. */
function activityStepHeadline(row: ActivityRow): string {
  return [activityStepLead(row), activityStepObject(row)]
    .filter(Boolean)
    .join(" · ");
}

/** What a settled step LEFT: the engine's own outcome, never a partial one. */
function activityStepOutcome(row: ActivityRow): string {
  if (row.error_summary) return row.error_summary;
  return row.state === "running" ? "" : (row.result_summary ?? "");
}

/** One counter in the margin: the words, and the tone that repeats them. */
export interface ActivityCostPart {
  readonly text: string;
  /** Empty for the margin's own ink — a count that needs no colour to be read. */
  readonly tone: string;
}

/**
 * WHAT THE ITERATION COST THE REPOSITORY, and the whole of what a closed band says.
 *
 * Three kinds and no fourth: these are the three the wire classifies and the
 * three a reader budgets differently — what CHANGED the repository, what only
 * looked at it, what checked it. `generic` is none of them and stays uncounted,
 * because "something else happened" is not a number anyone can act on.
 *
 * `0 mutations` always prints. "Did this iteration change anything" is the one
 * question a closed invocation is asked, and it is about the rows that are NOT
 * there, so no row on the axis can answer it. The other two print only when they
 * happened, because their zero is a fact the page already shows.
 *
 * The rows the engine's own bound DROPPED are counted here from
 * `omitted.by_classification`: this line covers the whole run, a chronology that
 * shows four of ten calls must not report the cost of four, and the axis tail
 * can say `+6 more` but never what the six WERE.
 *
 * Colour repeats each noun and never carries it — mutations in the accent, reads
 * in the theme's cool ink, checks in the margin's own — so a reader who cannot
 * separate two hues loses nothing.
 */
export function activityCostParts(
  activity?: ActivityProjection,
): readonly ActivityCostPart[] {
  const dropped: Record<string, number> =
    activity?.omitted.by_classification ?? {};
  const rows = activity?.rows ?? [];
  const tally = (signal: string) =>
    rows.filter((row) => row.signal === signal).length + (dropped[signal] ?? 0);
  const noun = (amount: number, word: string) =>
    `${amount} ${word}${amount === 1 ? "" : "s"}`;
  const observations = tally("observation");
  const checks = tally("verification");
  return [
    { text: noun(tally("mutation"), "mutation"), tone: "text-accent-ink" },
    ...(observations
      ? [
          {
            text: noun(observations, "observation"),
            tone: "text-code-syntax-keyword",
          },
        ]
      : []),
    ...(checks ? [{ text: noun(checks, "check"), tone: "" }] : []),
  ];
}

/**
 * WHAT THE STEP CALLED, in the order it called it — the receipt beside the chevron.
 *
 * A reader recognises a step by its calls, not by a word for how it ended: the
 * chevron, the tone and the elapsed time already say that, and `DONE` in front of
 * every settled receipt is one word repeated down the whole transcript. Three names
 * at most, then how many are left, because past three the line stops being a glance
 * and the chronology below is the whole list anyway; a lone call also prints its own
 * subject, the one place a name like SHELL is too thin to stand by itself.
 *
 * The state word survives ONLY for a state the reader has to be told — failed,
 * cancelled. The elapsed time is printed only once it is FINAL: a number that stops
 * moving while the run is still going is worse than no number.
 */
export function activityReceiptText(
  activity?: ActivityProjection,
  durationMs?: number,
): string {
  const state = activity?.state ?? "idle";
  const live = state === "running" || state === "idle";
  const rows = activity?.rows ?? [];
  const omitted = Math.max(0, activity?.omitted.rows ?? 0);
  const shown = rows.slice(0, 3);
  const left = rows.length - shown.length + omitted;
  const subject =
    rows.length === 1 && left === 0 ? activityRowSummary(rows[0]) : "";
  const names = [...shown.map((row) => row.operation.toUpperCase()), subject]
    .filter(Boolean)
    .join(" · ");
  const calls = names
    ? `${names}${left > 0 ? ` + ${left} more` : ""}`
    : left > 0
      ? `${left} ${left === 1 ? "activity" : "activities"}`
      : live
        ? "running activity"
        : "";
  const trouble =
    state === "failed" || state === "cancelled" ? state.toUpperCase() : "";
  return [trouble, calls, live ? "" : formatActivityDuration(durationMs)]
    .filter(Boolean)
    .join(" · ");
}

function diffLineInk(
  kind: ActivityDiffEvidence["lines"][number]["kind"],
): string {
  if (kind === "addition") return "bg-code-ok text-code-success";
  if (kind === "deletion") return "bg-code-err text-code-error";
  if (kind === "header" || kind === "hunk") return "text-code-syntax-keyword";
  return "text-code-foreground";
}

function diffLineMarker(
  kind: ActivityDiffEvidence["lines"][number]["kind"],
): string {
  if (kind === "addition") return "+";
  if (kind === "deletion") return "-";
  return " ";
}

/** The patch a step left, in the transcript's own diff ink. */
function ActivityDiff({ diff }: { diff: ActivityDiffEvidence }) {
  return (
    <div
      className="max-w-full overflow-x-auto overscroll-x-contain bg-code py-1 font-mono text-meta"
      aria-label="Unified diff"
    >
      <div className="w-max min-w-full">
        {diff.lines.map((line, index) => (
          <span
            key={`${index}-${line.kind}-${line.text}`}
            className={`flex w-max min-w-full whitespace-pre px-2 py-px ${diffLineInk(line.kind)}`}
          >
            <span
              className="w-3 shrink-0 select-none text-center"
              aria-hidden="true"
            >
              {diffLineMarker(line.kind)}
            </span>
            <span className="pr-3">{line.text || " "}</span>
          </span>
        ))}
      </div>
    </div>
  );
}

/** What a step's patches added and removed, summed over every diff it left. */
function activityStepDelta(row: ActivityRow): {
  additions: number;
  deletions: number;
} {
  return row.evidence.reduce(
    (total, item) =>
      item.kind === "diff"
        ? {
            additions: total.additions + item.additions,
            deletions: total.deletions + item.deletions,
          }
        : total,
    { additions: 0, deletions: 0 },
  );
}

/**
 * THE MARK ON THE BRANCH: one ring with a filled centre, and the COLOUR is the
 * state — green done, yellow running, red failed.
 *
 * Nine pixels of the page with a dot in it — the same mark for every step,
 * because a chronology is read as a sequence and a per-state SHAPE turns it
 * into a legend the reader has to learn. Only the tail the engine's bound
 * dropped is hollow, because nothing happened there yet to give a colour, and
 * the axis still ends on a mark rather than on a stray caption.
 *
 * The mark carries its own tick back to the rail. A dot floating beside a line
 * is a bullet in a list; a dot JOINED to it is a moment on a timeline, and this
 * axis is the second thing.
 */
function ActivityNode({ state }: { state: ActivityRow["state"] }) {
  const hollow = state === "idle" || state === "cancelled";
  const edge =
    state === "failed"
      ? "border-err-ink"
      : state === "running"
        ? "border-accent-ink"
        : state === "succeeded"
          ? "border-ok"
          : "border-dialog-hint";
  const core =
    state === "failed"
      ? "bg-err-ink"
      : state === "running"
        ? "bg-accent-ink motion-safe:animate-pulse"
        : "bg-ok";
  return (
    <span
      aria-hidden="true"
      className={`absolute -left-1 top-1 size-[9px] rounded-full border bg-page before:absolute before:top-[3px] before:right-full before:h-px before:w-[8px] before:bg-edge-strong before:content-[''] sm:before:w-[10px] ${edge}`}
    >
      {!hollow && (
        <span className={`absolute inset-0.5 rounded-full ${core}`} />
      )}
    </span>
  );
}

/**
 * A PATH THAT KEEPS ITS NAME.
 *
 * `truncate` eats the END of a string, which in a monorepo is the only part
 * that differs: eight rows of `src/com/blockether/vis/internal/…` are eight
 * identical rows. The directory is what gives way, so the file being looked for
 * is always whole, and it wears the darker ink because it is the answer.
 *
 * And the words are the path RELATIVE TO THE WORKSPACE, because `/Users/ana/vis/`
 * is on every row, is the same on every row, and is exactly the part `truncate`
 * keeps. The id underneath stays ABSOLUTE — it is what the engine called the file
 * and what a press opens — so only the reading is shortened, never the address.
 */
function ActivityPath({ id }: { id: string }) {
  const roots = useWorkspaceRoots();
  const shown = workspaceRelativePath(id, roots) || id;
  const cut = shown.lastIndexOf("/");
  const directory = cut < 0 ? "" : shown.slice(0, cut + 1);
  const name = cut < 0 ? shown : shown.slice(cut + 1);
  return (
    <span className="flex min-w-0" data-path={id}>
      {directory && (
        <span className="truncate text-dialog-hint">{directory}</span>
      )}
      <span className="shrink-0">{name}</span>
    </span>
  );
}

/**
 * FOUR PATHS, THEN A COUNT.
 *
 * A read that touched forty files is a read that touched forty files: the number
 * is the fact, and forty paths printed under one row spend the whole chronology
 * on the step with the least to say. Four lines show WHICH corner of the tree a
 * step was working in; the rest are one quiet press away, in the same `+N more`
 * the thread already ends with.
 */
const ACTIVITY_FILES_SHOWN = 4;

/**
 * THE LIST A STEP LEFT: every path it touched, each holding the diff that names it.
 *
 * A file the engine reported as CHANGED and the same file among the step's resources
 * are ONE row — a diff is named by the file it patched, which is the id that file
 * arrived under. A diff whose file is not in the list still gets a row of its own,
 * because a change that never reaches the screen is the one thing this axis may not
 * drop.
 */
function activityFileRows(
  resources: ActivityResource[],
  diffs: readonly ActivityDiffEvidence[],
): { key: string; id: string; diff?: ActivityDiffEvidence }[] {
  const byPath = new Map(diffs.map((diff) => [diff.text, diff]));
  const named = new Set(resources.map((resource) => resource.id));
  return [
    ...resources.map((resource) => ({
      key: `${resource.type}:${resource.id}`,
      id: resource.id,
      diff: byPath.get(resource.id),
    })),
    ...diffs
      .filter((diff) => !named.has(diff.text))
      .map((diff) => ({ key: `diff:${diff.text}`, id: diff.text, diff })),
  ];
}

/**
 * ONE PATH, AND THE PATCH IT OPENS.
 *
 * A row that changed a file is PRESSABLE and one that only read it is not, so they
 * differ by exactly that: the chevron replaces the guillemet in the same 12px cell,
 * the words keep the same ink and the same 18px start, and the taller row is the one
 * a thumb has to hit. Two heights in one list is the honest reading — a target that
 * measured 18px would be the design lying about what can be pressed.
 */
function ActivityFileRow({
  id,
  diff,
}: {
  id: string;
  diff?: ActivityDiffEvidence;
}) {
  const [open, setOpen] = useState(false);
  if (!diff)
    return (
      <li className="flex min-w-0 items-center gap-1.5 py-0.5 pr-1 font-mono text-chip text-code-result">
        <span
          aria-hidden="true"
          className="w-3 shrink-0 text-center text-code-duration"
        >
          &rsaquo;
        </span>
        <ActivityPath id={id} />
      </li>
    );
  return (
    <li className="min-w-0">
      <Disclosure
        isOpen={open}
        tone="chronology"
        bleed
        aria-label={`${open ? "Collapse" : "Expand"} the diff of ${id}`}
        onClick={() => setOpen((wasOpen) => !wasOpen)}
      >
        <ActivityPath id={id} />
      </Disclosure>
      {open && <ActivityDiff diff={diff} />}
    </li>
  );
}

/**
 * WHAT THE STEP TOUCHED, one path per line, in the machine's own hand.
 *
 * ONE COLUMN, AND EVERY MARK STANDS IN IT. A path and the patch that path opens are
 * the same list, so both hang their mark in the same 12px cell at the row's own left
 * edge and start their words 18px in. The cell is `w-3` because that is the box
 * `Disclosure` gives its chevron; a guillemet left to its own 5px advance put every
 * path two pixels off the row above it. What the list HOLDS BACK stands in no such
 * column: a cut is a rule with the words in it (`LoadMore`), never a fifth mark.
 */
function ActivityFiles({
  resources,
  diffs = [],
}: {
  resources: ActivityResource[];
  diffs?: readonly ActivityDiffEvidence[];
}) {
  const [showAll, setShowAll] = useState(false);
  const rows = activityFileRows(resources, diffs);
  const hidden = Math.max(0, rows.length - ACTIVITY_FILES_SHOWN);
  const shown = showAll ? rows : rows.slice(0, ACTIVITY_FILES_SHOWN);
  return (
    <div className="min-w-0">
      <ul className="mt-1.5 grid min-w-0 gap-px">
        {shown.map((row) => (
          <ActivityFileRow key={row.key} id={row.id} diff={row.diff} />
        ))}
      </ul>
      {hidden > 0 && (
        <LoadMore
          label={showAll ? "Show fewer paths" : `Show ${hidden} more paths`}
          onClick={() => setShowAll((wasOpen) => !wasOpen)}
        >
          {showAll ? "show fewer files" : `show ${moreCount(hidden, "file")}`}
        </LoadMore>
      )}
    </div>
  );
}

/**
 * WHAT THE PATCH CHANGED, under the paths it changed.
 *
 * No head and no card. The row above already says "Patched", prints its own
 * `+7 -3` and carries the paths under it, so a bordered box with the word
 * "Patch" set in bold across its top was that row said a second time, twenty
 * pixels lower and louder.
 *
 * And a step that changed SEVERAL files hands each file its OWN fold, on its own
 * path. One chevron over eleven concatenated patches made the reader find a file
 * by reading a header out of the diff, and any bound on the payload spent itself
 * on whichever file came first. The bare word `Diff` survives for the one case
 * with no path to stand on: a `patch` row, whose head already names the file.
 */
function ActivityChanges({
  row,
  diffs,
  files,
}: {
  row: ActivityRow;
  diffs: ActivityDiffEvidence[];
  files: ActivityResource[];
}) {
  const [open, setOpen] = useState(false);
  const headline = activityStepHeadline(row);
  const alone = files.length === 0 && diffs.length === 1 ? diffs[0] : undefined;
  if (!alone) return <ActivityFiles resources={files} diffs={diffs} />;
  return (
    <div className="min-w-0">
      <Disclosure
        isOpen={open}
        tone="muted"
        bleed
        aria-label={`${open ? "Collapse" : "Expand"} the patch of ${headline}`}
        onClick={() => setOpen((wasOpen) => !wasOpen)}
      >
        <span className="min-w-0 flex-1 font-sans text-meta normal-case">
          Diff
        </span>
      </Disclosure>
      {open && <ActivityDiff diff={alone} />}
    </div>
  );
}

/**
 * AN ERROR, WHOLE, ALREADY ON THE PAGE.
 *
 * Everything else on this axis is a summary the reader may follow; an error is
 * the one thing they are never asked to go and find, and never the one thing
 * they are shown three lines of. The machine's own text is what says why the
 * step failed, so all of it is here - the engine already bounds it in bytes
 * where the event is built.
 */
function ActivityError({ evidence }: { evidence: ActivityTextEvidence }) {
  const lines = evidence.text.split("\n");
  return (
    <div className="mt-1.5 min-w-0 border border-err-edge bg-err-surface">
      {/* No head. The step above already said which operation failed, on what,
          and in which word; a card repeating all three is that row printed
          twice. What the reader came here for is the machine's own text, and a
          wrapped line hangs under its own first character so that one line of it
          still reads as one. */}
      <div className="grid min-w-0 px-2 py-1">
        {lines.map((line, index) => (
          <p
            key={`${index}-${line}`}
            className={`-indent-4 whitespace-pre-wrap break-words pl-4 font-mono text-chip ${
              index === 0 ? "text-err-ink" : "text-dialog-hint"
            }`}
          >
            {line || " "}
          </p>
        ))}
      </div>
    </div>
  );
}

/** True when a step's own summary only counts the paths already listed under it. */
function countsVisibleFiles(summary: string, shown: number): boolean {
  const match = /^(\d+) files?$/.exec(summary.trim());
  return match !== null && shown > 0 && Number(match[1]) === shown;
}

/**
 * ONE STEP ON THE AXIS: a verb, its object, and how long it took.
 *
 * Nothing here is a control. The row states what the engine did and leaves what
 * it produced underneath it, so once the invocation is open there is no second
 * thing to go and find: the program, the bytes it printed and this chronology
 * are all one chevron down from the band's counters.
 *
 * And it says each of those things ONCE. "Read 4 files" over four visible paths
 * counts a list the eye is already on, so a summary that is nothing but that
 * count gives way to the paths themselves. A running step prints no elapsed
 * time because this wire carries none — only a duration once the call lands —
 * so its column says "still counting" rather than standing empty, which reads
 * as a number that went missing.
 */
/**
 * A ROW'S OWN WORDS, READ THE WAY THE ENGINE DECLARED THEM.
 *
 * Text on this axis is LITERAL unless its producer marked it, because most of what a
 * step says is a path, a glob or a command: `probe_1.json` italicised at its
 * underscores, or a glob set in bold from its stars, is the panel corrupting the fact
 * it was handed. So the format travels per field, is never sniffed from the
 * characters, and the renderer is the transcript's own — one markdown vocabulary on
 * this surface, never a second one grown inside this panel.
 */
function ActivityText({
  text,
  format,
  block = false,
}: {
  text: string;
  format?: ActivityTextFormat;
  block?: boolean;
}) {
  if (!format) return <>{text}</>;
  if (format === "markdown" && block)
    return <Markdown compact>{text}</Markdown>;
  return <InlineMarkdown>{text}</InlineMarkdown>;
}

/**
 * ONE STEP, AND WHAT IT DID UNDER IT.
 *
 * A step that stands for several changes at once — a code block that wrote its own
 * files — keeps ONE mark on the chronology and hangs its changes under it, sharing
 * that mark's left edge and indented from it. Not a card, not a second rail: the
 * group is the indent, exactly as the terminal draws it.
 *
 * The depth is HARD. Three levels — the step, the change, the paths the change
 * touched — is the whole tree either surface will draw, because a fourth is a file
 * tree printed into a chronology and nothing on this axis is worth that.
 */
function ActivityStep({
  row,
  depth = 0,
}: {
  row: ActivityRow;
  depth?: number;
}) {
  const nested = depth > 0;
  const failed = row.state === "failed";
  const lead = activityStepLead(row);
  const summary = activityStepObject(row);
  const delta = activityStepDelta(row);
  const duration = formatActivityDuration(row.duration_ms);
  const children = nested
    ? []
    : [...(row.children ?? [])].sort(
        (left, right) => left.sequence - right.sequence,
      );
  const hasChildren = children.length > 0;
  const Headline = nested ? "p" : "h4";
  const diffs = row.evidence.filter(
    (item): item is ActivityDiffEvidence => item.kind === "diff",
  );
  const error = row.evidence.find(
    (item): item is ActivityTextEvidence => item.kind === "error",
  );
  // A failed step says WHY once: the machine's own text when it left one, and
  // the engine's summary line only when it did not. There is no pill — a framed
  // word beside a filled red mark, under a verb that already says "refused",
  // was the same fact spelled a third time.
  const outcome = failed
    ? error
      ? ""
      : (row.error_summary ?? "")
    : activityStepOutcome(row);
  const touched = row.resources.filter((resource) => resource.id !== summary);
  const object = countsVisibleFiles(
    summary,
    Math.min(touched.length, ACTIVITY_FILES_SHOWN),
  )
    ? ""
    : summary;

  return (
    <li
      data-activity-row={row.id}
      data-activity-depth={depth}
      className={
        nested
          ? "relative mb-1.5 min-w-0 last:mb-0"
          : "relative mb-4 min-w-0 pl-5 last:mb-0"
      }
    >
      {!nested && <ActivityNode state={row.state} />}
      <div className="grid min-w-0 grid-cols-[minmax(0,1fr)_auto] items-start gap-2">
        <Headline
          className={
            nested
              ? "min-w-0 font-sans text-meta font-medium text-code-result"
              : "min-w-0 font-sans text-ui font-bold text-code-result"
          }
        >
          {lead}
          {object && (
            <>
              {" "}
              <span className="ml-[5px] font-normal text-dialog-hint">
                <ActivityText text={object} format={row.summary_format} />
              </span>
            </>
          )}
          {delta.additions + delta.deletions > 0 && (
            <>
              {" "}
              <span className="ml-[5px] font-mono font-normal text-dialog-hint">
                +{delta.additions} &minus;{delta.deletions}
              </span>
            </>
          )}
        </Headline>
        {duration && (
          <time className="min-w-[38px] shrink-0 text-right font-mono text-chip text-code-duration">
            {duration}
          </time>
        )}
        {!duration && row.state === "running" && (
          <span
            aria-hidden="true"
            className="min-w-[38px] shrink-0 text-right font-mono text-chip text-code-duration"
          >
            &hellip;
          </span>
        )}
      </div>
      {outcome && (
        <div className="mt-0.5 font-sans text-meta text-dialog-hint">
          <ActivityText
            text={outcome}
            format={failed ? undefined : row.result_format}
            block
          />
        </div>
      )}
      {/* A GROUP'S PATHS BELONG TO ITS CHANGES, not to the group as well: the head
          carries every child's resource, so painting them here and again under each
          child is the same twelve paths printed twice. */}
      {diffs.length === 0 && !hasChildren && touched.length > 0 && (
        <ActivityFiles resources={touched} />
      )}
      {diffs.length > 0 && (
        <ActivityChanges
          row={row}
          diffs={diffs}
          files={hasChildren ? [] : touched}
        />
      )}
      {error && <ActivityError evidence={error} />}
      {hasChildren && (
        <ol data-activity-children={row.id} className="mt-1.5 min-w-0 pl-4.5">
          {children.map((child) => (
            <ActivityStep key={child.id} row={child} depth={depth + 1} />
          ))}
        </ol>
      )}
    </li>
  );
}

/**
 * THE CHRONOLOGY: the turn's own line, and a tick from it to every mark.
 *
 * It draws NO rail. The line the marks hang on is the one the turn already runs
 * down its whole column (`RAIL_LINE` in `ChatContent`), with the invocation's
 * own ring standing on it directly above — so a step is a branch off the call
 * that made it, and one line carries the eye from a turn's first call to its
 * last step. A second hairline of its own, eighteen pixels to the right, was two
 * timelines drawn for one chronology.
 *
 * Every mark reaches that line with a tick: a dot floating beside a line is a
 * bullet in a list, a dot JOINED to it is a moment on a timeline, and this axis
 * is the second thing. The padding is measured, not chosen — the ring sits 4px
 * left of the text column with an 8px tick (10px from `sm`), so 19px and 21px
 * land the tick's far end exactly on the turn's line at 7px.
 *
 * Nothing closes the group: a bracket, like a card around it, is a second border
 * saying what the named row above it already said.
 */
function ActivityThread({ activity }: { activity?: ActivityProjection }) {
  const rows = [...(activity?.rows ?? [])].sort(
    (left, right) => left.sequence - right.sequence,
  );
  const omitted = activity?.omitted.rows ?? 0;

  return (
    <ol
      aria-label="Invocation chronology"
      data-activity-chronology
      className="relative -ml-6 mb-0.5 min-w-0 pt-4 pb-4.5 pl-[19px] sm:pl-[21px]"
    >
      {rows.map((row) => (
        <ActivityStep key={row.id} row={row} />
      ))}
      {omitted > 0 && (
        <li className="relative min-w-0">
          <LoadMore label={moreCount(omitted, "step")}>
            {moreCount(omitted, "step")}
          </LoadMore>
        </li>
      )}
      {rows.length === 0 && omitted === 0 && (
        <li className="relative min-w-0 pl-5 font-sans text-meta text-dialog-hint">
          No operations yet
        </li>
      )}
    </ol>
  );
}

/**
 * ONE FORM'S ACTIVITY, HUNG UNDER THE INVOCATION THAT PRODUCED IT.
 *
 * No frame, no header and no state of its own: the row above it already names
 * the invocation, says how it ended and prints what it cost, and a second
 * bordered box repeating that inside the first is what made a turn read as a
 * stack of panels.
 *
 * It opens no live region and it SILENCES the one it sits in. The trace around
 * it is the form's one `status`, and a chronology inside a live region is
 * re-read from the top on every re-render — twelve steps announced again
 * because a thirteenth arrived. The row above says RUNNING and then how it
 * ended; that is the announcement, and this is the page it points at.
 */
export function ActivityPanel({ activity }: { activity?: ActivityProjection }) {
  return (
    <div className="min-w-0" aria-live="off" data-activity-axis>
      <ActivityThread activity={activity} />
    </div>
  );
}

import { useState } from "react";
import { Disclosure } from "./ui";
import type {
  ActivityDiffEvidence,
  ActivityProjection,
  ActivityResource,
  ActivityRow,
  ActivityTextEvidence,
} from "../lib/activity";

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

function activityRowLabel(row: ActivityRow): string {
  return [row.operation.toUpperCase(), activityRowSummary(row)]
    .filter(Boolean)
    .join(" · ");
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

function activityTotal(activity?: ActivityProjection): number {
  const counts = activity?.counts;
  return counts
    ? counts.running + counts.succeeded + counts.failed + counts.cancelled
    : (activity?.rows.length ?? 0);
}

/**
 * WHAT WAS CALLED, in the invocation's own name.
 *
 * `INVOCATION` is true of every band in the transcript and so names none of
 * them. What a reader wants from an iteration is the PROGRAM — `grep · cat ·
 * patch`, the tools as they were typed, in the order the engine ran them. The
 * axis below says in prose what each call DID; this line is the call itself, so
 * the two are not one sentence printed twice.
 *
 * Three names, then a tally: a fourth pushes the state word off a phone. The
 * tally counts the rows the engine's own bound dropped as well, because a call
 * nobody can see is still a call that happened.
 */
const CALL_NAMES_SHOWN = 3;

export function activityCallText(activity?: ActivityProjection): string {
  const rows = [...(activity?.rows ?? [])].sort(
    (left, right) => left.sequence - right.sequence,
  );
  const named = rows
    .slice(0, CALL_NAMES_SHOWN)
    .map((row) => row.operation.trim());
  const rest = rows.length - named.length + (activity?.omitted.rows ?? 0);
  return [named.join(" · "), rest > 0 ? `+${rest}` : ""]
    .filter(Boolean)
    .join(" ");
}

/** One counter in the margin: the words, and the tone that repeats them. */
export interface ActivityCostPart {
  readonly text: string;
  /** Empty for the margin's own ink — a count that needs no colour to be read. */
  readonly tone: string;
}

/**
 * WHAT THE ITERATION COST THE REPOSITORY, in the invocation's own margin.
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
 * HOW THE CALL ENDED AND HOW LONG IT TOOK — the two facts a band's NAME cannot carry.
 *
 * The invocation row is named by what it CALLED, and everything factual stands
 * in its own margin. v36 spends that margin on identity — session, turn,
 * — session, turn, iteration, baseline commit — and none of that is on this wire;
 * a state and an elapsed time are, so the margin says those instead. The elapsed
 * time is printed only once it is FINAL: a number that stops moving while the run
 * is still going is worse than no number.
 */
export function activityStateText(
  activity?: ActivityProjection,
  durationMs?: number,
): string {
  const state = activity?.state ?? "idle";
  const settled = state !== "running" && state !== "idle";
  return [
    state === "succeeded" ? "DONE" : state.toUpperCase(),
    settled ? formatActivityDuration(durationMs) : "",
  ]
    .filter(Boolean)
    .join(" · ");
}
/** The one honest sentence a unified execution trace can state at this moment. */
export function activityReceiptText(
  activity?: ActivityProjection,
  durationMs?: number,
): string {
  const state = activity?.state ?? "idle";
  const total = activityTotal(activity);
  if (state === "running" || state === "idle") {
    const row = activity?.rows.find(
      (candidate) => candidate.state === "running",
    );
    const focus = row ? activityRowLabel(row) : "running activity";
    return [
      "RUNNING",
      focus,
      total > 1 || (activity?.omitted.rows ?? 0) > 0 ? "and more" : "",
    ]
      .filter(Boolean)
      .join(" · ");
  }

  const terminal = activity?.counts
    ? activity.counts.succeeded +
      activity.counts.failed +
      activity.counts.cancelled
    : (activity?.rows.length ?? 0);
  const primary =
    (state === "failed" &&
      activity?.rows.find((candidate) => candidate.state === "failed")) ||
    activity?.rows[0];
  const preview = primary
    ? `${primary.operation.toUpperCase()}${terminal > 1 || (activity?.omitted.rows ?? 0) > 0 ? " and more" : ""}`
    : "";
  const label = state === "succeeded" ? "DONE" : state.toUpperCase();
  return [label, preview, formatActivityDuration(durationMs)]
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
        {diff.omitted_lines > 0 && (
          <span className="flex px-2 py-px text-code-duration">
            +{diff.omitted_lines} more lines
          </span>
        )}
      </div>
    </div>
  );
}

/** The mark a path wears in a list: its own extension, or what the engine called it. */
function resourceMark(resource: ActivityResource): string {
  const name = resource.id.split("/").pop() ?? "";
  const dot = name.lastIndexOf(".");
  return ((dot > 0 ? name.slice(dot + 1) : "") || resource.type)
    .slice(0, 4)
    .toUpperCase();
}

/**
 * THE INK OF A FILE MARK, borrowed from the ink the transcript already reads code in.
 *
 * A card of five paths is scanned by SHAPE before it is read, and the language is
 * what separates them — so the mark takes the transcript's own syntax hues rather
 * than a new palette invented for this one card. The app's other colour system is
 * not available here: hue on the sessions list means WHICH MACHINE, and a file
 * type wearing it would be reporting something it does not know.
 */
const RESOURCE_INKS: Record<string, string> = {
  ts: "text-code-syntax-number",
  tsx: "text-code-syntax-number",
  js: "text-code-syntax-number",
  jsx: "text-code-syntax-number",
  mjs: "text-code-syntax-number",
  clj: "text-code-syntax-special",
  cljs: "text-code-syntax-special",
  cljc: "text-code-syntax-special",
  bb: "text-code-syntax-special",
  edn: "text-code-syntax-special",
  css: "text-code-syntax-keyword",
  html: "text-code-syntax-keyword",
  py: "text-code-syntax-keyword",
  json: "text-code-syntax-string",
  yaml: "text-code-syntax-string",
  yml: "text-code-syntax-string",
  toml: "text-code-syntax-string",
  md: "text-code-syntax-comment",
};

function resourceInk(resource: ActivityResource): string {
  const name = resource.id.split("/").pop() ?? "";
  const dot = name.lastIndexOf(".");
  return (
    RESOURCE_INKS[dot > 0 ? name.slice(dot + 1).toLowerCase() : ""] ??
    "text-code-duration"
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
 * THE MARK ON THE BRANCH: a ring, a disc when the step failed, hollow for what
 * is not on the page at all.
 *
 * Nine pixels of the page with a dot in it — the same mark for every step,
 * because a chronology is read as a sequence and a per-state glyph turns it
 * into a legend the reader has to learn. Failure is the exception the eye must
 * not miss, so it fills; the tail the engine's bound dropped is the exception
 * that must not look like a step, so it empties and the axis still ends on a
 * mark rather than on a stray caption.
 *
 * The mark carries its own tick back to the rail. A dot floating beside a line
 * is a bullet in a list; a dot JOINED to it is a moment on a timeline, and this
 * axis is the second thing.
 */
function ActivityNode({ state }: { state: ActivityRow["state"] }) {
  const failed = state === "failed";
  const running = state === "running";
  const hollow = state === "idle";
  return (
    <span
      aria-hidden="true"
      className={`absolute -left-1 top-1 size-[9px] rounded-full border before:absolute before:top-[3px] before:right-full before:h-px before:w-[8px] before:bg-edge-strong before:content-[''] sm:before:w-[10px] ${
        failed
          ? "border-err-ink bg-err-ink"
          : running
            ? "border-accent-ink bg-page"
            : "border-dialog-hint bg-page"
      }`}
    >
      {!failed && !hollow && (
        <span
          className={`absolute inset-0.5 rounded-full ${
            running
              ? "bg-accent-ink motion-safe:animate-pulse"
              : "bg-dialog-hint"
          }`}
        />
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
 */
function ActivityPath({ id }: { id: string }) {
  const cut = id.lastIndexOf("/");
  const directory = cut < 0 ? "" : id.slice(0, cut + 1);
  const name = cut < 0 ? id : id.slice(cut + 1);
  return (
    <span className="flex min-w-0" data-path={id}>
      {directory && (
        <span className="truncate text-dialog-hint">{directory}</span>
      )}
      <span className="shrink-0">{name}</span>
    </span>
  );
}

/** WHAT THE STEP TOUCHED, one path per line, in the machine's own hand. */
function ActivityFiles({ resources }: { resources: ActivityResource[] }) {
  return (
    <ul className="mt-1.5 grid min-w-0 gap-px">
      {resources.map((resource) => (
        <li
          key={`${resource.type}:${resource.id}`}
          className="flex min-w-0 items-center gap-[7px] px-1 py-0.5 font-mono text-chip text-code-result"
        >
          <span aria-hidden="true" className="shrink-0 text-code-duration">
            &rsaquo;
          </span>
          <ActivityPath id={resource.id} />
        </li>
      ))}
    </ul>
  );
}

/**
 * WHAT THE PATCH COST THE REPOSITORY, before its text.
 *
 * A step that edited files answers "which files, and how much" on one line and
 * lists the paths under it; the diff itself is the only thing on this axis long
 * enough to be worth folding, so it is the only thing that opens. The mark
 * beside a path is its own extension in the page's neutral ink — hue on this
 * app means WHICH MACHINE, and a file type borrowing it would be reporting
 * something it does not know.
 */
function ActivityChanges({
  row,
  diff,
  files,
}: {
  row: ActivityRow;
  diff: ActivityDiffEvidence;
  files: ActivityResource[];
}) {
  const [open, setOpen] = useState(false);
  const headline = activityStepHeadline(row);
  return (
    <div className="mt-1.5 min-w-0 border border-code-edge bg-panel">
      <div className="border-b border-code-edge px-2">
        <Disclosure
          isOpen={open}
          tone="chronology"
          bleed
          className="w-full"
          aria-label={`${open ? "Collapse" : "Expand"} the patch of ${headline}`}
          onClick={() => setOpen((wasOpen) => !wasOpen)}
        >
          {/* The head names what OPENS, never what is already listed under it:
              the row above printed the file count and the totals, and a card
              printing them again is the same line twice, twenty pixels apart. */}
          <span className="min-w-0 flex-1 font-bold text-code-result">
            Patch
          </span>
        </Disclosure>
      </div>
      {files.length > 0 && (
        <div className="grid min-w-0 gap-px px-2 pt-1.5 pb-2">
          {files.map((resource) => (
            <span
              key={`${resource.type}:${resource.id}`}
              className="grid min-w-0 grid-cols-[2rem_minmax(0,1fr)] items-center gap-x-[7px] py-0.5"
            >
              <span
                aria-hidden="true"
                className={`flex h-4 items-center justify-center border border-code-edge bg-page font-mono text-chip font-bold ${resourceInk(resource)}`}
              >
                {resourceMark(resource)}
              </span>
              <span className="min-w-0 font-mono text-chip text-code-result">
                <ActivityPath id={resource.id} />
              </span>
            </span>
          ))}
        </div>
      )}
      {open && <ActivityDiff diff={diff} />}
    </div>
  );
}

const ERROR_PREVIEW_LINES = 3;

/**
 * THE HEAD OF AN ERROR, ALREADY ON THE PAGE.
 *
 * Everything else on this axis is a summary the reader may follow; an error is
 * the one thing they are never asked to go and find. It is CLAMPED all the
 * same: the first line says what went wrong, the rest is the machine repeating
 * itself, and forty lines of it pasted into a chronology is how an axis stops
 * being readable. The whole of it is in the raw result the invocation row
 * already opens.
 */
function ActivityError({ evidence }: { evidence: ActivityTextEvidence }) {
  const lines = evidence.text.split("\n");
  const shown = lines.slice(0, ERROR_PREVIEW_LINES);
  const hidden = lines.length - shown.length;
  return (
    <div className="mt-1.5 min-w-0 border border-err-edge bg-err-surface">
      {/* No head. The step above already said which operation failed, on what,
          and in which word; a card repeating all three is that row printed
          twice. What the reader came here for is the machine's own text, and a
          wrapped line hangs under its own first character so that three lines
          of it still count as three. */}
      <div className="grid min-w-0 px-2 py-1">
        {shown.map((line, index) => (
          <p
            key={`${index}-${line}`}
            className={`-indent-4 whitespace-pre-wrap break-words pl-4 font-mono text-chip ${
              index === 0 ? "text-err-ink" : "text-dialog-hint"
            }`}
          >
            {line || " "}
          </p>
        ))}
        {hidden > 0 && (
          <p className="font-mono text-chip text-err-ink">
            +{hidden} more lines
          </p>
        )}
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
 * it produced underneath it, so the reader never has to open anything to learn
 * what the iteration DID — the program and its raw result stay behind the
 * invocation's own disclosure, one level up.
 *
 * And it says each of those things ONCE. "Read 4 files" over four visible paths
 * counts a list the eye is already on, so a summary that is nothing but that
 * count gives way to the paths themselves. A running step prints no elapsed
 * time because this wire carries none — only a duration once the call lands —
 * so its column says "still counting" rather than standing empty, which reads
 * as a number that went missing.
 */
function ActivityStep({ row }: { row: ActivityRow }) {
  const failed = row.state === "failed";
  const lead = activityStepLead(row);
  const summary = activityStepObject(row);
  const delta = activityStepDelta(row);
  const duration = formatActivityDuration(row.duration_ms);
  const diff = row.evidence.find(
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
  const object = countsVisibleFiles(summary, touched.length) ? "" : summary;

  return (
    <li
      data-activity-row={row.id}
      className="relative mb-4 min-w-0 pl-5 last:mb-0"
    >
      <ActivityNode state={row.state} />
      <div className="grid min-w-0 grid-cols-[minmax(0,1fr)_auto] items-start gap-2">
        <h4 className="min-w-0 font-sans text-ui font-bold text-code-result">
          {lead}
          {object && (
            <>
              {" "}
              <span className="ml-[5px] font-normal text-dialog-hint">
                {object}
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
        </h4>
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
        <p className="mt-0.5 font-sans text-meta text-dialog-hint">{outcome}</p>
      )}
      {!diff && touched.length > 0 && <ActivityFiles resources={touched} />}
      {diff && <ActivityChanges row={row} diff={diff} files={touched} />}
      {error && <ActivityError evidence={error} />}
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
        <li className="relative min-w-0 pl-5 font-sans text-meta text-dialog-hint">
          <ActivityNode state="idle" />+{omitted} more
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

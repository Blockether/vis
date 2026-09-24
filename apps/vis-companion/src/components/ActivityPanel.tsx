import { createContext, useContext, useEffect, useState, type ReactNode } from 'react';
import { InlineMarkdown, Markdown, SyntaxCodeBlock } from './ChatContent';
import { BandLabel, BandTally, CopyChip, Disclosure, LoadMore } from './ui';
import type {
  ActivityDiffEvidence,
  ActivityProjection,
  ActivityResource,
  ActivityRow,
  ActivityContent,
  ActivitySection,
  ActivityTextEvidence,
  ActivityTextFormat,
} from '../lib/activity';
import {
  activityCopyText,
  activityHistoryCopyText,
  argumentGroups,
  operationGroups,
  mergeActivity,
  type OperationGroup,
} from '../lib/activity';
import { workspaceRelativePath } from '../lib/path';
import { useOpenPath } from '../lib/open-path';
import { useWorkspaceRoots } from '../lib/workspace-roots';

/** `2 more files`, `3 more steps` — what a rule holds back, counted and named. */
function moreCount(n: number, noun: string) {
  return `${n} more ${noun}${n === 1 ? '' : 's'}`;
}

/**
 * Activity is an inline, whole-replaced field of one form, distinct from an
 * independently addressed live view. Its inert steps share the turn spine, state their
 * verb/object/duration once, and leave produced paths, patches or errors below.
 */

/** Select the verb from running, settled, failed or cancelled state. */
const ACTIVITY_VERBS: Record<string, readonly [running: string, settled: string, failed: string]> =
  {
    grep: ['Searching', 'Searched', 'Search failed'],
    ls: ['Listing', 'Listed', 'List failed'],
    cat: ['Reading', 'Read', 'Read failed'],
    patch: ['Patching', 'Patched', 'Patch refused'],
    shell: ['Running', 'Ran', 'Command failed'],
    // What a code block did to the tree with its own hands. The change is already past
    // when it is reported, and its head says how much of the tree moved at once.
    change: ['Changing', 'Changed', 'Change failed'],
    mkdir: ['Creating', 'Created', 'Create failed'],
    write: ['Writing', 'Wrote', 'Write failed'],
    copy: ['Copying', 'Copied', 'Copy failed'],
    move: ['Moving', 'Moved', 'Move failed'],
    link: ['Linking', 'Linked', 'Link failed'],
    delete: ['Deleting', 'Deleted', 'Delete failed'],
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
    (row.presenter === 'shell' || row.operation.toLowerCase() === 'shell') &&
    summary.startsWith('running: ')
  ) {
    return summary.slice('running: '.length);
  }
  return summary.toLowerCase() === row.operation.trim().toLowerCase() ? '' : summary;
}

/** The step's own sentence: a verb the reader knows, then what it was applied to. */
function activityStepLead(row: ActivityRow): string {
  const verb = ACTIVITY_VERBS[row.operation.trim().toLowerCase()];
  if (!verb) return row.operation.trim();
  return verb[row.state === 'failed' ? 2 : row.state === 'succeeded' ? 1 : 0];
}

/** What one step reads as end to end: the verb, then the thing it was applied to. */
function activityStepHeadline(row: ActivityRow): string {
  if (row.presentation) return row.presentation.headline;
  return [activityStepLead(row), activityStepObject(row)].filter(Boolean).join(' · ');
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
export function activityCostParts(activity?: ActivityProjection): readonly ActivityCostPart[] {
  const dropped: Record<string, number> = activity?.omitted.by_classification ?? {};
  const rows = activity?.rows ?? [];
  const tally = (signal: string) =>
    rows.filter((row) => row.signal === signal).length + (dropped[signal] ?? 0);
  const noun = (amount: number, word: string) => `${amount} ${word}${amount === 1 ? '' : 's'}`;
  const observations = tally('observation');
  const checks = tally('verification');
  return [
    { text: noun(tally('mutation'), 'mutation'), tone: 'text-accent-ink' },
    ...(observations
      ? [
          {
            text: noun(observations, 'observation'),
            tone: 'text-code-syntax-keyword',
          },
        ]
      : []),
    ...(checks ? [{ text: noun(checks, 'check'), tone: '' }] : []),
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
 * NO STATE WORD AT ALL, not even for a step that failed: the mark on the thread
 * beside the row is a struck or crossed ring in its own ink, and `FAILED` in
 * front of the calls says it a second time while pushing the calls — the part a
 * reader actually scans for — off the front of the line. The word survives for a
 * screen reader alone, which cannot see the ring. The elapsed time is printed
 * only once it is FINAL: a number that stops moving while the run is still going
 * is worse than no number.
 */
export function activityReceiptText(activity?: ActivityProjection, durationMs?: number): string {
  const state = activity?.state ?? 'idle';
  const live = state === 'running' || state === 'idle';
  const rows = activity?.rows ?? [];
  const omitted = Math.max(0, activity?.omitted.rows ?? 0);
  const shown = rows.slice(0, 3);
  const left = rows.length - shown.length + omitted;
  const subject = rows.length === 1 && left === 0 ? activityStepObject(rows[0]) : '';
  const names = [...shown.map((row) => row.operation.toUpperCase()), subject]
    .filter(Boolean)
    .join(' · ');
  const calls = names
    ? `${names}${left > 0 ? ` + ${left} more` : ''}`
    : left > 0
      ? `${left} ${left === 1 ? 'activity' : 'activities'}`
      : live
        ? 'running activity'
        : '';
  return [calls, live ? '' : formatActivityDuration(durationMs)].filter(Boolean).join(' · ');
}

function diffLineInk(kind: ActivityDiffEvidence['lines'][number]['kind']): string {
  if (kind === 'addition') return 'bg-code-ok text-code-success';
  if (kind === 'deletion') return 'bg-code-err text-code-error';
  if (kind === 'header' || kind === 'hunk') return 'text-code-syntax-keyword';
  return 'text-code-foreground';
}

function diffLineMarker(kind: ActivityDiffEvidence['lines'][number]['kind']): string {
  if (kind === 'addition') return '+';
  if (kind === 'deletion') return '-';
  return ' ';
}

/** The patch a step left, in the transcript's own diff ink. */
function ActivityDiff({ diff }: { diff: ActivityDiffEvidence }) {
  return (
    <div
      className="max-w-full overflow-x-auto overscroll-x-contain bg-code py-1 font-mono text-meta"
      aria-label="Unified diff"
    >
      <div className="w-max min-w-full">
        {diff.lines
          .filter((line) => !line.is_redacted)
          .map((line, index) => (
            <span
              key={`${index}-${line.kind}-${line.text}`}
              className={`flex w-max min-w-full whitespace-pre px-2 py-px ${diffLineInk(line.kind)}`}
            >
              <span className="w-3 shrink-0 select-none text-center" aria-hidden="true">
                {diffLineMarker(line.kind)}
              </span>
              <span className="pr-3">{line.text || ' '}</span>
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
      item.kind === 'diff'
        ? {
            additions: total.additions + item.additions,
            deletions: total.deletions + item.deletions,
          }
        : total,
    { additions: 0, deletions: 0 },
  );
}

function readPathCaption(id: string, caption: string): { id: string; note: string } {
  return { id, note: caption.startsWith(id) ? caption.slice(id.length) : '' };
}

/**
 * A PATH THAT KEEPS ITS NAME.
 *
 * `truncate` eats the END of a string, which in a monorepo is the only part
 * that differs: eight rows of `src/com/blockether/vis/internal/…` are eight
 * identical rows. Truncate the directory first; truncate the file name only
 * when it exceeds the available width. Keep the complete path in the title.
 *
 * And the words are the path RELATIVE TO THE WORKSPACE, because `/Users/ana/vis/`
 * is on every row, is the same on every row, and is exactly the part `truncate`
 * keeps. The id underneath stays ABSOLUTE — it is what the engine called the file
 * and what a press opens — so only the reading is shortened, never the address.
 *
 * And a press DOES open it, on the machine that ran the step: this row is the only
 * place the transcript says where the work happened, and a path you have to retype
 * into an editor is the part the screen can simply do for you. The press stops at
 * the path, so a row that also expands a diff keeps that for the rest of its width —
 * the TUI resolves the same overlap the same way, path first. With no opener
 * published the path stays plain words and the row keeps every press it had.
 *
 * A caller that already shows its own words for the file — a table cell naming it
 * inside a listed directory — passes `label`; the press, the hover title and the
 * opener still carry the whole path.
 */
function ActivityPath({ id, label, note }: { id: string; label?: string; note?: string }) {
  const roots = useWorkspaceRoots();
  const openPath = useOpenPath();
  const shown = workspaceRelativePath(id, roots) || id;
  const cut = shown.lastIndexOf('/');
  const directory = cut < 0 ? '' : shown.slice(0, cut + 1);
  const name = cut < 0 ? shown : shown.slice(cut + 1);
  const words = label ? (
    <span className="max-w-full shrink-0 truncate">{label}</span>
  ) : (
    <>
      {directory && <span className="truncate text-dialog-hint">{directory}</span>}
      <span className="max-w-full shrink-0 truncate">{name}</span>
    </>
  );
  const press = (event: { preventDefault: () => void; stopPropagation: () => void }) => {
    event.preventDefault();
    event.stopPropagation();
    openPath?.(id);
  };
  const path = !openPath ? (
    <span className="flex min-w-0 max-w-full" data-path={id} title={id}>
      {words}
    </span>
  ) : (
    <span
      role="button"
      tabIndex={0}
      aria-label={`Open ${id}`}
      className="flex min-w-0 max-w-full cursor-pointer hover:underline"
      data-path={id}
      title={id}
      onClick={press}
      onKeyDown={(event) => {
        if (event.key === 'Enter' || event.key === ' ') press(event);
      }}
    >
      {words}
    </span>
  );
  return note ? (
    <span className="flex min-w-0 max-w-full">
      {path}
      <span className="shrink-0 whitespace-pre text-dialog-hint">{note}</span>
    </span>
  ) : (
    path
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
function ActivityFileRow({ id, diff }: { id: string; diff?: ActivityDiffEvidence }) {
  const [open, setOpen] = useState(false);
  if (!diff)
    return (
      <li className="flex min-w-0 items-center gap-1.5 py-0.5 pr-1 font-mono text-chip text-code-result">
        <span aria-hidden="true" className="w-3 shrink-0 text-center text-code-duration">
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
        aria-label={`${open ? 'Collapse' : 'Expand'} the diff of ${id}`}
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
          label={showAll ? 'Show fewer paths' : `Show ${hidden} more paths`}
          onClick={() => setShowAll((wasOpen) => !wasOpen)}
        >
          {showAll ? 'show fewer files' : `show ${moreCount(hidden, 'file')}`}
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
 * A single patch opens directly with its operation. Multi-file changes retain
 * independent file disclosures so one large diff does not hide the others.
 */
function ActivityChanges({
  diffs,
  files,
}: {
  diffs: ActivityDiffEvidence[];
  files: ActivityResource[];
}) {
  const alone = files.length === 0 && diffs.length === 1 ? diffs[0] : undefined;
  return alone ? <ActivityDiff diff={alone} /> : <ActivityFiles resources={files} diffs={diffs} />;
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
  const lines = evidence.text.split('\n');
  return (
    <div className="mt-1.5 min-w-0 border border-err-edge bg-err-surface">
      {/* No head. The step above already said which operation failed, on what,
          and in which word; a card repeating all three is that row printed
          twice. What the reader came here for is the machine's own text, and a
          wrapped line hangs under its own first character so that one line of it
          still reads as one. */}
      <div className="grid min-w-0 px-2 py-1">
        {/* Two inks, both measured on THIS tinted paper: the machine's first line
            wears the error's own red, the trace under it the message ink. --dialog-hint
            is a DIALOG ink and lands 4.4:1 here in both solarized palettes, under the
            4.5 a small line owes; the message ink reads 6.0:1 to 13.4:1. The size is
            meta, not chip: chip is a short tag beside other copy, and here the
            machine's text is the only copy the card has. */}
        {lines.map((line, index) => (
          <p
            key={`${index}-${line}`}
            className={`-indent-4 whitespace-pre-wrap break-words pl-4 font-mono text-meta ${
              index === 0 ? 'text-err-ink' : 'text-vis-message'
            }`}
          >
            {line || ' '}
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

/** One inert activity step: verb, object, duration and any produced evidence. */
/** Render fields literally unless their producer explicitly marks Markdown. */
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
  if (format === 'markdown' && block) return <Markdown compact>{text}</Markdown>;
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
/** The containing trace resolves only attachments belonging to this transcript. */
export const ActivityAttachmentContext = createContext<((id: string) => ReactNode) | null>(null);

/** Consecutive tables with the same schema share intrinsic column sizing, not data. */
function activityContentRuns(content: ActivityContent[]): ActivityContent[][] {
  const runs: ActivityContent[][] = [];
  for (const block of content) {
    const previous = runs.at(-1);
    const first = previous?.[0];
    if (
      previous &&
      block.type === 'table' &&
      first?.type === 'table' &&
      block.columns.length === first.columns.length &&
      block.columns.every((column, index) => column === first.columns[index])
    ) {
      previous.push(block);
    } else {
      runs.push([block]);
    }
  }
  return runs;
}

function ActivityBody({ content, running }: { content: ActivityContent[]; running: boolean }) {
  const attachment = useContext(ActivityAttachmentContext);
  return (
    <div className="grid min-w-0 gap-1" data-activity-content>
      {activityContentRuns(content).map((blocks, index) => {
        const block = blocks[0];
        switch (block.type) {
          case 'heading':
            return (
              <h5
                key={index}
                className={`text-meta font-bold text-code-result ${index > 0 ? 'mt-3' : 'mt-[var(--text-meta--line-height)]'}`}
              >
                {block.text}
              </h5>
            );
          case 'text':
            return (
              <p key={index} className="whitespace-pre-wrap break-words text-meta text-code-result">
                {block.text}
              </p>
            );
          case 'markdown':
            return (
              <Markdown key={index} compact nested headingLevel={5}>
                {block.text}
              </Markdown>
            );
          case 'code':
          case 'diff':
            return (
              <SyntaxCodeBlock
                key={index}
                value={block.text}
                language={block.type === 'diff' ? 'diff' : (block.language ?? 'text')}
                compact
                bare
                frameless
              />
            );
          case 'table':
            return (
              <div
                key={index}
                className="min-w-0 overflow-x-auto"
                role="group"
                aria-label="Activity table"
                tabIndex={0}
              >
                <table className="w-full text-left text-meta text-code-result">
                  {blocks.map(
                    (table, group) =>
                      table.type === 'table' && (
                        <tbody key={group}>
                          <tr>
                            {table.columns.map((column, at) => (
                              <th
                                key={at}
                                scope="col"
                                className={`border-b border-edge px-2 pb-1 align-top font-bold ${group > 0 ? 'pt-2' : 'pt-1'}`}
                              >
                                {column}
                              </th>
                            ))}
                          </tr>
                          {table.rows.map((row, at) => (
                            <tr key={at}>
                              {row.map((cell, col) => (
                                <td
                                  key={col}
                                  className="px-2 py-1 align-top whitespace-pre-wrap [overflow-wrap:anywhere]"
                                >
                                  {col === 0 && table.paths?.[at] ? (
                                    <ActivityPath id={table.paths[at]} label={cell} />
                                  ) : (
                                    cell
                                  )}
                                </td>
                              ))}
                            </tr>
                          ))}
                          {!table.rows.length && (
                            <tr>
                              <td colSpan={table.columns.length} className="px-2 py-1 text-dialog-hint">
                                No rows
                              </td>
                            </tr>
                          )}
                        </tbody>
                      ),
                  )}
                </table>
              </div>
            );
          case 'progress':
            return (
              <div key={index} className="text-meta text-dialog-hint">
                <p>
                  {block.label}
                  {block.total !== undefined
                    ? ` · ${block.value} / ${block.total}`
                    : running
                      ? ' · In progress'
                      : ' · Stopped'}
                </p>
                {(running || block.total !== undefined) && (
                  <progress
                    className="block h-1.5 w-full overflow-hidden rounded-none border-0 bg-edge accent-accent-ink [&::-webkit-progress-bar]:bg-edge [&::-webkit-progress-value]:bg-accent-ink [&::-moz-progress-bar]:bg-accent-ink motion-safe:indeterminate:animate-pulse"
                    aria-label={block.label}
                    value={block.value}
                    max={block.total}
                  />
                )}
              </div>
            );
          default:
            return (
              <div key={index} className="min-w-0">
                <p className="text-meta text-dialog-hint">{block.label}</p>
                {attachment?.(block.attachment_id) ?? (
                  <p className="text-meta text-dialog-hint">Attachment unavailable</p>
                )}
              </div>
            );
        }
      })}
    </div>
  );
}

function ActivitySectionView({
  section,
  running,
  className,
}: {
  section: ActivitySection;
  running: boolean;
  className: string;
}) {
  const [open, setOpen] = useState(false);
  const hasContent = section.content.length > 0;
  const label = (
    <span className="min-w-0 truncate" title={section.headline}>
      {section.headline}
    </span>
  );

  return (
    <section data-activity-section className={`min-w-0 pl-4.5 ${className}`}>
      <h5 className="min-w-0 text-meta font-bold text-code-result">
        {hasContent ? (
          <Disclosure
            isOpen={open}
            tone="execution"
            density="compact"
            inlineChevron
            className="min-w-0 max-w-full"
            onClick={() => setOpen(!open)}
          >
            {label}
          </Disclosure>
        ) : (
          label
        )}
      </h5>
      {section.summary && (
        <p
          data-activity-summary
          className="truncate text-meta text-dialog-hint"
          title={section.summary}
        >
          {section.summary_format === 'markdown' ? (
            <InlineMarkdown links>{section.summary}</InlineMarkdown>
          ) : (
            section.summary
          )}
        </p>
      )}
      {open && hasContent && <ActivityBody content={section.content} running={running} />}
    </section>
  );
}

function ActivityStep({
  row,
  depth = 0,
  onToggle,
}: {
  row: ActivityRow;
  depth?: number;
  onToggle?: (open: boolean) => void;
}) {
  const nested = depth > 0;
  const failed = row.state === 'failed';
  const running = row.state === 'running';
  // A step opens shut. Only work still in flight shows itself, because it is still
  // changing and has nowhere else to say so; a settled step — a failed one included —
  // waits for a reader who pressed it, and folds itself away again once it settles.
  const listing = row.operation === 'ls';
  const [toggled, setToggled] = useState<boolean | null>(null);
  const open = toggled ?? (!listing && running);
  const presentation = row.presentation;
  const lead = presentation?.headline ?? activityStepLead(row);
  const content = presentation?.content;
  const sections = presentation?.sections ?? [];
  const summary = presentation ? '' : activityStepObject(row);
  // The head says WHAT the step was about — the authored summary, failed or not. WHY it
  // failed belongs to the body, said once, under the head, to a reader who opened it.
  const caption = presentation?.summary ?? '';
  const linkedSummary = Boolean(caption) && presentation?.summary_format === 'markdown';
  const delta = activityStepDelta(row);
  const duration = formatActivityDuration(row.duration_ms);
  const children = nested ? [] : (row.children ?? []);
  const hasChildren = children.length > 0;
  const Headline = nested ? 'p' : 'h4';
  const diffs = row.evidence.filter((item): item is ActivityDiffEvidence => item.kind === 'diff');
  const error = row.evidence.find((item): item is ActivityTextEvidence => item.kind === 'error');
  // A failed step says WHY once: the machine's own text when it left one, and
  // the engine's summary line only when it did not. There is no pill — a framed
  // word beside a filled red mark, under a verb that already says "refused",
  // was the same fact spelled a third time.
  const outcome = !error ? (row.error_summary ?? '') : '';
  const touched = row.resources.filter(
    (resource) =>
      resource.id !== summary &&
      !(row.operation === 'patch' && resource.id === presentation?.summary) &&
      !['shell-handle', 'council-group', 'council-thread', 'council-entry'].includes(resource.type),
  );
  const object = countsVisibleFiles(summary, Math.min(touched.length, ACTIVITY_FILES_SHOWN))
    ? ''
    : summary;
  const showsOutcome = Boolean(outcome) && (!presentation || failed);
  const showsFiles = diffs.length === 0 && !hasChildren && touched.length > 0;
  // A chevron that opens onto nothing is a promise the row cannot keep, so a step
  // with no content, no outcome, no paths, no patch, no error and no grouped
  // changes wears none and answers no press.
  const openable =
    Boolean(content?.length) ||
    (listing && sections.length > 0) ||
    showsOutcome ||
    showsFiles ||
    diffs.length > 0 ||
    Boolean(error) ||
    hasChildren;

  const captionLabel = caption && (
    <>
      <span aria-hidden="true" className="text-dialog-hint">
        ·
      </span>
      <span
        data-activity-summary
        className="min-w-0 flex-1 truncate font-normal text-dialog-hint"
        title={caption}
      >
        {linkedSummary ? (
          <InlineMarkdown links>{caption}</InlineMarkdown>
        ) : row.operation === 'cat' ? (
          <ActivityPath {...readPathCaption(row.summary, caption)} />
        ) : row.operation === 'patch' ? (
          <ActivityPath id={caption} />
        ) : (
          caption
        )}
      </span>
    </>
  );
  const metadata = (
    <>
      {delta.additions + delta.deletions > 0 ? ' ' : null}
      {delta.additions + delta.deletions > 0 && (
        <span className="ml-[5px] font-mono font-normal text-dialog-hint">
          +{delta.additions} &minus;{delta.deletions}
        </span>
      )}
      {duration && (
        <time
          aria-label={`Duration ${duration}`}
          className="ml-auto shrink-0 font-normal text-code-duration"
        >
          {duration}
        </time>
      )}
      {!duration && running && (
        <span aria-label="Running" className="ml-auto shrink-0 font-normal text-code-duration">
          …
        </span>
      )}
    </>
  );
  // Successful file reads are identified by the filename, not a repeated "Read" label.
  // Keep authored labels and non-success states so failures and running work stay clear.
  const filenameLabel =
    row.operation === 'cat' &&
    row.state === 'succeeded' &&
    lead === 'Read' &&
    !linkedSummary &&
    Boolean(caption || object);
  const label = filenameLabel ? (
    <span
      data-activity-summary
      className="min-w-0 truncate font-normal"
      title={caption || object}
    >
      <ActivityPath {...readPathCaption(caption ? row.summary : object, caption)} />
    </span>
  ) : (
    <span className="min-w-0 truncate font-semibold" title={activityStepHeadline(row)}>
      {lead}
    </span>
  );
  const detail = filenameLabel ? metadata : (
    <span className="flex min-w-0 flex-1 items-baseline gap-x-2">
      {object ? ' ' : null}
      {object && (
        <span className="min-w-0 flex-1 truncate font-normal text-dialog-hint" title={object}>
          {row.operation === 'cat' || row.operation === 'patch' ? (
            <ActivityPath id={object} />
          ) : (
            <ActivityText text={object} format={row.summary_format} />
          )}
        </span>
      )}
      {!linkedSummary && captionLabel}
      {!linkedSummary && metadata}
    </span>
  );

  return (
    <li data-activity-row={row.id} data-activity-depth={depth} className="relative min-w-0">
      <div className="min-w-0">
        <Headline
          className={`min-w-0 text-ui text-code-result mouse:text-meta ${
            nested ? 'font-medium' : 'font-semibold'
          } ${linkedSummary ? 'flex items-baseline gap-x-2' : ''}`}
        >
          {openable ? (
            <Disclosure
              isOpen={open}
              tone="execution"
              density="compact"
              inlineChevron
              className={linkedSummary ? 'min-w-0 w-auto! max-w-[45%]' : 'min-w-0 max-w-full'}
              tally={detail}
              onClick={() => {
                setToggled(!open);
                onToggle?.(!open);
              }}
            >
              {label}
            </Disclosure>
          ) : (
            <div
              className={`flex min-h-6 items-center gap-x-2 ${linkedSummary ? 'min-w-0 max-w-[45%]' : ''}`}
            >
              {label}
              {detail}
            </div>
          )}
          {linkedSummary && captionLabel}
          {linkedSummary && metadata}
        </Headline>
      </div>
      {row.state === 'cancelled' && (
        <p className="pb-1 pl-3 text-meta text-dialog-hint">Cancelled</p>
      )}
      {open && content && content.length > 0 && (
        <ActivityBody content={content} running={running} />
      )}
      {(!listing || open) &&
        sections.map((section, index) => (
          <ActivitySectionView
            key={index}
            section={section}
            running={running}
            className={
              index === 0 && !(open && content?.length) ? 'mt-1' : 'mt-[var(--text-ui--line-height)]'
            }
          />
        ))}
      {open && showsOutcome && (
        <p className="whitespace-pre-wrap break-words text-meta text-err-ink">{outcome}</p>
      )}
      {/* A GROUP'S PATHS BELONG TO ITS CHANGES, not to the group as well: the head
          carries every child's resource, so painting them here and again under each
          child is the same twelve paths printed twice. */}
      {open && showsFiles && <ActivityFiles resources={touched} />}
      {open && diffs.length > 0 && (
        <ActivityChanges diffs={diffs} files={hasChildren ? [] : touched} />
      )}
      {open && error && <ActivityError evidence={error} />}
      {open && hasChildren && (
        <ol data-activity-children={row.id} className="min-w-0 pl-4.5">
          {children.map((child) => (
            <ActivityStep key={child.id} row={child} depth={depth + 1} />
          ))}
        </ol>
      )}
    </li>
  );
}

function groupFacts(rows: readonly ActivityRow[]): string {
  const files = new Set(
    rows.flatMap((row) =>
      row.resources.filter((resource) => resource.type === 'file').map((resource) => resource.id),
    ),
  );
  const complete = rows.every(
    (row) => !row.is_truncated && row.resources.some((resource) => resource.type === 'file'),
  );
  const deltas = rows.map(activityStepDelta);
  const additions = deltas.reduce((sum, delta) => sum + delta.additions, 0);
  const deletions = deltas.reduce((sum, delta) => sum + delta.deletions, 0);
  return [
    files.size
      ? `${files.size} ${complete ? '' : 'known '}${files.size === 1 ? 'file' : 'files'}`
      : '',
    additions + deletions ? `+${additions} −${deletions}` : '',
    ...(['running', 'failed', 'cancelled'] as const).flatMap((state) => {
      const count = rows.filter((row) => row.state === state).length;
      return count ? [`${count} ${state}`] : [];
    }),
  ]
    .filter(Boolean)
    .join(' · ');
}

/** Coalesce read presentations, never the invocations used by counts, history or copy. */
function mergeReadRows(rows: readonly ActivityRow[]): ActivityRow[] {
  const groups: ActivityRow[][] = [];
  const byTarget = new Map<string, ActivityRow[]>();
  for (const row of rows) {
    const presentation = row.presentation;
    const key =
      row.operation === 'cat' &&
      row.state === 'succeeded' &&
      presentation?.headline === 'Read' &&
      presentation.content.every((block) => block.type === 'code') &&
      !presentation.sections?.length &&
      !row.children?.length &&
      !row.error_summary &&
      !row.evidence.some((item) => item.kind === 'error' || item.kind === 'diff')
        ? row.read_key
        : undefined;
    const existing = key ? byTarget.get(key) : undefined;
    if (existing) existing.push(row);
    else {
      const group = [row];
      groups.push(group);
      if (key) byTarget.set(key, group);
    }
  }
  return groups.map((reads) => {
    const first = reads[0];
    if (reads.length === 1) return first;
    const summaries = reads.map((row) => row.presentation!.summary);
    const ranges = summaries.map((summary) => /^(.*) · lines (\d+–\d+)$/.exec(summary));
    const summary = ranges.every((range) => range !== null)
      ? `${ranges[0]![1]} · lines ${[...new Set(ranges.map((range) => range![2]))].join(', ')}`
      : [...new Set(summaries)].join('; ');
    return {
      ...first,
      argument_key: undefined,
      duration_ms: reads.every((row) => row.duration_ms !== undefined)
        ? reads.reduce((sum, row) => sum + row.duration_ms!, 0)
        : undefined,
      resources: [
        ...new Map(
          reads
            .flatMap((row) => row.resources)
            .map((resource) => [JSON.stringify([resource.type, resource.id]), resource]),
        ).values(),
      ],
      evidence: reads.flatMap((row) => row.evidence),
      is_truncated: reads.some((row) => row.is_truncated),
      presentation: {
        ...first.presentation!,
        summary,
        // Keep separate excerpts, including overlaps and changed snapshots.
        content: reads.flatMap((row) => row.presentation!.content),
      },
    };
  });
}

/** Combine same-file patch previews, keeping the original invocations available for history. */
function mergePatchRows(rows: readonly ActivityRow[]): ActivityRow[] {
  const groups: ActivityRow[][] = [];
  const byTarget = new Map<string, ActivityRow[]>();
  for (const row of rows) {
    const diffs = row.evidence.filter((item): item is ActivityDiffEvidence => item.kind === 'diff');
    const presentation = row.presentation;
    const target = diffs.length === 1 ? diffs[0].text : '';
    const eligible =
      row.operation === 'patch' &&
      row.state === 'succeeded' &&
      target.trim() &&
      !['diff', '[REDACTED]'].includes(target) &&
      !row.children?.length &&
      !row.error_summary &&
      !row.evidence.some((item) => !['diff', 'arguments', 'result'].includes(item.kind)) &&
      presentation?.headline === 'Patched' &&
      !presentation.content.length &&
      !presentation.sections?.length;
    const existing = eligible ? byTarget.get(target) : undefined;
    if (existing) existing.push(row);
    else {
      const members = [row];
      groups.push(members);
      if (eligible) byTarget.set(target, members);
    }
  }
  return groups.map((patches) => {
    const first = patches[0];
    if (patches.length === 1) return first;
    const diffs = patches.flatMap((row) =>
      row.evidence.filter((item): item is ActivityDiffEvidence => item.kind === 'diff'),
    );
    const mergedDiff: ActivityDiffEvidence = {
      ...diffs[0],
      lines: diffs.flatMap((diff) => diff.lines),
      additions: diffs.reduce((sum, diff) => sum + diff.additions, 0),
      deletions: diffs.reduce((sum, diff) => sum + diff.deletions, 0),
      modifications: diffs.reduce((sum, diff) => sum + diff.modifications, 0),
      is_truncated: diffs.some((diff) => diff.is_truncated),
      is_redacted: diffs.some((diff) => diff.is_redacted),
    };
    return {
      ...first,
      argument_key: undefined,
      duration_ms: patches.every((row) => row.duration_ms !== undefined)
        ? patches.reduce((sum, row) => sum + row.duration_ms!, 0)
        : undefined,
      resources: [
        ...new Map(
          patches
            .flatMap((row) => row.resources)
            .map((resource) => [JSON.stringify([resource.type, resource.id]), resource]),
        ).values(),
      ],
      evidence: [
        ...patches.flatMap((row) => row.evidence.filter((item) => item.kind !== 'diff')),
        mergedDiff,
      ],
      is_truncated: patches.some((row) => row.is_truncated),
    };
  });
}

function ActivityGroup({
  group,
  repeated = false,
  onStepToggle,
}: {
  group: OperationGroup;
  repeated?: boolean;
  onStepToggle?: (open: boolean) => void;
}) {
  const singleton = group.rows.length === 1;
  // EVERY GROUP OPENS SHUT. A panel that opens half unfolded — these rows showing, their
  // neighbours counted — hands the reader a shape nobody chose; it is only the order the rows
  // happened to arrive in. A lone row has no head of its own to press, so it carries the press
  // it did get into the head it grows: what someone chose to read never folds away under them.
  const [open, setOpen] = useState(false);
  const remember = singleton
    ? (value: boolean) => {
        setOpen(value);
        onStepToggle?.(value);
      }
    : undefined;
  const expanded = singleton || open;
  const title = `${group.label} ×${group.rows.length}`;
  const facts = groupFacts(group.rows);
  // The counts ride in the disclosure's tally, after the chevron, so the mark that opens
  // this group stands beside the group's own name and keeps its secondary ink.
  const factsTally = facts && (
    <span className="ml-auto min-w-0 break-words text-right text-meta text-dialog-hint">
      {facts}
    </span>
  );
  // Even live steps stay folded: the group's tally already reports running work.
  return (
    <li
      className="min-w-0"
      data-activity-group={!singleton && !repeated ? group.id : undefined}
      data-activity-arguments={!singleton && repeated ? group.id : undefined}
    >
      {!singleton && (
        <Disclosure
          tone="execution"
          density="compact"
          inlineChevron
          isOpen={open}
          tally={factsTally}
          onClick={() => setOpen((value) => !value)}
        >
          <span className="min-w-0 break-words font-semibold">{title}</span>
        </Disclosure>
      )}
      {expanded && (
        <ol
          className={singleton ? 'min-w-0' : 'min-w-0 pl-3'}
          aria-label={singleton ? undefined : `${title} operations`}
        >
          {repeated
            ? group.rows.map((row) => <ActivityStep key={row.id} row={row} onToggle={remember} />)
            : argumentGroups(mergePatchRows(mergeReadRows(group.rows))).map((argumentsGroup) => (
                <ActivityGroup
                  key={argumentsGroup.id}
                  group={{
                    ...argumentsGroup,
                    label: activityStepObject(argumentsGroup.rows[0]) || group.label,
                  }}
                  onStepToggle={remember}
                  repeated
                />
              ))}
        </ol>
      )}
    </li>
  );
}

/** Show every operation group; only individual group contents are disclosed. */
function ActivityThread({ activity }: { activity?: ActivityProjection }) {
  const groups = operationGroups(activity?.rows ?? []);
  const omitted = activity?.omitted.rows ?? 0;
  return (
    <ol aria-label="Operation groups" data-activity-chronology className="min-w-0 pb-1">
      {groups.map((group) => (
        <ActivityGroup key={group.id} group={group} />
      ))}
      {omitted > 0 && (
        <li className="text-meta text-dialog-hint">
          {omitted} {omitted === 1 ? 'step' : 'steps'} omitted · Activity limit
        </li>
      )}
    </ol>
  );
}

export interface ActivityHistorySource {
  load: (
    id: string,
    after: number,
    query: string,
    signal: AbortSignal,
  ) => Promise<ActivityProjection>;
}

/** SessionScreen owns authenticated retrieval; stories replace only this boundary. */
export const ActivityHistoryContext = createContext<ActivityHistorySource | null>(null);

function operationCount(activity: ActivityProjection): number {
  return Math.max(
    activity.history?.total ?? 0,
    activity.rows.length + activity.omitted.rows,
    Object.values(activity.counts).reduce((sum, count) => sum + count, 0),
  );
}

/** One source, read whole: the band holds every retained operation, never a page of them. */
function ActivityHistoryThread({
  activities,
  historyKey,
  isOpen,
}: {
  activities: ActivityProjection[];
  historyKey: string;
  isOpen: boolean;
}) {
  const source = useContext(ActivityHistoryContext);
  const [loaded, setLoaded] = useState<Array<ActivityProjection | undefined>>([]);
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState('');
  const [attempt, setAttempt] = useState(0);
  // A retained history only ever grows, so a live revision re-reads the tail rather than
  // blanking the operations already on screen; another source does invalidate them (#233).
  const sourceKey = activities.map((activity) => activity.history?.id ?? 'inline').join(':');
  const [seenSourceKey, setSeenSourceKey] = useState(sourceKey);
  if (seenSourceKey !== sourceKey) {
    setSeenSourceKey(sourceKey);
    setLoaded([]);
    setBusy(false);
    setError('');
  }
  const pages = activities.map((activity, index) => loaded[index] ?? activity);
  const page = mergeActivity(pages);
  // What the source still holds back, and what this screen is still short of.
  const incomplete = activities.some(
    (activity) =>
      activity.history && (activity.history.after > 0 || activity.history.next_after !== null),
  );
  const pending = pages.some((page) => page.history?.next_after != null);
  const read = async (index: number, signal: AbortSignal) => {
    const start = activities[index].history;
    if (!source || !start || (start.after === 0 && start.next_after === null)) return;
    // A receipt that already starts at the head keeps its rows; any other window is re-read.
    let rows: ActivityProjection['rows'] = start.after === 0 ? activities[index].rows : [];
    let after = start.after === 0 ? start.next_after : 0;
    let history = start;
    while (after !== null) {
      const result = await source.load(start.id, after, '', signal);
      if (signal.aborted) return;
      const next = result.history;
      if (
        next?.id !== start.id ||
        next.after !== after ||
        (next.next_after !== null && next.next_after <= after)
      ) {
        throw new Error('Activity changed. Reload operations to view its latest history.');
      }
      rows = [...rows, ...result.rows];
      history = next;
      after = next.next_after;
    }
    // One commit for the whole walk: a live re-read must never shorten what is on screen.
    const complete = { ...activities[index], rows, history: { ...history, after: 0 } };
    setLoaded((current) => {
      const merged = [...current];
      merged[index] = complete;
      return merged;
    });
  };
  useEffect(() => {
    if (!isOpen || !source || !incomplete) return;
    const controller = new AbortController();
    setBusy(true);
    setError('');
    void (async () => {
      try {
        await Promise.all(activities.map((_, index) => read(index, controller.signal)));
      } catch (cause) {
        if (!controller.signal.aborted)
          setError(cause instanceof Error ? cause.message : 'Activity could not be loaded.');
      } finally {
        if (!controller.signal.aborted) setBusy(false);
      }
    })();
    return () => controller.abort();
  }, [attempt, historyKey, incomplete, isOpen, source]);
  return (
    <div className="min-w-0" aria-busy={busy}>
      <ActivityThread activity={page} />
      {page.rows.length === 0 && !busy && (
        <p className="pb-2 text-ui text-dialog-hint">No operations available.</p>
      )}
      {busy && pending && (
        <p role="status" className="pb-2 text-ui text-dialog-hint">
          Loading operations…
        </p>
      )}
      {error && (
        <>
          <p role="alert" className="pb-2 text-ui text-err-ink">
            {error}
          </p>
          <LoadMore label="Reload operations" onClick={() => setAttempt((count) => count + 1)}>
            Reload operations
          </LoadMore>
        </>
      )}
      {incomplete && !source && (
        <p className="pb-2 text-ui text-dialog-hint">Reconnect to load every operation.</p>
      )}
    </div>
  );
}

/** Joined execution band. Hiding it retains disclosure state and silences live re-announcements. */
export function ActivityPanel({
  activity: input,
}: {
  activity?: ActivityProjection | ActivityProjection[];
}) {
  const [open, setOpen] = useState(false);
  const [copyError, setCopyError] = useState('');
  const source = useContext(ActivityHistoryContext);
  const activities = Array.isArray(input) ? input : input ? [input] : [];
  const activity = mergeActivity(activities);
  const total = activities.reduce((sum, activity) => sum + operationCount(activity), 0);
  const hasHistory = activities.some((activity) => activity.history);
  const historyKey = activities
    .map((activity) =>
      activity.history ? `${activity.history.id}:${activity.history.revision}` : 'inline',
    )
    .join(':');
  if (!total) return null;
  const summary = `${total} ${total === 1 ? 'operation' : 'operations'}`;
  const states = (['running', 'failed', 'cancelled'] as const).flatMap((state) =>
    activity.counts[state] ? [`${activity.counts[state]} ${state}`] : [],
  );
  return (
    <section className="isolate min-w-0" aria-live="off" data-activity-axis>
      <div className="flex min-w-0 items-center gap-2">
        <Disclosure
          className="min-w-0 flex-1"
          tone="execution"
          inlineChevron
          tally={
            <BandTally placement="trailing">
              {[
                summary,
                ...states,
                !open && activity.omitted.rows ? `${activity.omitted.rows} omitted` : '',
              ]
                .filter(Boolean)
                .join(' · ')}
            </BandTally>
          }
          isOpen={open}
          aria-label={open ? 'Collapse Activity' : 'Expand Activity'}
          onClick={() => setOpen((value) => !value)}
        >
          <BandLabel className="shrink-0">ACTIVITY</BandLabel>
        </Disclosure>
        <CopyChip
          key={historyKey}
          value={
            activities.some(
              (activity) =>
                activity.history &&
                (activity.history.after > 0 || activity.history.next_after !== null),
            )
              ? (signal) =>
                  activityHistoryCopyText(
                    activities,
                    async (id, after, query, signal) => {
                      if (!source)
                        throw new Error('Reconnect to copy the complete activity history.');
                      return source.load(id, after, query, signal);
                    },
                    signal,
                  )
              : activityCopyText(activity)
          }
          label="Copy activity"
          onError={setCopyError}
          density="compact"
          edge
        />
      </div>
      {copyError && (
        <p role="alert" className="pb-2 text-ui text-err-ink">
          {copyError} Try again.
        </p>
      )}
      <div hidden={!open}>
        {hasHistory ? (
          <ActivityHistoryThread historyKey={historyKey} activities={activities} isOpen={open} />
        ) : (
          <ActivityThread activity={activity} />
        )}
      </div>
    </section>
  );
}

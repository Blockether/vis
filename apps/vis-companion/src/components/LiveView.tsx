import { startTransition, useEffect, useMemo, useRef, useState } from 'react';
import {
  Button,
  BandLabel,
  Disclosure,
  Input,
  LoadMore,
  Meter,
  PROSE,
  TableSelectionButton,
  TableSelectionRow,
} from './ui';
import { InlineMarkdown } from './ChatContent';
import {
  ArrowOutIcon,
  CircleAlertIcon,
  CircleCheckIcon,
  CircleDashedIcon,
  CircleDotIcon,
  CircleSlashIcon,
  CircleXIcon,
  MARK_NUDGE,
} from './icons';
import type { GatewayClient } from '../lib/gateway';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import type { SseEvent } from '../lib/types';
import { VIEW_CLOSE_EVENT, VIEW_PATCH_EVENT } from '../lib/view';
import {
  applyLiveViewEvent,
  LIVE_NOTE_CHARS,
  isLiveViewEvent,
  liveFraction,
  livePercent,
  orderedRows,
  type ActivityProjection,
  type LiveLinkNode,
  type LiveLogNode,
  type LiveLogPage,
  type LiveNode,
  type LiveProgressNode,
  type LiveStatNode,
  type LiveStatusNode,
  type LiveStepsNode,
  type LiveRow,
  type LiveTableNode,
  type LiveTone,
  type LiveView as LiveViewModel,
} from '../lib/live-view';

/**
 * A run SHOWING its work, on the session it is running in.
 *
 * The other half of human input is a QUESTION: it blocks, it takes the screen,
 * it wants an answer. This one wants nothing. A scan sweeping a fleet, a build
 * draining a log, a table filling in — the operator watches it, or does not,
 * and the run finishes either way. So it is a panel in the session's own
 * column, never a dialog: a scrim over a screen for something nobody has to
 * answer is the app taking a hostage.
 *
 * It paints the same picture the terminal pane paints, node for node, because
 * both fold the same patches through the same rules (`lib/live-view`). What
 * differs is only what each surface is good at: the terminal scrolls one band
 * with a keyboard, the phone scrolls the page it already sits in.
 *
 * An ordinary view LEAVES when it ends. Activity first overlays the close
 * event's terminal picture through the record-filing handoff, so the operator
 * never sees stale running counts. The record keeps every line, the model is
 * handed the closing picture as data, and the settled row returns to the
 * transcript.
 */

/** One ink per tone. `idle` is the screen's ordinary ink: nothing is wrong with it. */
const TONE_INK: Record<LiveTone, string> = {
  idle: 'text-white',
  running: 'text-accent-ink',
  ok: 'text-ok',
  warn: 'text-warn',
  error: 'text-err',
};

/**
 * What a node with nothing in it SAYS. The engine's own sentences
 * (`view.materializer/empty-line`), so the phone, the terminal and the document
 * report an empty table with one wording rather than three.
 */
const EMPTY_LINE = {
  stat: 'nothing counted yet',
  steps: 'no steps yet',
  log: 'no output yet',
  table: 'no rows yet',
  link: 'no links',
} as const;

/** How many earlier lines one press of `Load earlier` reads out of the record. */
const LOG_PAGE = 200;

/**
 * The node's name, in the label voice the dialog's fields already use.
 *
 * CAPS CARRY THE NAME, and only the name. A live label often says what it is a
 * label OF — `Failure · vis-agent + vis-contract (PyPI packages)`, `Timeline ·
 * macos-latest` — and setting the whole line in caps shouts the very part that
 * has to be read: caps strip the ascenders and descenders a word is recognised
 * by, so a long tail stops being scannable and starts competing with the rows it
 * introduces. Everything up to the first `·` is the name; what follows is
 * ordinary type at the same size and colour.
 */
function NodeLabel({ children }: { children: string }) {
  const [name, ...rest] = children.split(' · ');
  return (
    <span className="block font-mono text-meta text-dialog-hint">
      <span className="uppercase tracking-[0.08em]">{name}</span>
      {rest.length > 0 && <span> · {rest.join(' · ')}</span>}
    </span>
  );
}

function Empty({ children }: { children: string }) {
  return <p className="font-mono text-meta italic text-dialog-hint">{children}</p>;
}

/**
 * The tone, as a SHAPE. A tone arrives as a word and used to leave as colour
 * alone — one dot, five inks — and colour alone is not a state: `ok` green
 * measures 2.9:1 on the light theme's panel, under the 3:1 a mark owes, and the
 * readers who cannot separate it from the amber beside it are a twentieth of
 * everyone. One ring with five interiors says WHICH state; the colour agrees.
 */
const TONE_MARK: Record<LiveTone, typeof CircleDotIcon> = {
  idle: CircleDashedIcon,
  running: CircleDotIcon,
  ok: CircleCheckIcon,
  warn: CircleAlertIcon,
  error: CircleXIcon,
};

function ToneMark({ tone, className = 'size-3' }: { tone: LiveTone; className?: string }) {
  const Drawn = TONE_MARK[tone];
  return <Drawn className={`${className} ${MARK_NUDGE} ${TONE_INK[tone]}`} />;
}

/**
 * The MARK says the state; the sentence beside it keeps the panel's ordinary ink.
 *
 * Colouring every line by its tone painted a green row for every job that merely
 * passed — five inks down one column, and the row actually doing something no
 * longer stood out. Only a FAILURE speaks in colour, because it is why the reader
 * opened the view.
 */
function rowInk(tone: LiveTone): string {
  return tone === 'error' ? TONE_INK.error : '';
}

/** A phase that has not run yet is not part of the picture, so it steps back. */
function stepInk(tone: LiveTone): string {
  return tone === 'idle' ? 'text-dialog-hint' : rowInk(tone);
}

/**
 * WHETHER THE PICTURE IS STILL BEING WRITTEN, in one word.
 *
 * A spinner says "something is happening" and goes on saying it for ninety
 * minutes; what the reader wants from the head is whether this run is still live
 * and, once it is not, how it ended. One word carries both, and it stops moving
 * when the run does — which is also the motion the panel owes a phone.
 */
function ViewState({ view, isSettled }: { view: LiveViewModel; isSettled: boolean }) {
  const failed = view.nodes.some((node) => node.type === 'status' && node.tone === 'error');
  const [word, paint] = !isSettled
    ? (['Live', 'bg-accent text-accent-foreground'] as const)
    : failed
      ? (['Failed', 'bg-err/15 text-err'] as const)
      : (['Done', 'bg-hover text-dialog-hint'] as const);
  return (
    <span
      className={`shrink-0 px-1.5 py-0.5 font-mono text-meta font-bold uppercase tracking-[0.08em] ${paint}`}
    >
      {word}
    </span>
  );
}

/**
 * The headline owns the whole column and its detail sits UNDER it. Sharing one
 * line cost the headline every character the detail claimed: on a phone, "1 of 2
 * jobs finished" beside a workflow and a job name broke to one word per line.
 */
function StatusRow({ node }: { node: LiveStatusNode }) {
  return (
    <div className="grid grid-cols-[auto_minmax(0,1fr)] items-baseline gap-x-2 gap-y-1 font-mono text-title">
      <ToneMark tone={node.tone} className="size-3.5" />
      <p className={`min-w-0 ${PROSE} ${TONE_INK[node.tone]}`}>
        <InlineMarkdown>{node.text}</InlineMarkdown>
      </p>
      {node.detail && (
        <p className="col-start-2 min-w-0 text-meta text-dialog-hint">
          <InlineMarkdown>{node.detail}</InlineMarkdown>
        </p>
      )}
    </div>
  );
}

/**
 * A bar only when there is a fraction to draw. `done of total` is stated in
 * words beside it because a bar answers "how far", never "how many", and a scan
 * of 3 hosts and a scan of 3000 draw the same third of a bar.
 */
function ProgressRow({ node }: { node: LiveProgressNode }) {
  const fraction = liveFraction(node);
  const counted =
    node.done !== undefined && node.total !== undefined ? `${node.done}/${node.total}` : null;
  return (
    <div className="min-w-0">
      {fraction === null ? (
        <p className="font-mono text-ui text-dialog-hint">
          <span className="italic">working</span>
          {counted && <span className="ml-2 text-meta">{counted}</span>}
        </p>
      ) : (
        <>
          <Meter value={fraction} label={node.label ?? 'Progress'} />
          <p className="mt-1.5 flex items-baseline gap-2 font-mono text-meta text-dialog-hint">
            <span className="text-ui font-bold tabular-nums text-white">{livePercent(fraction)}%</span>
            {counted && <span className="tabular-nums">{counted}</span>}
          </p>
        </>
      )}
    </div>
  );
}

/** Counters that keep their slot as their numbers move. */
function StatRow({ node }: { node: LiveStatNode }) {
  if (node.stats.length === 0) return <Empty>{EMPTY_LINE.stat}</Empty>;
  return (
    <dl className="flex flex-wrap items-baseline gap-x-4 gap-y-1 font-mono">
      {node.stats.map((stat) => (
        <div key={stat.id} className="flex items-baseline gap-1.5">
          <dt className="text-meta text-dialog-hint">
            <InlineMarkdown>{stat.label}</InlineMarkdown>
          </dt>
          <dd className={`text-ui font-bold ${TONE_INK[stat.tone]}`}>{stat.value_text}</dd>
        </div>
      ))}
    </dl>
  );
}

function StepsRows({ node }: { node: LiveStepsNode }) {
  if (node.steps.length === 0) return <Empty>{EMPTY_LINE.steps}</Empty>;
  return (
    <ul className="space-y-1.5 font-mono text-ui">
      {node.steps.map((step) => (
        <li key={step.id} className="flex min-w-0 items-baseline gap-2">
          <ToneMark tone={step.tone} />
          <span className={`min-w-0 flex-1 truncate ${stepInk(step.tone)}`}>
            <InlineMarkdown>{step.label}</InlineMarkdown>
          </span>
          {step.value && <span className="shrink-0 font-bold tabular-nums text-white">{step.value}</span>}
          {step.detail && (
            <span className="shrink-0 text-meta text-dialog-hint">
              <InlineMarkdown>{step.detail}</InlineMarkdown>
            </span>
          )}
        </li>
      ))}
    </ul>
  );
}

/**
 * Output as it arrives, and a way BACK past it.
 *
 * The node carries a window; the record behind it can be a hundred thousand
 * lines, which is precisely what a phone must not hold. So the walk back is a
 * page at a time out of the record itself, and when the window has slid on
 * while the operator was reading, the hole between what was fetched and what is
 * live is STATED rather than closed over.
 */
function LogRows({
  node,
  load,
}: {
  node: LiveLogNode;
  load?: (from: number, limit: number) => Promise<LiveLogPage>;
}) {
  const [earlier, setEarlier] = useState<{ from: number; lines: string[] } | null>(null);
  const [isReading, setIsReading] = useState(false);

  const windowStart = Math.max(0, node.total_lines - node.lines.length);
  const knownFrom = earlier ? earlier.from : windowStart;
  const hole = earlier ? Math.max(0, windowStart - (earlier.from + earlier.lines.length)) : 0;

  const readEarlier = () => {
    if (!load || isReading) return;
    const from = Math.max(0, knownFrom - LOG_PAGE);
    const limit = knownFrom - from;
    if (limit <= 0) return;
    setIsReading(true);
    load(from, limit)
      .then((page) => {
        setEarlier((current) => ({
          from: page.from,
          lines: [...page.lines, ...(current ? current.lines : [])],
        }));
      })
      .catch(() => undefined)
      .finally(() => setIsReading(false));
  };

  if (node.lines.length === 0 && !earlier) return <Empty>{EMPTY_LINE.log}</Empty>;
  return (
    <div className="min-w-0">
      {load && knownFrom > 0 && (
        <LoadMore
          isEarlier
          label={`Load ${Math.min(LOG_PAGE, knownFrom)} earlier lines`}
          disabled={isReading}
          onClick={readEarlier}
        >
          {isReading ? 'Reading...' : `${knownFrom} earlier lines`}
        </LoadMore>
      )}
      <pre className="mt-2 max-h-64 overflow-auto overscroll-contain whitespace-pre-wrap break-words border border-dialog-edge bg-panel-2 p-2 font-mono text-meta text-dialog-hint">
        {earlier && earlier.lines.join('\n')}
        {hole > 0 && `\n... ${hole} lines scrolled past while you were reading\n`}
        {earlier && hole === 0 && node.lines.length > 0 && '\n'}
        {node.lines.join('\n')}
      </pre>
    </div>
  );
}

/**
 * Rows keyed by id, in the order the view DECLARED.
 *
 * The phone does not offer its own sort: a live table is being written to while
 * it is read, and a column the operator sorted by would re-shuffle the rows
 * under the thumb on every patch. The order is the extension's statement, and
 * it is the same one the terminal paints.
 */
/**
 * A ROW IS A SENTENCE, and its columns are the words after the first.
 *
 * A live table is read DOWN its first column and glanced at across the rest, so
 * a phone stops scrolling sideways to see the rest: the row's own line carries
 * the name and, at the right edge, the VALUE — the last column, when that column
 * is right-aligned, because a number is what a row is measured by. Every column
 * between them stacks under the name as one detail line. From `sm` there is
 * width for the columns the run declared and they take their own cells back.
 *
 * No cell is fenced. A rule between rows is what the eye needs to keep a row
 * together, and forty boxed cells at 8px were a grid to decode before a run
 * could be read.
 */
function tableShape(node: LiveTableNode) {
  const last = node.columns.length - 1;
  const valueAt = last > 0 && node.columns[last]?.align === 'right' ? last : -1;
  const detailAt = node.columns
    .map((_, index) => index)
    .filter((index) => index > 0 && index !== valueAt);
  return { valueAt, detailAt };
}

/** The row as a phone reads it: the name, the value beside it, the rest beneath. */
function RowFace({
  node,
  row,
  isIndented,
}: {
  node: LiveTableNode;
  row: LiveRow;
  isIndented: boolean;
}) {
  const { valueAt, detailAt } = tableShape(node);
  const value = valueAt >= 0 ? (row.cells[valueAt] ?? '') : '';
  const detail = detailAt
    .map((index) => row.cells[index] ?? '')
    .filter((cell) => cell !== '')
    .join(' · ');
  const name =
    row.branch && row.cells[0]?.startsWith(`${row.branch} / `)
      ? row.cells[0].slice(row.branch.length + 3)
      : (row.cells[0] ?? '');
  return (
    <span className={`block min-w-0 ${isIndented ? 'pl-3' : ''}`}>
      <span className="flex min-w-0 items-baseline gap-2">
        <span className="min-w-0 flex-1 truncate">
          <InlineMarkdown>{name}</InlineMarkdown>
        </span>
        {value !== '' && (
          <span className="shrink-0 tabular-nums text-meta text-dialog-hint sm:hidden">{value}</span>
        )}
      </span>
      {detail !== '' && (
        <span className="mt-0.5 block truncate text-meta text-dialog-hint sm:hidden">
          <InlineMarkdown>{detail}</InlineMarkdown>
        </span>
      )}
    </span>
  );
}

function TableRows({
  node,
  onSelect,
}: {
  node: LiveTableNode;
  onSelect?: (nodeId: string, itemIds: string[]) => void;
}) {
  const rows = orderedRows(node);
  const { valueAt, detailAt } = tableShape(node);
  const selected = useMemo(() => new Set(node.selected_ids), [node.selected_ids]);
  const grouped = useMemo(() => {
    const counts = new Map<string, number>();
    for (const row of rows) if (row.branch) counts.set(row.branch, (counts.get(row.branch) ?? 0) + 1);
    return counts;
  }, [rows]);
  const selectedGroups = useMemo(
    () => new Set(rows.filter((row) => selected.has(row.id) && row.branch).map((row) => row.branch as string)),
    [rows, selected],
  );
  const selectedGroupKey = JSON.stringify([...selectedGroups].sort());
  const [openGroups, setOpenGroups] = useState<Set<string>>(() => selectedGroups);
  useEffect(() => {
    if (selectedGroups.size === 0) return;
    setOpenGroups((was) => {
      if ([...selectedGroups].every((group) => was.has(group))) return was;
      return new Set([...was, ...selectedGroups]);
    });
  }, [selectedGroupKey]);

  const visible: Array<{ kind: 'group'; label: string } | { kind: 'row'; row: LiveRow }> = [];
  const seen = new Set<string>();
  for (const row of rows) {
    const group = row.branch && (grouped.get(row.branch) ?? 0) > 1 ? row.branch : undefined;
    if (group && !seen.has(group)) {
      seen.add(group);
      visible.push({ kind: 'group', label: group });
    }
    if (!group || openGroups.has(group)) visible.push({ kind: 'row', row });
  }

  const span = Math.max(1, node.columns.length);
  return (
    <div className="-mx-3 overflow-x-auto">
      <table className="w-full min-w-0 border-collapse font-mono text-ui">
        <thead className="hidden sm:table-header-group">
          <tr>
            {node.columns.map((column) => (
              <th
                key={column.id}
                scope="col"
                className={`px-3 pb-1.5 font-normal uppercase tracking-[0.08em] text-meta text-dialog-hint ${
                  column.align === 'right' ? 'text-right' : 'text-left'
                }`}
              >
                <InlineMarkdown>{column.label}</InlineMarkdown>
              </th>
            ))}
          </tr>
        </thead>
        <tbody className="divide-y divide-dialog-edge">
          {rows.length === 0 && (
            <tr>
              <td className="px-3 py-2" colSpan={span}>
                <Empty>{EMPTY_LINE.table}</Empty>
              </td>
            </tr>
          )}
          {visible.map((item) => {
            if (item.kind === 'group') {
              const isOpen = openGroups.has(item.label);
              // The branch NAMES itself and then qualifies itself, the way every
              // label in this panel does: `Build native image · 3 variants`. The
              // name leads the row; the qualifier steps back to the right edge.
              const [name, ...rest] = item.label.split(' · ');
              return (
                <tr key={`group:${item.label}`}>
                  <td className="px-3 py-1" colSpan={span}>
                    <Disclosure
                      isOpen={isOpen}
                      tone="branch"
                      aria-label={name}
                      onClick={() =>
                        setOpenGroups((was) => {
                          const next = new Set(was);
                          if (isOpen) next.delete(item.label);
                          else next.add(item.label);
                          return next;
                        })
                      }
                    >
                      <span className="min-w-0 flex-1 truncate">{name}</span>
                      {rest.length > 0 && (
                        <span className="shrink-0 font-normal text-meta text-dialog-hint">
                          {rest.join(' · ')}
                        </span>
                      )}
                    </Disclosure>
                  </td>
                </tr>
              );
            }
            const row = item.row;
            const isSelected = selected.has(row.id);
            return (
              <TableSelectionRow
                key={row.id}
                isSelected={isSelected}
                className={`${rowInk(row.tone)} ${node.is_selectable ? 'cursor-pointer' : ''}`}
                onClick={node.is_selectable ? () => onSelect?.(node.id, [row.id]) : undefined}
              >
                <td className="p-0 align-top">
                  {node.is_selectable ? (
                    <TableSelectionButton
                      isSelected={isSelected}
                      mark={<ToneMark tone={row.tone} />}
                      aria-label={`Select ${row.cells[0] || row.id}`}
                    >
                      <RowFace node={node} row={row} isIndented={Boolean(row.branch)} />
                    </TableSelectionButton>
                  ) : (
                    <span className="flex min-w-0 items-start gap-2 px-3 py-2">
                      <ToneMark tone={row.tone} />
                      <RowFace node={node} row={row} isIndented={Boolean(row.branch)} />
                    </span>
                  )}
                </td>
                {detailAt.map((index) => (
                  <td
                    key={node.columns[index]?.id ?? index}
                    className="hidden px-3 py-2 align-top text-meta text-dialog-hint sm:table-cell"
                  >
                    <InlineMarkdown>{row.cells[index] ?? ''}</InlineMarkdown>
                  </td>
                ))}
                {valueAt >= 0 && (
                  <td className="hidden py-2 pr-3 pl-2 text-right align-top tabular-nums text-meta text-dialog-hint sm:table-cell">
                    <InlineMarkdown>{row.cells[valueAt] ?? ''}</InlineMarkdown>
                  </td>
                )}
              </TableSelectionRow>
            );
          })}
        </tbody>
      </table>
    </div>
  );
}

/**
 * Where the work also lives. A `url` is reachable from the phone and opens; a
 * path or an attachment names a place on the MACHINE, so it is stated as text
 * rather than dressed as a link that would do nothing under the thumb.
 */
function LinkRows({ node }: { node: LiveLinkNode }) {
  if (node.links.length === 0) return <Empty>{EMPTY_LINE.link}</Empty>;
  return (
    <ul className="space-y-1.5 font-mono text-ui">
      {node.links.map((link) => (
        <li key={link.id} className="flex min-w-0 items-baseline gap-2">
          <ArrowOutIcon className={`size-3 ${MARK_NUDGE} text-dialog-hint`} />
          {link.target_kind === 'url' ? (
            <a
              href={link.target}
              target="_blank"
              rel="noreferrer"
              className="min-w-0 flex-1 truncate text-accent-ink underline underline-offset-2"
            >
              <InlineMarkdown>{link.label}</InlineMarkdown>
            </a>
          ) : (
            <span className="min-w-0 flex-1 truncate text-white">
              <InlineMarkdown>{link.label}</InlineMarkdown>
            </span>
          )}
          {link.target_kind !== 'url' && (
            <span className="min-w-0 shrink truncate text-meta text-dialog-hint">{link.target}</span>
          )}
        </li>
      ))}
    </ul>
  );
}

/**
 * ONE node, painted where its view put it.
 *
 * A GROUP paints nothing of its own: it stands the nodes it holds side by side
 * (`row`) or one under the other (`column`) — the FORM's own layout vocabulary,
 * declared once and carried by no op, so an arrangement never rearranges itself
 * under a reader. It is the run's statement, and the terminal splits its band on
 * the same key. A phone has no width to split, so below `sm` a row stacks; the
 * reading order is the declared order either way.
 */
function NodeCell({
  node,
  load,
  onSelect,
}: {
  node: LiveNode;
  load?: (nodeId: string, from: number, limit: number) => Promise<LiveLogPage>;
  onSelect?: (nodeId: string, itemIds: string[]) => void;
}) {
  if (node.type === 'group') {
    return (
      <div className="min-w-0 space-y-1.5">
        {node.label && <NodeLabel>{node.label}</NodeLabel>}
        <div
          className={
            node.direction === 'row'
              ? 'grid min-w-0 gap-x-4 gap-y-3 sm:auto-cols-fr sm:grid-flow-col'
              : 'min-w-0 space-y-3'
          }
        >
          {node.fields.map((child) => (
            <NodeCell key={child.id} node={child} load={load} onSelect={onSelect} />
          ))}
        </div>
      </div>
    );
  }
  return (
    <div className="min-w-0 space-y-1.5">
      {node.label && <NodeLabel>{node.label}</NodeLabel>}
      {node.type === 'status' && <StatusRow node={node} />}
      {node.type === 'progress' && <ProgressRow node={node} />}
      {node.type === 'stat' && <StatRow node={node} />}
      {node.type === 'steps' && <StepsRows node={node} />}
      {node.type === 'log' && (
        <LogRows
          node={node}
          load={load && ((from, limit) => load(node.id, from, limit))}
        />
      )}
      {node.type === 'table' && <TableRows node={node} onSelect={onSelect} />}
      {node.type === 'link' && <LinkRows node={node} />}
    </div>
  );
}

const ACTIVITY_FACE = {
  idle: {
    rail: 'border-dialog-hint',
    ink: 'text-dialog-hint',
    mark: <CircleDashedIcon className={`size-3 ${MARK_NUDGE}`} />,
    label: 'Idle',
  },
  running: {
    rail: 'border-accent',
    ink: 'text-accent-ink',
    mark: <CircleDotIcon className={`size-3 ${MARK_NUDGE}`} />,
    label: 'Running',
  },
  succeeded: {
    rail: 'border-ok',
    ink: 'text-ok',
    mark: <CircleCheckIcon className={`size-3 ${MARK_NUDGE}`} />,
    label: 'Done',
  },
  failed: {
    rail: 'border-err',
    ink: 'text-err-ink',
    mark: <CircleXIcon className={`size-3 ${MARK_NUDGE}`} />,
    label: 'Failed',
  },
  cancelled: {
    rail: 'border-dialog-hint',
    ink: 'text-dialog-hint',
    mark: <CircleSlashIcon className={`size-3 ${MARK_NUDGE}`} />,
    label: 'Cancelled',
  },
} as const;

function formatActivityDuration(value?: number): string | null {
  if (value == null || !Number.isFinite(value) || value <= 0) return null;
  const milliseconds = Math.trunc(value);
  if (milliseconds < 1_000) return `${milliseconds}ms`;
  if (milliseconds < 60_000) return `${(milliseconds / 1_000).toFixed(1)}s`;
  const minutes = Math.floor(milliseconds / 60_000);
  return `${minutes}m ${Math.floor((milliseconds % 60_000) / 1_000)}s`;
}

function activityRowSummary(row: ActivityProjection['rows'][number]): string {
  const summary = row.summary.trim();
  const command =
    (row.presenter === 'shell' || row.operation.toLowerCase() === 'shell') &&
    summary.startsWith('running: ')
      ? `cmd: ${summary.slice('running: '.length)}`
      : summary;
  return command.toLowerCase() === row.operation.trim().toLowerCase() ? '' : command;
}

function activityRowLabel(row: ActivityProjection['rows'][number]): string {
  return [row.operation.toUpperCase(), activityRowSummary(row)].filter(Boolean).join(' · ');
}

function activityTotal(activity?: ActivityProjection): number {
  const counts = activity?.counts;
  return counts
    ? counts.running + counts.succeeded + counts.failed + counts.cancelled
    : activity?.rows.length ?? 0;
}

/** The one honest sentence a unified execution trace can state at this moment. */
export function activityReceiptText(
  activity?: ActivityProjection,
  durationMs?: number,
): string {
  const state = activity?.state ?? 'idle';
  const total = activityTotal(activity);
  if (state === 'running' || state === 'idle') {
    const row = activity?.rows.find((candidate) => candidate.state === 'running');
    const focus = row ? activityRowLabel(row) : 'running activity';
    return ['RUNNING', focus, total > 1 || (activity?.omitted.rows ?? 0) > 0 ? 'and more' : '']
      .filter(Boolean)
      .join(' · ');
  }

  const terminal = activity?.counts
    ? activity.counts.succeeded + activity.counts.failed + activity.counts.cancelled
    : activity?.rows.length ?? 0;
  const primary =
    (state === 'failed' && activity?.rows.find((candidate) => candidate.state === 'failed')) ||
    activity?.rows[0];
  const preview = primary
    ? `${primary.operation.toUpperCase()}${terminal > 1 || (activity?.omitted.rows ?? 0) > 0 ? ' and more' : ''}`
    : '';
  const label = state === 'succeeded' ? 'DONE' : state.toUpperCase();
  return [
    label,
    preview,
    `${terminal} ${terminal === 1 ? 'activity' : 'activities'}`,
    formatActivityDuration(durationMs),
  ]
    .filter(Boolean)
    .join(' · ');
}

function activityPreview(activity?: ActivityProjection): string {
  const running = activity?.rows.find((candidate) => candidate.state === 'running');
  if (running) return activityRowLabel(running);
  const primary = activity?.rows[0];
  if (!primary) return 'No operation yet';
  return `${primary.operation.toUpperCase()}${activityTotal(activity) > 1 ? ' and more' : ''}`;
}

function ActivityRail({ activity }: { activity?: ActivityProjection }) {
  const rows = [...(activity?.rows ?? [])].sort((left, right) => left.sequence - right.sequence);

  return (
    <div className="max-h-80 overflow-y-auto overscroll-contain" data-activity-rail>
      <ol aria-label="Invocation chronology">
        {rows.map((row) => {
          const face = ACTIVITY_FACE[row.state];
          const summary = activityRowSummary(row);
          const duration = formatActivityDuration(row.duration_ms);
          return (
            <li
              key={row.id}
              data-activity-row={row.id}
              className="grid min-w-0 grid-cols-[auto_minmax(0,1fr)_auto] items-baseline gap-x-2 border-t border-code-edge bg-result px-2.5 py-1.5 first:border-t-0 font-mono text-meta"
            >
              <span aria-hidden="true" className={face.ink}>{face.mark}</span>
              <span className="min-w-0 break-words text-dialog-hint">
                {[row.operation, summary].filter(Boolean).join(' · ')}
              </span>
              {duration && <span className="text-code-duration">{duration}</span>}
            </li>
          );
        })}
        {rows.length === 0 && (
          <li className="bg-result px-2.5 py-1.5 font-mono text-meta text-dialog-hint">
            No operations yet
          </li>
        )}
      </ol>
    </div>
  );
}

/**
 * ACTIVITY, PAINTED WHERE IT BELONGS — inside the form that produced it.
 *
 * It takes the projection itself, not a view: protocol 7 stopped shipping
 * Activity as a classified Live View addressed from a distance by an anchor, so
 * there is no longer a record to unwrap and no `classification` to branch on.
 * The paint is unchanged, which is the point — the rail, the faces and the
 * receipt were never the problem with the old shape.
 */
export function ActivityPanel({
  activity,
  isSettled,
  initiallyExpanded = false,
}: {
  activity?: ActivityProjection;
  isSettled: boolean;
  initiallyExpanded?: boolean;
}) {
  const [expanded, setExpanded] = useState(initiallyExpanded);
  const state = activity?.state ?? 'idle';
  const face = ACTIVITY_FACE[state];
  const preview = activityPreview(activity);

  return (
    <section
      className={`min-w-0 overflow-hidden border-l-2 ${face.rail} bg-result`}
      aria-label="Activity"
      role={isSettled ? undefined : 'status'}
      aria-live={isSettled ? undefined : 'polite'}
    >
      <header className="flex min-h-8 items-center gap-1.5 bg-result px-2">
        <Disclosure
          isOpen={expanded}
          tone="step"
          className="min-w-0 flex-1"
          aria-label={expanded ? 'Collapse Activity' : 'Expand Activity'}
          onClick={() => setExpanded((open) => !open)}
        >
          <span className="flex min-w-0 flex-1 items-baseline gap-2">
            <BandLabel className="shrink-0">ACTIVITY</BandLabel>
            <span
              className={`shrink-0 font-mono text-chip font-bold normal-case tracking-normal ${face.ink}`}
            >
              {face.label}
            </span>
            <span className="min-w-0 flex-1 truncate font-normal tracking-normal text-code-result">
              {preview}
            </span>
          </span>
        </Disclosure>
      </header>
      {expanded && <ActivityRail activity={activity} />}
    </section>
  );
}

/**
 * ONE view, painted. Pure: everything it knows arrived as a prop, which is what
 * lets the whole picture be rendered from the engine's own fixture in a test.
 */
export function LiveViewPanel({
  view,
  onInterrupt,
  onSelect,
  isInterrupting = false,
  error,
  load,
  isSettled = false,
}: {
  view: LiveViewModel;
  /** Stop the view, carrying the comment the human left — `null` when they left none. */
  onInterrupt?: (note: string | null) => void;
  /** Replace the selected ids of one selectable table in shared engine state. */
  onSelect?: (nodeId: string, itemIds: string[]) => void;
  isInterrupting?: boolean;
  error?: string | null;
  load?: (nodeId: string, from: number, limit: number) => Promise<LiveLogPage>;
  /**
   * The run is OVER and this is its record. Nothing spins, and the section stops
   * being a live region: a picture that cannot change again must not announce
   * itself to a screen reader as one that can.
   */
  isSettled?: boolean;
}) {
  // The stop is ARMED before it is sent, exactly as Escape arms it in the
  // terminal: the comment travels WITH the interrupt, so the run reads WHY it
  // was stopped and not merely that it was. `null` is "not armed" — an empty
  // string is an armed stop nobody has typed into yet.
  const [note, setNote] = useState<string | null>(null);
  const isArmed = note !== null;
  const typed = note ?? '';
  // One armed stop, however it is sent: the comment travels trimmed, and an
  // empty line is no comment at all rather than an empty one.
  const sendStop = (send: (note: string | null) => void) => {
    setNote(null);
    send(typed.trim() === '' ? null : typed.trim());
  };
  return (
    <section
      className="overflow-hidden border border-dialog-edge bg-panel"
      role={isSettled ? undefined : 'status'}
      aria-live={isSettled ? undefined : 'polite'}
    >
      <header className="flex items-start gap-2 border-b border-dialog-edge bg-panel-2 px-3 py-2.5">
        <span className="min-w-0 flex-1">
          <span className="block truncate font-mono text-title font-bold text-white">
            {view.title}
          </span>
          {view.description && (
            <span className="block truncate font-mono text-meta text-dialog-hint">
              <InlineMarkdown>{view.description}</InlineMarkdown>
            </span>
          )}
        </span>
        <ViewState view={view} isSettled={isSettled} />
        {onInterrupt && !isArmed && (
          <Button variant="secondary" onClick={() => setNote('')} disabled={isInterrupting}>
            {isInterrupting ? 'Stopping...' : 'Interrupt'}
          </Button>
        )}
      </header>
      {isArmed && onInterrupt && (
        <form
          className="flex flex-wrap items-center gap-2 border-b border-dialog-edge bg-panel-2 px-3 py-2"
          onSubmit={(event) => {
            event.preventDefault();
            sendStop(onInterrupt);
          }}
          onKeyDown={(event) => {
            // Escape STOPS. It is the key that ARMED the interrupt, so it is the
            // key that sends it, note and all — the terminal answers the same
            // key the same way. `Keep watching` is the way back here, because a
            // phone has no Backspace to fall out of an empty line with.
            if (event.key !== 'Escape') return;
            event.preventDefault();
            event.stopPropagation();
            sendStop(onInterrupt);
          }}
        >
          <Input
            autoFocus
            className="min-w-40 flex-1"
            value={typed}
            maxLength={LIVE_NOTE_CHARS}
            onChange={(event) => setNote(event.target.value)}
            placeholder="why stop it? (optional)"
            aria-label={`Why are you stopping ${view.title}?`}
          />
          <Button type="submit" variant="primary" disabled={isInterrupting}>
            Interrupt
          </Button>
          <Button type="button" variant="secondary" onClick={() => setNote(null)}>
            Keep watching
          </Button>
        </form>
      )}
      {error && <p className="border-b border-dialog-edge px-3 py-2 font-mono text-chip text-err">{error}</p>}
      <ul className="divide-y divide-dialog-edge">
        {view.nodes.map((node) => (
          <li key={node.id} className="min-w-0 px-3 py-2.5">
            <NodeCell node={node} load={load} onSelect={onSelect} />
          </li>
        ))}
      </ul>
    </section>
  );
}

/**
 * Every live view this session is showing, kept current.
 *
 * The snapshot is read on mount and on every reconnect, and the three events
 * fold into it as they arrive — the same two doors the parked-form dialog uses,
 * for the same reason: a phone woken by a push never saw the frames that opened
 * the view it is about to paint.
 *
 * A HOOK rather than the panel's own state, because an open view is not only a
 * panel: the running row above the transcript stops saying "Vis is thinking"
 * and names what is on screen instead, and both must read one list.
 */
export function useLiveViews(
  client: GatewayClient,
  subscriptions: SessionSubscriptionHub,
  sid: string,
  onRecordFiled?: () => void,
): LiveViewModel[] {
  const [views, setViews] = useState<LiveViewModel[]>([]);
  const revision = useRef(0);

  useEffect(() => {
    revision.current += 1;
    let cancelled = false;
    const controller = new AbortController();
    const reload = () => {
      const requestedAt = revision.current;
      client
        .liveViews(sid, controller.signal)
        .then((open) => {
          if (!cancelled && requestedAt === revision.current) setViews(open);
        })
        .catch(() => undefined);
    };
    reload();
    const stopConnection = subscriptions.subscribeConnection((connected) => {
      if (connected) reload();
    });
    const recordRefreshTimers: ReturnType<typeof setTimeout>[] = [];
    let patchTimer: ReturnType<typeof setTimeout> | null = null;
    let pendingPatches: SseEvent[] = [];
    const flushPatches = () => {
      if (patchTimer !== null) clearTimeout(patchTimer);
      patchTimer = null;
      if (pendingPatches.length === 0) return;
      const batch = pendingPatches;
      pendingPatches = [];
      // Activity is informative, not input-critical. Let elapsed clocks, touch and
      // streamed prose interrupt this render instead of waiting behind a large table.
      startTransition(() => {
        setViews((current) => batch.reduce(applyLiveViewEvent, current));
      });
    };
    const revealRecord = () => {
      onRecordFiled?.();
      // A close on the gateway thread files into the running block's collector
      // immediately, but that collector reaches the persisted iteration only when
      // the block returns. The close frame can therefore beat one transcript read.
      // Re-read across that short handoff so the settled picture gives way to
      // the durable transcript record without a manual refresh.
      for (const delay of [250, 1_000, 3_000, 8_000]) {
        recordRefreshTimers.push(setTimeout(() => onRecordFiled?.(), delay));
      }
    };
    const stopEvents = subscriptions.subscribeSession(sid, (event) => {
      if (!isLiveViewEvent(event)) return;
      revision.current += 1;
      if (event.type === VIEW_PATCH_EVENT) {
        pendingPatches.push(event);
        // Activity can emit much faster than WKWebView can paint. Fold a burst in
        // memory and give React one picture, leaving the clock and touch handling
        // enough main-thread time to move independently.
        patchTimer ??= setTimeout(flushPatches, 80);
        return;
      }
      flushPatches();
      setViews((current) => applyLiveViewEvent(current, event));
      if (event.type === VIEW_CLOSE_EVENT) revealRecord();
    });
    return () => {
      cancelled = true;
      revision.current += 1;
      controller.abort();
      stopConnection();
      stopEvents();
      if (patchTimer !== null) clearTimeout(patchTimer);
      pendingPatches = [];
      for (const timer of recordRefreshTimers) clearTimeout(timer);
    };
  }, [client, sid, subscriptions, onRecordFiled]);

  // A patch frame that skipped a seq means frames were LOST — the coalescing
  // window states the range it stands for precisely so this is knowable. The
  // picture is behind, so it is RE-READ rather than patched further: a table
  // quietly missing a row is the failure this whole numbering exists to catch.
  const isStale = views.some((view) => view.is_stale === true);
  useEffect(() => {
    if (!isStale) return;
    let cancelled = false;
    const controller = new AbortController();
    const requestedAt = revision.current;
    client
      .liveViews(sid, controller.signal)
      .then((open) => {
        if (!cancelled && requestedAt === revision.current) setViews(open);
      })
      .catch(() => undefined);
    return () => {
      cancelled = true;
      controller.abort();
    };
  }, [isStale, client, sid]);

  return views;
}

/** Every open view of this session, painted where the transcript ends. */
export function LiveView({
  views,
  client,
  sid,
}: {
  views: LiveViewModel[];
  client: GatewayClient;
  sid: string;
}) {
  const [stopping, setStopping] = useState<string | null>(null);
  const [error, setError] = useState<string | null>(null);

  if (views.length === 0) return null;

  // The note the human typed rides WITH the stop: one call, so the run never
  // resumes on an interrupt whose reason is still in flight behind it.
  const interrupt = (viewId: string, note: string | null) => {
    setStopping(viewId);
    setError(null);
    client
      .viewAction(sid, viewId, { action: 'interrupt', ...(note ? { note } : {}) })
      .catch(() => setError('That view would not stop. It may have just finished.'))
      .finally(() => setStopping(null));
  };

  const select = (viewId: string, nodeId: string, itemIds: string[]) => {
    setError(null);
    client
      .viewAction(sid, viewId, { action: 'select', node_id: nodeId, item_ids: itemIds })
      .catch(() => setError('That job could not be selected. It may have just finished.'));
  };

  const readLog = (viewId: string) => (nodeId: string, from: number, limit: number) =>
    client.liveViewLog(sid, viewId, nodeId, from, limit);

  return (
    <div className="space-y-3">
      {views.map((view) => (
        <LiveViewPanel
          key={view.id}
          view={view}
          error={stopping === null ? error : null}
          isInterrupting={stopping === view.id}
          onInterrupt={(note) => interrupt(view.id, note)}
          onSelect={(nodeId, itemIds) => select(view.id, nodeId, itemIds)}
          load={readLog(view.id)}
        />
      ))}
    </div>
  );
}

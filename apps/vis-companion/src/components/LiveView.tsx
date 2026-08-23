import { useEffect, useMemo, useRef, useState } from 'react';
import {
  Button,
  BandLabel,
  BandTally,
  Disclosure,
  Input,
  LoadMore,
  Meter,
  PROSE,
  Spinner,
  TableFocusButton,
  TableFocusRow,
} from './ui';
import { InlineMarkdown } from './ChatContent';
import type { GatewayClient } from '../lib/gateway';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import {
  applyLiveViewEvent,
  LIVE_VIEW_CLOSE_EVENT,
  LIVE_NOTE_CHARS,
  isLiveViewEvent,
  liveFraction,
  livePercent,
  orderedRows,
  type ActivityDiffEvidence,
  type ActivityProjection,
  type ActivityRow,
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
 * (`human-input.live/empty-line`), so the phone, the terminal and the document
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

/** The node's name, in the label voice the dialog's fields already use. */
function NodeLabel({ children }: { children: string }) {
  return (
    <span className="block font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
      {children}
    </span>
  );
}

function Empty({ children }: { children: string }) {
  return <p className="font-mono text-meta italic text-dialog-hint">{children}</p>;
}

/** The tone as a mark, for the tone itself carries the meaning in colour alone. */
function ToneMark({ tone }: { tone: LiveTone }) {
  return (
    <span aria-hidden="true" className={`shrink-0 ${TONE_INK[tone]}`}>
      ●
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
    <div className="grid grid-cols-[auto_minmax(0,1fr)] items-baseline gap-x-2 gap-y-1 font-mono text-ui">
      <ToneMark tone={node.tone} />
      <p className={`min-w-0 ${PROSE} ${TONE_INK[node.tone]}`}>
        <InlineMarkdown>{node.text}</InlineMarkdown>
      </p>
      {node.detail && (
        <p className="col-start-2 min-w-0 text-chip text-dialog-hint">
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
          {counted && <span className="ml-2 text-chip">{counted}</span>}
        </p>
      ) : (
        <>
          <Meter value={fraction} label={node.label ?? 'Progress'} />
          <p className="mt-1.5 font-mono text-chip text-dialog-hint">
            <span className="font-bold text-white">{livePercent(fraction)}%</span>
            {counted && <span className="ml-2">{counted}</span>}
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
          <dt className="text-chip text-dialog-hint">
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
    <ul className="space-y-1 font-mono text-chip">
      {node.steps.map((step) => (
        <li key={step.id} className="flex min-w-0 items-baseline gap-2">
          <ToneMark tone={step.tone} />
          <span className={`min-w-0 flex-1 truncate ${TONE_INK[step.tone]}`}>
            <InlineMarkdown>{step.label}</InlineMarkdown>
          </span>
          {step.value && <span className="shrink-0 font-bold text-white">{step.value}</span>}
          {step.detail && (
            <span className="shrink-0 text-dialog-hint">
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
function TableRows({
  node,
  onFocus,
}: {
  node: LiveTableNode;
  onFocus?: (nodeId: string, itemIds: string[]) => void;
}) {
  const rows = orderedRows(node);
  const focused = useMemo(() => new Set(node.focused_ids), [node.focused_ids]);
  const grouped = useMemo(() => {
    const counts = new Map<string, number>();
    for (const row of rows) if (row.branch) counts.set(row.branch, (counts.get(row.branch) ?? 0) + 1);
    return counts;
  }, [rows]);
  const focusedGroups = useMemo(
    () => new Set(rows.filter((row) => focused.has(row.id) && row.branch).map((row) => row.branch as string)),
    [rows, focused],
  );
  const focusedGroupKey = JSON.stringify([...focusedGroups].sort());
  const [openGroups, setOpenGroups] = useState<Set<string>>(() => focusedGroups);
  useEffect(() => {
    if (focusedGroups.size === 0) return;
    setOpenGroups((was) => {
      if ([...focusedGroups].every((group) => was.has(group))) return was;
      return new Set([...was, ...focusedGroups]);
    });
  }, [focusedGroupKey]);

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

  return (
    <div className="-mx-1 overflow-x-auto">
      <table className="w-full min-w-0 border-collapse border border-dialog-edge font-mono text-chip">
        <thead>
          <tr>
            {node.columns.map((column) => (
              <th
                key={column.id}
                scope="col"
                className={`border border-dialog-edge px-1.5 py-1 font-bold uppercase tracking-[0.08em] text-dialog-hint ${
                  column.align === 'right' ? 'text-right' : 'text-left'
                }`}
              >
                <InlineMarkdown>{column.label}</InlineMarkdown>
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {rows.length === 0 && (
            <tr>
              <td className="border border-dialog-edge px-1.5 py-1" colSpan={Math.max(1, node.columns.length)}>
                <Empty>{EMPTY_LINE.table}</Empty>
              </td>
            </tr>
          )}
          {visible.map((item) => {
            if (item.kind === 'group') {
              const isOpen = openGroups.has(item.label);
              return (
                <tr key={`group:${item.label}`} className="bg-panel-2 text-dialog-hint">
                  <td className="border border-dialog-edge px-1.5" colSpan={Math.max(1, node.columns.length)}>
                    <Disclosure
                      isOpen={isOpen}
                      aria-label={item.label}
                      onClick={() =>
                        setOpenGroups((was) => {
                          const next = new Set(was);
                          if (isOpen) next.delete(item.label);
                          else next.add(item.label);
                          return next;
                        })
                      }
                    >
                      <span className="truncate font-bold">{item.label}</span>
                    </Disclosure>
                  </td>
                </tr>
              );
            }
            const row = item.row;
            const isFocused = focused.has(row.id);
            const firstCell =
              row.branch && row.cells[0]?.startsWith(`${row.branch} / `)
                ? row.cells[0].slice(row.branch.length + 3)
                : (row.cells[0] ?? '');
            return (
              <TableFocusRow
                key={row.id}
                isFocused={isFocused}
                className={`${TONE_INK[row.tone]} ${node.is_focusable ? 'cursor-pointer' : ''}`}
                onClick={node.is_focusable ? () => onFocus?.(node.id, [row.id]) : undefined}
              >
                {node.columns.map((column, cell) => (
                  <td
                    key={column.id}
                    className={`border border-dialog-edge align-top ${
                      node.is_focusable && cell === 0 ? 'p-0' : 'px-1.5 py-1'
                    } ${column.align === 'right' ? 'text-right tabular-nums' : 'text-left'}`}
                  >
                    {node.is_focusable && cell === 0 ? (
                      <TableFocusButton isFocused={isFocused} aria-label={`Focus ${row.cells[cell] || row.id}`}>
                        <span className={row.branch ? 'pl-3' : ''}>
                          <InlineMarkdown>{firstCell}</InlineMarkdown>
                        </span>
                      </TableFocusButton>
                    ) : (
                      <InlineMarkdown>{row.cells[cell] ?? ''}</InlineMarkdown>
                    )}
                  </td>
                ))}
              </TableFocusRow>
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
    <ul className="space-y-1 font-mono text-chip">
      {node.links.map((link) => (
        <li key={link.id} className="flex min-w-0 items-baseline gap-2">
          <span aria-hidden="true" className="shrink-0 text-dialog-hint">
            →
          </span>
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
            <span className="min-w-0 shrink truncate text-dialog-hint">{link.target}</span>
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
  onFocus,
}: {
  node: LiveNode;
  load?: (nodeId: string, from: number, limit: number) => Promise<LiveLogPage>;
  onFocus?: (nodeId: string, itemIds: string[]) => void;
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
            <NodeCell key={child.id} node={child} load={load} onFocus={onFocus} />
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
      {node.type === 'table' && <TableRows node={node} onFocus={onFocus} />}
      {node.type === 'link' && <LinkRows node={node} />}
    </div>
  );
}

const ACTIVITY_FACE = {
  idle: {
    rail: 'border-dialog-hint',
    ink: 'text-dialog-hint',
    mark: '◇',
    label: 'Idle',
  },
  running: {
    rail: 'border-accent',
    ink: 'text-accent-ink',
    mark: '●',
    label: 'Running',
  },
  succeeded: {
    rail: 'border-ok',
    ink: 'text-ok',
    mark: '✓',
    label: 'Completed',
  },
  failed: {
    rail: 'border-err',
    ink: 'text-err-ink',
    mark: '×',
    label: 'Failed',
  },
  cancelled: {
    rail: 'border-dialog-hint',
    ink: 'text-dialog-hint',
    mark: '■',
    label: 'Cancelled',
  },
} as const;

const ACTIVITY_NUMBER = new Intl.NumberFormat('en-US');

function plural(count: number, one: string, many = `${one}s`) {
  return `${ACTIVITY_NUMBER.format(count)} ${count === 1 ? one : many}`;
}

function activityFacts(activity?: ActivityProjection): string[] {
  if (!activity) return [];
  const total = Object.values(activity.counts).reduce((sum, count) => sum + count, 0);
  if (total === 0) return [];

  const mutations = activity.rows.filter((row) => row.signal === 'mutation').length;
  const verifications = activity.rows.filter((row) => row.signal === 'verification');
  const failedVerifications = verifications.filter((row) => row.state === 'failed').length;
  const verificationResult = verifications.find((row) => row.result_summary)?.result_summary;
  const facts = [plural(total, 'operation')];

  if (failedVerifications > 0) {
    facts.unshift(`${plural(failedVerifications, 'verification')} failed`);
  } else if (activity.counts.failed > 0) {
    facts.unshift(`${plural(activity.counts.failed, 'operation')} failed`);
  } else if (activity.counts.running > 0) {
    facts.push(`${ACTIVITY_NUMBER.format(activity.counts.running)} running`);
  }
  if (mutations > 0) facts.push(plural(mutations, 'change'));
  if (verificationResult) facts.push(verificationResult);
  else if (verifications.length > 0) facts.push(plural(verifications.length, 'verification'));
  if (activity.counts.cancelled > 0)
    facts.push(`${ACTIVITY_NUMBER.format(activity.counts.cancelled)} cancelled`);

  return facts.slice(0, 3);
}

function activityElapsed(startedAt?: number, endedAt?: number): string | null {
  if (startedAt === undefined || endedAt === undefined) return null;
  const milliseconds = Math.max(0, endedAt - startedAt);
  if (milliseconds < 1000) return `${Math.round(milliseconds)}ms`;
  const seconds = milliseconds / 1000;
  if (seconds < 60) return `${seconds.toFixed(1)}s`;
  const minutes = Math.floor(seconds / 60);
  const remaining = Math.floor(seconds % 60);
  if (minutes < 60) return `${minutes}m ${remaining}s`;
  return `${Math.floor(minutes / 60)}h ${minutes % 60}m`;
}

const ACTIVITY_PRESENTER = {
  generic: { label: 'GENERIC', mark: '◇' },
  shell: { label: 'SHELL', mark: '┌' },
  tests: { label: 'TESTS', mark: '✓' },
  patch: { label: 'PATCH', mark: '↗' },
  observation: { label: 'OBSERVE', mark: '⌕' },
  lint: { label: 'LINT', mark: '✓' },
  repl: { label: 'REPL', mark: '✓' },
  format: { label: 'FORMAT', mark: '✓' },
  list: { label: 'LIST', mark: '✓' },
} as const;

function activityPresenter(row: ActivityRow) {
  if (row.presenter === 'list' && row.operation.toLowerCase() === 'ls')
    return { label: 'LS', mark: '✓' };
  if (row.presenter !== 'observation') return ACTIVITY_PRESENTER[row.presenter];
  const operation = row.operation.toLowerCase();
  if (operation === 'grep' || operation.includes('search'))
    return { label: 'SEARCH', mark: '⌕' };
  if (operation === 'cat' || operation.includes('read'))
    return { label: 'READ', mark: '⌞' };
  return ACTIVITY_PRESENTER.observation;
}

function activityRowFace(row: ActivityRow) {
  if (row.state === 'failed')
    return {
      ink: 'text-err-ink',
      rail: 'border-err',
      mark: '×',
      surface: 'bg-err-surface',
    };
  if (row.state === 'running')
    return {
      ink: 'text-accent-ink',
      rail: 'border-accent',
      mark: '●',
      surface: 'bg-result',
    };
  if (row.state === 'cancelled')
    return {
      ink: 'text-dialog-hint',
      rail: 'border-dialog-hint',
      mark: '■',
      surface: 'bg-result',
    };
  if (row.signal === 'mutation')
    return {
      ink: 'text-warn',
      rail: 'border-warn-strong',
      mark: '↗',
      surface: 'bg-warn-surface',
    };
  if (row.signal === 'verification')
    return { ink: 'text-ok', rail: 'border-ok', mark: '✓', surface: 'bg-result' };
  return {
    ink: 'text-code-duration',
    rail: 'border-dialog-hint',
    mark: activityPresenter(row).mark,
    surface: 'bg-result',
  };
}

const DIFF_LINE_FACE = {
  header: { mark: '◆', label: 'diff header', className: 'bg-panel-2 text-dialog-hint' },
  hunk: { mark: '@@', label: 'diff hunk', className: 'bg-panel-2 text-accent-ink' },
  context: { mark: ' ', label: 'unchanged line', className: 'bg-result text-code-result' },
  addition: { mark: '+', label: 'added line', className: 'bg-ok-surface text-ok-foreground' },
  deletion: { mark: '−', label: 'deleted line', className: 'bg-err-surface text-err-ink' },
} as const;

function ActivityDiff({ evidence }: { evidence: ActivityDiffEvidence }) {
  return (
    <figure
      className="mt-1.5 min-w-0 overflow-hidden border-l-2 border-warn-strong bg-result font-mono text-meta"
      aria-label={`Diff ${evidence.text}`}
    >
      <figcaption className="flex min-w-0 items-center gap-2 bg-panel-2 px-2 py-1 text-code-duration">
        <BandLabel className="shrink-0">DIFF</BandLabel>
        <span className="min-w-0 flex-1 truncate">{evidence.text}</span>
        <span className="shrink-0 tabular-nums" aria-label={`${evidence.additions} additions`}>
          +{ACTIVITY_NUMBER.format(evidence.additions)}
        </span>
        <span className="shrink-0 tabular-nums" aria-label={`${evidence.deletions} deletions`}>
          −{ACTIVITY_NUMBER.format(evidence.deletions)}
        </span>
        {evidence.modifications > 0 && (
          <span
            className="shrink-0 tabular-nums"
            aria-label={`${evidence.modifications} modifications`}
          >
            ~{ACTIVITY_NUMBER.format(evidence.modifications)}
          </span>
        )}
      </figcaption>
      <div className="min-w-0" role="list" aria-label="Diff lines">
        {evidence.lines.map((line, index) => {
          const face = DIFF_LINE_FACE[line.kind];
          return (
            <div
              key={`${line.kind}-${index}`}
              role="listitem"
              aria-label={`${face.label}${line.is_redacted ? ', redacted' : ''}`}
              className={`grid min-w-0 grid-cols-[2rem_minmax(0,1fr)] ${face.className}`}
            >
              <span aria-hidden="true" className="select-none px-1.5 text-right font-bold">
                {face.mark}
              </span>
              <code className="min-w-0 whitespace-pre-wrap break-all px-1.5 py-0.5">
                {line.text}
              </code>
            </div>
          );
        })}
      </div>
      {(evidence.is_truncated || evidence.is_redacted) && (
        <p className="border-t border-code-edge px-2 py-1 text-warn">
          {evidence.is_truncated
            ? `Evidence truncated${evidence.omitted_lines > 0 ? ` · ${ACTIVITY_NUMBER.format(evidence.omitted_lines)} more lines omitted` : ''}`
            : null}
          {evidence.is_truncated && evidence.is_redacted ? ' · ' : null}
          {evidence.is_redacted ? 'Sensitive lines redacted' : null}
        </p>
      )}
    </figure>
  );
}

function ActivityRailRow({
  row,
  child = false,
}: {
  row: ActivityRow;
  child?: boolean;
}) {
  const presenter = activityPresenter(row);
  const face = activityRowFace(row);
  const duration = activityElapsed(0, row.duration_ms);
  const diffs = row.evidence.filter(
    (item): item is ActivityDiffEvidence => item.kind === 'diff',
  );
  const evidence = [
    ...row.evidence.filter((item) => item.kind !== 'diff'),
    ...(row.result_summary
      ? [{ kind: 'result' as const, text: row.result_summary }]
      : []),
    ...(row.error_summary ? [{ kind: 'error' as const, text: row.error_summary }] : []),
  ];
  const emphasized =
    row.state === 'failed' || row.signal === 'mutation' || row.signal === 'verification';

  return (
    <li
      className={`min-w-0 border-t border-code-edge px-2.5 py-1.5 first:border-t-0 ${face.surface}`}
      data-activity-row={row.id}
    >
      <div
        className={`grid min-w-0 grid-cols-[auto_auto_minmax(0,1fr)_auto] items-baseline gap-x-2 font-mono text-meta ${child ? 'pl-2' : ''}`}
      >
        <span aria-hidden="true" className={face.ink}>
          {child ? '└' : face.mark}
        </span>
        <span
          className={`font-bold ${emphasized ? face.ink : 'text-code-duration'}`}
        >
          {presenter.label}
        </span>
        <span
          className={`min-w-0 break-words ${emphasized ? 'font-medium text-code-result' : 'text-dialog-hint'}`}
        >
          {row.operation} · {row.summary || row.state}
        </span>
        {duration && (
          <span className="tabular-nums text-code-duration">{duration}</span>
        )}
      </div>
      {(evidence.length > 0 || row.is_truncated) && (
        <div
          className={`mt-1.5 grid gap-1 overflow-x-auto border-l-2 pl-3 font-mono text-meta ${face.rail}`}
        >
          {evidence.map((item, index) => (
            <p
              key={`${item.kind}-${index}`}
              className={`whitespace-pre-wrap break-words ${item.kind === 'error' ? 'text-err-ink' : item.kind === 'result' ? face.ink : 'text-dialog-hint'}`}
            >
              {item.text}
            </p>
          ))}
          {row.is_truncated && <p className="text-warn">… evidence truncated</p>}
        </div>
      )}
      {diffs.map((diff, index) => (
        <ActivityDiff key={`${diff.text}-${index}`} evidence={diff} />
      ))}
      {(row.children?.length ?? 0) > 0 && (
        <ol
          className="mt-1.5 border-l border-code-edge"
          aria-label={`${presenter.label} chronology`}
        >
          {[...row.children!]
            .sort((left, right) => left.sequence - right.sequence)
            .map((entry) => (
              <ActivityRailRow key={entry.id} row={entry} child />
            ))}
        </ol>
      )}
    </li>
  );
}

function ActivityRail({ activity }: { activity?: ActivityProjection }) {
  const rows = [...(activity?.rows ?? [])].sort(
    (left, right) => left.sequence - right.sequence,
  );
  const omitted = activity?.omitted.rows ?? 0;
  const omittedKinds = Object.entries(activity?.omitted.by_classification ?? {})
    .filter(([, count]) => count > 0)
    .map(([kind, count]) => `${ACTIVITY_NUMBER.format(count)} ${kind}`)
    .join(' · ');

  return (
    <div className="max-h-80 overflow-y-auto overscroll-contain" data-activity-rail>
      <ol aria-label="Invocation chronology">
        {rows.map((row) => (
          <ActivityRailRow key={row.id} row={row} />
        ))}
        {omitted > 0 && (
          <li className="border-t border-code-edge bg-result px-2.5 py-1.5 font-mono text-meta text-dialog-hint">
            … {ACTIVITY_NUMBER.format(omitted)} omitted
            {omittedKinds && ` · ${omittedKinds}`}
          </li>
        )}
        {rows.length === 0 && omitted === 0 && (
          <li className="bg-result px-2.5 py-1.5 font-mono text-meta text-dialog-hint">
            No retained invocations
          </li>
        )}
      </ol>
    </div>
  );
}

function ActivityViewPanel({
  view,
  isSettled,
  endedAt,
}: {
  view: LiveViewModel;
  isSettled: boolean;
  endedAt?: number;
}) {
  const [expanded, setExpanded] = useState(false);
  const [now, setNow] = useState(() => Date.now());
  const activity = view.activity;
  const state = activity?.state ?? 'idle';
  const bounded =
    (activity?.omitted.rows ?? 0) > 0 ||
    (activity?.rows.some((row) => row.is_truncated) ?? false);
  const face = ACTIVITY_FACE[state];
  const rail = bounded ? 'border-warn-strong' : face.rail;
  const ink = bounded ? 'text-warn' : face.ink;
  const mark = bounded ? '!' : face.mark;
  const facts = activityFacts(activity);
  const duration = activityElapsed(view.created_at, endedAt ?? (isSettled ? undefined : now));

  useEffect(() => {
    if (isSettled || state !== 'running' || view.created_at === undefined) return;
    const timer = window.setInterval(() => setNow(Date.now()), 250);
    return () => window.clearInterval(timer);
  }, [isSettled, state, view.created_at]);

  return (
    <section
      className={`min-w-0 overflow-hidden border border-dialog-edge border-l-2 ${rail} bg-result shadow-[2px_2px_0_var(--dialog-shadow)]`}
      aria-label="Activity"
      role={isSettled ? undefined : 'status'}
      aria-live={isSettled ? undefined : 'polite'}
    >
      <header className="flex min-h-10 items-center gap-2 bg-result px-2.5 mouse:min-h-8">
        {state === 'running' && !bounded ? (
          <Spinner tone="accent" />
        ) : (
          <span aria-hidden="true" className={`font-mono text-ui font-bold ${ink}`}>
            {mark}
          </span>
        )}
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
              className={`shrink-0 font-mono text-chip font-bold normal-case tracking-normal ${ink}`}
            >
              {face.label}
            </span>
            <span className="min-w-0 flex-1 truncate font-normal tracking-normal text-code-result">
              {facts.length > 0 ? facts.join(' · ') : 'No operations yet'}
              {bounded && (
                <>
                  {' · '}
                  <BandTally>
                    {(activity?.omitted.rows ?? 0) > 0
                      ? `+${ACTIVITY_NUMBER.format(activity?.omitted.rows ?? 0)} omitted`
                      : 'truncated'}
                  </BandTally>
                </>
              )}
            </span>
            {duration && (
              <span
                aria-hidden="true"
                className="shrink-0 font-mono text-chip font-normal tabular-nums tracking-normal text-code-duration"
              >
                {duration}
              </span>
            )}
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
  onFocus,
  isInterrupting = false,
  error,
  load,
  isSettled = false,
  endedAt,
}: {
  view: LiveViewModel;
  /** Stop the view, carrying the comment the human left — `null` when they left none. */
  onInterrupt?: (note: string | null) => void;
  /** Replace the focused ids of one focusable table in shared engine state. */
  onFocus?: (nodeId: string, itemIds: string[]) => void;
  isInterrupting?: boolean;
  error?: string | null;
  load?: (nodeId: string, from: number, limit: number) => Promise<LiveLogPage>;
  /**
   * The run is OVER and this is its record. Nothing spins, and the section stops
   * being a live region: a picture that cannot change again must not announce
   * itself to a screen reader as one that can.
   */
  isSettled?: boolean;
  /** Record seal time; paired with `created_at` for a frozen settled duration. */
  endedAt?: number;
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
  if (view.classification === 'activity') {
    return <ActivityViewPanel view={view} isSettled={isSettled} endedAt={endedAt} />;
  }

  return (
    <section
      className="overflow-hidden border border-dialog-edge bg-panel"
      role={isSettled ? undefined : 'status'}
      aria-live={isSettled ? undefined : 'polite'}
    >
      <header className="flex items-center gap-2 border-b border-dialog-edge bg-panel-2 px-3 py-2.5">
        {!isSettled && <Spinner tone="accent" />}
        <span className="min-w-0 flex-1">
          <span className="block truncate font-mono text-body font-bold text-white">
            {view.title}
          </span>
          {view.description && (
            <span className="block truncate font-mono text-chip text-dialog-hint">
              <InlineMarkdown>{view.description}</InlineMarkdown>
            </span>
          )}
        </span>
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
            <NodeCell node={node} load={load} onFocus={onFocus} />
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
      setViews((current) => applyLiveViewEvent(current, event));
      if (event.type === LIVE_VIEW_CLOSE_EVENT) revealRecord();
    });
    return () => {
      cancelled = true;
      revision.current += 1;
      controller.abort();
      stopConnection();
      stopEvents();
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
      .interruptLiveView(sid, viewId, note ?? undefined)
      .catch(() => setError('That view would not stop. It may have just finished.'))
      .finally(() => setStopping(null));
  };

  const focus = (viewId: string, nodeId: string, itemIds: string[]) => {
    setError(null);
    client
      .focusLiveView(sid, viewId, nodeId, itemIds)
      .catch(() => setError('That job could not be focused. It may have just finished.'));
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
          onFocus={(nodeId, itemIds) => focus(view.id, nodeId, itemIds)}
          load={readLog(view.id)}
        />
      ))}
    </div>
  );
}

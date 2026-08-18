import { useEffect, useState } from 'react';
import { Button, Input, LoadMore, Meter, Spinner } from './ui';
import type { GatewayClient } from '../lib/gateway';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import {
  applyLiveViewEvent,
  LIVE_NOTE_CHARS,
  isLiveViewEvent,
  liveFraction,
  livePercent,
  orderedRows,
  type LiveLinkNode,
  type LiveLogNode,
  type LiveLogPage,
  type LiveNode,
  type LiveProgressNode,
  type LiveStatNode,
  type LiveStatusNode,
  type LiveStepsNode,
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
 * A view LEAVES when it ends. The record keeps every line, the model is handed
 * the closing picture as data, and a finished panel on a phone is a screen
 * telling the operator about work that is over — so the rows go back to the
 * transcript the moment the close event lands, exactly as the band does.
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

function StatusRow({ node }: { node: LiveStatusNode }) {
  return (
    <p className="flex min-w-0 items-baseline gap-2 font-mono text-ui">
      <ToneMark tone={node.tone} />
      <span className={`min-w-0 flex-1 ${TONE_INK[node.tone]}`}>{node.text}</span>
      {node.detail && <span className="shrink-0 text-chip text-dialog-hint">{node.detail}</span>}
    </p>
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
          <dt className="text-chip text-dialog-hint">{stat.label}</dt>
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
          <span className={`min-w-0 flex-1 truncate ${TONE_INK[step.tone]}`}>{step.label}</span>
          {step.value && <span className="shrink-0 font-bold text-white">{step.value}</span>}
          {step.detail && <span className="shrink-0 text-dialog-hint">{step.detail}</span>}
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
function TableRows({ node }: { node: LiveTableNode }) {
  const rows = orderedRows(node);
  return (
    <div className="-mx-1 overflow-x-auto">
      <table className="w-full min-w-0 border-collapse font-mono text-chip">
        <thead>
          <tr className="border-b border-dialog-edge">
            {node.columns.map((column) => (
              <th
                key={column.id}
                scope="col"
                className={`px-1 py-1 font-bold uppercase tracking-[0.08em] text-dialog-hint ${
                  column.align === 'right' ? 'text-right' : 'text-left'
                }`}
              >
                {column.label}
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {rows.map((row) => (
            <tr key={row.id} className={TONE_INK[row.tone]}>
              {node.columns.map((column, cell) => (
                <td
                  key={column.id}
                  className={`px-1 py-1 align-top ${
                    column.align === 'right' ? 'text-right tabular-nums' : 'text-left'
                  }`}
                >
                  {row.cells[cell] ?? ''}
                </td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
      {rows.length === 0 && <Empty>{EMPTY_LINE.table}</Empty>}
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
              {link.label}
            </a>
          ) : (
            <span className="min-w-0 flex-1 truncate text-white">{link.label}</span>
          )}
          {link.target_kind !== 'url' && (
            <span className="min-w-0 shrink truncate text-dialog-hint">{link.target}</span>
          )}
        </li>
      ))}
    </ul>
  );
}

function NodeRow({
  node,
  load,
}: {
  node: LiveNode;
  load?: (nodeId: string, from: number, limit: number) => Promise<LiveLogPage>;
}) {
  return (
    <li className="min-w-0 space-y-1.5 px-3 py-2.5">
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
      {node.type === 'table' && <TableRows node={node} />}
      {node.type === 'link' && <LinkRows node={node} />}
    </li>
  );
}

/**
 * ONE view, painted. Pure: everything it knows arrived as a prop, which is what
 * lets the whole picture be rendered from the engine's own fixture in a test.
 */
export function LiveViewPanel({
  view,
  onInterrupt,
  isInterrupting = false,
  error,
  load,
}: {
  view: LiveViewModel;
  /** Stop the view, carrying the comment the human left — `null` when they left none. */
  onInterrupt?: (note: string | null) => void;
  isInterrupting?: boolean;
  error?: string | null;
  load?: (nodeId: string, from: number, limit: number) => Promise<LiveLogPage>;
}) {
  // The stop is ARMED before it is sent, exactly as Escape arms it in the
  // terminal: the comment travels WITH the interrupt, so the run reads WHY it
  // was stopped and not merely that it was. `null` is "not armed" — an empty
  // string is an armed stop nobody has typed into yet.
  const [note, setNote] = useState<string | null>(null);
  const isArmed = note !== null;
  const typed = note ?? '';
  return (
    <section
      className="overflow-hidden border border-dialog-edge bg-panel"
      role="status"
      aria-live="polite"
    >
      <header className="flex items-center gap-2 border-b border-dialog-edge bg-panel-2 px-3 py-2.5">
        <Spinner tone="accent" />
        <span className="min-w-0 flex-1">
          <span className="block truncate font-mono text-body font-bold text-white">
            {view.title}
          </span>
          {view.description && (
            <span className="block truncate font-mono text-chip text-dialog-hint">
              {view.description}
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
            setNote(null);
            onInterrupt(typed.trim() === '' ? null : typed.trim());
          }}
          onKeyDown={(event) => {
            // Escape keeps watching — the terminal's own second Escape.
            if (event.key === 'Escape') setNote(null);
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
          <NodeRow key={node.id} node={node} load={load} />
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
 */
export function LiveView({
  client,
  subscriptions,
  sid,
}: {
  client: GatewayClient;
  subscriptions: SessionSubscriptionHub;
  sid: string;
}) {
  const [views, setViews] = useState<LiveViewModel[]>([]);
  const [stopping, setStopping] = useState<string | null>(null);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    let cancelled = false;
    const controller = new AbortController();
    const reload = () => {
      client
        .liveViews(sid, controller.signal)
        .then((open) => {
          if (!cancelled) setViews(open);
        })
        .catch(() => undefined);
    };
    reload();
    const stopConnection = subscriptions.subscribeConnection((connected) => {
      if (connected) reload();
    });
    const stopEvents = subscriptions.subscribeSession(sid, (event) => {
      if (!isLiveViewEvent(event)) return;
      setViews((current) => applyLiveViewEvent(current, event));
    });
    return () => {
      cancelled = true;
      controller.abort();
      stopConnection();
      stopEvents();
    };
  }, [client, sid, subscriptions]);

  // A patch frame that skipped a seq means frames were LOST — the coalescing
  // window states the range it stands for precisely so this is knowable. The
  // picture is behind, so it is RE-READ rather than patched further: a table
  // quietly missing a row is the failure this whole numbering exists to catch.
  const isStale = views.some((view) => view.is_stale === true);
  useEffect(() => {
    if (!isStale) return;
    let cancelled = false;
    const controller = new AbortController();
    client
      .liveViews(sid, controller.signal)
      .then((open) => {
        if (!cancelled) setViews(open);
      })
      .catch(() => undefined);
    return () => {
      cancelled = true;
      controller.abort();
    };
  }, [isStale, client, sid]);

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
          load={readLog(view.id)}
        />
      ))}
    </div>
  );
}

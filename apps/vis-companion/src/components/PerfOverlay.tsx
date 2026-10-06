import { useEffect, useState } from 'react';
import { createRoot } from 'react-dom/client';
import {
  buildHeatmap,
  PERF_BUILD,
  perfReport,
  reloadWithStoredPerf,
  setPerfEnabled,
  type HeatMetric,
  type HeatRow,
  type ListenerGroupReport,
  type PerfReport,
} from '../lib/perf';
import { DotIcon, MinusIcon } from './icons';
import { Banner, Button } from './ui';

const REFRESH_MS = 2_000;
/** Samples kept for the trend lines: three minutes at the default refresh. */
const HISTORY = 90;

interface Sample {
  heap: number | null;
  nodes: number;
  listeners: number;
}

interface Baseline {
  heap: number | null;
  nodes: number;
  listeners: number;
  groups: Map<string, number>;
}

/** `1.2 MB`, `640 KB`, `12 B`. */
export function formatBytes(bytes: number): string {
  if (bytes >= 1024 * 1024 * 1024) return `${(bytes / 1024 / 1024 / 1024).toFixed(1)} GB`;
  if (bytes >= 1024 * 1024) return `${(bytes / 1024 / 1024).toFixed(1)} MB`;
  if (bytes >= 1024) return `${Math.round(bytes / 1024)} KB`;
  return `${bytes} B`;
}

function groupKey(group: ListenerGroupReport): string {
  return `${group.target}\u0000${group.type}\u0000${group.site}`;
}

function signed(delta: number): string {
  return delta > 0 ? `+${delta.toLocaleString()}` : delta.toLocaleString();
}

function openSession(): string | null {
  const match = /^#\/s\/([^?]+)/.exec(typeof location === 'undefined' ? '' : location.hash);
  return match ? decodeURIComponent(match[1]) : null;
}

function Trend({ values, label }: { values: number[]; label: string }) {
  if (values.length < 2) return null;
  const peak = Math.max(...values);
  const floor = Math.min(...values);
  const span = peak - floor || 1;
  const points = values
    .map((value, index) => `${(index / (values.length - 1)) * 100},${20 - ((value - floor) / span) * 18 - 1}`)
    .join(' ');
  return (
    <svg viewBox="0 0 100 20" preserveAspectRatio="none" className="h-5 w-full" role="img" aria-label={label}>
      <polyline points={points} fill="none" stroke="currentColor" strokeWidth="1.5" vectorEffect="non-scaling-stroke" />
    </svg>
  );
}

function Figure({ label, value, delta, warn = false }: { label: string; value: string; delta?: string; warn?: boolean }) {
  return (
    <div className="min-w-0 border border-edge px-2 py-1">
      <div className="truncate text-muted">{label}</div>
      <div className={`truncate font-bold ${warn ? 'text-err' : ''}`}>
        {value}
        {delta ? <span className="ml-1 font-normal text-muted">{delta}</span> : null}
      </div>
    </div>
  );
}

function HeatCell({ value, peak, metric }: { value: number; peak: number; metric: HeatMetric }) {
  if (!value) return <td className="border border-edge px-1.5 py-0.5 text-right text-muted">·</td>;
  const heat = peak > 0 ? Math.log1p(value) / Math.log1p(peak) : 0;
  return (
    <td
      className="border border-edge px-1.5 py-0.5 text-right"
      style={{ background: `color-mix(in srgb, var(--color-err) ${Math.round(8 + heat * 72)}%, transparent)` }}
      title={metric === 'bytes' ? `${value.toLocaleString()} bytes` : `${value.toLocaleString()} items`}
    >
      {metric === 'bytes' ? formatBytes(value) : value.toLocaleString()}
    </td>
  );
}

function HeatRowView({
  row,
  columns,
  peak,
  metric,
  current,
}: {
  row: HeatRow;
  columns: string[];
  peak: number;
  metric: HeatMetric;
  current: boolean;
}) {
  return (
    <tr aria-current={current ? 'true' : undefined} className={current ? 'outline outline-1 outline-accent' : undefined}>
      <th
        scope="row"
        className={`max-w-40 truncate border border-edge px-1.5 py-0.5 text-left ${current ? 'font-bold' : 'font-normal'}`}
        title={row.session ?? row.title}
      >
        {row.title}
      </th>
      {columns.map((column) => (
        <HeatCell key={column} value={row.values[column] ?? 0} peak={peak} metric={metric} />
      ))}
      <td className="border border-edge px-1.5 py-0.5 text-right font-bold">
        {metric === 'bytes' ? formatBytes(row.total) : row.total.toLocaleString()}
      </td>
    </tr>
  );
}

/** The app bar's wordmark carries this attribute; the closed dot sits just after it. */
const WORDMARK = '[data-wordmark]';
/** How often the dot follows the wordmark, which moves with the sidebar and the window. */
const ANCHOR_MS = 500;

interface Anchor {
  left: number;
  top: number;
}

/**
 * Where the closed dot goes: just after the VIS wordmark, at the bar's middle. `null` while
 * the overlay is open or the wordmark is not on screen, as under the splash.
 */
function useWordmarkAnchor(active: boolean): Anchor | null {
  const [anchor, setAnchor] = useState<Anchor | null>(null);
  useEffect(() => {
    if (!active) return undefined;
    const place = () => {
      const box = document.querySelector(WORDMARK)?.getBoundingClientRect();
      const next = box && box.width > 0 ? { left: Math.round(box.right), top: Math.round(box.top + box.height / 2) } : null;
      setAnchor((previous) => (previous?.left === next?.left && previous?.top === next?.top ? previous : next));
    };
    place();
    const timer = setInterval(place, ANCHOR_MS);
    window.addEventListener('resize', place);
    return () => {
      clearInterval(timer);
      window.removeEventListener('resize', place);
    };
  }, [active]);
  return active ? anchor : null;
}

/**
 * Turn the overlay off for the next loads and load the page again. Answers `false` when
 * this device did not save the choice.
 */
function turnOffOverlay(): boolean {
  if (!setPerfEnabled(false)) return false;
  reloadWithStoredPerf();
  return true;
}

/**
 * The memory overlay behind `?perf=1` and `npm run perf`: live platform counters,
 * a heatmap of what each cache holds per session, and what grew since a baseline.
 */
export function PerfOverlay({
  read = perfReport,
  refreshMs = REFRESH_MS,
  turnOff = PERF_BUILD ? null : turnOffOverlay,
  startOpen = false,
}: {
  read?: () => PerfReport;
  /** Milliseconds between readings; `0` keeps the first reading, as a story needs. */
  refreshMs?: number;
  /** Turns the overlay off; `null` in the `perf` build, which always shows it. */
  turnOff?: (() => boolean) | null;
  /** Open the details at once. By default the overlay starts as a dot. */
  startOpen?: boolean;
}) {
  const [report, setReport] = useState<PerfReport>(read);
  const [history, setHistory] = useState<Sample[]>([]);
  const [baseline, setBaseline] = useState<Baseline | null>(null);
  const [metric, setMetric] = useState<HeatMetric>('bytes');
  const [open, setOpen] = useState(startOpen);
  const [copied, setCopied] = useState(false);
  const [turnOffFailed, setTurnOffFailed] = useState(false);

  useEffect(() => {
    if (refreshMs <= 0) return undefined;
    const timer = setInterval(() => {
      const next = read();
      setReport(next);
      setHistory((previous) => [
        ...previous.slice(-(HISTORY - 1)),
        { heap: next.heap?.used ?? null, nodes: next.domNodes, listeners: next.listeners.live },
      ]);
    }, refreshMs);
    return () => clearInterval(timer);
  }, [read, refreshMs]);

  const heap = report.heap?.used ?? null;
  const heatmap = buildHeatmap(open ? report.cells : [], metric);
  const current = openSession();
  const deltas = baseline
    ? report.listeners.groups
        .map((group) => ({ group, delta: group.live - (baseline.groups.get(groupKey(group)) ?? 0) }))
        .filter((entry) => entry.delta > 0)
        .sort((a, b) => b.delta - a.delta)
        .slice(0, 8)
    : [];
  const heapTrend = history.flatMap((sample) => (sample.heap === null ? [] : [sample.heap]));
  const summary = `Memory ${heap === null ? '' : `${formatBytes(heap)} · `}${report.listeners.live.toLocaleString()} listeners`;
  const anchor = useWordmarkAnchor(!open);

  return (
    <section
      aria-label={open ? 'Memory overlay' : undefined}
      className={`pointer-events-auto font-mono text-meta ${
        open
          ? 'flex h-[calc(100dvh-env(safe-area-inset-top)-env(safe-area-inset-bottom)-1rem)] w-[calc(100dvw-env(safe-area-inset-left)-env(safe-area-inset-right)-1rem)] flex-col gap-2 overflow-hidden border border-dialog-edge bg-panel p-2 shadow-float'
          : anchor
            ? 'fixed -translate-y-1/2'
            : ''
      }`}
      style={anchor ? { left: anchor.left, top: anchor.top } : undefined}
    >
      <header className="flex shrink-0 flex-wrap items-center gap-x-2 gap-y-3 mouse:gap-y-2">
        {open ? <h2 className="min-w-0 flex-1 font-bold">Memory</h2> : null}
        {/* Keep the same button mounted so one tap cannot collapse and reopen it. */}
        <Button
          type="button"
          variant="quiet"
          density="compact"
          className="inline-flex max-w-full items-center justify-center mouse:order-last"
          aria-label={open ? 'Minimize memory overlay' : summary}
          title={open ? undefined : summary}
          aria-expanded={open}
          onClick={() => setOpen(!open)}
        >
          {open ? (
            <span className="inline-flex items-center gap-2">
              <MinusIcon className="size-5 mouse:size-4" />
              <span>Minimize</span>
            </span>
          ) : (
            // Closed, the overlay is only a dot: the figures wait behind a click.
            <DotIcon className="size-2.5" />
          )}
        </Button>
        {open ? (
          <div className="flex w-full flex-wrap gap-3 mouse:w-auto mouse:gap-2">
            <Button type="button" variant="quiet" density="compact" onClick={() => setMetric(metric === 'bytes' ? 'entries' : 'bytes')}>
              {metric === 'bytes' ? 'Show items' : 'Show bytes'}
            </Button>
            <Button
              type="button"
              variant="quiet"
              density="compact"
              onClick={() =>
                setBaseline({
                  heap,
                  nodes: report.domNodes,
                  listeners: report.listeners.live,
                  groups: new Map(report.listeners.groups.map((group) => [groupKey(group), group.live])),
                })
              }
            >
              Set baseline
            </Button>
            <Button
              type="button"
              variant="quiet"
              density="compact"
              onClick={() => {
                void navigator.clipboard?.writeText(JSON.stringify(read(), null, 2)).then(() => setCopied(true));
              }}
            >
              {copied ? 'Copied' : 'Copy report'}
            </Button>
            {turnOff ? (
              <Button type="button" variant="quiet" density="compact" onClick={() => setTurnOffFailed(!turnOff())}>
                Turn off
              </Button>
            ) : null}
          </div>
        ) : null}
        {open && turnOffFailed ? (
          <div className="w-full">
            <Banner kind="err">This device did not save the setting.</Banner>
          </div>
        ) : null}
      </header>

      {open ? (
        <div role="region" aria-label="Memory details" tabIndex={0} className="min-h-0 flex-1 space-y-2 overflow-auto overscroll-contain">
          <div className="grid grid-cols-4 gap-1">
            <Figure
              label="JS heap"
              value={heap === null ? 'Not reported' : formatBytes(heap)}
              delta={baseline?.heap != null && heap !== null ? `${heap >= baseline.heap ? '+' : '−'}${formatBytes(Math.abs(heap - baseline.heap))}` : undefined}
            />
            <Figure
              label="Elements"
              value={report.domNodes.toLocaleString()}
              delta={baseline ? signed(report.domNodes - baseline.nodes) : undefined}
            />
            <Figure
              label="Listeners"
              value={report.listeners.live.toLocaleString()}
              delta={baseline ? signed(report.listeners.live - baseline.listeners) : undefined}
            />
            <Figure label="On removed elements" value={report.listeners.detached.toLocaleString()} warn={report.listeners.detached > 0} />
            <Figure label="Intervals" value={report.intervals.live.toLocaleString()} />
            <Figure label="Pending timeouts" value={report.timeouts.pending.toLocaleString()} />
            <Figure
              label="Observed elements"
              value={report.observers.reduce((sum, entry) => sum + entry.targets, 0).toLocaleString()}
              delta={(() => {
                const lost = report.observers.reduce((sum, entry) => sum + entry.detached, 0);
                return lost ? `${lost} removed` : undefined;
              })()}
              warn={report.observers.some((entry) => entry.detached > 0)}
            />
            <Figure
              label="Object URLs"
              value={report.objectUrls.live.toLocaleString()}
              delta={report.objectUrls.bytes ? formatBytes(report.objectUrls.bytes) : undefined}
            />
          </div>

          {heapTrend.length > 1 ? <Trend values={heapTrend} label="JS heap trend" /> : null}
          {history.length > 1 ? <Trend values={history.map((sample) => sample.listeners)} label="Listener trend" /> : null}

          <div className="overflow-auto">
            <table className="w-full border-collapse" aria-label="Memory by session">
              <caption className="pb-1 text-left text-muted">
                What each cache holds per session ({metric === 'bytes' ? 'approximate bytes' : 'items'})
                {heatmap.hidden ? `, ${heatmap.hidden} lighter ${heatmap.hidden === 1 ? 'session' : 'sessions'} not shown` : ''}
              </caption>
              <thead>
                <tr>
                  <th scope="col" className="border border-edge px-1.5 py-0.5 text-left">
                    Session
                  </th>
                  {heatmap.columns.map((column) => (
                    <th key={column} scope="col" className="border border-edge px-1.5 py-0.5 text-right">
                      {column}
                    </th>
                  ))}
                  <th scope="col" className="border border-edge px-1.5 py-0.5 text-right">
                    Total
                  </th>
                </tr>
              </thead>
              <tbody>
                {heatmap.rows.map((row) => (
                  <HeatRowView
                    key={row.session}
                    row={row}
                    columns={heatmap.columns}
                    peak={heatmap.peak}
                    metric={metric}
                    current={row.session === current}
                  />
                ))}
                {heatmap.machine.total > 0 ? (
                  <HeatRowView row={heatmap.machine} columns={heatmap.columns} peak={heatmap.peak} metric={metric} current={false} />
                ) : null}
              </tbody>
            </table>
          </div>

          {baseline ? (
            <div>
              <h3 className="font-bold">Listeners added since the baseline</h3>
              {deltas.length ? (
                <ul>
                  {deltas.map(({ group, delta }) => (
                    <li key={groupKey(group)} className="truncate" title={group.site}>
                      <span className="text-err">+{delta}</span> {group.target} · {group.type} · {group.site}
                    </li>
                  ))}
                </ul>
              ) : (
                <p className="text-muted">None</p>
              )}
            </div>
          ) : (
            <p className="text-muted">Set a baseline, use the app, then look here for what kept growing.</p>
          )}

          <div>
            <h3 className="font-bold">Top listener sources</h3>
            <ul>
              {report.listeners.groups.slice(0, 6).map((group) => (
                <li key={groupKey(group)} className="truncate" title={group.site}>
                  {group.live} · {group.target} · {group.type} · {group.site}
                </li>
              ))}
            </ul>
          </div>
          {report.intervals.sites.length ? (
            <div>
              <h3 className="font-bold">Intervals</h3>
              <ul>
                {report.intervals.sites.slice(0, 6).map((entry) => (
                  <li key={entry.site} className="truncate" title={entry.site}>
                    {entry.live} · {entry.site}
                  </li>
                ))}
              </ul>
            </div>
          ) : null}
        </div>
      ) : null}
    </section>
  );
}

/**
 * Mount the overlay in its own root, outside the app's tree and layout. Its dot sits just
 * after the VIS wordmark, or at the middle of the right edge while the app bar is away.
 */
export function mountPerfOverlay(): void {
  if (document.getElementById('vis-perf')) return;
  const host = document.createElement('div');
  host.id = 'vis-perf';
  host.className =
    'pointer-events-none fixed inset-0 z-[2147483000] flex items-center justify-end pb-[calc(env(safe-area-inset-bottom)+0.5rem)] pl-[calc(env(safe-area-inset-left)+0.5rem)] pr-[calc(env(safe-area-inset-right)+0.5rem)] pt-[calc(env(safe-area-inset-top)+0.5rem)]';
  document.body.append(host);
  createRoot(host).render(<PerfOverlay />);
}

import { useEffect, useState } from 'react';
import { createRoot } from 'react-dom/client';
import {
  buildHeatmap,
  perfReport,
  type HeatMetric,
  type HeatRow,
  type ListenerGroupReport,
  type PerfReport,
} from '../lib/perf';
import { MinusIcon } from './icons';

const REFRESH_MS = 2_000;
const CONTROL_CLASS = 'min-h-11 min-w-11 shrink-0 border border-edge px-2 mouse:min-h-7 mouse:min-w-7';
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

/**
 * The memory overlay behind `?perf=1` and `npm run perf`: live platform counters,
 * a heatmap of what each cache holds per session, and what grew since a baseline.
 */
export function PerfOverlay({
  read = perfReport,
  refreshMs = REFRESH_MS,
}: {
  read?: () => PerfReport;
  /** Milliseconds between readings; `0` keeps the first reading, as a story needs. */
  refreshMs?: number;
}) {
  const [report, setReport] = useState<PerfReport>(read);
  const [history, setHistory] = useState<Sample[]>([]);
  const [baseline, setBaseline] = useState<Baseline | null>(null);
  const [metric, setMetric] = useState<HeatMetric>('bytes');
  const [open, setOpen] = useState(true);
  const [copied, setCopied] = useState(false);

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
  if (!open) {
    return (
      <button
        type="button"
        onClick={() => setOpen(true)}
        className="pointer-events-auto min-h-11 max-w-full border border-dialog-edge bg-panel px-2 py-1 font-mono text-meta shadow-float mouse:min-h-7"
      >
        Memory {heap === null ? '' : `${formatBytes(heap)} · `}
        {report.listeners.live.toLocaleString()} listeners
      </button>
    );
  }

  const heatmap = buildHeatmap(report.cells, metric);
  const current = openSession();
  const deltas = baseline
    ? report.listeners.groups
        .map((group) => ({ group, delta: group.live - (baseline.groups.get(groupKey(group)) ?? 0) }))
        .filter((entry) => entry.delta > 0)
        .sort((a, b) => b.delta - a.delta)
        .slice(0, 8)
    : [];
  const heapTrend = history.flatMap((sample) => (sample.heap === null ? [] : [sample.heap]));

  return (
    <section
      aria-label="Memory overlay"
      className="pointer-events-auto flex max-h-[min(80dvh,calc(100dvh-env(safe-area-inset-top)-env(safe-area-inset-bottom)-1rem))] w-[min(40rem,calc(100dvw-env(safe-area-inset-left)-env(safe-area-inset-right)-1rem))] flex-col gap-2 overflow-hidden border border-dialog-edge bg-panel p-2 font-mono text-meta shadow-float"
    >
      <header className="flex shrink-0 flex-wrap items-center gap-2">
        <h2 className="min-w-0 flex-1 font-bold">Memory</h2>
        <button type="button" className={`${CONTROL_CLASS} mouse:order-last`} aria-label="Minimize memory overlay" onClick={() => setOpen(false)}>
          <MinusIcon />
        </button>
        <div className="flex w-full flex-wrap gap-2 mouse:w-auto">
          <button type="button" className={CONTROL_CLASS} onClick={() => setMetric(metric === 'bytes' ? 'entries' : 'bytes')}>
            {metric === 'bytes' ? 'Show items' : 'Show bytes'}
          </button>
          <button
            type="button"
            className={CONTROL_CLASS}
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
          </button>
          <button
            type="button"
            className={CONTROL_CLASS}
            onClick={() => {
              void navigator.clipboard?.writeText(JSON.stringify(read(), null, 2)).then(() => setCopied(true));
            }}
          >
            {copied ? 'Copied' : 'Copy report'}
          </button>
        </div>
      </header>

      <div role="region" aria-label="Memory details" tabIndex={0} className="min-h-0 space-y-2 overflow-auto overscroll-contain">
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
    </section>
  );
}

/** Mount the overlay in its own root, outside the app's tree and layout. */
export function mountPerfOverlay(): void {
  if (document.getElementById('vis-perf')) return;
  const host = document.createElement('div');
  host.id = 'vis-perf';
  host.className = 'pointer-events-none fixed right-[calc(env(safe-area-inset-right)+0.5rem)] top-[calc(env(safe-area-inset-top)+0.5rem)] z-[2147483000] max-w-[calc(100dvw-env(safe-area-inset-left)-env(safe-area-inset-right)-1rem)]';
  document.body.append(host);
  createRoot(host).render(<PerfOverlay />);
}

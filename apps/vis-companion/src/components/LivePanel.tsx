/**
 * Live views outside the transcript: the side panel of a session. A panel view belongs
 * to an extension and stays while the session is open.
 */

import { useLayoutEffect, useRef, useState, type KeyboardEvent as ReactKeyboardEvent, type ReactNode } from 'react';
import { Chip, CloseButton, Disclosure } from './ui';
import { ResizeEdge } from './ResizeEdge';
import { clampWidth, useChosenWidth } from '../lib/chosen-width';
import { LiveView, ToneMark, runSummary, runTone } from './LiveView';
import type { GatewayClient } from '../lib/gateway';
import type { LiveView as LiveViewModel } from '../lib/live-view';

/**
 * The side panel width in CSS pixels, and the step of one arrow key. A new panel
 * takes `share` of the window between `fitMin` and `fitMax`, like the T3 Code diff
 * panel, so a review toolbar fits on one row.
 */
export const LIVE_PANEL_WIDTH = { min: 280, max: 1200, step: 16, share: 0.42, fitMin: 360, fitMax: 560 } as const;

/** The transcript beside the panel keeps the width of a phone, in CSS pixels. */
const TRANSCRIPT_MIN_WIDTH = 320;

/** Only a width that the person chose is kept. The old key held every first width. */
const WIDTH_KEY = 'vis.live-panel.chosen-width';

/** The extension that owns `view`. A view without an owner stands under `session`. */
export function liveViewOwner(view: LiveViewModel): string {
  return view.extension ?? 'session';
}

/** The width of a panel that the person has not resized, for a window `windowWidth` wide. */
export function initialPanelWidth(windowWidth: number): number {
  const { share, fitMin, fitMax } = LIVE_PANEL_WIDTH;
  return Math.round(Math.min(fitMax, Math.max(fitMin, windowWidth * share)));
}

/** Views as fold rows. A folded view shows only its title and state. */
export function LiveViewList({
  views,
  client,
  sid,
}: {
  views: LiveViewModel[];
  client: GatewayClient;
  /** The session of the views. */
  sid: string;
}) {
  const [folded, setFolded] = useState<ReadonlySet<string>>(() => new Set());
  const toggle = (id: string) =>
    setFolded((held) => {
      const next = new Set(held);
      if (next.has(id)) next.delete(id);
      else next.add(id);
      return next;
    });
  return (
    <ul className="min-w-0">
      {views.map((view) => {
        const isOpen = !folded.has(view.id);
        const summary = isOpen ? '' : runSummary(view.nodes);
        return (
          <li key={view.id} className="min-w-0 border-b border-dialog-edge/60 px-3" data-live-view={view.id}>
            <span className="flex min-w-0 items-center gap-2">
              <ToneMark tone={runTone(view.nodes)} />
              <Disclosure
                className="min-w-0 flex-1"
                isOpen={isOpen}
                tone="branch"
                inlineChevron
                aria-label={view.title}
                tally={
                  summary ? (
                    <span className="min-w-0 truncate font-mono text-meta text-dialog-hint">{summary}</span>
                  ) : undefined
                }
                onClick={() => toggle(view.id)}
              >
                <span className="min-w-0 truncate text-body font-semibold text-white">{view.title}</span>
              </Disclosure>
            </span>
            {isOpen && (
              <div className="-mx-3 min-w-0 pb-2">
                <LiveView views={[view]} client={client} sid={sid} inPanel />
              </div>
            )}
          </li>
        );
      })}
    </ul>
  );
}

/**
 * The panel of live views. With views from two or more extensions, one tab for each
 * extension chooses the views it shows.
 */
export function LivePanel({
  views,
  client,
  sid,
  onClose,
  initialViewId,
}: {
  views: LiveViewModel[];
  client: GatewayClient;
  sid: string;
  onClose: () => void;
  initialViewId?: string;
}) {
  const owners = [...new Set(views.map(liveViewOwner))];
  const [tab, setTab] = useState<string | null>(() => {
    const initial = views.find((view) => view.id === initialViewId);
    return initial ? liveViewOwner(initial) : null;
  });
  const current = tab !== null && owners.includes(tab) ? tab : (owners[0] ?? '');
  const shown = owners.length > 1 ? views.filter((view) => liveViewOwner(view) === current) : views;
  const selectByKey = (event: ReactKeyboardEvent<HTMLDivElement>) => {
    const step = event.key === 'ArrowRight' ? 1 : event.key === 'ArrowLeft' ? -1 : 0;
    if (step === 0) return;
    event.preventDefault();
    const next = owners[(owners.indexOf(current) + step + owners.length) % owners.length];
    setTab(next);
    event.currentTarget.querySelector<HTMLElement>(`[data-owner="${CSS.escape(next)}"]`)?.focus();
  };
  return (
    <aside aria-label="Live views" className="flex h-full min-h-0 min-w-0 flex-col bg-panel">
      {/* The height of the session header beside it: its rule and its close button
          continue the header's line. */}
      <div className="box-content flex min-h-13 shrink-0 items-center gap-2 border-b border-dialog-edge pl-3 pr-[max(0.75rem,env(safe-area-inset-right))]">
        {owners.length > 1 ? (
          <div
            role="tablist"
            aria-label="Extensions"
            className="flex min-w-0 flex-1 items-center gap-1 overflow-x-auto"
            onKeyDown={selectByKey}
          >
            {owners.map((owner) => {
              const isSelected = owner === current;
              return (
                <Chip
                  key={owner}
                  role="tab"
                  data-owner={owner}
                  isOn={isSelected}
                  aria-pressed={undefined}
                  aria-selected={isSelected}
                  tabIndex={isSelected ? 0 : -1}
                  onClick={() => setTab(owner)}
                >
                  {owner}
                </Chip>
              );
            })}
          </div>
        ) : (
          <h2 className="min-w-0 flex-1 truncate font-mono text-body font-semibold text-white">
            {current}
          </h2>
        )}
        <CloseButton label="Close live views" isBand onClick={onClose} />
      </div>
      <div className="min-h-0 flex-1 overflow-y-auto overscroll-contain" role={owners.length > 1 ? 'tabpanel' : undefined}>
        <LiveViewList views={shown} client={client} sid={sid} />
      </div>
    </aside>
  );
}

/**
 * The widest panel in a row `rowWidth` wide: the transcript beside it keeps its width.
 * A row that has no layout yet (0) sets no limit.
 */
function panelMaxWidth(rowWidth: number): number {
  const { min, max } = LIVE_PANEL_WIDTH;
  return rowWidth > 0 ? Math.max(min, Math.min(max, rowWidth - TRANSCRIPT_MIN_WIDTH)) : max;
}

/**
 * A panel beside the transcript. Drag its edge, or use the arrow keys on it, to resize it.
 * The panel stays in its row, so it never hides the transcript or the session list.
 * A row that became narrower shows a narrower panel, and keeps the chosen width.
 */
export function ResizablePanel({ children }: { children: ReactNode }) {
  const box = useRef<HTMLDivElement>(null);
  const [rowWidth, setRowWidth] = useState(0);
  useLayoutEffect(() => {
    const row = box.current?.parentElement;
    if (!row) return;
    const measure = () => setRowWidth(row.clientWidth);
    measure();
    const observer = new ResizeObserver(measure);
    observer.observe(row);
    return () => observer.disconnect();
  }, []);
  const bounds = { ...LIVE_PANEL_WIDTH, max: panelMaxWidth(rowWidth) };
  const [chosen, choose] = useChosenWidth(WIDTH_KEY, bounds);
  const [initial] = useState(() => initialPanelWidth(window.innerWidth));
  const width = clampWidth(chosen ?? initial, bounds);
  return (
    <div ref={box} className="relative flex h-full min-h-0 shrink-0 border-l border-dialog-edge" style={{ width }}>
      <ResizeEdge label="Resize live views" side="start" bounds={bounds} width={width} onWidth={choose} />
      <div className="min-h-0 min-w-0 flex-1">{children}</div>
    </div>
  );
}

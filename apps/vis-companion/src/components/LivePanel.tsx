/**
 * Live views outside the transcript: the side panel of a session and the header pins
 * of the machine. A panel view belongs to an extension and stays while the session is
 * open. A header view belongs to the machine and comes from the fleet stream.
 */

import {
  useEffect,
  useState,
  type KeyboardEvent as ReactKeyboardEvent,
  type PointerEvent as ReactPointerEvent,
  type ReactNode,
} from 'react';
import { Chip, CloseButton, Disclosure, TextButton } from './ui';
import { LiveView, RunDialog, ToneMark, runSummary, runTone } from './LiveView';
import type { GatewayClient } from '../lib/gateway';
import type { LiveView as LiveViewModel } from '../lib/live-view';

/** The header shows this many pins, then one `+N` chip. */
export const HEADER_PIN_LIMIT = 4;

/** The side panel width in CSS pixels, and the step of one arrow key. */
export const LIVE_PANEL_WIDTH = { min: 280, initial: 384, max: 720, step: 16 } as const;

const WIDTH_KEY = 'vis.live-panel.width';

/** The extension that owns `view`. A view without an owner stands under `session`. */
export function liveViewOwner(view: LiveViewModel): string {
  return view.extension ?? 'session';
}

function clampWidth(width: number): number {
  return Math.round(Math.min(LIVE_PANEL_WIDTH.max, Math.max(LIVE_PANEL_WIDTH.min, width)));
}

function storedWidth(): number {
  try {
    const stored = Number(window.localStorage.getItem(WIDTH_KEY));
    return Number.isFinite(stored) && stored > 0 ? clampWidth(stored) : LIVE_PANEL_WIDTH.initial;
  } catch {
    return LIVE_PANEL_WIDTH.initial;
  }
}

/** Views as fold rows. A folded view shows only its title and state. */
export function LiveViewList({
  views,
  client,
  sid,
}: {
  views: LiveViewModel[];
  client: GatewayClient;
  /** The session of the views, or `null` for the header views of the machine. */
  sid: string | null;
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
}: {
  views: LiveViewModel[];
  client: GatewayClient;
  sid: string | null;
  onClose: () => void;
}) {
  const owners = [...new Set(views.map(liveViewOwner))];
  const [tab, setTab] = useState<string | null>(null);
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
      <div className="flex min-h-13 shrink-0 items-center gap-2 border-b border-dialog-edge pl-3 pr-[max(0.75rem,env(safe-area-inset-right))] mouse:min-h-11">
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

/** A panel beside the transcript. Drag its edge, or use the arrow keys on it, to resize it. */
export function ResizablePanel({ children }: { children: ReactNode }) {
  const [width, setWidth] = useState(storedWidth);
  useEffect(() => {
    try {
      window.localStorage.setItem(WIDTH_KEY, String(width));
    } catch {
      // Private browsing can refuse storage. The width then lasts for this page only.
    }
  }, [width]);
  const drag = (event: ReactPointerEvent<HTMLDivElement>) => {
    event.preventDefault();
    const startX = event.clientX;
    const startWidth = width;
    const move = (moved: PointerEvent) => setWidth(clampWidth(startWidth + startX - moved.clientX));
    const stop = () => {
      window.removeEventListener('pointermove', move);
      window.removeEventListener('pointerup', stop);
      window.removeEventListener('pointercancel', stop);
    };
    window.addEventListener('pointermove', move);
    window.addEventListener('pointerup', stop);
    window.addEventListener('pointercancel', stop);
  };
  const resizeByKey = (event: ReactKeyboardEvent<HTMLDivElement>) => {
    const step = event.key === 'ArrowLeft' ? 1 : event.key === 'ArrowRight' ? -1 : 0;
    if (step === 0) return;
    event.preventDefault();
    setWidth((held) => clampWidth(held + step * LIVE_PANEL_WIDTH.step));
  };
  return (
    <div className="relative flex h-full min-h-0 shrink-0 border-l border-dialog-edge" style={{ width }}>
      <div
        role="separator"
        aria-orientation="vertical"
        aria-label="Resize live views"
        aria-valuemin={LIVE_PANEL_WIDTH.min}
        aria-valuemax={LIVE_PANEL_WIDTH.max}
        aria-valuenow={width}
        tabIndex={0}
        className="absolute inset-y-0 -left-1.5 z-10 w-3 cursor-col-resize touch-none focus-visible:bg-accent/40"
        onPointerDown={drag}
        onKeyDown={resizeByKey}
      />
      <div className="min-h-0 min-w-0 flex-1">{children}</div>
    </div>
  );
}

/**
 * The header views as pins: a state mark and a title for each of the first four,
 * then `+N`. A pin opens its view. `+N` opens every pinned view.
 */
export function HeaderPins({ views, client }: { views: LiveViewModel[]; client: GatewayClient }) {
  const [opened, setOpened] = useState<string | null>(null);
  const [isListOpen, setListOpen] = useState(false);
  if (views.length === 0) return null;
  const shown = views.slice(0, HEADER_PIN_LIMIT);
  const more = views.length - shown.length;
  const openedView = views.find((view) => view.id === opened);
  return (
    <>
      <ul aria-label="Pinned views" className="flex min-w-0 shrink items-center gap-1 overflow-hidden">
        {shown.map((view) => (
          <li key={view.id} className="min-w-0 shrink">
            <TextButton
              type="button"
              title={view.title}
              aria-label={`${view.title}${runSummary(view.nodes) ? `, ${runSummary(view.nodes)}` : ''}`}
              className="flex min-w-0 max-w-44 items-center gap-1.5"
              onClick={() => setOpened(view.id)}
            >
              <ToneMark tone={runTone(view.nodes)} />
              <span className="hidden min-w-0 truncate sm:inline">{view.title}</span>
            </TextButton>
          </li>
        ))}
        {more > 0 && (
          <li className="shrink-0">
            <TextButton
              type="button"
              aria-label={`Show all ${views.length} pinned views`}
              onClick={() => setListOpen(true)}
            >
              +{more}
            </TextButton>
          </li>
        )}
      </ul>
      {openedView && (
        <LiveView views={[openedView]} client={client} sid={null} onClose={() => setOpened(null)} />
      )}
      {isListOpen && (
        <RunDialog title="Pinned views" onClose={() => setListOpen(false)}>
          <div className="min-h-0 flex-1 overflow-y-auto">
            <LiveViewList views={views} client={client} sid={null} />
          </div>
        </RunDialog>
      )}
    </>
  );
}

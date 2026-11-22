import { useEffect, useLayoutEffect, useMemo, useRef, useState, type ComponentProps, type RefObject } from 'react';
import { flushSync } from 'react-dom';
import { agentStep, LEADER_STEP, PathStep, type AgentStep } from '../components/AgentCrumb';
import { agentsByTurn, storedLeaderLane, TurnAgents } from '../components/AgentTeam';
import { SessionStatsPanel } from '../components/SessionList';
import {
  OUTLINE_MIN_TURNS,
  OUTLINE_RAIL_CLASS,
  StoredTurnRow,
  TRANSCRIPT_SCROLLER_CLASS,
  transcriptColumnClass,
} from '../components/TranscriptRows';
import { TranscriptOutline } from '../components/TranscriptOutline';
import { SyntaxCodeBlock } from '../components/ChatContent';
import { ChevronIcon } from '../components/icons';
import { BAND_EDGE_CLASS, Disclosure } from '../components/ui';
import type { GatewayClient } from '../lib/gateway';
import {
  failedToolCalls,
  heldTurnSteps,
  turnSteps,
  type ExportTurn,
  type SessionExport,
  type TranscriptExport as TranscriptExportData,
} from '../lib/transcript-export';
import { answerMarkdown, JUMP_MARGIN, outlineEntry } from '../lib/transcript-outline';
import type { TranscriptIteration } from '../lib/types';
import { WorkspaceRootsContext } from '../lib/workspace-roots';

/** One opened subagent session of an export, and the scroll offset of its parent at the click. */
type OpenedAgent = Readonly<{ sid: string; parentTop: number }>;

/**
 * An HTML export: the transcript of the exported session, and of each subagent session below it.
 * A subagent row opens the transcript of its session from the export, so the page needs no
 * gateway. Above an opened subagent, a band shows its path from the leader. The arrow of the band
 * opens the parent, and each earlier step of the path opens its session. A session comes back at
 * the scroll offset that it had. An export that does not hold the session of a row says so in
 * place of the transcript, with the same band.
 */
export function TranscriptExport({ data, client }: { data: TranscriptExportData; client: GatewayClient }) {
  // The opened subagent sessions, from a child of the leader down, and the offset to restore.
  const [view, setView] = useState<{ opened: readonly OpenedAgent[]; top: number }>({ opened: [], top: 0 });
  const { opened } = view;
  const documentAt = (depth: number): SessionExport | undefined =>
    depth === 0 ? data : data.descendants[opened[depth - 1].sid];
  const doc = documentAt(opened.length);
  const steps = opened.map(({ sid }, index): AgentStep => {
    const team = documentAt(index)?.agents ?? [];
    const agent = team.find((row) => row.session_id === sid);
    return agent ? agentStep(agent, team) : { label: data.descendants[sid]?.session.title || sid };
  });
  const openDepth = (depth: number) => setView({ opened: opened.slice(0, depth), top: opened[depth].parentTop });
  return (
    <div className="flex h-full min-h-0 flex-col bg-ink">
      {opened.length > 0 && (
        <nav
          aria-label="Subagent path"
          className="flex min-h-13 shrink-0 items-stretch border-b border-dialog-edge bg-panel-2"
        >
          <button
            type="button"
            aria-label="Back to the parent session"
            className={BAND_EDGE_CLASS}
            onClick={() => openDepth(opened.length - 1)}
          >
            <ChevronIcon back className="size-4" aria-hidden />
          </button>
          <ol className="flex min-w-0 flex-1 flex-wrap items-center gap-x-2 self-center py-1.5 pr-4 text-title font-semibold text-white mouse:text-body">
            {[LEADER_STEP, ...steps].map((step, depth) => (
              <li key={depth} className="flex min-w-0 items-center gap-x-2">
                {depth > 0 && <span aria-hidden="true">→</span>}
                {depth < opened.length ? (
                  <button
                    type="button"
                    className="min-w-0 truncate rounded-none text-left hover:underline focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-accent"
                    onClick={() => openDepth(depth)}
                  >
                    <PathStep step={step} />
                  </button>
                ) : (
                  <span aria-current="page" className="min-w-0 truncate">
                    <PathStep step={step} />
                  </span>
                )}
              </li>
            ))}
          </ol>
        </nav>
      )}
      {doc ? (
        <SessionTranscript
          key={doc.session.id}
          data={doc}
          client={client}
          top={view.top}
          onOpen={(sid, parentTop) => setView({ opened: [...opened, { sid, parentTop }], top: 0 })}
        />
      ) : (
        <div className={`${TRANSCRIPT_SCROLLER_CLASS} bg-ink`} role="region" aria-label="Transcript">
          <div className={transcriptColumnClass({ zen: false, outline: false, empty: true })}>
            <p className="text-body text-dialog-hint">
              This export does not hold the conversation of this subagent. To get it, export the leader session
              again with the current Vis.
            </p>
          </div>
        </div>
      )}
    </div>
  );
}

/**
 * A session transcript as an HTML export draws it: the session screen's reading column, its
 * scroller, its turn rows and its outline rail. `client` reads the artifacts that the export
 * stores, so the rows show them as the screen does. A turn shows the subagents that it started,
 * and a row of them calls `onOpen` with its session and the scroll offset of this transcript. The
 * transcript opens at the offset `top`. A wake from a subagent of this session stays
 * hidden, as on the screen. A turn draws when the reader comes near it (`ExportTurnRow`), so a
 * session of any length opens at once, and its trace then mounts whole. The outline holds every
 * turn. The session's metrics panel, as the session list shows it, stands above the
 * first turn, closed under a METRICS row and as wide as the column when it opens. A closed FAILED
 * TOOLS row under it lists each failed tool call with its error and its code; the step of a call
 * jumps to its turn.
 */
function SessionTranscript({ data, client, top, onOpen }: {
  data: SessionExport;
  client: GatewayClient;
  top: number;
  onOpen: (sid: string, parentTop: number) => void;
}) {
  const { session, turns } = data;
  const scrollRef = useRef<HTMLDivElement>(null);
  const columnRef = useRef<HTMLDivElement>(null);
  useLayoutEffect(() => {
    if (scrollRef.current) scrollRef.current.scrollTop = top;
  }, [top]);
  const anchor = useReadingAnchor(scrollRef, columnRef);
  const near = useNearRows(scrollRef);
  const children = useMemo(() => new Set(data.child_session_ids), [data.child_session_ids]);
  const roots = useMemo(
    () => [session.workspace?.root],
    [session.workspace?.root],
  );
  const outline = useMemo(() => turns.map((turn) => outlineEntry(turn)), [turns]);
  const teams = useMemo(() => agentsByTurn(data.agents), [data.agents]);
  const showsOutline = turns.length >= OUTLINE_MIN_TURNS;
  const [showsMetrics, setShowsMetrics] = useState(false);
  const [showsFailures, setShowsFailures] = useState(false);
  const failures = useMemo(() => failedToolCalls(turns, data.failed_tools), [turns, data.failed_tools]);
  // The jump of the session screen: the turn's top lands `JUMP_MARGIN` below the scroller's top.
  const jumpToTurn = (id: string) => {
    const viewport = scrollRef.current;
    const row = Array.from(columnRef.current?.children ?? []).find(
      (child): child is HTMLElement => child instanceof HTMLElement && child.dataset.turnId === id,
    );
    if (!viewport || !row) return;
    viewport.scrollTop += row.getBoundingClientRect().top - viewport.getBoundingClientRect().top - JUMP_MARGIN;
    // The turns around the target draw now and change height: the target keeps its place.
    anchor.current.pin(id, JUMP_MARGIN);
  };
  return (
    <WorkspaceRootsContext.Provider value={roots}>
      <div className="relative flex min-h-0 flex-1 flex-col bg-ink">
        <div ref={scrollRef} className={`${TRANSCRIPT_SCROLLER_CLASS} bg-ink`} role="region" aria-label="Transcript">
          <div
            ref={columnRef}
            className={transcriptColumnClass({ zen: false, outline: showsOutline, empty: turns.length === 0 })}
          >
            <div className="mb-8">
              <section aria-label="Session metrics" className="border-b border-dialog-edge">
                <Disclosure tone="step" isOpen={showsMetrics} onClick={() => setShowsMetrics((open) => !open)}>
                  Metrics
                </Disclosure>
                {showsMetrics && <SessionStatsPanel session={session} usage={data.usage} phase="ready" wide />}
              </section>
              {failures.length > 0 && (
                <section aria-label="Failed tools" className="border-b border-dialog-edge">
                  <Disclosure
                    tone="step"
                    isOpen={showsFailures}
                    onClick={() => setShowsFailures((open) => !open)}
                    tally={<span className="font-normal text-dialog-hint">{failures.length}</span>}
                  >
                    Failed tools
                  </Disclosure>
                  {showsFailures && (
                    <ol className="divide-y divide-dialog-edge/40 border-t border-dialog-edge font-mono text-ui">
                      {failures.map((failure, index) => (
                        <li key={`${failure.scope}-${index}`} className="py-3">
                          <button
                            type="button"
                            title={failure.message}
                            onClick={() => jumpToTurn(failure.turnId)}
                            className="flex min-h-8 w-full min-w-0 items-baseline gap-2 text-left mouse:min-h-6 mouse:hover:text-accent-ink"
                          >
                            <span className="shrink-0 tabular-nums text-dialog-hint">{failure.scope}</span>
                            <span className="shrink-0 text-dialog-hint" aria-hidden="true">
                              ·
                            </span>
                            <span className="min-w-0 truncate text-err">{failure.message}</span>
                          </button>
                          {failure.code && (
                            <div className="mt-1 max-h-72 overflow-auto bg-code px-3 py-2">
                              <SyntaxCodeBlock
                                value={failure.code}
                                language={failure.language}
                                compact
                                bare
                                frameless
                                padded={false}
                              />
                            </div>
                          )}
                        </li>
                      ))}
                    </ol>
                  )}
                </section>
              )}
            </div>
            {turns.length === 0 && <p className="text-body text-dialog-hint">This session has no turns.</p>}
            {turns.map((turn, index) => (
              <ExportTurnRow
                key={turn.turn_id}
                near={near}
                anchor={anchor}
                turn={turn}
                isFirst={index === 0}
                hidesRequest={
                  turn.request_kind === 'council' &&
                  turn.council?.author_session_id != null &&
                  children.has(turn.council.author_session_id)
                }
                agentName={session.agent_name}
                settled
                whole
                client={client}
                sid={session.id}
                team={
                  teams.has(turn.turn_id) ? (
                    <TurnAgents
                      agents={teams.get(turn.turn_id)!}
                      onOpen={(sid) => onOpen(sid, scrollRef.current?.scrollTop ?? 0)}
                      leader={storedLeaderLane(turn)}
                      clock={turn.team_clock}
                    />
                  ) : undefined
                }
              />
            ))}
          </div>
        </div>
        {showsOutline && (
          <div className={OUTLINE_RAIL_CLASS}>
            <TranscriptOutline
              className="pointer-events-auto"
              entries={outline}
              total={outline.length}
              scroller={scrollRef}
              column={columnRef}
              onJump={jumpToTurn}
            />
          </div>
        )}
      </div>
    </WorkspaceRootsContext.Provider>
  );
}

/** A turn draws when it comes within this margin of the scroller: one and a half screens. */
const NEAR_MARGIN = '150% 0px';

/** Calls one function when a box comes near the reader. */
type NearRows = { observe: (box: Element, onNear: () => void) => () => void };

/**
 * One observer for every turn of a transcript. `observe` calls `onNear` one time, when `box`
 * comes within `NEAR_MARGIN` of the scroller, and at once in an engine without
 * `IntersectionObserver`.
 */
function useNearRows(scrollRef: RefObject<HTMLDivElement | null>): NearRows {
  const [rows] = useState(() => {
    const calls = new Map<Element, () => void>();
    let observer: IntersectionObserver | null = null;
    return {
      observe(box: Element, onNear: () => void) {
        if (typeof IntersectionObserver === 'undefined') {
          onNear();
          return () => {};
        }
        observer ??= new IntersectionObserver(
          (entries) => {
            for (const entry of entries) {
              const call = entry.isIntersecting ? calls.get(entry.target) : undefined;
              if (!call) continue;
              calls.delete(entry.target);
              observer?.unobserve(entry.target);
              call();
            }
          },
          { root: scrollRef.current, rootMargin: NEAR_MARGIN },
        );
        calls.set(box, onNear);
        observer.observe(box);
        return () => {
          calls.delete(box);
          observer?.unobserve(box);
        };
      },
      disconnect() {
        observer?.disconnect();
        calls.clear();
      },
    };
  });
  useEffect(() => () => rows.disconnect(), [rows]);
  return rows;
}

/**
 * `pin` fixes the turn `id` at `offset` pixels below the top of the scroller, after a jump. `hold`
 * runs `change`, which must write the DOM before it returns, and keeps the anchor in place across it.
 */
type ReadingAnchor = { pin: (id: string, offset: number) => void; hold: (change: () => void) => void };
const NO_ANCHOR: ReadingAnchor = { pin: () => {}, hold: (change) => change() };

/**
 * Keep the text that the reader sees in place while turns above it change height: a turn that
 * draws replaces a box of estimated height. The anchor is the first turn that starts in view, else
 * the turn that fills the view, and its offset from the top of the scroller. A box that draws
 * keeps its top, so the anchor is a turn below any box that can still change. When the column
 * changes size, the scroller moves by the distance that the anchor moved. A scroll since the last
 * look sets a new anchor; a jump pins its target at once. A turn that draws uses `hold`: the
 * anchor is read before the change and set back in the same task. A page that becomes shorter
 * pulls the scroller up to its new end before the next resize report, and that report cannot tell
 * this pull from a scroll of the reader.
 */
function useReadingAnchor(
  scrollRef: RefObject<HTMLDivElement | null>,
  columnRef: RefObject<HTMLDivElement | null>,
): RefObject<ReadingAnchor> {
  const handle = useRef<ReadingAnchor>(NO_ANCHOR);
  useLayoutEffect(() => {
    const scroller = scrollRef.current;
    const column = columnRef.current;
    if (!scroller || !column || typeof ResizeObserver === 'undefined') return;
    let anchor: { id: string; offset: number; scrollTop: number } | null = null;
    const rowOf = (id: string) => column.querySelector<HTMLElement>(`:scope > [data-turn-id="${CSS.escape(id)}"]`);
    const capture = () => {
      anchor = null;
      const view = scroller.getBoundingClientRect();
      const rows = column.querySelectorAll<HTMLElement>(':scope > [data-turn-id]');
      // The rows stand in order, so the first one below the top is found in log(n) reads.
      let low = 0;
      let high = rows.length - 1;
      while (low < high) {
        const middle = (low + high) >> 1;
        if (rows[middle].getBoundingClientRect().bottom > view.top) high = middle;
        else low = middle + 1;
      }
      const next = rows[low + 1]?.getBoundingClientRect();
      const row = rows[low] && rows[low].getBoundingClientRect().top < view.top && next && next.top < view.bottom ? rows[low + 1] : rows[low];
      if (!row) return;
      anchor = { id: row.dataset.turnId ?? '', offset: row.getBoundingClientRect().top - view.top, scrollTop: scroller.scrollTop };
    };
    /** Moves the scroller by the distance that the anchor row moved. False when that row is gone. */
    const settle = () => {
      const row = anchor ? rowOf(anchor.id) : null;
      if (!anchor || !row) return false;
      const shift = row.getBoundingClientRect().top - scroller.getBoundingClientRect().top - anchor.offset;
      if (Math.abs(shift) >= 0.5) scroller.scrollTop += shift;
      anchor.scrollTop = scroller.scrollTop;
      return true;
    };
    handle.current = {
      pin: (id, offset) => {
        anchor = { id, offset, scrollTop: scroller.scrollTop };
      },
      hold: (change) => {
        if (!anchor || scroller.scrollTop !== anchor.scrollTop) capture();
        change();
        if (!settle()) capture();
      },
    };
    const restore = () => {
      if (!anchor || scroller.scrollTop !== anchor.scrollTop || !settle()) capture();
    };
    const resizes = new ResizeObserver(restore);
    resizes.observe(column);
    scroller.addEventListener('scroll', capture, { passive: true });
    capture();
    return () => {
      resizes.disconnect();
      scroller.removeEventListener('scroll', capture);
      handle.current = NO_ANCHOR;
    };
  }, [scrollRef, columnRef]);
  return handle;
}

/** The lines of `text` at about `width` characters a line. */
function lineCount(text: string, width: number): number {
  return text.split('\n').reduce((lines, line) => lines + Math.max(1, Math.ceil(line.length / width)), 0);
}

/**
 * The estimated height of a turn that is not drawn yet, in pixels: its request, its answer and
 * one digest row for about every two steps. The reading anchor absorbs the error.
 */
function estimatedHeight(turn: ExportTurn, answer: string): number {
  const request = turn.request ? 56 + 24 * lineCount(turn.request, 90) : 0;
  const steps = Math.min(turn.iteration_count ?? 0, 40);
  return request + 48 + 26 * lineCount(answer, 100) + 22 * steps;
}

/**
 * One turn of an export. Until the reader comes near it, a turn whose steps are in a chunk is a
 * box of its estimated height that holds its request and its answer as plain text, so the find of
 * the browser reaches them in every engine. `content-visibility` skips the layout of a box out of
 * view. Near the reader, the page reads the steps and draws the row, as the session screen does.
 */
function ExportTurnRow({
  turn,
  near,
  anchor,
  ...row
}: Omit<ComponentProps<typeof StoredTurnRow>, 'turn'> & {
  turn: ExportTurn;
  near: NearRows;
  anchor: RefObject<ReadingAnchor>;
}) {
  const [steps, setSteps] = useState<TranscriptIteration[] | undefined>(() => heldTurnSteps(turn));
  const boxRef = useRef<HTMLDivElement>(null);
  useEffect(() => {
    const box = boxRef.current;
    if (steps || !box) return;
    let isLive = true;
    const show = (read: TranscriptIteration[]) => {
      if (isLive) anchor.current.hold(() => flushSync(() => setSteps(read)));
    };
    const draw = () =>
      turnSteps(turn).then(show, (cause: unknown) => {
        console.error(`The steps of turn ${turn.turn_id} cannot be read from the export`, cause);
        show([]);
      });
    const stop = near.observe(box, draw);
    return () => {
      isLive = false;
      stop();
    };
  }, [anchor, near, steps, turn]);
  const drawn = useMemo(() => (steps ? { ...turn, iterations: steps } : null), [steps, turn]);
  const answer = useMemo(() => (drawn ? '' : answerMarkdown(turn.content ?? [])), [drawn, turn.content]);
  if (drawn) return <StoredTurnRow {...row} turn={drawn} />;
  const height = estimatedHeight(turn, answer);
  return (
    <div
      ref={boxRef}
      data-turn-id={turn.turn_id}
      className={`overflow-clip ${row.isFirst ? '' : 'mt-10'}`}
      style={{ height, contentVisibility: 'auto', containIntrinsicSize: `auto ${height}px` }}
    >
      <div className="whitespace-pre-wrap break-words text-body text-dialog-hint">
        {row.hidesRequest ? '' : turn.request}
        {'\n'}
        {answer}
      </div>
    </div>
  );
}

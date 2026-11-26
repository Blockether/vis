import { useLayoutEffect, useMemo, useRef, useState, type ComponentProps, type RefObject } from 'react';
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
  readTurnSteps,
  turnAttachments,
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
  const near = useNearRows(scrollRef, columnRef, anchor);
  const roots = useMemo(
    () => [session.workspace?.root],
    [session.workspace?.root],
  );
  const outline = useMemo(() => turns.map(outlineEntry), [turns]);
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
                          {failure.code && <FailureCode near={near} code={failure.code} language={failure.language} />}
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
                turn={turn}
                isFirst={index === 0}
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

/** A turn draws when it comes within this many screens of the scroller. */
const NEAR_SCREENS = 1.5;
const NEAR_MARGIN = `${NEAR_SCREENS * 100}% 0px`;

/** The most passes of `fill`: a drawn turn that is shorter than its box can show the next box. */
const FILL_PASSES = 16;

/**
 * Draws a box when it comes near the reader, after `prepare`, or at once when it is in view.
 * `sees` tells whether a box is near the reader now.
 */
type NearRows = {
  observe: (box: Element, onNear: () => void, prepare: () => Promise<void>) => () => void;
  sees: (box: Element) => boolean;
};

/**
 * One observer for every turn of a transcript. `observe` calls `onNear`, which must draw its turn
 * synchronously, one time: when `box` comes within `NEAR_MARGIN` of the scroller and `prepare`
 * has read what the draw needs, and at once in an engine without `IntersectionObserver`. The
 * observer reports after a frame, so it draws the turns ahead of the reader, and their reads stay
 * out of the frames of the draws. A box that is already in view draws before the next frame: at the
 * first frame of the transcript and in each scroll event, which comes before the frame of that
 * scroll. Thus the reader never sees a box, also after a jump. Each draw holds the reading anchor.
 */
function useNearRows(
  scrollRef: RefObject<HTMLDivElement | null>,
  columnRef: RefObject<HTMLDivElement | null>,
  anchor: RefObject<ReadingAnchor>,
): NearRows {
  const [rows] = useState(() => {
    const calls = new Map<Element, () => void>();
    const preparations = new Map<Element, () => Promise<void>>();
    let observer: IntersectionObserver | null = null;
    const take = (box: Element) => {
      const call = calls.get(box);
      if (call) {
        calls.delete(box);
        preparations.delete(box);
        observer?.unobserve(box);
      }
      return call;
    };
    const draw = (due: (() => void)[]) => {
      if (due.length) anchor.current.hold(() => flushSync(() => due.forEach((call) => call())));
    };
    /** The calls of the waiting boxes in view. The boxes stand in order: log(n) reads find the first. */
    const inView = () => {
      const scroller = scrollRef.current;
      const column = columnRef.current;
      if (!scroller || !column || calls.size === 0) return [];
      const view = scroller.getBoundingClientRect();
      const boxes = column.querySelectorAll<HTMLElement>(':scope > [data-turn-id]');
      let low = 0;
      let high = boxes.length;
      while (low < high) {
        const middle = (low + high) >> 1;
        if (boxes[middle].getBoundingClientRect().bottom > view.top) high = middle;
        else low = middle + 1;
      }
      const due: (() => void)[] = [];
      for (let at = low; at < boxes.length && boxes[at].getBoundingClientRect().top < view.bottom; at++) {
        const call = take(boxes[at]);
        if (call) due.push(call);
      }
      return due;
    };
    return {
      observe(box: Element, onNear: () => void, prepare: () => Promise<void>) {
        if (typeof IntersectionObserver === 'undefined') {
          onNear();
          return () => {};
        }
        observer ??= new IntersectionObserver(
          (entries) => {
            for (const { target, isIntersecting } of entries) {
              const ready = isIntersecting ? preparations.get(target) : undefined;
              if (!ready) continue;
              preparations.delete(target);
              observer?.unobserve(target);
              void ready().then(() => draw([take(target) ?? []].flat()));
            }
          },
          { root: scrollRef.current, rootMargin: NEAR_MARGIN },
        );
        calls.set(box, onNear);
        preparations.set(box, prepare);
        observer.observe(box);
        return () => {
          calls.delete(box);
          preparations.delete(box);
          observer?.unobserve(box);
        };
      },
      sees(box: Element) {
        const scroller = scrollRef.current;
        if (!scroller) return true;
        const view = scroller.getBoundingClientRect();
        const rect = box.getBoundingClientRect();
        const margin = NEAR_SCREENS * view.height;
        return rect.bottom >= view.top - margin && rect.top <= view.bottom + margin;
      },
      /** Draws each waiting box in view, until no box is in view. */
      fill() {
        for (let pass = 0; pass < FILL_PASSES; pass++) {
          const due = inView();
          if (!due.length) return;
          draw(due);
        }
      },
      disconnect() {
        observer?.disconnect();
        calls.clear();
        preparations.clear();
      },
    };
  });
  useLayoutEffect(() => {
    const scroller = scrollRef.current;
    if (!scroller) return;
    scroller.addEventListener('scroll', rows.fill, { passive: true });
    // After this commit and before its frame: React cannot draw again inside its own commit.
    queueMicrotask(rows.fill);
    return () => {
      scroller.removeEventListener('scroll', rows.fill);
      rows.disconnect();
    };
  }, [rows, scrollRef]);
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

/**
 * The code of one failed tool call. It is plain text until it comes near the reader, and then
 * Prism colours it: a list of some hundred calls opens without a stall. A box near the reader
 * when the list opens has its colours in the first frame. Plain text has the same lines, so the
 * box keeps its height.
 */
function FailureCode({ near, code, language }: { near: NearRows; code: string; language: string }) {
  const [isNear, setNear] = useState(false);
  const boxRef = useRef<HTMLDivElement>(null);
  useLayoutEffect(() => {
    const box = boxRef.current;
    if (isNear || !box) return;
    if (near.sees(box)) {
      setNear(true);
      return;
    }
    return near.observe(box, () => setNear(true), () => Promise.resolve());
  }, [isNear, near]);
  return (
    <div ref={boxRef} className="mt-1 max-h-72 overflow-auto bg-code px-3 py-2">
      <SyntaxCodeBlock value={code} language={isNear ? language : ''} compact bare frameless padded={false} />
    </div>
  );
}

/** The lines of `text` at about `width` characters a line. */
function lineCount(text: string, width: number): number {
  return text.split('\n').reduce((lines, line) => lines + Math.max(1, Math.ceil(line.length / width)), 0);
}

/**
 * The estimated height of a turn that is not drawn yet, in pixels: its request, its answer and
 * one digest row for about every two steps. The reading anchor absorbs the error.
 */
function estimatedHeight(turn: ExportTurn, answer: string, requestLines: number): number {
  const request = requestLines > 0 ? 56 + 24 * requestLines : 0;
  const steps = Math.min(turn.iteration_count ?? 0, 40);
  return request + 48 + 26 * lineCount(answer, 100) + 22 * steps;
}

/**
 * One turn of an export. Until the reader comes near it, a turn whose steps are in a chunk is a
 * box of its estimated height that holds its request and its answer as plain text, so the find of
 * the browser reaches them in every engine. `content-visibility` skips the layout of a box out of
 * view. Near the reader, the page reads the steps and draws the row, as the session screen does,
 * before a frame shows the box (`useNearRows`).
 */
function ExportTurnRow({
  turn,
  near,
  ...row
}: Omit<ComponentProps<typeof StoredTurnRow>, 'turn'> & {
  turn: ExportTurn;
  near: NearRows;
}) {
  const [steps, setSteps] = useState<TranscriptIteration[] | undefined>(() => heldTurnSteps(turn));
  const boxRef = useRef<HTMLDivElement>(null);
  // Before the frame: a box in view at the first frame draws in that frame.
  useLayoutEffect(() => {
    const box = boxRef.current;
    if (steps || !box) return;
    return near.observe(
      box,
      () => {
        try {
          setSteps(turnSteps(turn));
        } catch (cause) {
          console.error(`The steps of turn ${turn.turn_id} cannot be read from the export`, cause);
          setSteps([]);
        }
      },
      () => readTurnSteps(turn),
    );
  }, [near, steps, turn]);
  const drawn = useMemo(
    () => (steps ? { ...turn, attachments: turnAttachments(turn), iterations: steps } : null),
    [steps, turn],
  );
  const answer = useMemo(() => (drawn ? '' : answerMarkdown(turn.content ?? [])), [drawn, turn.content]);
  if (drawn) return <StoredTurnRow {...row} turn={drawn} />;
  const isCouncil = turn.request_kind === 'council';
  const note = isCouncil ? turn.council?.note : undefined;
  const request = note ?? (isCouncil ? turn.council?.content : undefined) ?? turn.request;
  const height = estimatedHeight(turn, answer, note !== undefined ? 1 : request ? lineCount(request, 90) : 0);
  return (
    <div
      ref={boxRef}
      data-turn-id={turn.turn_id}
      className={`overflow-clip ${row.isFirst ? '' : 'mt-10'}`}
      style={{ height, contentVisibility: 'auto', containIntrinsicSize: `auto ${height}px` }}
    >
      <div className="whitespace-pre-wrap break-words text-body text-dialog-hint">
        {isCouncil && <div>Council</div>}
        {note !== undefined ? <div className="truncate">{note}</div> : request}
        {'\n'}
        {answer}
      </div>
    </div>
  );
}

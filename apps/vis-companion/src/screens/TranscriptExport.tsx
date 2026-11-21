import { useLayoutEffect, useMemo, useRef, useState } from 'react';
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
  type SessionExport,
  type TranscriptExport as TranscriptExportData,
} from '../lib/transcript-export';
import { JUMP_MARGIN, outlineEntry } from '../lib/transcript-outline';
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
 * hidden, as on the screen. The rows are complete, so every trace mounts whole and the outline
 * holds every turn. The session's metrics panel, as the session list shows it, stands above the
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
  const failures = useMemo(() => failedToolCalls(turns), [turns]);
  // The jump of the session screen: the turn's top lands `JUMP_MARGIN` below the scroller's top.
  const jumpToTurn = (id: string) => {
    const viewport = scrollRef.current;
    const row = Array.from(columnRef.current?.children ?? []).find(
      (child): child is HTMLElement => child instanceof HTMLElement && child.dataset.turnId === id,
    );
    if (!viewport || !row) return;
    viewport.scrollTop += row.getBoundingClientRect().top - viewport.getBoundingClientRect().top - JUMP_MARGIN;
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
              <StoredTurnRow
                key={turn.turn_id}
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

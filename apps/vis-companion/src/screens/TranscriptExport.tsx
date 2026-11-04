import { useMemo, useRef, useState } from 'react';
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
import { Disclosure } from '../components/ui';
import type { GatewayClient } from '../lib/gateway';
import type { TranscriptExport as TranscriptExportData } from '../lib/transcript-export';
import { JUMP_MARGIN, outlineEntry } from '../lib/transcript-outline';
import { WorkspaceRootsContext } from '../lib/workspace-roots';

/**
 * A session transcript as an HTML export draws it: the session screen's reading column, its
 * scroller, its turn rows and its outline rail. `client` reads the artifacts that the export
 * stores, so the rows show them as the screen does. A turn shows the subagents that it started,
 * but a row of them opens nothing, because the export holds only this session. A wake from a subagent of this session stays
 * hidden, as on the screen. The rows are complete, so every trace mounts whole and the outline
 * holds every turn. The session's metrics panel, as the session list shows it, stands above the
 * first turn, closed under a METRICS row and as wide as the column when it opens.
 */
export function TranscriptExport({ data, client }: { data: TranscriptExportData; client: GatewayClient }) {
  const { session, turns } = data;
  const scrollRef = useRef<HTMLDivElement>(null);
  const columnRef = useRef<HTMLDivElement>(null);
  const children = useMemo(() => new Set(data.child_session_ids), [data.child_session_ids]);
  const roots = useMemo(
    () => [session.workspace?.root, session.workspace?.repo_root],
    [session.workspace?.root, session.workspace?.repo_root],
  );
  const outline = useMemo(() => turns.map((turn) => outlineEntry(turn)), [turns]);
  const teams = useMemo(() => agentsByTurn(data.agents), [data.agents]);
  const showsOutline = turns.length >= OUTLINE_MIN_TURNS;
  const [showsMetrics, setShowsMetrics] = useState(false);
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
      <div className="relative flex h-full min-h-0 flex-col bg-ink">
        <div ref={scrollRef} className={`${TRANSCRIPT_SCROLLER_CLASS} bg-ink`} role="region" aria-label="Transcript">
          <div
            ref={columnRef}
            className={transcriptColumnClass({ zen: false, outline: showsOutline, empty: turns.length === 0 })}
          >
            <section aria-label="Session metrics" className="mb-8">
              <Disclosure tone="step" isOpen={showsMetrics} onClick={() => setShowsMetrics((open) => !open)}>
                Metrics
              </Disclosure>
              {showsMetrics && <SessionStatsPanel session={session} usage={data.usage} phase="ready" wide />}
            </section>
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
                    <TurnAgents agents={teams.get(turn.turn_id)!} onOpen={() => {}} leader={storedLeaderLane(turn)} />
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

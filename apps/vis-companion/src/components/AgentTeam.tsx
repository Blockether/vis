import { useCallback, useEffect, useState } from 'react';
import type { GatewayClient } from '../lib/gateway';
import type { Subagent } from '../lib/types';
import {
  CircleAlertIcon,
  CircleCheckIcon,
  CircleDashedIcon,
  CircleDotIcon,
  ChevronIcon,
  CircleXIcon,
  MARK_NUDGE,
} from './icons';

// Each state is a SHAPE, with colour that agrees, like the live-view rail.
// Colour alone is not a state for every reader.
function agentMark(agent: Subagent): { Mark: typeof CircleDotIcon; ink: string; text: string } {
  if (agent.pending_input) {
    const open = agent.pending_input_count ?? 1;
    return { Mark: CircleAlertIcon, ink: 'text-warn', text: open > 1 ? `needs you ×${open}` : 'needs you' };
  }
  if (agent.question) return { Mark: CircleDotIcon, ink: 'text-warn', text: 'asks leader' };
  switch (agent.status) {
    case 'running':
      return { Mark: CircleDotIcon, ink: 'text-accent-ink', text: `${agent.iterations_used}/${agent.iteration_budget}` };
    case 'completed':
      return { Mark: CircleCheckIcon, ink: 'text-ok', text: 'done' };
    case 'failed':
      return { Mark: CircleXIcon, ink: 'text-err', text: 'failed' };
    case 'queued':
      return { Mark: CircleDashedIcon, ink: 'text-dialog-hint', text: 'queued' };
    case 'cancelled':
      return { Mark: CircleDashedIcon, ink: 'text-dialog-hint', text: 'stopped' };
    default:
      return { Mark: CircleAlertIcon, ink: 'text-dialog-hint', text: 'budget used' };
  }
}

// A finished agent shows only its green check.
const quiet = (agent: Subagent) => agent.status === 'completed' && !agent.pending_input && !agent.question;

const isLive = (agent: Subagent) => ['queued', 'running'].includes(agent.status);

// Actions read as text, so they start on the same edge as the lines they act on.
const LINK = 'rounded-none font-bold hover:underline focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-accent disabled:opacity-50';

/**
 * The subagents that one turn started, in that turn of the transcript. The rows
 * stay where the work happened: live while the turn runs, then for review.
 */
export function TurnAgents({ agents, pending, error, onOpen, onCancel }: {
  agents: readonly Subagent[];
  pending: string | null;
  error?: string | null;
  onOpen: (sid: string) => void;
  onCancel: (sid: string) => void;
}) {
  const [open, setOpen] = useState<string | null>(null);
  const [confirm, setConfirm] = useState<string | null>(null);
  if (agents.length === 0) return null;
  // The state column is as wide as its longest shown state, so tasks start near the left.
  const stateWidth = Math.max(0, ...agents.filter((agent) => !quiet(agent)).map((agent) => agentMark(agent).text.length));
  return (
    <ul aria-label="Subagents" className="my-3 space-y-1 font-mono text-ui text-white" data-transcript-part>
      {agents.map((agent, index) => {
        const { Mark, ink, text } = agentMark(agent);
        const shown = open === agent.session_id;
        const route = `${agent.provider ? `${agent.provider} / ` : ''}${agent.model ?? 'Router default'}`;
        return (
          <li key={agent.session_id}>
            <button
              type="button"
              aria-expanded={shown}
              onClick={() => setOpen(shown ? null : agent.session_id)}
              className="group flex w-full min-w-0 items-center gap-2 rounded-none text-left focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-inset focus-visible:ring-accent"
            >
              <ChevronIcon open={shown} className={`size-3 shrink-0 ${MARK_NUDGE} text-dialog-hint`} />
              <Mark className={`size-3 shrink-0 ${MARK_NUDGE} ${ink}`} />
              <span className="w-[3ch] shrink-0 font-bold">A{index + 1}</span>
              {/* The green check already says done; the word stays for screen readers only. */}
              {stateWidth > 0 ? (
                <span
                  style={{ width: `${stateWidth}ch` }}
                  className={`shrink-0 tabular-nums ${agent.pending_input || agent.question ? 'text-warn' : 'text-dialog-hint'}`}
                >
                  {quiet(agent) ? <span className="sr-only">{text}</span> : text}
                </span>
              ) : <span className="sr-only">{text}</span>}
              <span className="min-w-0 flex-1 truncate group-hover:underline">{agent.task}</span>
            </button>
            {shown && (
              <div className="ml-10 mt-1 space-y-1 pb-2">
                <p className="whitespace-pre-wrap break-words">{agent.task}</p>
                {agent.question && <p className="break-words text-warn">{agent.question}</p>}
                <div className="flex flex-wrap items-baseline justify-between gap-x-4 gap-y-1">
                  <span className="min-w-0 truncate text-dialog-hint" title={route}>
                    {agent.model ?? 'Router default'}
                    {agent.routing_locked && ' · Human model lock'}
                    {` · ${agent.iterations_used} / ${agent.iteration_budget} iterations`}
                    {agent.usage?.cost_usd != null && ` · $${agent.usage.cost_usd.toFixed(4)}`}
                  </span>
                  <span className="flex shrink-0 gap-4">
                    <button type="button" className={LINK} onClick={() => onOpen(agent.session_id)}>Inspect agent</button>
                    {isLive(agent) && (confirm === agent.session_id ? <>
                      <button
                        type="button"
                        className={`${LINK} text-err-ink`}
                        disabled={pending === agent.session_id}
                        onClick={() => { setConfirm(null); onCancel(agent.session_id); }}
                      >
                        Confirm stop
                      </button>
                      <button type="button" className={LINK} onClick={() => setConfirm(null)}>Keep working</button>
                    </> : (
                      <button
                        type="button"
                        className={LINK}
                        disabled={pending === agent.session_id}
                        onClick={() => setConfirm(agent.session_id)}
                      >
                        Stop agent
                      </button>
                    ))}
                  </span>
                </div>
                {confirm === agent.session_id && (
                  <p className="text-dialog-hint">Stops this agent and its descendants, including queued work.</p>
                )}
              </div>
            )}
          </li>
        );
      })}
      {error && <li role="alert" className="text-err-ink">{error}</li>}
    </ul>
  );
}

/** The session's subagents grouped by the turn that started them (`team_id`). */
export function agentsByTurn(agents: readonly Subagent[]): ReadonlyMap<string, Subagent[]> {
  const byTurn = new Map<string, Subagent[]>();
  for (const agent of agents) byTurn.set(agent.team_id, [...(byTurn.get(agent.team_id) ?? []), agent]);
  return byTurn;
}

/**
 * One reader for a session's subagents. It polls while a turn runs or an agent
 * works or waits, and reads again when `generation` changes.
 */
export function useAgentTeam(client: GatewayClient, sid: string, running: boolean, generation = 0) {
  const [state, setState] = useState<{ sid: string; agents: Subagent[] }>({ sid, agents: [] });
  const [pending, setPending] = useState<string | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [revision, setRevision] = useState(0);
  const current = state.sid === sid ? state.agents : [];
  const live = running || current.some((agent) => isLive(agent) || agent.pending_input || agent.question);
  useEffect(() => {
    const controller = new AbortController();
    let fetching = false;
    const refresh = async () => {
      if (fetching || controller.signal.aborted || document.hidden) return;
      fetching = true;
      try {
        const rows = await client.agents(sid, controller.signal);
        if (!controller.signal.aborted && Array.isArray(rows)) setState({ sid, agents: rows });
      } catch {
        // The rows that the reader has stay; the next read tries again.
      } finally {
        fetching = false;
      }
    };
    void refresh();
    const timer = live ? window.setInterval(() => { void refresh(); }, 3000) : undefined;
    document.addEventListener('visibilitychange', refresh);
    return () => {
      controller.abort();
      if (timer !== undefined) window.clearInterval(timer);
      document.removeEventListener('visibilitychange', refresh);
    };
  }, [client, sid, revision, generation, live]);
  const cancel = useCallback(async (child: string) => {
    setPending(child);
    setError(null);
    try {
      await client.cancelAgent(sid, child);
    } catch (cause) {
      setError(cause instanceof Error ? cause.message : 'Could not stop agent. Try again.');
    } finally {
      setPending(null);
      setRevision((value) => value + 1);
    }
  }, [client, sid]);
  return { agents: current, pending, error, cancel };
}

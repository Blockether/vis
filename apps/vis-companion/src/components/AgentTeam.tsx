import { useCallback, useEffect, useState } from 'react';
import agentsSchema from '../../../../packages/vis-contract/resources/vis-contract/schema/agents.json';
import type { GatewayClient } from '../lib/gateway';
import type { AgentTimeline, AgentTimelineItem, Subagent } from '../lib/types';
import { Button, DialogFrame, Modal } from './ui';

/** The roles of the agents contract, in its order. A role is a label: it sets no limit. */
export const ROLES: readonly { const: string; title: string }[] = agentsSchema.$defs.role.oneOf;

/** The agent waits for a person or for its parent: a form, a question or a request. */
export const isAsking = (agent: Subagent) =>
  Boolean(agent.pending_input || agent.question || agent.budget_request || agent.files_request);

// The agent asks its parent, not a person: a question, more iterations or files.
const asksParent = (agent: Subagent) => Boolean(agent.question || agent.budget_request || agent.files_request);

export const isLive = (agent: Subagent) => ['queued', 'running'].includes(agent.status);

/** The agents of one team in spawn order: by `created_at`, then in the order of the gateway. */
export function spawnOrder(agents: readonly Subagent[]): Subagent[] {
  return agents
    .map((agent, index) => ({ agent, index }))
    .sort((a, b) => (a.agent.created_at ?? 0) - (b.agent.created_at ?? 0) || a.index - b.index)
    .map(({ agent }) => agent);
}

/** The name of an agent for people: its title, else `#N`, its place in the spawn order of its team. */
export function agentLabel(agent: Subagent, agents: readonly Subagent[]): string {
  if (agent.title?.trim()) return agent.title.trim();
  const team = spawnOrder(agents.filter((other) => other.team_id === agent.team_id));
  return `#${team.findIndex((other) => other.session_id === agent.session_id) + 1}`;
}

/** The status words of an agent, with the theme ink that agrees with them. Text only, no marks. */
export function agentStatus(agent: Subagent): { text: string; ink: string } {
  if (agent.pending_input) return { text: 'needs you', ink: 'text-warn' };
  if (asksParent(agent)) return { text: 'asking', ink: 'text-warn' };
  switch (agent.status) {
    case 'queued':
      return { text: 'queued', ink: 'text-dialog-hint' };
    case 'running':
      return { text: 'running', ink: 'text-accent-ink' };
    case 'completed':
      if (agent.outcome === 'passed') return { text: 'passed', ink: 'text-ok-ink' };
      if (agent.outcome === 'failed') return { text: 'failed', ink: 'text-err-ink' };
      return { text: 'done', ink: 'text-ok-ink' };
    case 'failed':
      return { text: 'failed', ink: 'text-err-ink' };
    case 'cancelled':
      return { text: 'cancelled', ink: 'text-dialog-hint' };
    default:
      return { text: 'out of budget', ink: 'text-warn' };
  }
}

// The start of the current task: the spawn, or the last resume.
function startOf(agent: Subagent): number | undefined {
  const marks = [agent.created_at, agent.resumed_at].filter((mark): mark is number => typeof mark === 'number');
  return marks.length ? Math.max(...marks) : undefined;
}

/**
 * The time of the current task in ms: the time so far while the agent runs, the
 * duration when its task ended. A queued agent has no time yet.
 */
export function agentTime(agent: Subagent, now: number): number | undefined {
  const start = startOf(agent);
  if (start === undefined || agent.status === 'queued') return undefined;
  if (agent.status === 'running') return Math.max(0, now - start);
  return agent.finished_at == null ? undefined : Math.max(0, agent.finished_at - start);
}

/** `45s`, `3m 04s`, `1h 02m`. */
export function formatAgentTime(ms: number): string {
  const seconds = Math.floor(ms / 1000);
  if (seconds < 60) return `${seconds}s`;
  const minutes = Math.floor(seconds / 60);
  if (minutes < 60) return `${minutes}m ${String(seconds % 60).padStart(2, '0')}s`;
  return `${Math.floor(minutes / 60)}h ${String(minutes % 60).padStart(2, '0')}m`;
}

/** The second part of a row: the open request, what the agent does now, or its result. */
export function agentLine(agent: Subagent): string | undefined {
  if (asksParent(agent)) return agent.question ?? agent.budget_request?.reason ?? agent.files_request?.reason;
  return isLive(agent) ? agent.activity : agent.result;
}

// The clock of the rows. It ticks once a second only while an agent runs.
function useNow(ticking: boolean): number {
  const [now, setNow] = useState(() => Date.now());
  useEffect(() => {
    if (!ticking) return undefined;
    setNow(Date.now());
    const timer = window.setInterval(() => setNow(Date.now()), 1000);
    return () => window.clearInterval(timer);
  }, [ticking]);
  return now;
}

// One grid for every row: role, name, status, line, time. On a phone the line is the second line.
const ROW = 'group grid w-full min-w-0 items-baseline gap-x-3 rounded-none py-0.5 text-left focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-inset focus-visible:ring-accent';
const COLUMNS_WITH_ROLE = 'grid-cols-[9ch_minmax(0,1fr)_auto_auto] sm:grid-cols-[9ch_minmax(0,24ch)_13ch_minmax(0,1fr)_7ch]';
const COLUMNS = 'grid-cols-[minmax(0,1fr)_auto_auto] sm:grid-cols-[minmax(0,24ch)_13ch_minmax(0,1fr)_7ch]';

/** The props of a team block. `timeline` reads the steps of one child. */
export interface TeamProps {
  agents: readonly Subagent[];
  pending: string | null;
  error?: string | null;
  onOpen: (sid: string) => void;
  onCancel: (sid: string) => void;
  timeline: (child: string) => Promise<AgentTimeline>;
}

/**
 * The subagents of one turn: one row each, in spawn order. A row opens the timeline
 * of its agent. The form of an agent that needs you opens in the form prompt of
 * this session: the gateway sends it to every ancestor session.
 */
export function TurnAgents({ agents, pending, error, onOpen, onCancel, timeline, placement = 'transcript' }: TeamProps & {
  /** The dock above the composer while the team works, or the turn of the transcript after. */
  placement?: 'dock' | 'transcript';
}) {
  const [selected, setSelected] = useState<string | null>(null);
  const now = useNow(agents.some((agent) => agent.status === 'running'));
  if (agents.length === 0) return null;
  const rows = spawnOrder(agents);
  const roles = rows.some((agent) => agent.role);
  const chosen = rows.find((agent) => agent.session_id === selected);
  return (
    <>
      <ul
        aria-label="Subagents"
        className={`font-mono text-meta text-white sm:text-ui ${placement === 'transcript' ? 'my-3' : ''}`}
        {...(placement === 'transcript' ? { 'data-transcript-part': true } : {})}
      >
        {rows.map((agent) => {
          const status = agentStatus(agent);
          const line = agentLine(agent);
          const time = agentTime(agent, now);
          return (
            <li key={agent.session_id}>
              <button
                type="button"
                data-agent-row={agent.session_id}
                onClick={() => setSelected(agent.session_id)}
                className={`${ROW} ${roles ? COLUMNS_WITH_ROLE : COLUMNS}`}
              >
                {roles && <span className="truncate text-dialog-hint">{agent.role ?? ''}</span>}
                <span className="truncate font-bold group-hover:underline" title={agent.task}>{agentLabel(agent, rows)}</span>
                <span className={`whitespace-nowrap ${status.ink}`}>{status.text}</span>
                <span
                  className={`order-last col-span-full truncate text-dialog-hint sm:order-none sm:col-span-1 ${line ? '' : 'hidden sm:block'}`}
                  title={line}
                >
                  {line ?? ''}
                </span>
                <span className="whitespace-nowrap text-right text-dialog-hint tabular-nums">
                  {time === undefined ? '' : formatAgentTime(time)}
                </span>
              </button>
            </li>
          );
        })}
        {error && <li role="alert" className="text-err-ink">{error}</li>}
      </ul>
      {chosen && (
        <AgentTimelineSheet
          agent={chosen}
          label={agentLabel(chosen, rows)}
          now={now}
          pending={pending}
          load={timeline}
          onOpen={(child) => { setSelected(null); onOpen(child); }}
          onCancel={onCancel}
          onClose={() => setSelected(null)}
        />
      )}
    </>
  );
}

const STEP_WORDS: Record<AgentTimelineItem['kind'], string> = {
  started: 'Started',
  continued: 'Continued',
  note: 'Note',
  message: 'Message',
  finished: 'Finished',
};

const pad = (value: number) => String(value).padStart(2, '0');

/** The local clock time of an epoch ms, `14:03:07`. */
export function clockTime(at: number): string {
  const date = new Date(at);
  return `${pad(date.getHours())}:${pad(date.getMinutes())}:${pad(date.getSeconds())}`;
}

/**
 * The steps of one agent with their times, read from the timeline route, and the
 * way into its session. A working agent can also be stopped here.
 */
export function AgentTimelineSheet({ agent, label, now, pending, load, onOpen, onCancel, onClose }: {
  agent: Subagent;
  label: string;
  now: number;
  pending: string | null;
  load: (child: string) => Promise<AgentTimeline>;
  onOpen: (sid: string) => void;
  onCancel: (sid: string) => void;
  onClose: () => void;
}) {
  const [steps, setSteps] = useState<{ items: AgentTimelineItem[] } | { error: string } | null>(null);
  const [confirm, setConfirm] = useState(false);
  const child = agent.session_id;
  // Read again when the agent moves on, not on a clock.
  const progress = `${agent.status}:${agent.iterations_used}:${agent.finished_at ?? ''}:${agent.resumed_at ?? ''}`;
  useEffect(() => {
    let current = true;
    load(child).then(
      (timeline) => { if (current) setSteps({ items: Array.isArray(timeline?.items) ? timeline.items : [] }); },
      (cause: unknown) => {
        if (current) setSteps({ error: cause instanceof Error && cause.message ? cause.message : 'Could not read the timeline. Try again.' });
      },
    );
    return () => { current = false; };
  }, [load, child, progress]);
  const status = agentStatus(agent);
  const time = agentTime(agent, now);
  return (
    <Modal size="fit" within="session" onDismiss={onClose}>
      <DialogFrame title={label} onClose={onClose}>
        <div className="space-y-3 px-4 py-3 font-mono text-meta text-white sm:text-ui" data-agent-timeline>
          <p className="flex flex-wrap gap-x-3">
            {agent.role && <span className="text-dialog-hint">{agent.role}</span>}
            <span className={status.ink}>{status.text}</span>
            {time !== undefined && <span className="text-dialog-hint tabular-nums">{formatAgentTime(time)}</span>}
          </p>
          <p className="whitespace-pre-wrap break-words text-dialog-hint">{agent.task}</p>
          {steps === null ? (
            <p className="text-dialog-hint">Reading the timeline…</p>
          ) : 'error' in steps ? (
            <p role="alert" className="text-err-ink">{steps.error}</p>
          ) : steps.items.length === 0 ? (
            <p className="text-dialog-hint">No steps yet.</p>
          ) : (
            <ol aria-label="Timeline" className="space-y-1">
              {steps.items.map((item, index) => {
                const end = item.kind === 'finished' && item.status
                  ? agentStatus({ ...agent, status: item.status, outcome: item.outcome, pending_input: false, question: undefined, budget_request: undefined, files_request: undefined })
                  : null;
                return (
                  <li key={index} className="grid grid-cols-[8ch_9ch_minmax(0,1fr)] gap-x-3">
                    <time dateTime={new Date(item.at).toISOString()} className="text-dialog-hint tabular-nums">{clockTime(item.at)}</time>
                    <span className="text-dialog-hint">{STEP_WORDS[item.kind] ?? item.kind}</span>
                    <span className="whitespace-pre-wrap break-words">
                      {end && <span className={end.ink}>{end.text}</span>}
                      {end && item.text && ' · '}
                      {item.text}
                    </span>
                  </li>
                );
              })}
            </ol>
          )}
          <div className="flex flex-wrap items-center gap-2 pt-1">
            <Button density="inline" onClick={() => onOpen(child)}>Open session</Button>
            {isLive(agent) && (confirm ? <>
              <Button density="inline" variant="danger" disabled={pending === child} onClick={() => { setConfirm(false); onCancel(child); }}>
                Confirm stop
              </Button>
              <Button density="inline" variant="secondary" onClick={() => setConfirm(false)}>Keep working</Button>
            </> : (
              <Button density="inline" variant="secondary" disabled={pending === child} onClick={() => setConfirm(true)}>
                Stop agent
              </Button>
            ))}
          </div>
          {confirm && <p className="text-dialog-hint">Stops this agent and its descendants, including queued work.</p>}
        </div>
      </DialogFrame>
    </Modal>
  );
}

/** The session's subagents grouped by the turn that started them (`team_id`). */
export function agentsByTurn(agents: readonly Subagent[]): ReadonlyMap<string, Subagent[]> {
  const byTurn = new Map<string, Subagent[]>();
  for (const agent of agents) byTurn.set(agent.team_id, [...(byTurn.get(agent.team_id) ?? []), agent]);
  return byTurn;
}

/**
 * The teams of `sid` in the dock: the team of the running turn, and each team that
 * still has an agent at work or waiting after its turn ended. Spawn order.
 */
export function dockTeams(agents: readonly Subagent[], sid: string, runningTurnId?: string | null): Subagent[][] {
  const teams = new Map<string, Subagent[]>();
  for (const agent of agents) {
    if (agent.parent_id === sid) teams.set(agent.team_id, [...(teams.get(agent.team_id) ?? []), agent]);
  }
  return [...teams.entries()]
    .filter(([id, team]) => id === runningTurnId || team.some((agent) => isLive(agent) || isAsking(agent)))
    .map(([, team]) => team);
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
  const live = running || current.some((agent) => isLive(agent) || isAsking(agent));
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
  // One control call at a time for each child. It reads the team again after the call.
  // The result is the error text, or null when the call worked.
  const control = useCallback(async (child: string, call: () => Promise<unknown>, fallback: string) => {
    setPending(child);
    try {
      await call();
      return null;
    } catch (cause) {
      return cause instanceof Error && cause.message ? cause.message : fallback;
    } finally {
      setPending(null);
      setRevision((value) => value + 1);
    }
  }, []);
  const cancel = useCallback(async (child: string) => {
    setError(null);
    setError(await control(child, () => client.cancelAgent(sid, child), 'Could not stop agent. Try again.'));
  }, [client, sid, control]);
  const timeline = useCallback(
    (child: string) => client.agentTimeline(sid, child),
    [client, sid],
  );
  return { agents: current, pending, error, cancel, timeline };
}

/** What `useAgentTeam` gives its views. */
export type AgentTeam = ReturnType<typeof useAgentTeam>;
